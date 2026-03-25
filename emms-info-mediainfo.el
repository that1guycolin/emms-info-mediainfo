;;; emms-info-mediainfo.el --- EMMS info backend using mediainfo  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Loeffler, Colin

;; Author: Colin Loeffler (that1guycolin)
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (emms "0"))
;; Keywords: multimedia, emms, mediainfo
;; URL: https://github.com/that1guycolin/emms-info-mediainfo

;;; Commentary:

;; This package provides an EMMS info backend that uses the `mediainfo'
;; command-line tool to populate track metadata.
;;
;; `mediainfo' supports a very wide range of audio and video formats and
;; is often more reliable than ffprobe for certain container types (e.g.
;; Matroska, MP4, OGG).  The two backends complement each other well.
;;
;; Setup (manual):
;;
;;   (require 'emms-info-mediainfo)
;;   (add-to-list 'emms-info-functions #'emms-info-mediainfo)
;;
;; Setup (use-package):
;;
;;   (use-package emms-info-mediainfo
;;     :after emms
;;     :config
;;     (add-to-list 'emms-info-functions #'emms-info-mediainfo))
;;
;; You can use this alongside other backends.  Each backend in
;; `emms-info-functions' is called in turn; fields already populated by
;; an earlier backend are NOT overwritten by later ones, so ordering
;; determines priority.
;;
;; To make mediainfo the primary backend, replace the line beginning
;; '(add-to-list...' with:
;;
;;   (setq emms-info-functions '(emms-info-mediainfo))
;;
;; Requirements:
;;   - Emacs 27.1 or later  (for `json-parse-string')
;;   - The `mediainfo' CLI tool installed and on PATH
;;     (Arch: pacman -S mediainfo, Debian: apt install mediainfo)

;;; Code:

(require 'emms-info)

;;;; ──────────────────────────────────────────────────────────────────
;;;; Customization

(defgroup emms-info-mediainfo nil
  "EMMS info backend using the mediainfo command-line tool."
  :group 'emms-info
  :prefix "emms-info-mediainfo-")

(defcustom emms-info-mediainfo-program-name "mediainfo"
  "Name or full path of the mediainfo executable."
  :type 'string
  :group 'emms-info-mediainfo)

(defcustom emms-info-mediainfo-ignored-extensions
  '("cda" "mid" "midi" "mod" "xm" "it" "s3m")
  "List of file extensions (without dots, lowercase) that this backend skips.
These are formats that mediainfo handles poorly or not at all."
  :type '(repeat string)
  :group 'emms-info-mediainfo)

;;;; ──────────────────────────────────────────────────────────────────
;;;; Internal constants

;; When json-parse-string converts a JSON object to an alist, each key
;; string is passed through `intern', turning it into an Emacs symbol.
;; "@type" is a legal JSON key but not a typical Lisp symbol name, so we
;; intern it explicitly here rather than relying on the reader to handle
;; the @ character correctly in source code.
(defconst emms-info-mediainfo--type-key (intern "@type")
  "Symbol for the mediainfo JSON '@type' track discriminator key.")

;; This alist maps mediainfo General-track field names (as symbols, exactly
;; as `json-parse-string' would produce them) to the corresponding EMMS
;; track-info property symbols.  All of these are simple string fields;
;; Duration and Track_Position get special handling below.
(defconst emms-info-mediainfo--field-map
  '((Title     . info-title)
    (Performer . info-artist)
    (Album     . info-album)
    (Genre     . info-genre)
    (Composer  . info-composer)
    (Lyricist  . info-lyricist)
    (Comment   . info-note))
  "Alist of (MEDIAINFO-KEY . EMMS-INFO-KEY) for plain string fields.")

;;;; ──────────────────────────────────────────────────────────────────
;;;; Helpers

(defun emms-info-mediainfo--program-available-p ()
  "Return non-nil if the mediainfo executable can be found."
  ;; `executable-find' searches PATH and returns the full path, or nil.
  (executable-find emms-info-mediainfo-program-name))

(defun emms-info-mediainfo--run (filename)
  "Run mediainfo on FILENAME and return a parsed JSON alist, or nil on error.

\='mediainfo\ --Ouput=JSON\=' emits a single JSON document on stdout.
We capture that into a temp buffer, then hand the string to
`json-parse-string'.

  :object-type 'alist  → JSON objects become association lists
  :array-type  'list   → JSON arrays become regular lists
  :null-object  nil    → JSON null  → Lisp nil
  :false-object nil    → JSON false → Lisp nil

On any failure (non-zero exit, JSON parse error, program missing) we
signal a message and return nil so the caller can bail out gracefully."
  (condition-case err
      (with-temp-buffer
        (let ((exit-code
               (call-process emms-info-mediainfo-program-name
                             nil   ; no stdin
                             t     ; stdout → current buffer
                             nil   ; don't redisplay
                             "--Output=JSON"
                             (expand-file-name filename))))
          (if (zerop exit-code)
              (json-parse-string
               (buffer-string)
               :object-type 'alist
               :array-type  'list
               :null-object  nil
               :false-object nil)
            (message "emms-info-mediainfo: mediainfo exited %d for %s"
                     exit-code (file-name-nondirectory filename))
            nil)))
    (error
     (message "emms-info-mediainfo: error running mediainfo: %s" (error-message-string err))
     nil)))

(defun emms-info-mediainfo--general-track (parsed)
  "Extract the General-type track alist from PARSED mediainfo JSON.

The top-level JSON structure is:
  { \"media\": { \"track\": [ { \"@type\": \"General\", ... },
                              { \"@type\": \"Audio\",   ... },
                              ... ] } }

We walk to `media.track', then use `seq-find' to pick the first
element whose @type is \"General\".  That element contains the
file-level metadata we care about."
  (when-let* ((media  (alist-get 'media parsed))
              (tracks (alist-get 'track media)))
    (seq-find
     (lambda (tr)
       (equal (alist-get emms-info-mediainfo--type-key tr) "General"))
     tracks)))

(defun emms-info-mediainfo--extract-year (date-str)
  "Return a 4-digit year string from DATE-STR, or nil.

mediainfo's Recorded_Date can take many forms:
  \"2020\"
  \"2020-07-15\"
  \"2020-07-15T12:00:00+00:00\"
  \"UTC 2020-07-15 12:00:00\"

We grab the first run of exactly four digits that looks like a year."
  (when (and date-str (not (string-empty-p date-str)))
    (when (string-match (rx (group (repeat 4 digit))) date-str)
      (match-string 1 date-str))))

(defun emms-info-mediainfo--extract-tracknumber (general)
  "Return the track number string from GENERAL alist, or nil.

mediainfo may expose this as Track_Position (\"3\") or as Track in
the form \"3/12\" (track / total).  We try Track_Position first;
if absent, we fall back to Track and strip the \"/total\" part."
  (or
   ;; Preferred: explicit position field, already just the number.
   (let ((pos (alist-get 'Track_Position general)))
     (and pos (not (string-empty-p pos)) pos))
   ;; Fallback: "Track" field, strip any "/total" suffix.
   (let ((trk (alist-get 'Track general)))
     (and trk
          (not (string-empty-p trk))
          (car (split-string trk "/"))))))

(defun emms-info-mediainfo--parse-duration (duration-str)
  "Convert DURATION-STR (a float in seconds) to a rounded integer, or nil.

EMMS stores `info-playing-time' as an integer number of seconds.
mediainfo reports Duration as a decimal string like \"245.324\"."
  (when (and duration-str (not (string-empty-p duration-str)))
    (let ((secs (string-to-number duration-str)))
      (when (> secs 0)
        (round secs)))))

(defun emms-info-mediainfo--set-fields (track general)
  "Populate EMMS TRACK with metadata from mediainfo GENERAL alist.

We iterate over `emms-info-mediainfo--field-map' for the simple
string fields, then handle the three fields that need special
treatment: Recorded_Date (year extraction), Track_Position/Track
\(number parsing), and Duration (float → integer seconds).

We only call `emms-track-set' for non-nil, non-empty values so
we never clobber existing data with empty strings."
  ;; ── Plain string fields ──────────────────────────────────────────
  (dolist (mapping emms-info-mediainfo--field-map)
    (let* ((mkey  (car mapping))
           (ekey  (cdr mapping))
           (value (alist-get mkey general)))
      (when (and value (stringp value) (not (string-empty-p value)))
        ;; Only set if the track doesn't already have a value for this
        ;; key (respects earlier backends in emms-info-functions).
        (unless (emms-track-get track ekey)
          (emms-track-set track ekey value)))))

  ;; ── Year (extracted from Recorded_Date) ─────────────────────────
  (unless (emms-track-get track 'info-year)
    (when-let* ((raw  (alist-get 'Recorded_Date general))
                (year (emms-info-mediainfo--extract-year raw)))
      (emms-track-set track 'info-year year)))

  ;; ── Track number ────────────────────────────────────────────────
  (unless (emms-track-get track 'info-tracknumber)
    (when-let ((num (emms-info-mediainfo--extract-tracknumber general)))
      (emms-track-set track 'info-tracknumber num)))

  ;; ── Duration ────────────────────────────────────────────────────
  (unless (emms-track-get track 'info-playing-time)
    (when-let* ((raw (alist-get 'Duration general))
                (sec (emms-info-mediainfo--parse-duration raw)))
      (emms-track-set track 'info-playing-time sec))))

;;;; ──────────────────────────────────────────────────────────────────
;;;; Public entry point

;;;###autoload
(defun emms-info-mediainfo (track)
  "Populate info fields on EMMS TRACK using the mediainfo CLI.

This is the function to add to `emms-info-functions'.

We skip the track (returning nil) if:
  - it is not a plain file (e.g. it is a URL or a stream),
  - its extension appears in `emms-info-mediainfo-ignored-extensions',
  - the mediainfo executable is not available, or
  - mediainfo fails to parse the file.

Otherwise we run mediainfo, extract the General track from the JSON
output, and set whatever info fields are present."
  (when (eq (emms-track-type track) 'file)
    (let* ((filename  (emms-track-name track))
           (extension (downcase (or (file-name-extension filename) ""))))
      (unless (member extension emms-info-mediainfo-ignored-extensions)
        (unless (emms-info-mediainfo--program-available-p)
          (error "emms-info-mediainfo: `%s' not found; install mediainfo or set `emms-info-mediainfo-program-name'"
                 emms-info-mediainfo-program-name))
        (when-let* ((parsed  (emms-info-mediainfo--run filename))
                    (general (emms-info-mediainfo--general-track parsed)))
          (emms-info-mediainfo--set-fields track general))))))

(provide 'emms-info-mediainfo)
;;; emms-info-mediainfo.el ends here
