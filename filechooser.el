;;; filechooser.el --- An xdg-desktop-portal filechooser -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: rahguzar <rahguzar@zohomail.eu>
;; Maintainer: rahguzar <rahguzar@zohomail.eu>
;; Created: May 20, 2023
;; Version: 0.0.1
;; Keywords: convenience files tools unix
;; Homepage: https://codeberg.org/rahguzar/filechooser
;; Package-Requires: ((emacs "28.1") (compat "29.1"))
;;
;; This file is part of GNU Emacs.
;;
;;; Commentary:
;; An implementation of xdg-desktop-portal filechooser in Emacs. This allows
;; for choosing files in applications like firefox (with GTK_USE_PORTAL set)
;; from an Emacs frame. The default is to use `read-file-name' for choosing
;; a single file and a pair of Dired buffers for choosing multiple files.
;;
;;
;;; Code:
(require 'compat)
(require 'dbus)
(require 'xdg)
(require 'dired)

;;; Variables
;;;; Keymaps
(defvar filechooser-dired-overriding-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'exit-recursive-edit)
    (define-key map (kbd "C-c C-k") #'filechooser-abort )
    (define-key map (kbd "C-f") #'filechooser-toggle-filter)
    (define-key map [remap abort-recursive-edit] #'filechooser-abort)
    map)
  "Keymap used as `override-global-map' for Dired based file selection.")

(defvar filechooser-mininuffer-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-f") #'filechooser-toggle-filter)
    (define-key map [remap abort-recursive-edit] #'filechooser-abort)
    map))

(defvar filechooser-multiple-selection-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-TAB") #'filechooser-multiple-continue)
    (define-key map (kbd "M-RET") #'filechooser-multiple-finalize-current-selection)
    (define-key map (kbd "M-a") #'filechooser-multiple-select-all)
    (make-composed-keymap map filechooser-mininuffer-map)))

;;;; Custom variables
(defgroup filechooser nil
  "An xdg-desktop-portal filechooser."
  :link '(url-link :tag "Homepage" "https://codeberg.org/rahguzar/filechooser")
  :group 'files)

(defcustom filechooser-save-existing-files 'uniquify
  "Determines behavior when attempting to save an existing file FILENAME.
If it is symbol `yes-or-no-p', `yes-or-no-p' is used to confirm if the
file should be overwritten.  If it is the symbol `y-or-n-p', `y-or-n-p'
is used to prompt.  In both cases if a the answer is negative, the file
selection is started again.  If it is the symbol `uniquify', the
FILENAME is made unique by appedning -N to it where N is a positive
number.  If it is a function, it is called with FILENAME and the return
value is used as the filename."
  :type '(choice
          (const :tag "Uniquify" uniquify)
          (const :tag "Prompt Yes/No" yes-or-no-p)
          (const :tag "Prompt Y/N" y-or-n-p)
          (function :tag "Custom Function")))

(defcustom filechooser-use-popup-frame t
  "Whether to popup a new frame for choosing files.
If it is nil the selected frame is used instead."
  :type 'boolean)

(defcustom filechooser-filters `(("Directories" filechooser-file-directory-p . t)
                              ("Elisp files" ,(rx ".el" eos))
                              ("Not dot files" ,(rx bos (not ?.))))
  "An alist of (NAME FILTER . BOOL).
NAME should describe the filter which can either be a regexp
or else a predicate function which takes a filename as argument.
If BOOL is non-nil filter is active by default otherwise it is inactive."
  :type '(alist :key-type (string :tag "Name")
                :value-type
                (cons :tag "Value:"
                      (choice :tag "Filter" regexp function)
                      (boolean :tag "Default"))))

(defcustom filechooser-choose-file #'filechooser-read-file-name
  "Function used to choose a single file.
It should have the same calling calling convention as
`filechooser-read-file-name' which see for expected behavior."
  :type 'function)

(defcustom filechooser-choose-files #'filechooser-read-file-names
  "Function used to choose multiple files.
It should have the same calling as `filechooser-with-dired' which see for
expected behavior."
  :type 'function)

(defcustom filechooser-choose-directory #'filechooser-save-files
  "Function used to choose a directory for saving files in.
It should have the same calling convention as
`filechooser-save-files' which see for expected behavior."
  :type 'function)

(defcustom filechooser-multiple-selection-key "RET"
  "The key that is used to exit minibuffer to do completion.
I.e. the key that binds the equivalent of `exit-minibuffer' for the completion
UI of choice: usually RET."
  :type (if (> emacs-major-version 27) 'key 'key-sequence))

;;;; Others
(defvar filechooser-current-operation nil
  "When filechooser is active, this variable is set to the command being used.")

;;;; Internal Variables
(defvar filechooser--filters nil)
(defvar filechooser--selection nil)
(defvar filechooser--multiple-selection nil)

;;; Filters
(defun filechooser--filters-group-fn (cand transform)
  "Group function for selecting filters.
See Info node `(elisp) Programmed Completion' for CAND and TRANSFORM."
  (if transform
      cand
    (if (cdr (alist-get cand filechooser--filters nil nil #'equal))
        "Active"
      "Inactive")))

(defun filechooser-file-directory-p (name)
  "Return non-nil if NAME is an existing directory."
  (file-directory-p (if (derived-mode-p 'dired-mode)
                        (expand-file-name name (dired-current-directory))
                      name)))

(defun filechooser-toggle-filter (arg)
  "Toggle a filter.
With prefix ARG toggle multiple filters using `completing-read-multiple'."
  (interactive "P")
  (cl-flet ((table (str predicate action)
              (if (eq action 'metadata)
                  `(metadata (group-function . filechooser--filters-group-fn))
                (complete-with-action
                 action filechooser--filters str predicate))))
    (when-let ((names
                (ensure-list
                 (let ((minibuffer-completing-file-name nil))
                   (funcall (if arg #'completing-read-multiple #'completing-read)
                            (if arg "Toggle filters: " "Toggle a filter: ")
                            #'table nil t)))))
      (dolist (name names)
        (cl-callf not (cdr (alist-get name filechooser--filters nil nil #'equal))))
      (if (minibufferp)
          (throw 'continue t)
        (dolist (buf (match-buffers
                      `(derived-mode . dired-mode)
                      (frame-parameter (selected-frame) 'buffer-list)))
          (with-current-buffer buf (jit-lock-refontify)))))))

(defun filechooser--make-filters (opts)
  "Convert globs filters in OPTS to regexps."
  (let ((filters (caar (alist-get "filters" opts nil nil #'equal)))
        (current (caar (alist-get "current_filter" opts nil nil #'equal)))
        (regex-filters)
        (glob-to-regexp (lambda (cell) (if (eq  0 (car cell))
                                      `(regexp ,(wildcard-to-regexp (nth 1 cell)))
                                    ""))))
    (unless (alist-get (car current) filters nil nil #'equal)
      (when current (push current filters)))
    (pcase-dolist (`(,name ,globs) filters)
      (push (list name (mapconcat glob-to-regexp globs "\\|"))
            regex-filters))
    (when (and current (not (caar (alist-get "directory" opts nil nil #'equal))))
      (cl-callf not (cdr (alist-get (car current) regex-filters nil nil #'equal))))
    (nreverse regex-filters)))

(defun filechooser--filters-predicate (filters)
  "Make a predicate out of FILTERS."
  (lambda (name)
    (catch 'match
      (dolist (filter filters)
        (when (cond
               ((stringp filter)
                (string-match filter name))
               ((functionp filter)
                (funcall filter name))
               ((error "Unknown filter %S" filter)))
          (throw 'match t))))))

;;; Utility definitions
(defmacro filechooser--maybe-with-new-frame (minibuffer &rest body)
  "Excute BODY in a new frame if `filechooser-use-popup-frame' is non-nil.
MINIBUFFER is the value of minibuffer frame paramter."
  (declare (indent 1))
  (let ((framevar (make-symbol "frame")))
    `(let ((minibuffer-completing-file-name ,(eq minibuffer 'only)))
      (if filechooser-use-popup-frame
         (let ((,framevar (make-frame '((name . ,(if (eq minibuffer 'only)
                                                     "filechooser-miniframe"
                                                   "filechooser-frame"))
                                        (minibuffer . ,minibuffer)))))
           (unwind-protect
               (with-demoted-errors "%S"
                 (with-selected-frame ,framevar
                   ,@body))
             (delete-frame ,framevar 'force)))
       (with-demoted-errors "%S"
         ,@body)))))

(defun filechooser-abort ()
  "Abort the file selection."
  (interactive)
  (throw 'exit "Error: File selection aborted."))

(defun filechooser--return-value (filenames &rest entries)
  "Construct return value list of FILENAMES and alist ENTRIES."
  `(,(if filenames 0 1)
    ,(if filenames
         `(:array
           (:dict-entry "uris" (:variant (:array ,@(mapcar #'xdg-thumb-uri
                                                           (ensure-list filenames)))))
           ,@(mapcar (lambda (cell) `(:dict-entry ,(car cell) (:variant ,(cdr cell))))
                     entries))
       `(:array :signature "{sv}"))))

;;; Minibuffer based selection
(defun filechooser--multiple-group-function (completion transform)
  "Group function for `filechooser--multiple-loop-table'.
See Info node `(elisp) Programmed Completion' for COMPLETION and TRANSFORM."
  (if-let ((display (rassoc completion filechooser--multiple-selection)))
      (if transform
          (abbreviate-file-name (car display))
        "Selected")
    (if transform completion "Select Multiple")))

(defun filechooser--multiple-loop-table (str pred action)
  "Completion table used for calling `completing-read' in a loop.
See Info node `(elisp) Programmed Completion' for STR, PRED and ACTION."
  (pcase action
    ('t (let ((dir (or (file-name-directory str) default-directory)))
          (cl-callf2 mapcar (lambda (sel) (cons (car sel)
                                           (file-relative-name (car sel) dir)))
                     filechooser--multiple-selection)
          (append (mapcar #'cdr filechooser--multiple-selection)
                  (completion-file-name-table str pred t))))
    ('metadata '(metadata
                 (category . file)
                 (group-function . filechooser--multiple-group-function)))
    (_ (completion-file-name-table str pred action))))

(defun filechooser--read-file-name-1 (prompt &optional mustmatch filters dir default)
  "Read a filename with PROMPT and predicate made from FILTERS.
MUSTMATCH and DIR are as in `read-file-name'.  DEFAULT is the default filename.
If MULTIPLE is non-nil `completing-read-multiple' is used."
  (catch 'continue
    (minibuffer-with-setup-hook
        (lambda () (use-local-map (make-composed-keymap filechooser-mininuffer-map
                                                   (current-local-map)))
          (when dir (setq default-directory dir)))
      (read-file-name
       prompt dir default mustmatch nil
       (when filters (filechooser--filters-predicate filters))))))

(defun filechooser--handle-exisiting-file (filename &optional dir filters)
  "Handle an existing FILENAME according to `filechooser-save-existing-files'.
DIR is the directory to use if a new file name needs to be choosen and FILTERS
are the filters to use in that case."
  (pcase filechooser-save-existing-files
    ('uniquify
     (let ((n 1)
           (name (file-name-sans-extension filename))
           (ext (file-name-extension filename t)))
       (while (file-exists-p (format "%s-%s%s" name n ext))
         (cl-incf n))
       (format "%s-%s%s" name n ext)))
    ((or 'yes-or-no-p 'y-or-n-p)
     (if (funcall filechooser-save-existing-files
                  (format "File %s exists. Overwrite?" filename))
         filename
       (filechooser--read-file-name "Choose a new file name: "
                                    nil filters dir
                                    (file-relative-name filename dir))))
    (_ (funcall filechooser-save-existing-files filename))))

(defun filechooser--read-file-name (prompt &optional mustmatch filters dir default)
  "Read a filename with PROMPT and predicate made from FILTERS.
MUSTMATCH and DIR are as in `read-file-name'.  DEFAULT is the default filename.
If MULTIPLE is non-nil `completing-read-multiple' is used."
  (setq filechooser--filters (cl-delete-duplicates
                              (append filechooser-filters filters)
                              :test #'equal :key #'car))
  (let ((result t))
    (while (eq t result)
      (when (minibufferp nil t)
        (abort-minibuffers))
      (setq result (filechooser--read-file-name-1
                    prompt mustmatch
                    (delq nil (mapcar (lambda (flt) (if (cddr flt) (cadr flt)))
                                      filechooser--filters))
                    dir default)))
    (if (or mustmatch (not (file-exists-p result)))
        result
      (filechooser--handle-exisiting-file result dir filters))))

(defun filechooser-read-file-name (prompt &optional dir filters mustmatch default)
  "Read a file name.
If `filechooser-use-popup-frame' is non-nil a new minibuffer only popup frame
is used, othewise the selected frame is used.
PROMPT is the minibuffer prompt.  MUSTMATCH and DIR are as in `read-file-name'.
FILTERS take the same form as elements of `filechooser-filters'. Only those
files which satisfy one of the active filters from FILTERS or
`filechooser-filters' are presented for completions."
  (filechooser--maybe-with-new-frame only
    (filechooser--read-file-name prompt mustmatch filters dir default)))

(defun filechooser-multiple-continue ()
  "Select current file and exit multiple file selection."
  (interactive)
  (setq this-command 'filechooser-multiple-continue)
  (call-interactively
   (key-binding (kbd filechooser-multiple-selection-key))))

(defun filechooser-multiple-finalize-current-selection ()
  "Exit the file selection with currently selected candidates."
  (interactive)
  (throw 'done nil))

(defun filechooser-multiple-select-all ()
  "Select all of current completion candidates."
  (interactive)
  (setq this-command 'filechooser-multiple-continue)
  (let* ((str (minibuffer-contents))
         (dir (file-name-directory str))
         (cands (completion-all-completions
                 str minibuffer-completion-table nil (length str))))
    (when (cdr (last cands))
      (setf (cdr (last cands)) nil))
    ;; Handle `completion-ignored-extensions'.
    (cl-callf completion-pcm--filename-try-filter cands)
    (setq filechooser--multiple-selection
          (delete-dups
           (mapcar (lambda (cand) (cons (expand-file-name cand dir) t)) cands)))
    (throw 'done dir)))

(defun filechooser--multiple-read-file-name (prompt &optional dir)
  "Read a filename with PROMPT and starting from DIR.
MAP contains additional key bindigs."
  (let ((result t)
        filters)
    (while (eq t result)
      (when (minibufferp nil t)
        (abort-minibuffers))
      (setq filters (delq nil (mapcar
                               (lambda (flt) (if (cddr flt) (cadr flt)))
                               filechooser--filters)))
      (setq result
            (catch 'continue
              (minibuffer-with-setup-hook
                  (lambda () (use-local-map (make-composed-keymap
                                        filechooser-multiple-selection-map
                                        (current-local-map))))
                (completing-read prompt #'filechooser--multiple-loop-table
                                 (filechooser--filters-predicate filters) t
                                 (abbreviate-file-name dir)
                                 'file-name-history)))))
    result))

(defun filechooser-read-file-names (prompt &optional dir filters)
  "Read multiple file names using `completing-read-multiple'.
If `filechooser-use-popup-frame' is non-nil a new minibuffer only popup frame
is used, othewise the selected frame is used.
PROMPT is the minibuffer prompt. DIR is the directory where selection starts.
FILTERS take the same form as elements of `filechooser-filters'. Only those
files which satisfy one of the active filters from FILTERS or
`filechooser-filters' are presented for completions."
  (setq filechooser--filters
        (cl-delete-duplicates
         (append filechooser-filters filters)
         :test #'equal :key #'car))
  (setq dir (file-name-as-directory
             (expand-file-name (or dir default-directory))))
  (let (selected filechooser--multiple-selection)
    (filechooser--maybe-with-new-frame only
      (while (setq dir
                   (catch 'done
                     (setq selected (expand-file-name
                                     (filechooser--multiple-read-file-name
                                      prompt dir)))
                     (cl-callf not
                         (alist-get selected filechooser--multiple-selection
                                    nil t #'equal))
                     (when (eq this-command 'filechooser-multiple-continue)
                       (expand-file-name (file-name-directory selected))))))
      (nreverse (mapcar #'car filechooser--multiple-selection)))))

(defun filechooser-save-files (prompt &optional dir files)
  "Read a directory name to save FILES in it.
If `filechooser-use-popup-frame' is non-nil a new minibuffer only popup frame
is used, othewise the selected frame is used.  PROMPT and DIR are as in
`read-directory-name'."
  (filechooser--maybe-with-new-frame only
    (when-let ((save-dir (read-directory-name prompt dir))
               (names nil))
      (make-directory save-dir t)
      (catch 'abort
        (dolist (file files)
          (push (if (file-exists-p (expand-file-name file save-dir))
                    (or (filechooser--handle-exisiting-file file save-dir)
                        (throw 'abort nil))
                  (expand-file-name file save-dir))
                names))
        names))))

;;; Dired based selection
(let (marked unmarked timer)
  (defun filechooser--adjust-selection-buffer ()
    (when (buffer-live-p (cdr filechooser--selection))
      (with-current-buffer (cdr filechooser--selection)
        (cl-callf cl-set-difference (cdr dired-directory) unmarked :test #'equal)
        (when marked
          (cl-callf nreverse (cdr dired-directory))
          (dolist (file marked)
            (cl-pushnew file (cdr dired-directory) :test #'equal))
          (cl-callf nreverse (cdr dired-directory)))
        (revert-buffer)))
    (setq marked nil unmarked nil timer nil))

  (defun filechooser--process-changed-marks (beg end length)
    "Deal with change in mark from BEG to END with LENGTH."
    (when (and (derived-mode-p 'dired-mode)
               (eq length 0) (eq (1+ beg) end)
               (not (invisible-p (1- (pos-eol)))))
      (save-excursion
        (goto-char beg)
        (when (and (re-search-forward dired-re-mark end t)
                   (eq (preceding-char) dired-marker-char))
          (push (dired-get-filename nil t) marked))
        (when (re-search-forward dired-re-maybe-mark (1+ end) t)
          (push (dired-get-filename nil t) unmarked))
        (unless timer
          (setq timer (run-with-timer
                       0.2 nil 'filechooser--adjust-selection-buffer)))))))

(defun filechooser-dired (&optional dir filters)
  "Select some files using Dired.
Running this command pops a Dired for directory DIR, and enters a recursive
editing session.  FILTERS are in the format of `filechooser-filters'."
  (unless (file-directory-p (car filechooser--selection))
    (setq filechooser--selection (list (make-temp-file "filechooser-selection-" t))))
  (let ((overriding-map `((t . ,filechooser-dired-overriding-map)))
        (apply-filters (lambda (_)
                         (when (and (derived-mode-p 'dired-mode)
                                    (not (eq (current-buffer)
                                             (cdr filechooser--selection)))
                                    (not (memq 'filechooser--dired-jit-filter
                                               jit-lock-functions)))
                           (add-hook 'jit-lock-functions
                                     #'filechooser--dired-jit-filter 95 t)
                           (jit-lock-mode t)
                           (add-to-invisibility-spec 'filechooser-filter))))
        (selection-buffer (progn (setcdr filechooser--selection nil)
                                 (dired-noselect filechooser--selection))))
    (unwind-protect
        (progn (setcdr filechooser--selection
                       (dired-noselect (list (car filechooser--selection))))
               (display-buffer selection-buffer '(display-buffer-in-side-window
                                                  (side . left) (window-width . 0.3)))
               (when-let ((win (get-buffer-window (cdr filechooser--selection))))
                 (select-window win)
                 (redisplay))
               (with-current-buffer (cdr filechooser--selection)
                 (setq mode-line-format " Selected files")
                 (dired-hide-details-mode)
                 (add-hook 'jit-lock-functions #'filechooser--dired-jit-abbreviate 95 t)
                 (jit-lock-mode t))
               (push overriding-map emulation-mode-map-alists)
               (add-hook 'window-buffer-change-functions apply-filters)
               (add-hook 'after-change-functions 'filechooser--process-changed-marks)
               (setq filechooser--filters (append filechooser-filters filters))
               (other-window 1)
               (dired (or dir default-directory))
               (funcall apply-filters nil)
               (unless (recursive-edit)
                 (with-current-buffer (cdr filechooser--selection)
                   (cdr dired-directory))))
      (cl-callf2 delq overriding-map emulation-mode-map-alists)
      (remove-hook 'window-buffer-change-functions apply-filters)
      (remove-hook 'after-change-functions 'filechooser--process-changed-marks)
      (kill-buffer (cdr filechooser--selection))
      (setcdr filechooser--selection nil)
      (dolist (buf (match-buffers `(derived-mode . dired-mode) (frame-parameter nil 'buffer-list)))
        (with-current-buffer buf
          (jit-lock-unregister #'filechooser--dired-jit-filter)
          (remove-from-invisibility-spec 'filechooser-filter))))))

(defun filechooser--dired-jit-filter (beg end)
  "Hide files that don't match current filters from BEG to END."
  (if-let ((active (delq nil (mapcar (lambda (flt) (if (cddr flt) (cadr flt)))
                                     filechooser--filters))))
      (progn
        (setq end (progn (goto-char end) (forward-line 1) (pos-eol)))
        (setq beg (progn (goto-char beg) (forward-line -1) (goto-char (pos-bol))))
        (let ((pred (filechooser--filters-predicate active)))
          (while (< (point) end)
            (when-let ((name (dired-get-filename 'no-dir t)))
              (alter-text-property (1- (point)) (pos-eol) 'invisible
                                   (lambda (val)
                                     (setq val (ensure-list val))
                                     (if (funcall pred name)
                                         (cl-callf2 delq 'filechooser-filter val)
                                       (cl-pushnew 'filechooser-filter val)))))
            (forward-line))))
    (remove-from-invisibility-spec 'filechooser-filter))
  `(jit-lock-bounds ,beg . ,end))

(defun filechooser--dired-jit-abbreviate (beg end)
  "Ellipesize filenames from BEG to END."
  (setq end (progn (goto-char end) (pos-eol)))
  (setq beg (progn (goto-char beg) (goto-char (pos-eol))))
  (while (< (point) end)
    (when-let ((file (dired-get-filename nil t))
               (name (file-name-directory file))
               (name-end (+ (dired-move-to-filename) (length name))))
      (put-text-property (point) name-end 'display ".../")
      (put-text-property (point) name-end 'help-echo file))
    (forward-line))
  `(jit-lock-bounds ,beg . ,end))

(defun filechooser-with-dired (_prompt &optional dir filters)
  "Select some files using Dired.
If `filechooser-use-popup-frame' is non-nil a new frame is used for selection,
otherwise selected frame is used. DIR is the directory for initial Dired
buffer.  FILTERS are used to restrict selection to a subset of files."
  (filechooser--maybe-with-new-frame t (filechooser-dired dir filters)))

;;; Method handlers
(defun filechooser-handle-open-file (_handle _app_id _parent title &rest opts)
  "Handle OpenFile request with prompt TITLE and options OPTS."
  (setq opts (or (plist-get opts :array) (car opts)))
  (let ((filters (filechooser--make-filters opts)))
    (filechooser--return-value
     (if (caar (alist-get "multiple" opts nil nil #'equal))
         (progn
           (setq filechooser-current-operation filechooser-choose-files)
           (funcall filechooser-choose-files (format "%s: " title) nil filters))
       (setq filechooser-current-operation filechooser-choose-file)
       (funcall filechooser-choose-file (format "%s: " title) nil filters t)))))

(defun filechooser-handle-save-file (_handle _app_id _parent title &rest opts)
  "Handle SaveFile request with prompt TITLE and options OPTS."
  (setq opts (or (plist-get opts :array) (car opts)))
  (setq filechooser-current-operation filechooser-choose-file)
  (filechooser--return-value
   (funcall
    filechooser-choose-file
    (format "%s: " title)
    (file-name-as-directory
     (dbus-byte-array-to-string
      (butlast (caar (alist-get "current_folder" opts nil nil #'equal)))))
    (filechooser--make-filters opts) nil
    (caar (alist-get "current_name" opts nil nil #'equal)))))

(defun filechooser-handle-save-files (_handle _app_id _parent title &rest opts)
  "Handle SaveFiles request with prompt TITLE and options OPTS."
  (setq opts (or (plist-get opts :array) (car opts)))
  (setq filechooser-current-operation filechooser-choose-directory)
  (filechooser--return-value
   (funcall
    filechooser-choose-directory
    (format "%s: " title)
    (dbus-byte-array-to-string
     (butlast (caar (alist-get "current_folder" opts nil nil #'equal)))
     (mapcar (lambda (file) (dbus-byte-array-to-string (butlast file)))
             (caar (alist-get "files" opts nil nil #'equal)))))))

;;; Starting Service
;;;###autoload
(defun filechooser-start ()
  "Start the filechooser.

It works by using d-bus and xdg-desktop-portal and implements
`org.freedesktop.impl.portal.FileChooser'"
  (interactive)
  (cl-flet ((register (method handler)
              (dbus-register-method
               :session "org.gnu.Emacs.FileChooser"
               "/org/freedesktop/portal/desktop"
               "org.freedesktop.impl.portal.FileChooser"
               method handler t)))
    (register "OpenFile" 'filechooser-handle-open-file)
    (register "SaveFile" 'filechooser-handle-save-file)
    (register "SaveFiles" 'filechooser-handle-save-files)
    (dbus-register-method :session "org.gnu.Emacs.FileChooser"
                          nil "org.freedesktop.DBus.Properties"
                          "GetAll" (lambda (&rest _args) :ignore) t)
    (dbus-register-service :session "org.gnu.Emacs.FileChooser")))

(provide 'filechooser)
;;; filechooser.el ends here
