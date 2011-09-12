;;; sn.el -- Source Navigator interface for Emacs

;;; Copyright (C) 1997 Cygnus Solutions

;;; Known problems and things to do:
;;; * SN tags workalike should work by making phony tags file and
;;; using tags-table-format-hooks.  Then we'd get all the tags
;;; functionality for free.
;;; * Should stick a function on find-file-hook that asks each SN
;;; session if the new file is part of the session.


;;; Constants.

;; non-nil if using XEmacs.
(defconst sn-is-xemacs (string-match "XEmacs" emacs-version))

;;; Variable definitions.

;; History list for tags finding.
(defvar sn-history-list nil)

;; This holds the connection to SN.  It is local to each buffer; this
;; lets us have multiple SN projects share an Emacs.
(defvar sn-process nil)
(make-variable-buffer-local 'sn-process)

;; Name of the current process.  This is only set when running a
;; function from a process filter.  It is only defvar'd because I
;; don't like to use variables that aren't declared.
(defvar sn-current-process nil)

(defvar sn-minor-mode nil "t if source navigator mode is active")
(make-variable-buffer-local 'sn-minor-mode)
(or (assoc 'sn-minor-mode minor-mode-alist)
    (setq minor-mode-alist (cons '(sn-minor-mode " SN") minor-mode-alist)))
(setplist 'sn-minor-mode (plist-put (symbol-plist 'sn-minor-mode)
				    'permanent-local t))

(defun sn-minor-mode (arg)
  "Minor mode for working with Source Navigator.
Adds some commands for looking up stuff in SN:
\\{sn-keymap}
This mode is automatically activated when files are opened by SN and cannot
be activated for other buffers.  You can toggle it for SN-related buffers
though.  This lets you access the command bindings that this mode overrides."
  (interactive "P")
  (unless sn-process
    (error "This buffer has no Source Navigator connection"))
  (setq sn-minor-mode (if (null arg) (not sn-minor-mode)
			(> (prefix-numeric-value arg) 0))))

;; When we tell SN about a file, we must always send it exactly the
;; same name as it sent us.  So we stash the original filename here.
(defvar sn-file-name nil)
(make-variable-buffer-local 'sn-file-name)

(defvar sn-keymap nil
  "Keymap for Source Navigator minor mode.")
(unless sn-keymap
  (setq sn-keymap (make-sparse-keymap))
  (define-key sn-keymap "\M-." 'sn-find-tag)
  (define-key sn-keymap "\C-x4." 'sn-tag-unimplemented)
  (define-key sn-keymap "\C-x5." 'sn-tag-unimplemented)
  (define-key sn-keymap "\M-," 'sn-tag-unimplemented)
  (define-key sn-keymap "\M-\t" 'sn-tag-unimplemented)
  (define-key sn-keymap "\C-c.c" 'sn-classbrowser)
  (define-key sn-keymap "\C-c.h" 'sn-classtree)
  (define-key sn-keymap "\C-c.r" 'sn-retrieve)
  (define-key sn-keymap "\C-c.x" 'sn-xref)
  (define-key sn-keymap "\C-c.i" 'sn-retrieve-implementation)
  (cond (sn-is-xemacs
	 (define-key sn-keymap '(meta control ?.) 'sn-tag-unimplemented))
	;; GNU Emacs.
	(t (define-key sn-keymap [\M-\C-.] 'sn-tag-unimplemented))))

(or (assoc 'sn-minor-mode minor-mode-map-alist)
    (setq minor-mode-map-alist (cons (cons 'sn-minor-mode sn-keymap)
				     minor-mode-map-alist)))

;;;
;;; Commands that the user can run to interact with SN.
;;;

;; Hide the current project.
(defun sn-hide-project ()
  "Hide the Source Navigator project associated with this buffer."
  (interactive)
  (sn-send "tkbHideShow withdraw"))

;; Like find-tag, but use SN to look up the tag.
(defun sn-find-tag (tagname)
  "Like find-tag, but use Source Navigator to look up name."
  (interactive
   (progn
     (require 'etags)
     (list (read-string "Find tag: "
			(find-tag-default)
			'sn-history-list))))
  (sn-send (concat "sn_emacs_display_object "
		   (sn-tcl-quote tagname)))
  ;; We know a response is coming.  This makes things look a little
  ;; more synchronous.
  (accept-process-output))

(defun sn-classbrowser (class)
  "Browse the contents of a class in the Source Navigator."
  (interactive
   (progn
     (require 'etags)
     (list (read-string "Browse class: "
			(find-tag-default)
			'sn-history-list))))
  (sn-send (concat "sn_classbrowser " (sn-tcl-quote class))))

(defun sn-classtree (class)
  "Browse a class in the Source Navigator hierarchy browser."
  (interactive
   (progn
     (require 'etags)
     (list (read-string "Browse class: "
			(find-tag-default)
			'sn-history-list))))
  (sn-send (concat "sn_classtree " (sn-tcl-quote class))))

(defun sn-retrieve (pattern)
  "Tell Source Navigator to retrieve all symbols matching pattern.
If there is only one match SN will take Emacs there.  If there are
several they are listed in a pop-up where you can select one to edit."
  (interactive
   (progn
     (require 'etags)
     (list (read-string "Retrieve pattern: "
                        (find-tag-default)
                        'sn-history-list))))
  (sn-send (concat "sn_retrieve_symbol " (sn-tcl-quote pattern) " all")))

;; Similar to retrieve, but prefer implementation to definition.
(defun sn-retrieve-implementation (pattern)
  "Like find-tag, but use Source Navigator to look up name."
  (interactive
   (progn
     (require 'etags)
     (list (read-string "Find implementation: "
			(find-tag-default)
			'sn-history-list))))
  (sn-send (concat "sn_retrieve_symbol " (sn-tcl-quote pattern) " {fu mi}")))

(defun sn-xref (symbol)
  "Look up a symbol in the Source Navigator cross-referencer."
  (interactive
   (progn
     (require 'etags)
     (list (read-string "Xref symbol: "
			(find-tag-default)
			'sn-history-list))))
  (sn-send (concat "sn_xref both " (sn-tcl-quote symbol))))

(defun sn-tag-unimplemented ()
  "Bound to tags-finding keys that Source Navigator can't (yet) handle."
  (interactive)
  (error "this keybinding is unimplemented in Source Navigator"))

;; find-tag-other-frame and find-tag-other-window versions are harder
;; to do; there is a synchronization problem here.
;; (defun sn-find-tag-other-frame)
;;(defun sn-find-tag-other-window)
;; (defun sn-find-tag-regexp) ; FIXME do it?
;; FIXME what about tags-query-replace, tags-loop-continue,
;; tags-search, tags-table-files, find-tag-hook, find-tag-noselect?

;; Turn off menus for now.  Why bother when there is only one item?
;    (progn
;      (define-key sn-keymap [menu-bar SN] (cons "SN" (make-sparse-keymap)))
;      (define-key sn-keymap [menu-bar SN hide] '("Hide project" 
;						 . sn-hide-project)))
;    )

;;;
;;; Internal functions that can talk to SN.
;;;

;; Connect to Source Navigator.  Arguments are:
;; * TMPFILENAME - a temp file containing some lisp code; remove it
;; here.  This can be nil, meaning no file exists.
;; * HOSTNAME - name of host to connect to
;; * DIRECTORY - directory where temp file might be (if not absolute)
;; * PORT - port to connect to
(defun sn-startup (tmpfilename hostname directory port)
  (save-excursion
    (let ((buffer (generate-new-buffer " sn")))
      (set-buffer buffer)
      (setq sn-process (open-network-stream "sn" buffer hostname port))
      (process-kill-without-query sn-process nil)
      (set-process-filter sn-process 'sn-filter)
      (set-process-sentinel sn-process 'sn-sentinel)
      (and tmpfilename
	   (delete-file (expand-file-name tmpfilename directory))))))

;; This quoting is sufficient to protect eg a filename from any sort
;; of expansion or splitting.  Tcl quoting sure sucks.
(defun sn-tcl-quote (string)
  (mapconcat (function (lambda (char)
			 (if (memq char '(?[ ?] ?{ ?} ?\\ ?\" ?$ ?  ?\;))
			     (concat "\\" (char-to-string char))
			   (char-to-string char))))
	     string ""))

;; Send a command to SN.
(defun sn-send (string)
  (process-send-string sn-process (concat string "\n")))

;; This is run on a hook after a file is saved.  If we have to, we
;; notify the appropriate SN.
(defun sn-after-save ()
  (if sn-minor-mode
      (sn-send (concat "sn_parse_uptodate " (sn-tcl-quote sn-file-name)
                       " 0"))))         ; Disable annoying popup.

;; This is the process filter for reading from SN.  It just tries to
;; read the process buffer as a lisp object; when the read succeeds,
;; the result is evalled.
(defun sn-filter (proc string)
  ;; Only do the work if the process buffer is alive.
  (if (buffer-name (process-buffer proc))
      (let ((inhibit-quit t)
	    (sn-current-process proc)
	    form form-list)
	(save-match-data
	  (save-excursion
	    (set-buffer (process-buffer proc))
	    ;; If process marker not already set, we must set it.
	    ;; This seems to contradict the docs; go figure.
	    (or (marker-position (process-mark proc))
		(set-marker (process-mark proc) (point-min)))
	    (goto-char (process-mark proc))
	    (insert string)
	    (set-marker (process-mark proc) (point))
	    (goto-char (point-min))
	    ;; Note that we only catch end-of-file.  invalid-read-syntax
	    ;; we let through; that indicates an SN bug that we really
	    ;; want to see.
	    (while (progn
		     (setq form (condition-case nil
				    (read (current-buffer))
				  (end-of-file nil)))
		     form)
	      ;; Remove the stuff we've read.
	      (delete-region (point-min) (point))
	      (setq form-list (cons form form-list)))))
	;; Now go through each form on our list and eval it.  We do
	;; this outside the save-excursion because we want the
	;; expression to be able to move point around.  We also turn
	;; C-g back on.
	(nreverse form-list)
	(setq inhibit-quit nil)
	(while form-list
	  (eval (car form-list))
	  (setq form-list (cdr form-list))))))

;; This is run when the SN connection dies.  We go through each buffer
;; and do some cleaning up.  We also remove our own process buffer.
(defun sn-sentinel (process event)
  (save-excursion
    (let ((b-list (buffer-list)))
      (while b-list
	(set-buffer (car b-list))
	(if (eq sn-process process)
	    (progn
	      ;; This buffer belongs to the current invocation.  Close
	      ;; down.
	      (setq sn-process nil)
	      (setq sn-minor-mode nil)))
	(setq b-list (cdr b-list)))))
  (kill-buffer (process-buffer process)))

;;;
;;; Functions that are run by SN.  These functions can assume that
;;; sn-current-process is set, if they like.
;;;

;; Sent by SN when we should visit a file.
;; Arguments are:
;; * DIRECTORY    - base directory of project
;; * PARTIAL-FILE - possibly-relative filename
;; * LINE, COLUMN - where cursor should end up
;; * STATE        - either "normal" or "disabled"; the latter means read-only
(defun sn-visit (directory partial-file line column state)
  (let* ((file (expand-file-name partial-file directory))
	 (obuf (get-file-buffer file)))
    (cond (obuf (switch-to-buffer obuf)
		(push-mark))
	  (t (set-buffer (if (string= state "disabled")
			     (find-file-read-only file)
			   (find-file file))))))
  (setq sn-process sn-current-process)
  (goto-line line)
  (forward-char column)
  (setq sn-minor-mode t)
  (setq sn-file-name partial-file)
  (add-hook 'after-save-hook 'sn-after-save nil t))

;; This command is sent by SN when a buffer we have should be put into
;; SN mode.  It actually sends a list of (possibly relative) filenames
;; and the project's root directory.
(defun sn-mark-for-project (directory file-list)
  (save-excursion
    (let (buffer
	  file)
      (while file-list
	(setq file (expand-file-name (car file-list) directory))
	(setq buffer (get-file-buffer file))
	(if buffer
	    (progn
	      (set-buffer buffer)
	      (if (not sn-minor-mode)
		  (progn
		    (setq sn-minor-mode t)
		    (setq sn-process sn-current-process)))))
	(setq file-list (cdr file-list))))))

(provide 'sn)

;;; sn.el ends here
