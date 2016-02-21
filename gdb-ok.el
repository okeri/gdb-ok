;; gdb-ok.el is a config for gdb-mi debugging using custom
;; layout. Main goal is correct support pretty printing and always-on
;; speedbar for exploring variables

;; Copyright (C) 2014-2016 Oleg Keri
;; Homepage: http://www.github.com/okeri/gdb-ok

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require 'gdb-mi)
(require 'speedbar)

;; variables. feel free to edit
(defvar gdb-notasm t "Layout switch (source/asm)")
(defvar gdb-switch2 0 "Layout switch (breakpoints/io/memory)")
(defvar gdb-insource-vars t
  "Set to t if you want show variables values under cursor")
(defvar gdb-speedbar-def-simple-keys t "t if define simple keys")

(setq gdb-many-windows t)
(setq gdb-stack-buffer-addresses t)
(setq gdb-stack-buffer-locations nil)
(setq gdb-non-stop-setting nil)
(setq gdb-memory-address "$esp")
(setq gdb-create-source-file-list nil)

;; do not edit below
(defconst gdb-formats '("natural" "hexadecimal" "binary" "octal"))
(defvar gdb-idle-timer nil)
(defvar gdb-io-window nil)
(defvar gdb-regs-window nil)


;; speedbar bindings
(when gdb-speedbar-def-simple-keys
  (add-hook 'speedbar-reconfigure-keymaps-hook
	    (lambda()
	      (local-set-key [?d] (lambda() (interactive)
				    (gdb-var-delete) (speedbar-update-contents)))
	      (local-set-key [?h] 'gdb-switch-format)
	      (local-set-key [down] 'speedbar-next)
	      (local-set-key [up] 'speedbar-prev )
	      (local-set-key [right] 'speedbar-expand-line)
	      (local-set-key [left] 'speedbar-toggle-line-expansion)))
  (add-hook 'gdb-breakpoints-mode-hook
	    (lambda() (local-set-key [?d] 'gdb-delete-breakpoint))))

(when gdb-insource-vars
  (add-hook 'gdb-mode-hook
	    (lambda() (setq gdb-idle-timer (run-with-idle-timer
					    1 t #'gdb-idle-handler)))))

(defun gdb-quit()
  "Quit gdb session and reset layout"
  (interactive)
  (gdb-input "-gdb-exit" 'ignore)
  (gdb-reset)
  (delete-process (get-buffer-process gud-comint-buffer))
  (kill-buffer gud-comint-buffer)
  (with-current-buffer speedbar-buffer
    (fundamental-mode))
  (kill-buffer speedbar-buffer)
  (setq speedbar-buffer nil)
  (delete-other-windows)
  (read-only-mode nil)
  (if (timerp gdb-idle-timer)
      (cancel-timer gdb-idle-timer))
  (setq gdb-idle-timer nil))


(defun gdb-update-switch2()
  (cond
   ((eq gdb-switch2 0)
    (gdb-set-window-buffer (gdb-get-buffer-create
			    'gdb-breakpoints-buffer) t gdb-io-window)
    (let ((mem (get-buffer (gdb-memory-buffer-name))))
      (when (buffer-live-p mem)
	(kill-buffer mem))))
   ((eq gdb-switch2 1)
    (gdb-set-window-buffer (gdb-get-buffer-create 'gdb-inferior-io)
			   t gdb-io-window))
   ((eq gdb-switch2 2)
    (gdb-set-window-buffer (gdb-get-buffer-create
			    'gdb-memory-buffer) t gdb-io-window))))
(defun gdb-switch2()
  (interactive)
  (setq gdb-switch2 (1+ gdb-switch2))
  (when (> gdb-switch2 2)
    (setq gdb-switch2 0))
  (gdb-update-switch2))

(defun gdb-switch()
  (interactive)
  (if gdb-notasm
      (let ((speedbar-window (get-buffer-window speedbar-buffer)))
	(setq gdb-regs-window (split-window speedbar-window (/ ( * (window-width speedbar-window) 2) 3) 'right))
	(gdb-set-window-buffer (gdb-get-buffer-create 'gdb-registers-buffer)  t gdb-regs-window)
	(set-window-buffer gdb-source-window
			   (gdb-get-buffer-create 'gdb-disassembly-buffer))
	(setq gdb-notasm nil)
	(message "asm enabled"))
    (when gud-last-frame
      (setq gud-last-last-frame gud-last-frame))
    (if gud-last-last-frame
	(let ((active_file (gud-find-file (car gud-last-last-frame))))
	  (if active_file
	      (progn
		(delete-window gdb-regs-window)
		(set-window-buffer gdb-source-window active_file)

		(kill-buffer (get-buffer (gdb-registers-buffer-name)))
		(kill-buffer (get-buffer (gdb-disassembly-buffer-name)))

		(setq gdb-notasm t)
		(gud-display-frame)
		(gdb-place-breakpoints)
		(speedbar-update-contents)
		(message "asm disabled"))
	    (message "Cannot find source file for this location")))
      (message "Cannot find bounds of this location"))))

;; custom
(defun gdb-go(step reverse)
  (let ((command
	 (concat "-exec-"
		 (if step "step" "next")
		 (if (not gdb-notasm)
		     "-instruction" "")
		 (if reverse
		     " --reverse" ""))))
    (gdb-gud-context-call command "%p" 1 t)))

;; custom
(defun gdb-var-clear()
  (interactive)
  (dolist (varchild gdb-var-list)
    (let ((root (car varchild)))
      (if (not (string-match "\\." root))
	  (gdb-input (concat "-var-delete " root) 'ignore))))
  (setq gdb-var-list '())
  (speedbar-update-contents))

;; custom
(defun gdb-toggle-break()
  "Add/toggle breakpoint"
  (interactive)
  (if (not (equal gdb-source-window (selected-window)))
      (select-window gdb-source-window))
  (save-restriction
    (widen)
    (save-excursion
      (beginning-of-line)
      (let* ((line (number-to-string (1+ (count-lines 1 (point)))))
	     (addr (if gdb-notasm (buffer-name) (current-word)))
	     (location (concat addr ":" line))
	     (cmd))
	(dolist (bp gdb-breakpoints-list)
	  (when (or (and (equal line (bindat-get-field bp 'line))
			 (string-suffix-p addr (bindat-get-field bp 'file)))
		    (equal addr (bindat-get-field bp 'addr))
		    (equal location (bindat-get-field bp 'original-location)))
	    (unless cmd
	      (setq cmd (if (equal "y"
				   (bindat-get-field bp 'enabled))
			    "-break-disable " "-break-enable ")))
	    (gud-basic-call
	     (concat cmd (bindat-get-field bp 'number)))))
	(unless cmd
	  (gud-call (concat "break " (if gdb-notasm location (concat "*" addr)))))))))

(defun gdb-edit-value (_text _token _indent)
  "Assign a value to a variable displayed in the speedbar."
  (let* ((var (nth (- (count-lines (point-min) (point)) 2) gdb-var-list))
	 (varnum (car var))
	 (value (read-string "New value: ")))
    (gdb-input (concat "-var-assign " varnum " " value)
	       `(lambda () (gdb-edit-value-handler ,varnum, "")))))

(defun gdb-edit-value-handler (value format)
  (let* ((res (gdb-json-partial-output))
	 (err-msg (bindat-get-field res 'msg))
	 (nval (bindat-get-field res 'value)))
    (if err-msg (message (replace-regexp-in-string
			  "\-var\-assign\: " "" err-msg))
      (let ((changed (catch 'found
		       (dolist (v gdb-var-list)
			 (if (string-equal (car v) value)
			     (throw 'found v)))
		       nil)))
	(when changed
	  (if (not (equal format "binary"))
	      (setcar (nthcdr 4 changed) nval)
	    (setcar (nthcdr 4 changed) (concat "0b" nval)))
	  (speedbar-update-contents))))))

(defun gdb-show-format-handler (varnum)
  (let* ((output (gdb-json-partial-output))
	 (format (bindat-get-field output 'format))
	 (pos (cl-position format gdb-formats :test #'equal)))
    (if pos
	(setq pos (1+ pos))
      (setq pos 1))
    (while (= pos (length gdb-formats))
      (setq pos 0))

    (let ((newfmt (nth pos gdb-formats)))
      (dframe-message "Format changed to %s" newfmt)
      (gdb-input (concat "-var-set-format " varnum " " newfmt)
		 `(lambda () (gdb-edit-value-handler, varnum, newfmt))))))

(defun gdb-switch-format()
  (interactive)
  (let* ((var (nth (- (count-lines (point-min) (point)) 2) gdb-var-list))
	 (varnum (car var)))
    (gdb-input (concat "-var-show-format " varnum)
	       `(lambda () (gdb-show-format-handler, varnum)))))

(defun gdb-idle-handler()
  (when (equal gdb-source-window (selected-window))
    (let ((sym (thing-at-point 'symbol)))
      (when sym
	(gdb-input (concat "-var-create temp_var * " sym)
		   `(lambda () (gdb-var-temp-handler, sym)))))))

(defun gdb-var-temp-handler(expr)
  (let* ((result (gdb-json-partial-output)))
    (unless (bindat-get-field result 'msg)
      (let ((type (bindat-get-field result 'type)))
	(put-text-property
	 0 (length expr) 'face font-lock-variable-name-face expr)
	(put-text-property
	 0 (length type) 'face font-lock-type-face type)
	(dframe-message "%s %s = %s" type expr
			(bindat-get-field result 'value))
	(gdb-input "-var-delete temp_var" 'ignore)))))


;;custom function
(defun vars-handler ()
  (let ((locals-list (bindat-get-field
		      (gdb-json-partial-output) 'variables)))
    (dolist (var locals-list)
      (let ((name (bindat-get-field var 'name)))
	(when (catch 'found
		(dolist (v gdb-var-list)
		  (while (string-equal (nth 1 v) name)
		    (if (string-equal (nth 7 v ) gdb-selected-frame)
			(throw 'found nil)
		      (progn
			(gdb-var-delete-1 v (car v))
			(throw 'found t)))))
		t)
	  (gdb-input (concat "-var-create - * " name)
		     `(lambda () (gdb-var-create-handler ,name, t)))))))
  (gdb-input "-var-update --all-values *"
	     'gdb-var-update-handler 'gdb-var-update))

(eval-after-load "gdb-mi"
  '(progn
     (defun gdb-var-create-handler (expr &optional noupd)
       (let* ((result (gdb-json-partial-output)))
	 (if (not (bindat-get-field result 'msg))
	     (let ((var
		    (list (bindat-get-field result 'name)
			  expr
			  (bindat-get-field result 'numchild)
			  (bindat-get-field result 'type)
			  (bindat-get-field result 'value)
			  nil
			  (bindat-get-field result 'has_more)
			  gdb-selected-frame)))
	       (push var gdb-var-list)
	       (unless (string-equal
			speedbar-initial-expansion-list-name "GUD")
		 (speedbar-change-initial-expansion-list "GUD"))
	       (unless noupd
		 (speedbar-update-contents)))
	   (dframe-message "No symbol \"%s\" in current context." expr))))


     (defun gud-speedbar-item-info ()
       (let ((var (nth (- (line-number-at-pos (point)) 2) gdb-var-list)))
	 (when (nth 3 var)
	   (dframe-message  "%s [%s]" (nth 4 var) (nth 3 var)))))


     ;; remove buggy update
     (defun gdb-var-list-children (varnum)
       (gdb-input (concat "-var-list-children --all-values " varnum)
		  `(lambda () (gdb-var-list-children-handler ,varnum))))

     ;; implement speedbar-update
     (defun gdb-speedbar-update ()
       (when (and (boundp 'speedbar-frame) (frame-live-p speedbar-frame))
	 (speedbar-update-contents)))

     ;; remove raise-buffer for us, we dont need it
     (defun gdb-inferior-filter (proc string)
       (with-current-buffer (gdb-get-buffer-create 'gdb-inferior-io)
	 (comint-output-filter proc string)))

     (defun gdb-var-update ()
       (gdb-input "-stack-list-variables --skip-unavailable --no-values"
		  'vars-handler))

     (defun gud-find-file (file)
       ;; Don't get confused by double slashes in the name that comes from GDB.
       (while (string-match "//+" file)
	 (setq file (replace-match "/" t t file)))
       (let ((minor-mode gud-minor-mode)
	     (buf (funcall (or gud-find-file 'gud-file-name) file)))
	 (when (stringp buf)
	   (setq buf (and (file-readable-p buf) (find-file-noselect buf 'nowarn))))
	 (when buf
	   ;; Copy `gud-minor-mode' to the found buffer to turn on the menu.
	   (with-current-buffer buf
	     (setq-local gud-minor-mode minor-mode)
	     (if (boundp 'tool-bar-map)      ; not --without-x
		 (setq-local tool-bar-map gud-tool-bar-map))
	     (when (and gud-tooltip-mode
			(eq gud-minor-mode 'gdbmi))
	       (make-local-variable 'gdb-define-alist)
	       (unless  gdb-define-alist (gdb-create-define-alist))
	       (add-hook 'after-save-hook 'gdb-create-define-alist nil t))
	     (read-only-mode t)
	     (make-local-variable 'gud-keep-buffer))
	   buf)))

     ;; window layout
     (defun gdb-setup-windows ()
       (set-window-dedicated-p (selected-window) nil)
       (switch-to-buffer gud-comint-buffer)
       (delete-other-windows)
       (let ((win0 (selected-window))
	     (win1 (split-window nil ( / ( * (window-height) 3) 4)))
	     (win2 (split-window-right))
	     (win3 (split-window nil ( / (window-height) 3))))
	 (set-window-buffer win3
			    (if gdb-notasm
				(if gud-last-last-frame
				    (gud-find-file (car gud-last-last-frame))
				  (if gdb-main-file
				      (gud-find-file gdb-main-file)
				    (list-buffers-noselect)))
			      (gdb-get-buffer-create 'gdb-disassembly-buffer)))
	 (select-window win1)
	 (gdb-set-window-buffer (gdb-get-buffer-create 'gdb-stack-buffer))
	 (let ((win4 (split-window-right)))
	   (select-window win4)
	   (setq gdb-io-window  win4))
	 (gdb-update-switch2)
	 (select-window win2)
	 (setq speedbar-buffer (get-buffer-create "GUD")
	       speedbar-frame (selected-frame)
	       gdb-source-window win3)
	 (set-buffer speedbar-buffer)
	 (speedbar-mode)
	 (speedbar-change-initial-expansion-list "GUD")
	 (gdb-set-window-buffer speedbar-buffer t win2)
	 (unless gdb-notasm
	   (setq gdb-regs-window
		 (split-window win2 (/ ( * (window-width win2) 2) 3) 'right))
	   (gdb-set-window-buffer (gdb-get-buffer-create 'gdb-registers-buffer)  t gdb-regs-window))
	 (buffer-disable-undo speedbar-buffer)
	 (select-window win0)
	 (set-window-dedicated-p win0 t)))

     ;; TODO: fix it
     (defun gud-gdb-complete-command (command arg2 arg3))

     (defun gdb-read-memory-custom ()
       (let* ((res (gdb-json-partial-output))
	      (err-msg (bindat-get-field res 'msg)))
	 (if (not err-msg)
	     (let ((memory (bindat-get-field res 'memory)))
	       (setq gdb-memory-next-page (bindat-get-field res 'next-page))
	       (setq gdb-memory-prev-page (bindat-get-field res 'prev-page))
	       (setq gdb-memory-last-address gdb-memory-address)
	       (dolist (row memory)
		 (insert (concat (bindat-get-field row 'addr) ":"))
		 (dolist (column (bindat-get-field row 'data))
		   (insert (gdb-pad-string column
					   (+ 2 (gdb-memory-column-width
						 gdb-memory-unit
						 gdb-memory-format)))))
		 (newline)))
	   ;; Show last page instead of empty buffer when out of bounds
	   (progn
	     (let ((gdb-memory-address gdb-memory-last-address))
	       (gdb-invalidate-memory 'update)
	       (error err-msg))))))

     ;; Fixed annoying bug with set-window-point
     (defun gud-display-frame ()
       (interactive)
       (when gud-last-frame
	 (gud-set-buffer)
	 (when gdb-notasm
	   (gud-display-line (car gud-last-frame) (cdr gud-last-frame))
	   (unless (pos-visible-in-window-p gud-overlay-arrow-position
					       gdb-source-window)
	     (set-window-start gdb-source-window gud-overlay-arrow-position))
	   (setq gud-last-last-frame gud-last-frame
		 gud-last-frame nil))))

     ;; changed condition for string-equal scope true
     (defun gdb-var-update-handler ()
       (let ((changelist (bindat-get-field (gdb-json-partial-output) 'changelist)))
	 (dolist (var gdb-var-list)
	   (setcar (nthcdr 5 var) nil))
	 (let ((temp-var-list gdb-var-list))
	   (dolist (change changelist)
	     (let* ((varnum (bindat-get-field change 'name))
		    (value (bindat-get-field change 'value))
		    (var (assoc varnum gdb-var-list))
		    (new-num (bindat-get-field change 'new_num_children)))
	       (when var
		 (let ((scope (bindat-get-field change 'in_scope))
		       (has-more (bindat-get-field change 'has_more)))
		   (cond ((string-equal scope "false")
			  (if gdb-delete-out-of-scope
			      (gdb-var-delete-1 var varnum)
			    (setcar (nthcdr 5 var) 'out-of-scope)))
			 ((string-equal scope "true")
			  (setcar (nthcdr 6 var) has-more)
			  (unless (equal (nth 4 var) value)
			    (setcar (nthcdr 4 var) value)
			    (setcar (nthcdr 5 var) 'changed)))
			 ((string-equal scope "invalid")
			  (gdb-var-delete-1 var varnum)))))
	       (let ((var-list nil) var1
		     (children (bindat-get-field change 'new_children)))
		 (when new-num
		   (setq var1 (pop temp-var-list))
		   (while var1
		     (if (string-equal varnum (car var1))
			 (let ((new (string-to-number new-num))
			       (previous (string-to-number (nth 2 var1))))
			   (setcar (nthcdr 2 var1) new-num)
			   (push var1 var-list)
			   (cond
			    ((> new previous)
			     ;; Add new children to list.
			     (dotimes (_ previous)
			       (push (pop temp-var-list) var-list))
			     (dolist (child children)
			       (let ((varchild
				      (list (bindat-get-field child 'name)
					    (bindat-get-field child 'exp)
					    (bindat-get-field child 'numchild)
					    (bindat-get-field child 'type)
					    (bindat-get-field child 'value)
					    'changed
					    (bindat-get-field child 'has_more))))
				 (push varchild var-list))))
			    ;; Remove deleted children from list.
			    ((< new previous)
			     (dotimes (_ new)
			       (push (pop temp-var-list) var-list))
			     (dotimes (_ (- previous new))
			       (pop temp-var-list)))))
		       (push var1 var-list))
		     (setq var1 (pop temp-var-list)))
		   (setq gdb-var-list (nreverse var-list))))))))
       (gdb-speedbar-update))

     ;; added workarounds for stl pretty
     (defun gdb-var-list-children-handler (varnum)
       (let* ((var-list nil)
	      (output (bindat-get-field (gdb-json-partial-output "child")))
	      (children (bindat-get-field output 'children)))
	 (catch 'child-already-watched
	   (dolist (var gdb-var-list)
	     (if (string-equal varnum (car var))
		 (progn
		   ;; With dynamic varobjs numchild may have increased.
		   (setcar (nthcdr 2 var) (bindat-get-field output 'numchild))
		   (push var var-list)
		   (dolist (child children)
		     (let ((_type (bindat-get-field child 'type))
			   (_value (bindat-get-field child 'value)))
		       (let ((varchild (list (bindat-get-field child 'name)
					     (bindat-get-field child 'exp)
					     (bindat-get-field child 'numchild)
					     _type
					     _value
					     nil
					     (if (or (string= _value "0x0")
						     (string= _type "std::string")
						     (string= _type
							      "std::basic_string<char, std::char_traits<char>, std::allocator<char> >")
						     ) "0"
					       (if (string= (bindat-get-field child 'dynamic) "1") "1"
						 (bindat-get-field child 'has_more))))))

			 (if (assoc (car varchild) gdb-var-list)
			     (throw 'child-already-watched nil))
			 (push varchild var-list)))))
	       (push var var-list)))
	   (setq gdb-var-list (nreverse var-list))))
       (gdb-speedbar-update))

     ;; added "mute" parameter to find-file-noselect
     (defun gdb-get-location (bptno line flag)
       (goto-char (point-min))
       (catch 'file-not-found
	 (if (re-search-forward gdb-source-file-regexp nil t)
	     (delete (cons bptno "File not found") gdb-location-alist)
	   ;; FIXME: Why/how do we use (match-string 1) when the search failed?
	   (push (cons bptno (match-string 1)) gdb-location-alist)
	   (gdb-resync)
	   (unless (assoc bptno gdb-location-alist)
	     (push (cons bptno "File not found") gdb-location-alist)
	     (message-box "Cannot find source file for breakpoint location.
Add directory to search path for source files using the GDB command, dir."))
	   (throw 'file-not-found nil))
	 (with-current-buffer (find-file-noselect (match-string 1) t)
	   (read-only-mode)
	   (gdb-init-buffer)
	   ;; only want one breakpoint icon at each location
	   (gdb-put-breakpoint-icon (eq flag ?y) bptno (string-to-number line)))))
     ;; fixed string-match regexp
     (defun gdb-speedbar-expand-node (text token indent)
       (cond ((string-match "+" text)        ;expand this node
	      (let* ((var (assoc token gdb-var-list))
		     (expr (nth 1 var)) (children (nth 2 var)))
		(if (or (<= (string-to-number children) gdb-max-children)
			(y-or-n-p
			 (format "%s has %s children. Continue? " expr children)))
		    (gdb-var-list-children token))))
	     ((string-match "-" text)	;contract this node
	      (dolist (var gdb-var-list)
		(if (string-match (concat (regexp-quote token) "\\.") (car var))
		    (setq gdb-var-list (delq var gdb-var-list))))
	      (gdb-var-delete-children token)
	      (speedbar-change-expand-button-char ?+)
	      (speedbar-delete-subblock indent))
	     (t (error "Ooops...  not sure what to do")))
       (speedbar-center-buffer-smartly))

     (defun gdb-var-delete-1 (var varnum)
       (gdb-input (concat "-var-delete " varnum) 'ignore)
       (setq gdb-var-list (delq var gdb-var-list))
       (dolist (varchild gdb-var-list)
	 (if (string-match (concat (regexp-quote (car var)) "\\.")
			   (car varchild))
	     (setq gdb-var-list (delq varchild gdb-var-list)))))

     ;;fixed regexp string-match
     (defun gud-speedbar-buttons (buffer)
       (when (and gud-comint-buffer
		  ;; gud-comint-buffer might be killed
		  (buffer-name gud-comint-buffer))
	 (let* ((minor-mode (with-current-buffer buffer gud-minor-mode))
		(window (get-buffer-window (current-buffer) 0))
		(start (window-start window))
		(p (window-point window)))
	   (cond
	    ((eq minor-mode 'gdbmi)
	     (erase-buffer)
	     (insert "Watch Expressions:\n")
	     (let ((var-list gdb-var-list) parent)
	       (while var-list
		 (let* (char (depth 0) (start 0) (var (car var-list))
			     (varnum (car var)) (expr (nth 1 var))
			     (type (if (nth 3 var) (nth 3 var) " "))
			     (value (nth 4 var)) (status (nth 5 var))
			     (has-more (nth 6 var)))
		   (put-text-property
		    0 (length expr) 'face font-lock-variable-name-face expr)
		   (put-text-property
		    0 (length type) 'face font-lock-type-face type)
		   (while (string-match "\\." varnum start)
		     (setq depth (1+ depth)
			   start (1+ (match-beginning 0))))
		   (if (eq depth 0) (setq parent nil))
		   (if (and (or (not has-more) (string-equal has-more "0"))
			    (or (equal (nth 2 var) "0")
				(and (equal (nth 2 var) "1")
				     (string-match "char \\*$" type)) ))
		       (speedbar-make-tag-line
			'bracket ?? nil nil
			(concat expr "\t" value)
			(if (or parent (eq status 'out-of-scope))
			    nil 'gdb-edit-value)
			nil
			(if gdb-show-changed-values
			    (or parent (pcase status
					 (`changed 'font-lock-warning-face)
					 (`out-of-scope 'shadow)
					 (_ t)))
			  t)
			depth)
		     (if (eq status 'out-of-scope) (setq parent 'shadow))
		     (if (and (nth 1 var-list)
			      (string-match (concat (regexp-quote varnum) "\\.")
					    (car (nth 1 var-list))))
			 (setq char ?-)
		       (setq char ?+))
		     (if (string-match "\\*$\\|\\*&$" type)
			 (speedbar-make-tag-line
			  'bracket char
			  'gdb-speedbar-expand-node varnum
			  (concat expr "\t" type "\t" value)
			  (if (or parent (eq status 'out-of-scope))
			      nil 'gdb-edit-value)
			  nil
			  (if gdb-show-changed-values
			      (or parent (pcase status
					   (`changed 'font-lock-warning-face)
					   (`out-of-scope 'shadow)
					   (_ t)))
			    t)
			  depth)
		       (speedbar-make-tag-line
			'bracket char
			'gdb-speedbar-expand-node varnum
			(concat expr "\t" type)
			nil nil
			(if (and (or parent status) gdb-show-changed-values)
			    (if (eq status 'out-of-scope) 'shadow
			      'font-lock-warning-face) t)
			depth))))
		 (setq var-list (cdr var-list)))))
	    (t (unless (and (save-excursion
			      (goto-char (point-min))
			      (looking-at "Current Stack:"))
			    (equal gud-last-last-frame gud-last-speedbar-stackframe))
		 (let ((gud-frame-list
			(cond ((eq minor-mode 'gdb)
			       (gud-gdb-get-stackframe buffer))
			      ;; Add more debuggers here!
			      (t (speedbar-remove-localized-speedbar-support buffer)
				 nil))))
		   (erase-buffer)
		   (if (not gud-frame-list)
		       (insert "No Stack frames\n")
		     (insert "Current Stack:\n"))
		   (dolist (frame gud-frame-list)
		     (insert (nth 1 frame) ":\n")
		     (if (= (length frame) 2)
			 (progn
			   (speedbar-insert-button (car frame)
						   'speedbar-directory-face
						   nil nil nil t))
		       (speedbar-insert-button
			(car frame)
			'speedbar-file-face
			'speedbar-highlight-face
			(cond ((memq minor-mode '(gdbmi gdb))
			       'gud-gdb-goto-stackframe)
			      (t (error "Should never be here")))
			frame t))))
		 (setq gud-last-speedbar-stackframe gud-last-last-frame))))
	   (set-window-start window start)
	   (set-window-point window p))))))
