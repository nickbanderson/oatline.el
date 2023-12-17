;;; oatline.el --- Print modeline info in echo area and set minimal window borders

;;; Commentary:

;; Read the source; it's short and simple. Customize via variable oatline-contents.
;; Also provides utility functions to ease customization.

;; Seeded by feebleline, but significantly trimmed down.

;;; Code:

(defcustom oatline-contents #'oatline-contents-preset-one
  "Returns a string with contents to show on the oatline"
  :type 'function
  :group 'oatline)

;; Buffer name cannot be anything else lol
(defvar oatline--minibuf " *Minibuf-0*")

(defun oatline-set-defaults-for-rendering-in-terminal ()
  "Sets emacs settings for nil `window-system` (e.g. emacs running in terminal).
   Uses the mode-line itself as a border, setting its content to a horizontal line."
  (setq-default mode-line-format (make-string 500 ?─)) ; 500 width should fill up any bot border
  (set-display-table-slot (make-display-table)
			  'vertical-border 
			  (make-glyph-code ?│)))

(defun oatline-set-defaults-for-rendering-in-gui ()
  "Sets emacs settings for 'x `window-system` (i.e. GUI emacs).
   Uses window-divider-mode, which is GUI-only."
  (setq-default mode-line-format nil
		window-divider-default-bottom-width 1
		window-divider-default-places 'bottom-only)
  (window-divider-mode 1))

(defun oatline-set-defaults ()
  "Sets relevant emacs settings based on `window-system`.
   See oatline-set-defaults-for-*."
  (pcase window-system
    (`nil (oatline-set-defaults-for-rendering-in-terminal))
    (`x (oatline-set-defaults-for-rendering-in-gui))
    (`w32 (oatline-set-defaults-for-rendering-in-gui))
    ;; TODO address other cases I encounter in the wild
    (_ (error "oatline: unmatched 'window-system value"))))

(defun oatline--update-contents ()
  "Insert (oatline-contents) into the oatline minibuffer."
  (unless (current-message)
    (let ((contents (funcall oatline-contents)))
      (with-current-buffer oatline--minibuf
	;; prevent flicker from erase-buffer
	(with-silent-modifications 
	 (erase-buffer)
	 (insert contents))))))

(defvar oatline--msg-timer nil)

(define-minor-mode global-oatline-echo-area-mode
  "Display modeline in the echo area."
  :init-value nil
  :global t
  (if global-oatline-echo-area-mode
      (setq oatline--msg-timer (run-with-timer 0 0.1 'oatline--update-contents))
    (cancel-timer oatline--msg-timer)))

(provide 'oatline)


;;; Utility functions for user to use in their custom 'oatline-contents

(defun oatline-format-left-and-right-aligned (left-contents right-contents)
  "Given strings for left aligned and right aligned text,  return a single
   string that uses the oatline minibuffer width to align them."
  (let* ((echo-area-width (window-width (get-buffer-window oatline--minibuf)))
         (available-width (- echo-area-width 1))
         (left-width (length left-contents))
         (right-width (length right-contents))
         (total-width (+ left-width right-width)))
    (concat left-contents
            (make-string (max 0 (- available-width total-width)) ?\s)
            right-contents)))

;;; Presets for 'oatline-contents

(defun oatline-contents-preset-one ()
  (oatline-format-left-and-right-aligned
   ;; left aligned portion
   (format "%s %s: %s%s%s"
	   (pcase evil-state
	     ('normal (propertize "[N]" 'face '(:foreground "lightgreen")))
	     ('insert (propertize "[I]" 'face '(:foreground "cornflowerblue")))
	     ('visual (propertize "[V]" 'face '(:foreground "violet")))
	     ('motion (propertize "[M]" 'face '(:foreground "cyan")))
	     ;; TODO this never displays
	     ('replace (propertize "[R]" 'face '(:foreground "red")))
	     ('emacs (propertize "[E]" 'face '(:foreground "yellow")))
	     (_ "[_]"))
	   (persp-name (persp-curr))
	   ;; TODO add hostname and full filepath in gray?
	   (buffer-name (current-buffer))
	   ;; TODO make [+] and [R/O] mutually exclusive here once I'm sure that's the case
	   (if (and (buffer-modified-p) (buffer-file-name))
	       (propertize " [+]" 'face '(:foreground "orange"))
	     "")
           (if buffer-read-only
               (propertize " [R/O]" 'face '(:foreground "orange"))
             ""))
   ;; right aligned portion
   (concat
    ;; TODO make this a symbol or shorter or something prettier
    (format "[%s]" (symbol-name major-mode))
    "  "
    (format "%d:%d" (line-number-at-pos) (current-column))
    " "
    (format "(%d%%)" (min 100 (* 100 (/ (float (line-number-at-pos))
					(count-lines (point-min) (point-max))))))
    )))

