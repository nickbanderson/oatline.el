;;; oatline.el --- Print modeline info in echo area and set minimal window borders

;;; Commentary:

;; Read the source; it's short and simple. Customize via variable oatline-contents.

;; Seeded by feebleline. 

;;; Code:

(defcustom oatline-contents
  (lambda ()
    "Default string with contents to show on the oatline"
    "lok'tar ogar")
  "Returns a string with contents to show on the oatline"
  :type 'function
  :group 'oatline)

;; Buffer name cannot be anything else lol
(defvar oatline--minibuf " *Minibuf-0*")

(defun oatline-set-defaults-for-rendering-in-terminal ()
  "Sets emacs settings for nil `window-system` (e.g. emacs running in terminal).
   Uses the mode-line itself as a border, setting its content to a horizontal line."
  (setq-default mode-line-format (make-string 500 ?─)) ; 500 width should fill up any bot border
  (set-display-table-slot standard-display-table
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
    ;; TODO address other cases I encounter in the wild
    (_ (error "oatline: unmatched 'window-system value"))))

(defun oatline--update-contents ()
  "Insert (oatline-contents) into the oatline minibuffer."
  ;; TODO what does this coditional do lol
  (unless (current-message)
    (with-current-buffer oatline--minibuf
      (erase-buffer)
      (insert (funcall oatline-contents)))))

(setq oatline--msg-timer
      (run-with-timer 0 0.1 'oatline--update-contents))

(provide 'oatline)
