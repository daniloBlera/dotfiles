;;;; The journey into structured editing begins here

;; Specify the file for `customize` and friends
(setq custom-file "~/.config/emacs/custom.el")
(load custom-file)

;; Add Melpa repositories
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Configure the font
(let ((name "UbuntuMono Nerd Font Mono-12"))
  (add-to-list 'default-frame-alist `(font . ,name))
  (set-face-attribute 'default t :font name))

;; Scroll line-by-line instead of in chunks
(setq scroll-step 1
      scroll-conservatively 10000)

;; Hooks
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook #'whitespace-mode)
(add-hook 'prog-mode-hook #'olivetti-mode)
(add-hook 'org-mode-hook #'whitespace-mode)
(add-hook 'org-mode-hook #'olivetti-mode)
(add-hook 'text-mode-hook #'olivetti-mode)
(add-hook 'text-mode-hook #'jinx-mode)
(add-hook 'before-save-hook #'delete-trailing-whitespace)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
(add-hook 'prog-mode-hook #'enable-paredit-mode)

;; Program to run when starting a Lisp environment
(setq inferior-lisp-program "ros -Q run")

;; Backup edited files under a centralized location
(setq backup-directory-alist '(("." . "~/.cache/emacs/backups"))
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2)

;; Keymaps
(keymap-global-set "C-c t o" #'olivetti-mode)
(keymap-global-set "C-x C-b" #'ibuffer)

(keymap-global-set "C-;" #'avy-goto-char)
(keymap-global-set "C-:" #'avy-goto-char-2)
(keymap-global-set "M-o" #'ace-window)

(global-unset-key (kbd "C-z"))  ; disable suspend

;; Re-enable uppercase and lowercase region commands
(put 'upcase-region 'disabled nil)      ; C-x C-u
(put 'downcase-region 'disabled nil)    ; C-x C-l

;; Features to disable
(put 'enriched-mode 'disabled
     "this inserts lots of markup, which can be difficult to remove.")

(put 'enriched-toggle-markup 'disabled
     "maybe avoid `enriched-mode' if you don't know what you're doing.")

;; Save an SVG screenshot of emacs into /tmp
(defun screenshot-svg ()
  "Save a screenshot of the current frame as an SVG image.
Saves to a temp file and puts the filename in the kill ring."
  (interactive)
  (let* ((filename (make-temp-file "emacs" nil ".svg"))
         (data (x-export-frames nil 'svg)))
    (with-temp-file filename
      (insert data))
    (kill-new filename)
    (message filename)))
