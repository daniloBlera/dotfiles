;;;; A relatively clean config your journey into structured editing

;; Skip the startup buffer and open *scratch*
(setq inhibit-startup-message t)

;; Specify the file for `customize`
(setq custom-file "~/.config/emacs/custom.el")
(load custom-file)

;; Add Melpa repositories
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Configure the font
;; some options:
(let ((name "FiraCode Nerd Font Mono-11"))
  (add-to-list 'default-frame-alist `(font . ,name))
  (set-face-attribute 'default t :font name))

;; Scroll line-by-line instead of in chunks
(setq scroll-step 1
      scroll-conservatively 10000)

;; Hooks
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook 'whitespace-mode)
(add-hook 'prog-mode-hook 'olivetti-mode)
(add-hook 'org-mode-hook 'whitespace-mode)
(add-hook 'org-mode-hook 'olivetti-mode)
(add-hook 'text-mode-hook 'olivetti-mode)
(add-hook 'sly-mode-hook (lambda ()
			   (unless (sly-connected-p)
			     (save-excursion (sly)))))

;; Program to run when starting a Lisp environment
(setq inferior-lisp-program "ros -Q run")

;; Backup edited files under a centralized location
(setq backup-directory-alist '(("." . "~/.cache/emacs/backups"))
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2)

;; Customize the chars to display
;; (setq whitespace-style'(face
;; 			tabs tab-mark
;; 			spaces space-mark
;; 			trailing lines-trail
;; 			newline newline-mark))

;; Keymapsa
(keymap-global-set "<XF86Paste>" 'clipboard-yank)
(keymap-global-set "<XF86Copy>" 'clipboard-kill-ring-save)
(keymap-global-set "<XF86Cut>" 'clipboard-kill-region)
(keymap-global-set "C-c t o" 'olivetti-mode)
