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
(let ((name "UbuntuMono Nerd Font-13"))
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

;; Keymaps
(keymap-global-set "C-c t o" 'olivetti-mode)

;; Disable suspend
(global-unset-key (kbd "C-z"))

;; Re-enable uppercase/losercase region `C-x C-u' and `C-x C-l', respectively
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
