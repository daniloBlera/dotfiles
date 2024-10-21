;;;; The journey into structured editing begins here
;;; obs.: run `M-x package-install-selected-packages RET` to install the missing packages
;;; indicated in the `custom.el` file

;; Add Melpa repositories
(use-package package
  :config
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

;; Specify the file for `customize` and friends
(setq custom-file "~/.config/emacs/custom.el")
(load custom-file)

;; Install missing packages on startup -- check `package-selected-packages` in the custom
;; file
(package-install-selected-packages)

;; Backup edited files under a centralized location
(setq backup-directory-alist '(("." . "~/.cache/emacs/backups"))
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2)

;; Configure the font
(let ((name "UbuntuMono Nerd Font Mono-13"))
  (add-to-list 'default-frame-alist `(font . ,name))
  (set-face-attribute 'default t :font name))

;; Scroll line-by-line instead of in chunks
(setq scroll-step 1
      scroll-conservatively 10000)

;; Hooks
(dolist (mode '(rainbow-delimiters-mode whitespace-mode
                olivetti-mode jinx-mode breadcrumb-mode))
  (add-hook 'prog-mode-hook mode))

(dolist (mode '(olivetti-mode jinx-mode))
  (dolist (hook '(text-mode-hook org-mode-hook))
    (add-hook mode hook)))

(dolist (hook '(emacs-lisp-mode-hook lisp-mode-hook))
  (add-hook hook 'enable-paredit-mode))

(add-hook 'before-save-hook #'delete-trailing-whitespace)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
(add-hook 'occur-hook (lambda () (switch-to-buffer-other-window "*Occur*")))

;; Replacing default major modes with their respective tree-sitter versions
(setq major-mode-remap-alist
      '((bash-mode . bash-ts-mode)
        (c-mode . c-ts-mode)
        (json-mode . json-ts-mode)
        (python-mode . python-ts-mode)
        (toml-mode . toml-ts-mode)
        (yaml-mode . yaml-ts-mode)))

;; The path to the common lisp subprocess for Sly to run
(setq inferior-lisp-program "/usr/bin/sbcl")

;; Keymaps
(keymap-global-set "C-c d b" #'fill-region)
(keymap-global-set "C-c d c" #'comment-dwim)
(keymap-global-set "C-c d o" #'occur)
(keymap-global-set "C-c d r" #'repeat)
(keymap-global-set "C-c d s" #'screenshot-svg)
(keymap-global-set "C-c d u" #'vundo)
(keymap-global-set "C-c t j" #'jinx-mode)
(keymap-global-set "C-c t o" #'olivetti-mode)
(keymap-global-set "C-c t r" #'view-mode)
(keymap-global-set "C-x C-b" #'ibuffer)
(keymap-global-set "C-;" #'avy-goto-char)
(keymap-global-set "C-:" #'avy-goto-char-2)
(keymap-global-set "M-o" #'ace-window)
(global-unset-key (kbd "C-z"))  ; disable suspend

;; Re-enable uppercase and lowercase region commands
(put 'upcase-region 'disabled nil)      ; C-x C-u
(put 'downcase-region 'disabled nil)    ; C-x C-l

;; Re-enable narrowing region -- check C-x n {d,n,p,w}
(put 'narrow-to-region 'disabled nil)   ; C-x n n

;; Features to disable
(put 'enriched-mode 'disabled
     "this inserts lots of markup, which can be difficult to remove.")

(put 'enriched-toggle-markup 'disabled
     "maybe avoid `enriched-mode' if you don't know what you're doing.")

;; Save an SVG screenshot of emacs into `/tmp`
(defun screenshot-svg ()
  "Save a screenshot of the current frame as an SVG image.  Saves to a temp file and puts
the filename in the kill ring."
  (interactive)
  (let* ((filename (make-temp-file "emacs" nil ".svg"))
         (data (x-export-frames nil 'svg)))
    (with-temp-file filename
      (insert data))
    (kill-new filename)
    (message filename)))
