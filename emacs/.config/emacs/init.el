;;; -*- lexical-binding: t; -*-
;;; The journey into structured editing begins here
;;;
;;; obs.: The first time running this file should download all the necessary packages and
;;; tree-sitter grammars
;;;
;;; tested under Emacs version 31.0.50

;; Load private configurations that shouldn't be found in vcs
(let ((user-settings (expand-file-name "default.el" user-emacs-directory)))
  (if (file-exists-p user-settings)
      (load-file user-settings)))

;;; Packages
;; add MELPA repository
(use-package package
  :config
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize))

(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?j ?k ?l ?\; ?h)))

(use-package avy
  :ensure t
  :hook (after-init . avy-setup-default)
  :bind (("C-c g c" . avy-goto-char)
         ("C-c g C" . avy-goto-char-2)
         ("C-c g t" . avy-goto-char-timer)
         ("C-c g ;" . avy-goto-line)
         ("C-c g r" . avy-resume)))

(use-package breadcrumb
  :ensure t
  :hook (prog-mode . breadcrumb-mode))

(use-package comment-dwim-2
  :ensure t
  :bind ("M-;" . comment-dwim-2))

(use-package completion-preview
  :ensure t
  :init (global-completion-preview-mode t)
  :config (setq completion-preview-minimum-symbol-length 2)
  :bind (("M-n" . completion-preview-next-candidate)
         ("M-p" . completion-preview-prev-candidate)))

(use-package doric-themes
  :ensure t)

(use-package ef-themes
  :ensure t)

(use-package envrc
  :ensure t
  :hook (after-init . envrc-global-mode))

(use-package erlang-ts
 :ensure t
 :defer t
 :mode ("\\.erl\\'" . erlang-ts-mode))

(use-package ffap
  :hook (after-init . ffap-bindings))

(use-package flycheck
  :ensure t
  :defer t
  :bind (("C-c t c" . flycheck-mode)))

(use-package format-all
  :ensure t
  :defer t
  :config (setq-default format-all-mode-lighter "FMT"
                        format-all-formatters '(("Python" black isort))))

(use-package git-link
  :ensure t
  :bind (("C-c g l" . git-link)))       ; use `C-u C-c g l' to select the remote

(use-package go-mode
  :ensure t
  :defer t)

(use-package haskell-mode
  :ensure t
  :defer t)

(use-package htmlize
  :ensure t
  :defer t)

(use-package imenu
  :hook ((prog-mode org-mode) . imenu-add-menubar-index)
  :config (setq imenu-sort-function 'imenu--sort-by-name))

(use-package indent-bars
  :ensure t
  :config (setq indent-bars-prefer-character t)
  :hook ((python-ts-mode lua-ts-mode json-ts-mode) . indent-bars-mode))

(use-package jinx
  :ensure t
  :hook (emacs-startup . global-jinx-mode)
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages)))

(use-package magit
  :ensure t)

(use-package marginalia
  :ensure t
  :bind (:map minibuffer-local-map ("M-A" . marginalia-cycle))
  :init (marginalia-mode))

(use-package markdown-mode
  :ensure t)

(use-package lua-mode
  :ensure t
  :mode ("\\.lua\\'" . lua-ts-mode)
  :interpreter ("lua" . lua-ts-mode)
  :config (setq-default lua-indent-nested-calls t))

(use-package olivetti
  :ensure t
  :init (setq olivetti-body-width 110
              olivetti-style 'fancy
              olivetti-lighter "")
  :bind (("C-c t o" . olivetti-mode))
  :hook ((org-mode prog-mode text-mode) . olivetti-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package popper
  :ensure t
  :bind (("C-`" . popper-toggle)
         ("M-`" . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :config
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "\\*Output\\*"
          "\\*Async Shell Command\\*"
          "\\*Apropos\\*"
          "\\*Python\\*"
          "\\*sly-mrepl for sbcl\\*"
          "\\*sly-description\\*"
          "\\*sly-scratch\\*"
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package recentf
  :bind (("C-x C-r" . recentf-open-files))
  :config (recentf-mode t))

(use-package replace
  :bind ("C-c o" . occur)
  :hook (occur . (lambda () (switch-to-buffer-other-window "*Occur*"))))

(use-package side-hustle
  :ensure t
  :bind (("M-s l" . side-hustle-toggle)))

(use-package sly
  :ensure t
  :init (setq inferior-lisp-program "sbcl")
  :bind (:map sly-mode-map ("M-h" . sly-documentation-lookup))
  :hook (lisp-mode . sly-editing-mode))

(use-package transpose-frame
  :ensure t
  :bind (("C-c r t" . transpose-frame)
         ("C-c r c" . rotate-frame-clockwise)
         ("C-c r C" . rotate-frame-anticlockwise)
         ("C-c r -" . flip-frame)
         ("C-c r /" . flop-frame)))

(use-package unfill
  :ensure t
  ;; replace `fill-paragraph', normally under `M-q', with `unfill-toggle'
  :bind ([remap fill-paragraph] . unfill-toggle))

(use-package unicode-fonts
  :ensure t
  :defer t
  :config (unicode-fonts-setup))

(use-package vertico
  :ensure t
  :init (vertico-mode)
  :bind (:map vertico-map ("TAB" . minibuffer-complete)))

(use-package vlf
  :ensure t)

(use-package vundo
  :ensure t
  :bind ("C-z" . vundo))

;;; General configuration
;; specify the file for `customize` and friends
(setq custom-file "~/.config/emacs/custom.el")
(load custom-file)

;; backup edited files under a centralized location
(setq backup-directory-alist '(("." . "~/.cache/emacs/backups"))
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2)

;; setting the default font
(add-to-list 'default-frame-alist '(font . "0xProto Nerd Font Mono-11"))

;; helpers to check if the current time is inside a sunrise-sunset window -- p.s.: try circadian.el
(require 'solar)

(defun my/sunrise-sunset-hours ()
  "Get the sunrise and sunset hours, if configured."
  (if (and calendar-latitude calendar-longitude)
      (cl-destructuring-bind ((sunrise-hour _) (sunset-hour _) _)
          (solar-sunrise-sunset (calendar-current-date))
        (list sunrise-hour sunset-hour))
    (list 5 17)))

(defun my/daylight-p ()
  "Check if the current time is between sunrise and sunset."
  (cl-destructuring-bind (_ current-minutes current-hour &rest params) (decode-time)
    (cl-destructuring-bind (sunrise-hour sunset-hour) (my/sunrise-sunset-hours)
      (< (* sunrise-hour 60)
         (+ (* current-hour 60) current-minutes)
         (* sunset-hour 60)))))

(defun my/set-theme-type ()
  "Set a light or a dark theme according to the current time."
  (interactive)
  (load-theme (if (my/daylight-p) 'ef-summer 'ef-winter)))

;; and setting the theme according to the time
(my/set-theme-type)

;;; Hooks
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;;; Tree-Sitter configuration
;; mapping major modes to their tree-sitter variants
(setq major-mode-remap-alist
      '((bash-mode . bash-ts-mode)
        (c-mode . c-ts-mode)
        (erlang-mode . erlang-ts-mode)
        (go-mode . go-ts-mode)
        (html-mode . html-ts-mode)
        (js-json-mode . json-ts-mode)
        (lua-mode . lua-ts-mode)
        (python-mode . python-ts-mode)
        (toml-mode . toml-ts-mode)
        (yaml-mode . yaml-ts-mode)))

;; defining the grammar sources
(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (c "https://github.com/tree-sitter/tree-sitter-c")
        (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
        (erlang "https://github.com/WhatsApp/tree-sitter-erlang")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (lua "https://github.com/tree-sitter-grammars/tree-sitter-lua")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (toml "https://github.com/tree-sitter-grammars/tree-sitter-toml")
        (yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml")))

;; download missing grammars
(dolist (grammar treesit-language-source-alist)
  (let ((language (car grammar)))
    (unless (treesit-language-available-p language)
      (treesit-install-language-grammar language))))

;; open files under specific directories as read-only
(add-hook 'find-file-hook
          (lambda ()
            (dolist (pattern '("~/.config/glirc/logs/.*" "~/.config/glirc/logs-acer/.*"))
              (if (string-match (expand-file-name pattern) buffer-file-name)
                  (read-only-mode)))))

;; org stuff
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)))

;; screenshots of Emacs
(defun my/screenshot-svg ()
  "Save a screenshot of the current frame as an SVG image.

Saves to a temp file and puts the filename in the kill ring."
  (interactive)
  (let* ((filename (make-temp-file "emacs" nil ".svg"))
         (data (x-export-frames nil 'svg)))
    (with-temp-file filename
      (insert data))
    (kill-new filename)
    (message filename)))

;;; Global key bindings
(global-set-key (kbd "C-c e") #'eval-buffer)
(global-set-key (kbd "C-c s") #'my/screenshot-svg)
(global-set-key (kbd "C-c t v") #'view-mode)
(global-set-key (kbd "C-c t b") #'menu-bar-mode)
(global-set-key (kbd "C-x C-b") #'ibuffer)
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)
(global-set-key (kbd "C-c f s") #'isearch-forward)
(global-set-key (kbd "C-c f S") #'isearch-backward)
(global-set-key (kbd "C-c f r") #'isearch-forward-regexp)
(global-set-key (kbd "C-c f R") #'isearch-backward-regexp)
(global-set-key (kbd "C-c f w") #'isearch-forward-word)
(global-set-key (kbd "C-h a") #'apropos)

;; re-enable uppercase and lowercase region commands
(put 'upcase-region 'disabled nil)      ; C-x C-u
(put 'downcase-region 'disabled nil)    ; C-x C-l

;; re-enable narrowing region -- check C-x n [dnpw]
(put 'narrow-to-region 'disabled nil)   ; C-x n n

;;; Features to disable
(put 'enriched-mode 'disabled
     "this inserts lots of markup, which can be difficult to remove.")

(put 'enriched-toggle-markup 'disabled
     "maybe avoid `enriched-mode' if you don't know what you're doing.")
