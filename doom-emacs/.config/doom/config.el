;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
(setq doom-font (font-spec :family "FiraCode Nerd Font Mono" :size 15 :weight 'regular)
      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))

;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-one)
;; (setq doom-theme 'doom-molokai)
;; (setq doom-theme 'doom-moonlight)
;; (setq doom-theme 'doom-old-hope)
;; (setq doom-theme 'doom-outrun-electric)
(setq doom-theme 'doom-shades-of-purple)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; Enable line numbering by default
(global-display-line-numbers-mode)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/org/")

(setq scroll-margin 10) ; Min lines above/below the cursor
(setq hscroll-margin 5) ; Min columns to the left/right of the cursor

;; Emacs version of vim's list-mode
(setq whitespace-style '(face
                         tabs tab-mark
                         spaces space-mark
                         trailing lines-trail
                         newline newline-mark))

;; Display special chars (list-mode)
(global-whitespace-mode)

;; Display fill column margin indicator
(global-display-fill-column-indicator-mode)

;; Configure fill-column and olivetti-mode body width
(setq-default fill-column 90)
(setq-default olivetti-body-width 100)

;; Enable placing the cursor past EOL and disable VIM's move-cursor-left after
;; transitioning from INSERT to NORMAL mode.
(setq evil-move-cursor-back nil)
(setq evil-move-beyond-eol t)

;; Center screen after jumps
(map! :n "G" (cmd! (evil-goto-line) (evil-scroll-line-to-center nil)))
(map! :n "n" (cmd! (evil-ex-search-next) (evil-scroll-line-to-center nil)))
(map! :n "N" (cmd! (evil-ex-search-previous) (evil-scroll-line-to-center nil)))

;; Toggle on/off text centering with olivetti-mode
(map! :leader
      (:prefix ("t" . "Toggle")
       :desc "Olivetti mode" "o"
       #'olivetti-mode))

;; Enable deletion by system's trash
(setq delete-by-moving-to-trash t)

;; Configuring Sly to use the REPL environment from roswell
(setq inferior-lisp-program "ros -Q run")

;; Set the dashboard's splash image, if it exists
(let ((filepath "~/Pictures/emacs.png"))
  (and (file-exists-p filepath)
       (setq fancy-splash-image filepath)))

;; Setting the minimap's minimum width and location
(after! minimap
  (setq minimap-minimum-width 20)
  (setq minimap-window-location 'left))

;; As instructed by
;; https://github.com/doomemacs/doomemacs/blob/develop/modules/lang/python/README.org
(setq +python-ipython-repl-args '("-i" "--simple-prompt" "--no-color-info"))
(setq +python-jupyter-repl-args '("--simple-prompt"))

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Enable transposing and rotating frames
(require 'transpose-frame)

;; Automatically start tree-sitter
(use-package! tree-sitter
  :hook (prog-mode . turn-on-tree-sitter-mode)
  :hook (tree-sitter-after-on . tree-sitter-hl-mode)
  :config (require 'tree-sitter-langs))
