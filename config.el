;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name (getenv "DEBFULLNAME")
      user-mail-address (getenv "DEBEMAIL"))

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-molokai)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
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
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.


;; from viccuad
;; FIXME
;; curly and coloured underscores in terminal (Emacs bugs opened: #45230, #45231)
;; treemacs molokai fonts
;; lsp-mode format
;; grammar module

(after! evil-escape
  ;; Allow to escape from the visual state as from insert.
  (delete 'visual evil-escape-excluded-states))

(setq +treemacs-git-mode 'extended) ;; highlight files in addition to folders. Requires python3

;; open flycheck errors as transient bottom
(set-popup-rule! "^\\*Flycheck errors\\*$" :select nil :side 'bottom :size 0.15)

(custom-set-faces!
  ;; fix solaire for molokai:
  `(default :background ,(doom-color 'bg)) ;; bg-alt is taken by markdown code blocks
  `(solaire-default-face :background ,(doom-darken 'base3 0.5)) ;; darker text background
  `(solaire-hl-line-face :background ,(doom-color 'base3)) ;; used for highlighting current window. bg-alt is taken by markdown code blocks
  `(fill-column-indicator :foreground ,(doom-color 'bg)) ;; subtle indicator
  ;; italics:
  `(font-lock-comment-face :slant italic)
  ;;'(font-lock-keyword-face :slant italic)
  )

;; remove character on vertical separators
(defun my-change-window-divider ()
  (let ((display-table (or buffer-display-table standard-display-table)))
    (set-display-table-slot display-table 'vertical-border ? ) ;; put no character after `?`
    (set-window-display-table (selected-window) display-table)))
(add-hook 'window-configuration-change-hook 'my-change-window-divider)

(setq doom-modeline-unicode-fallback t) ;; unicode symbols on modeline, for terminal

;; move checker group, with flycheck errors, to the left
(after! doom-modeline
  (doom-modeline-def-modeline 'main
    '(bar matches checker buffer-info remote-host buffer-position parrot selection-info)
    '(misc-info minor-modes input-method buffer-encoding major-mode process vcs)))

(setq evil-move-cursor-back nil) ;; don't move cursor back when going to normal mode from insert

;; doom disables by default:
;;   lsp-ui-doc-enable
;;   lsp-enable-folding
;;   lsp-enable-text-document-color
;;   lsp-enable-on-type-formatting
;; always enable lsp-ui-doc instead of using K in normal mode
(after! lsp-ui
  (setq lsp-ui-doc-enable t))

(after! magit
   ;; autorefresh magit status buffer after save
   (add-hook 'after-save-hook 'magit-after-save-refresh-status t)
   ;; show all diffs with word-granularity, not only current
   ;; (setq magit-diff-refine-hunk 'all)
   )

;; Change authinfo's default path:
(setq auth-sources
      '((:source "~/.spacemacs.d/secrets/authinfo.gpg"))) ;; FIXME fix path

(setq epg-pinentry-mode 'loopback) ;; avoid system pinentry, use emacs read password

;; Configure flycheck indicators on the left side. We can't use
;; flycheck-set-indication-mode because it acts buffer-locally, not globally.
(setq +vc-gutter-default-style nil) ;; Disable default fringe styling
(setq-default left-margin-width 1) ;; Make sure the margin is visible to begin with
(setq-default flycheck-indication-mode 'left-margin) ;; Move flycheck to left margin

;; Use C-{h,j,k,l} to move between panes, either emacs or tmux
(use-package! tmux-pane
  :config
  (tmux-pane-mode)
  (map! :leader
        (:prefix ("v" . "tmux pane")
         :desc "Open vpane" :nv "o" #'tmux-pane-open-vertical
         :desc "Open hpane" :nv "h" #'tmux-pane-open-horizontal
         :desc "Open hpane" :nv "s" #'tmux-pane-open-horizontal
         :desc "Open vpane" :nv "v" #'tmux-pane-open-vertical
         :desc "Close pane" :nv "c" #'tmux-pane-close
         :desc "Rerun last command" :nv "r" #'tmux-pane-rerun))
  (map! :leader
        (:prefix "t"
         :desc "vpane" :nv "v" #'tmux-pane-toggle-vertical
         :desc "hpane" :nv "h" #'tmux-pane-toggle-horizontal)))
