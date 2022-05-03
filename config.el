;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
(setq user-full-name (getenv "DEBFULLNAME")
      user-mail-address (getenv "DEBEMAIL"))
;; clients, file templates and snippets. It is optional.

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
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-material-dark)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


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


;; from viccuad
;; FIXME
;; curly and coloured underscores in terminal (Emacs bugs opened: #45230, #45231)
;; grammar module

;; enable emacs' 27 fill-column on text mode
(add-hook! (prog-mode conf-mode text-mode) #'display-fill-column-indicator-mode)

(after! evil-escape
  ;; Allow to escape from the visual state as from insert.
  (delete 'visual evil-escape-excluded-states))

;; open flycheck errors as transient bottom
;;(set-popup-rule! "^\\*Flycheck errors\\*$" :select t :side 'bottom :size 0.15)

(custom-set-faces!
  ;; simplify molokai/monokais:
  ;; bg-alt is taken by markdown code blocks
;;  `(default :background ,(doom-darken 'bg-alt 0.3)) ;; bg
;;  `(hl-line :background ,(doom-lighten 'bg-alt 0.1))  ;; base3
;;  `(vertical-border :background ,(doom-darken 'bg-alt 0.3)) ;; same as default
 `(fill-column-indicator :foreground ,(doom-color 'bg-alt)) ;; subtle indicator
;;  `(treemacs-git-modified-face :foreground ,(doom-color 'cyan))
  ;; fix solaire for molokai:
  ;; `(solaire-default-face :background ,(doom-darken 'base3 0.5)) ;; darker text background
  ;; italics:
  `(font-lock-comment-face :slant italic)
  ;;'(font-lock-keyword-face :slant italic)
  )


;; remove character on vertical separators
(defun my-change-window-divider ()
  (let ((display-table (or standard-display-table (make-display-table))))
    (set-display-table-slot display-table 'vertical-border ?│) ;; put no character after `?`
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
;;   lsp-enable-folding
;;   lsp-enable-text-document-color
;;   lsp-enable-on-type-formatting
(setq lsp-ui-doc-enable 'true)
;; (setq lsp-ui-doc-show-with-mouse nil)
(setq lsp-ui-doc-show-with-cursor 'true)
;(setq lsp-lens-enable 'true)
;;;(setq lsp-headerline-breadcrumb-enable 'true)
;(setq lsp-ui-sideline-show-hover 'true)
;(setq lsp-signature-auto-activate 'true) ;; you could manually request them via `lsp-signature-activate`
;(setq lsp-signature-render-documentation nil)
;(setq lsp-completion-show-detail 'true)
;(setq lsp-completion-show-kind 'true)

;; call clippy via rust-analyzer instead of rustic (the latter doesn't work):
(setq lsp-rust-analyzer-cargo-watch-command "clippy")

;; show inlay hints, interacts badly with lsp-ui-sideline:
(setq lsp-rust-analyzer-inlay-hints-mode 'true)
(setq lsp-rust-analyzer-server-display-inlay-hints t)


(after! magit
  ;; autorefresh magit status buffer after save
  (add-hook 'after-save-hook 'magit-after-save-refresh-status t)
  ;; (setq magit-diff-refine-hunk 'all) ;; show all diffs with word-granularity, not only current
  (setq magit-save-repository-buffers 'dontask) ;; save buffers when opening magit. We have git, so this is safe
  )

;; Change authinfo's default path:
(setq auth-sources
      '((:source "~/.doom.d/secrets/authinfo.gpg")))

(setq epg-pinentry-mode 'loopback) ;; avoid system pinentry, use emacs read password

;; Configure flycheck indicators on the left side. We can't use
;; flycheck-set-indication-mode because it acts buffer-locally, not globally:
(setq +vc-gutter-default-style nil) ;; Disable default doom fringe styling. This makes git-gutter show on margin, yet doom doesn't overwrite our flycheck sets
(setq-default left-margin-width 1) ;; Make sure the margin is visible for git-gutter
(setq-default right-margin-width 1) ;; Make sure the margin is visible to begin with
(setq-default flycheck-indication-mode 'left-margin)

;;(after! git-gutter-fringe
;;  global-git-gutter-mode nil)

;;(global-diff-hl-mode)
;;(unless (window-system) (diff-hl-margin-mode))
;;(setq diff-hl-side 'right)

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

(setq +treemacs-git-mode 'extended) ;; highlight files in addition to folders. Requires python3
(after! treemacs
  (setq treemacs-follow-mode t)
  (setq treemacs-no-png-images t) ;; use TUI chars for icons even in GUI
;;  (setq treemacs-width 25)
  (setq doom-themes-treemacs-theme "Default"))

;; (setq auto-save-default t
;;       make-backup-files t)

;; (setq which-key-idle-delay 0.2
;;       which-key-idle-secondary-delay 0.2)

;; dap configuration for rust:
(require 'dap-gdb-lldb) ;; for rust-gdb and dap-mode
(dap-register-debug-template "Rust::GDB Run Configuration"
                             (list :type "gdb"
                                   :request "launch"
                                   :name "GDB::Run"
                                   :gdbpath "rust-gdb"
                                   :target nil
                                   :cwd nil))

;; enable mouse scroll:
(load "mwheel")
(mwheel-install)
(mouse-wheel-mode 1)
;; (setq mouse-wheel-progressive-speed nil)
;; (setq mouse-wheel-scroll-amount
;; 	        '(1 ((shift) . 1)
;; 			            ((control) . nil)))
