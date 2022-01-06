;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Tuomo Virolainen"
      user-mail-address "tuomo.virolainen@siili.com")

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
;; (setq doom-font "SF Mono-13")
;; (setq doom-font "SF Mono-13"
;;      doom-variable-pitch-font "Input Mono-12")
(setq doom-font (font-spec :family "SF Mono" :size 14 :weight 'regular)
      doom-variable-pitch-font (font-spec :family "Fira Code" :size 13 :weight 'regular))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-gruvbox)

(add-to-list 'exec-path "/Users/tuomo.virolainen/bin")

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/org/")

(plist-put org-calc-default-modes 'calc-float-format '(fix 2))

(setq mac-option-modifier nil
      mac-command-modifier 'meta
      select-enable-clipboard t)

;; Web mode fontification seems to slow down insertions unbearably, so it's
;; disabled for now.
(setq web-mode-skip-fontification t)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

(setq org-roam-directory "~/Dropbox/org/roam")

(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

(with-eval-after-load 'evil-maps
  (define-key evil-normal-state-map (kbd "C-u") #'evil-scroll-up)
  (define-key evil-normal-state-map (kbd "C-h") #'evil-window-left)
  (define-key evil-normal-state-map (kbd "C-j") #'evil-window-down)
  (define-key evil-normal-state-map (kbd "C-k") #'evil-window-up)
  (define-key evil-normal-state-map (kbd "C-l") #'evil-window-right)
  (define-key evil-normal-state-map (kbd "C-ö") #'split-window-below)
  (define-key evil-normal-state-map (kbd "Ö") 'xref-find-definitions)
  (define-key evil-normal-state-map (kbd "ä") #'delete-other-windows)
  (define-key evil-normal-state-map (kbd "C-ä") #'split-window-right)
  (define-key evil-normal-state-map (kbd "ö") #'save-buffer)
  (define-key evil-normal-state-map (kbd "Ä") #'projectile-ag)
  (define-key evil-normal-state-map (kbd "¨") #'evil-search-forward)
  (define-key evil-normal-state-map (kbd "Q") #'kill-buffer-and-window))

(defun setup-input-decode-map ()
  (define-key input-decode-map (kbd "C-h") (kbd "C-x o"))
  (define-key input-decode-map (kbd "C-p") #'projectile-find-file)
  (define-key input-decode-map (kbd "C-l") (kbd "C-x o"))
  (define-key input-decode-map (kbd "C-b") (kbd "C-x b"))
  (define-key input-decode-map (kbd "C-n") (kbd "C-x C-f"))
  (define-key input-decode-map (kbd "C-M-<left>") (kbd "C-x <left>"))
  (define-key input-decode-map (kbd "C-M-<right>") (kbd "C-x <right>")))

(defun setup-global-keys ()
  (define-key evil-normal-state-map (kbd "C-M-b") 'ibuffer)
  (global-set-key (kbd "M-<right>") 'forward-word)
  (global-set-key (kbd "M-<left>") 'backward-word)
  (global-set-key (kbd "C-<tab>") #'switch-to-prev-buffer)
  (global-set-key (kbd "<backtab>") #'switch-to-next-buffer)
  (global-set-key (kbd "´") 'kill-buffer)
  (global-set-key (kbd "C-M-ä") 'kill-buffer-and-window)
  (global-set-key (kbd "C-M-y") 'reverse-transpose-sexps)
  (global-set-key "\C-x\ \C-r" 'recentf-open-files)
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))

(setup-global-keys)
(setup-input-decode-map)

;; Clojure settings

(defun clojure-mappings ()
  (evil-local-set-key 'normal (kbd "°") 'cider-eval-buffer)
  (evil-local-set-key 'normal (kbd "M-§") 'cider-eval-buffer)
  (evil-local-set-key 'normal (kbd "§") 'cider-eval-defun-at-point)
  (evil-local-set-key 'normal (kbd "Ö") 'cider-find-var)
  (evil-local-set-key 'normal (kbd "q") 'cider-popup-buffer-quit)
  (evil-local-set-key 'normal (kbd "K") 'cider-doc)
  (evil-local-set-key 'normal (kbd "DEL") 'paredit-splice-sexp))

(defun cider-custom-test-ns-fn (ns)
  "Recognize namespaces (NS) with suffix -spec or -test as test namespaces."
  (when ns
    (cond ((string-match-p "-spec" ns) ns)
          ((string-match-p "-test" ns) ns)
          (t (concat ns "-test")))))

(setq cider-test-infer-test-ns #'cider-custom-test-ns-fn)

(defun my-clojure-mode-hook ()
  (clj-refactor-mode 1)
  (yas-minor-mode 1)        ; for adding require/use/import statements
  ;; This choice of keybinding leaves cider-macroexpand-1 unbound
  (cljr-add-keybindings-with-prefix "C-c C-m")
  (paredit-mode 1)
  (subword-mode 1)
  (flycheck-mode 1)
  (clojure-mappings))

(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)
(add-hook 'cider-repl-mode-hook #'paredit-mode)

;;(add-hook 'clojure-mode-hook 'lsp)
;;(add-hook 'clojurescript-mode-hook 'lsp)

;; (setq gc-cons-threshold (* 100 1024 1024)
;;       read-process-output-max (* 1024 1024)
;;       treemacs-space-between-root-nodes nil
;;       company-minimum-prefix-length 1
;;       lsp-lens-enable nil ; Show the "1 references" etc text above definitions.
;;       lsp-signature-auto-activate nil
;;       lsp-enable-indentation nil ; uncomment to use cider indentation instead of lsp
;;       ; lsp-enable-completion-at-point nil ; uncomment to use cider completion instead of lsp
;;       )

;; (dolist (checker '(clj-kondo-clj clj-kondo-cljs clj-kondo-cljc clj-kondo-edn))
;;   (setq flycheck-checkers (cons checker (delq checker flycheck-checkers))))

;; TypeScript etc.

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))

(defun eslint-fix-file ()
  "Run the buffer through eslint --fix."
  (interactive)
  (message "eslint --fix the file" (buffer-file-name))
  (call-process-shell-command
   (concat "yarn eslint --fix " (buffer-file-name))
   nil "*Shell Command Output*" t)
  (revert-buffer t t))

;; EWW

(defun eww-mappings ()
  (evil-local-set-key 'normal (kbd "M-h") 'eww-back-url)
  (evil-local-set-key 'normal (kbd "M-l") 'eww-forward-url))

(add-hook 'eww-mode-hook #'eww-mappings)
(add-hook 'eww-mode-hook #'visual-line-mode)

;; Common Lisp settings

(defun clisp-mappings ()
  (evil-local-set-key 'normal (kbd "°") 'slime-eval-buffer)
  (evil-local-set-key 'normal (kbd "M-§") 'slime-eval-buffer)
  (evil-local-set-key 'normal (kbd "§") 'slime-eval-defun)
  (evil-local-set-key 'normal (kbd "DEL") 'paredit-splice-sexp))

;; (add-hook 'lisp-mode-hook #'linum-mode)
(add-hook 'lisp-mode-hook #'paredit-mode)
(add-hook 'lisp-mode-hook #'flycheck-mode)
(add-hook 'lisp-mode-hook #'clisp-mappings)

;; Emacs Lisp settings

(defun elisp-mappings ()
  (evil-local-set-key 'normal (kbd "°") 'eval-buffer)
  (evil-local-set-key 'normal (kbd "M-§") 'eval-buffer)
  (evil-local-set-key 'normal (kbd "§") 'eval-defun)
  (evil-local-set-key 'normal (kbd "DEL") 'paredit-splice-sexp))

(add-hook 'emacs-lisp-mode-hook #'paredit-mode)
(add-hook 'emacs-lisp-mode-hook #'flycheck-mode)
(add-hook 'emacs-lisp-mode-hook #'elisp-mappings)

;;; esc quits

(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

(defun org-mode-remaps ()
  ;; Remap org-mode meta keys for convenience
  (mapcar (lambda (state)
            (evil-declare-key state org-mode-map
              (kbd "M-l") 'org-metaright
              (kbd "M-h") 'org-metaleft
              (kbd "M-k") 'org-metaup
              (kbd "M-j") 'org-metadown
              (kbd "M-L") 'org-shiftmetaright
              (kbd "M-H") 'org-shiftmetaleft
              (kbd "M-K") 'org-shiftmetaup
              (kbd "M-J") 'org-shiftmetadown))
          '(normal insert)))

(add-hook 'org-mode-hook #'(lambda ()
                             ;; make the lines in the buffer wrap around the edges of the screen.
                             ;; to press C-c q  or fill-paragraph ever again!
                             (visual-line-mode)
                             (org-mode-remaps)
                             (org-indent-mode)))

(global-set-key (kbd "C-c c") 'org-capture)

;;

(defun kill-magit-diff-buffer-in-current-repo (&rest _)
  "Delete the magit-diff buffer related to the current repo."
  (let ((magit-diff-buffer-in-current-repo
         (magit-mode-get-buffer 'magit-diff-mode)))
    (kill-buffer magit-diff-buffer-in-current-repo)))
;;
;; When 'C-c C-c' is pressed in the magit commit message buffer,
;; delete the magit-diff buffer related to the current repo.
;;
(add-hook 'git-commit-setup-hook
          (lambda ()
            (add-hook 'with-editor-post-finish-hook
                      #'kill-magit-diff-buffer-in-current-repo
                      nil t)))

(with-eval-after-load 'magit
  (defun mu-magit-kill-buffers ()
    "Restore window configuration and kill all Magit buffers."
    (interactive)
    (let ((buffers (magit-mode-get-buffers)))
      (magit-restore-window-configuration)
      (mapc #'kill-buffer buffers)))

  (bind-key "q" #'mu-magit-kill-buffers magit-status-mode-map))

;; Treat Emacs symbol as word in Evil mode

(with-eval-after-load 'evil
  (defalias #'forward-evil-word #'forward-evil-symbol)
  ;; make evil-search-word look for symbol rather than word boundaries
  (setq-default evil-symbol-word-search t))

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
