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
;; (setq doom-font (font-spec :family "SF Mono" :size 13 :weight 'regular)
;;       doom-variable-pitch-font (font-spec :family "Fira Code" :size 12 :weight 'regular))
(setq doom-font (font-spec :family "SF Mono" :size 13 :weight 'regular)
      doom-variable-pitch-font (font-spec :family "JetBrains Mono" :size 12 :weight 'regular))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-dracula)
;; (setq doom-theme 'doom-material)
(setq doom-theme 'doom-gruvbox)
;; (setq doom-theme 'doom-fairy-floss)

(add-to-list 'exec-path "/Users/tuomo.virolainen/bin")

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/org/")

(setq mac-option-modifier 'nil
      mac-command-modifier 'meta
      mac-function-modifier 'super
      select-enable-clipboard t)

(use-package! tree-sitter
   :hook (prog-mode . turn-on-tree-sitter-mode)
   :hook (tree-sitter-after-on . tree-sitter-hl-mode)
   :config
   (require 'tree-sitter-langs)
   ;; This makes every node a link to a section of code
   (setq tree-sitter-debug-jump-buttons t
         ;; and this highlights the entire sub tree in your code
         tree-sitter-debug-highlight-jump-region t))

;; https://merrick.luois.me/posts/better-tsx-support-in-doom-emacs
(use-package! typescript-mode
  :mode ("\\.tsx\\'" . typescript-tsx-tree-sitter-mode)
  :config
  (setq typescript-indent-level 2)

  (define-derived-mode typescript-tsx-tree-sitter-mode typescript-mode "TypeScript TSX"
    (setq-local indent-line-function 'rjsx-indent-line))

  (add-hook! 'typescript-tsx-tree-sitter-mode-local-vars-hook
             #'+javascript-init-lsp-or-tide-maybe-h
             #'rjsx-minor-mode)
  (map! :map typescript-tsx-tree-sitter-mode-map
        "<" 'rjsx-electric-lt
        ">" 'rjsx-electric-gt))

(after! tree-sitter
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-tsx-tree-sitter-mode . tsx)))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

(setq org-roam-directory "~/Dropbox/org/roam")

(setq org-roam-completion-everywhere t)

(after! org-roam
  (setq org-roam-capture-templates
        `(("n" "default note" plain "%?"
           :if-new
           (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                      "#+title: ${title}\n#+date: %t\n#+filetags: \n\n ")
           :unnarrowed t))))

(setq history-length 25)

(savehist-mode 1)

;; Remember and restore the last cursor location of opened files
(save-place-mode 1)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

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
  (define-key evil-normal-state-map (kbd "¨") #'evil-ex-search-forward)
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
  (define-key evil-normal-state-map (kbd "C-M--") 'ibuffer)
  (global-set-key (kbd "M-<right>") 'forward-word)
  (global-set-key (kbd "M-<left>") 'backward-word)
  (global-set-key (kbd "´") 'kill-buffer)
  (global-set-key (kbd "C-M-ä") 'kill-buffer-and-window)
  (global-set-key (kbd "C-M-y") 'reverse-transpose-sexps)
  (global-set-key "\C-x\ \C-r" 'recentf-open-files)
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))

(setup-global-keys)
(setup-input-decode-map)

;; Clojure settings

(global-company-mode)

(defun clojure-mappings ()
  (evil-local-set-key 'normal (kbd "°") 'cider-eval-buffer)
  (evil-local-set-key 'normal (kbd "M-§") 'cider-eval-buffer)
  (evil-local-set-key 'normal (kbd "§") 'cider-eval-defun-at-point)
  (evil-local-set-key 'normal (kbd "Ö") 'cider-find-var)
  (evil-local-set-key 'normal (kbd "q") 'cider-popup-buffer-quit)
  (evil-local-set-key 'normal (kbd "K") 'cider-doc)
  (evil-local-set-key 'normal (kbd "DEL") 'paredit-splice-sexp)
  (evil-local-set-key 'normal (kbd "C-DEL") 'paredit-splice-sexp))

(defun cider-custom-test-ns-fn (ns)
  "Recognize namespaces (NS) with suffix -spec or -test as test namespaces."
  (when ns
    (cond ((string-match-p "-spec" ns) ns)
          ((string-match-p "-test" ns) ns)
          (t (concat ns "-test")))))

(setq cider-test-infer-test-ns #'cider-custom-test-ns-fn)

(defun initialize-kondo ()
  (dolist (checker '(clj-kondo-clj clj-kondo-cljs clj-kondo-cljc clj-kondo-edn))
    (setq flycheck-checkers (cons checker (delq checker flycheck-checkers)))))

(defun my-clojure-mode-hook ()
  (clj-refactor-mode 1)
  (yas-minor-mode 1)        ; for adding require/use/import statements
  ;; This choice of keybinding leaves cider-macroexpand-1 unbound
  (cljr-add-keybindings-with-prefix "C-c C-m")
  (paredit-mode 1)
  (subword-mode 1)
  (flycheck-mode 1)
  (clojure-mappings)
  (initialize-kondo))

(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)
(add-hook 'cider-repl-mode-hook #'paredit-mode)

(add-hook 'clojure-mode-hook 'lsp)
(add-hook 'clojurescript-mode-hook 'lsp)

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-minimum-prefix-length 1
      lsp-lens-enable nil ; Show the "1 references" etc text above definitions.
      lsp-signature-auto-activate nil
      lsp-enable-indentation nil ; uncomment to use cider indentation instead of lsp
      lsp-completion-enable t ; uncomment to use cider completion instead of lsp
      )

;; Suppress the 'starting look process' message from ispell:
;; https://github.com/company-mode/company-mode/issues/912
(advice-add 'ispell-lookup-words :around
            (lambda (orig &rest args)
              (shut-up (apply orig args))))

(add-to-list 'auto-mode-alist '("\\.sparql\\'" . sparql-mode))

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

(add-hook 'json-mode-hook
          (lambda ()
            (make-local-variable 'js-indent-level)
            (setq js-indent-level 2)))

;; EWW

(add-hook 'eww-mode-hook #'visual-line-mode)

;; Common Lisp settings

(defun clisp-mappings ()
  (evil-local-set-key 'normal (kbd "°") 'sly-compile-file)
  (evil-local-set-key 'normal (kbd "M-§") 'sly-compile-file)
  (evil-local-set-key 'normal (kbd "§") 'sly-compile-defun)
  (evil-local-set-key 'normal (kbd "C-DEL") 'paredit-splice-sexp)
  (evil-local-set-key 'normal (kbd "DEL") 'paredit-splice-sexp))

(use-package! slime
  :defer t ; don't load the package immediately
  :init ; runs this immediately
  (setq inferior-lisp-program "sbcl")
  :config ; runs this when slime loads
  (set-repl-handler! 'lisp-mode #'sly-mrepl)
  (set-eval-handler! 'lisp-mode #'sly-eval-region)
  (set-lookup-handlers! 'lisp-mode
    :definition #'sly-edit-definition
    :documentation #'sly-describe-symbol))

(add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)
(add-hook 'lisp-mode-hook #'paredit-mode)
(add-hook 'lisp-mode-hook #'flycheck-mode)
(add-hook 'lisp-mode-hook #'clisp-mappings)

;; (add-hook 'lisp-mode-hook #'linum-mode)

;; Emacs Lisp settings

(defun elisp-mappings ()
  (evil-local-set-key 'normal (kbd "°") 'eval-buffer)
  (evil-local-set-key 'normal (kbd "M-§") 'eval-buffer)
  (evil-local-set-key 'normal (kbd "§") 'eval-defun)
  (evil-local-set-key 'normal (kbd "C-DEL") 'paredit-splice-sexp)
  (evil-local-set-key 'normal (kbd "DEL") 'paredit-splice-sexp))

(add-hook 'emacs-lisp-mode-hook #'paredit-mode)
(add-hook 'emacs-lisp-mode-hook #'flycheck-mode)
(add-hook 'emacs-lisp-mode-hook #'elisp-mappings)

;; Restclient settings

(defun restclient-mappings ()
  (define-key evil-normal-state-map (kbd "§") 'restclient-http-send-current))

(add-hook 'restclient-mode-hook 'restclient-mappings)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((restclient . t)))

;; https://github.com/doomemacs/doomemacs/issues/6073
(after! restclient (require 'gnutls))

;;; esc quits

(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark t)
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
  (evil-declare-key '(normal insert) org-mode-map
    (kbd "M-l") 'org-metaright
    (kbd "M-h") 'org-metaleft
    (kbd "M-k") 'org-metaup
    (kbd "M-j") 'org-metadown
    (kbd "M-L") 'org-shiftmetaright
    (kbd "M-H") 'org-shiftmetaleft
    (kbd "M-K") 'org-shiftmetaup
    (kbd "M-J") 'org-shiftmetadown))

(add-hook 'org-mode-hook #'(lambda ()
                             ;; make the lines in the buffer wrap around the edges of the screen.
                             ;; to press C-c q  or fill-paragraph ever again!
                             (visual-line-mode)
                             (org-mode-remaps)
                             (org-indent-mode)))

(global-set-key (kbd "C-c c") 'org-capture)

(setq org-todo-keywords
      '((sequence "TODO(t)" "PROJ(p)" "LOOP(r)" "STRT(s)" "WAIT(w)" "HOLD(h)" "IDEA(i)" "DOING(g)" "|" "DONE(d)" "KILL(k)")
        (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)")
        (sequence "|" "OKAY(o)" "YES(y)" "NO(n)")))

(with-eval-after-load 'org-journal
  (setq org-journal-date-prefix "#+TITLE: ")
  (setq org-journal-file-format "%Y-%m-%d.org")
  (setq org-journal-date-format "%a, %Y-%m-%d")
  (setq org-journal-enable-agenda-integration t)
  (setq org-journal-carryover-items
        "TODO=\"TODO\"|TODO=\"PROJ\"|TODO=\"STRT\"|TODO=\"WAIT\"|TODO=\"HOLD\"|TODO=\"[ ]\"|TODO=\"DOING\""))

(setq org-startup-folded 'nofold)

(add-hook 'sql-mode-hook 'sql-indent-enable)

(setq sql-postgres-login-params nil)

;; Separate work laptop -specific connection configurations to a separate file.
(let ((sql-config-file "./sql-connections.el"))
  (when (file-exists-p sql-config-file)
    (load-file sql-config-file)))

;; Capitalize keywords in SQL mode
(add-hook 'sql-mode-hook 'sqlup-mode)
;; Capitalize keywords in an interactive session (e.g. psql)
(add-hook 'sql-interactive-mode-hook 'sqlup-mode)
;; Set a global keyword to use sqlup on a region
(global-set-key (kbd "C-c u") 'sqlup-capitalize-keywords-in-region)

;; SOURCE: https://christiantietze.de/posts/2021/02/emacs-org-todo-doing-done-checkbox-cycling/
(defun org-todo-if-needed (state)
  "Change header state to STATE unless the current item is in STATE already."
  (unless (string-equal (org-get-todo-state) state)
    (org-todo state)))

(defun ct/org-summary-todo-cookie (n-done n-not-done)
  "Switch header state to DONE when all subentries are DONE, to TODO when none are DONE, and to DOING otherwise"
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo-if-needed (cond ((= n-done 0)
                               "TODO")
                              ((= n-not-done 0)
                               "DONE")
                              (t
                               "DOING")))))
(add-hook 'org-after-todo-statistics-hook #'ct/org-summary-todo-cookie)

(defun ct/org-summary-checkbox-cookie ()
  "Switch header state to DONE when all checkboxes are ticked, to TODO when none are ticked, and to DOING otherwise"
  (let (beg end)
    (unless (not (org-get-todo-state))
      (save-excursion
        (org-back-to-heading t)
        (setq beg (point))
        (end-of-line)
        (setq end (point))
        (goto-char beg)
        ;; Regex group 1: %-based cookie
        ;; Regex group 2 and 3: x/y cookie
        (if (re-search-forward "\\[\\([0-9]*%\\)\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]"
                               end t)
            (if (match-end 1)
                ;; [xx%] cookie support
                (cond ((equal (match-string 1) "100%")
                       (org-todo-if-needed "DONE"))
                      ((equal (match-string 1) "0%")
                       (org-todo-if-needed "TODO"))
                      (t
                       (org-todo-if-needed "DOING")))
              ;; [x/y] cookie support
              (if (> (match-end 2) (match-beginning 2)) ; = if not empty
                  (cond ((equal (match-string 2) (match-string 3))
                         (org-todo-if-needed "DONE"))
                        ((or (equal (string-trim (match-string 2)) "")
                             (equal (match-string 2) "0"))
                         (org-todo-if-needed "TODO"))
                        (t
                         (org-todo-if-needed "DOING")))
                (org-todo-if-needed "DOING"))))))))

(add-hook 'org-checkbox-statistics-hook #'ct/org-summary-checkbox-cookie)

(use-package prettier
  :hook ((typescript-mode . prettier-mode)
         (js-mode . prettier-mode)
         (web-mode . prettier-mode)
         (json-mode . prettier-mode)
         (yaml-mode . prettier-mode)
         (ruby-mode . prettier-mode)))

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
