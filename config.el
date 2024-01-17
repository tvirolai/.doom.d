;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Tuomo Virolainen"
      user-mail-address "tuomo.virolainen@siili.com")

;; appearance

(defvar required-fonts '("Martian Mono" "Overpass" "JuliaMono"))

(defvar available-fonts
  (delete-dups (or (font-family-list)
                   (split-string (shell-command-to-string "fc-list : family")
                                 "[,\n]"))))

(defvar missing-fonts
  (delq nil (mapcar
             (lambda (font)
               (unless (delq nil (mapcar (lambda (f)
                                           (string-match-p (format "^%s$" font) f))
                                         available-fonts))
                 font))
             required-fonts)))

(if missing-fonts
    (pp-to-string
     `(unless noninteractive
        (add-hook! 'doom-init-ui-hook
          (run-at-time nil nil
                       (lambda ()
                         (message "%s missing the following fonts: %s"
                                  (propertize "Warning!" 'face '(bold warning))
                                  (mapconcat (lambda (font)
                                               (propertize font 'face 'font-lock-variable-name-face))
                                             ',missing-fonts
                                             ", "))
                         (sleep-for 0.5))))))
  ";; No missing fonts detected")

;; Martian Mono, SF Mono and JetBrains Mono are the favourites.
;; For future ideas: 'https://emacsredux.com/blog/2021/12/22/check-if-a-font-is-available-with-emacs-lisp/'.
(setq doom-font (font-spec :family "Martian Mono" :size 12 :weight 'regular)
      doom-variable-pitch-font (font-spec :family "Overpass" :size 14)
      doom-symbol-font (font-spec :family "JuliaMono"))


(custom-set-faces!
  '(doom-modeline-buffer-modified :foreground "orange"))

(defvar fancy-splash-image-directory
  (expand-file-name "images/" doom-user-dir)
  "Directory in which to look for splash image templates.")

(defvar fancy-splash-image-template
  (expand-file-name "emacs-e-template.svg" fancy-splash-image-directory)
  "Default template svg used for the splash image.
Colours are substituted as per `fancy-splash-template-colours'.")

(setq fancy-splash-image (expand-file-name "emacs-e-template.svg" fancy-splash-image-directory))

(setq doom-theme 'doom-dracula)

(when (version< "29.0.50" emacs-version)
  (pixel-scroll-precision-mode))

;; Adapted from 'tecosaur/emacs-config'

(display-time-mode 1)

(setq undo-limit 80000000
      evil-want-fine-undo t
      auto-save-default t
      truncate-string-ellipsis "…"
      password-cache-expiry nil
      display-time-default-load-average nil)

(global-subword-mode 1)

(setq-default major-mode 'org-mode)

;; Show info about the file under editing, see: 'https://github.com/Artawower/file-info.el'
(use-package file-info
  :ensure t
  :bind (("C-c d" . 'file-info-show))
  :config
  (setq hydra-hint-display-type 'posframe)
  (setq hydra-posframe-show-params `(:poshandler posframe-poshandler-frame-center
                                     :internal-border-width 2
                                     :internal-border-color "#61AFEF"
                                     :left-fringe 16
                                     :right-fringe 16)))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

(setq projectile-enable-caching nil)

;; General settings

(add-hook 'window-setup-hook 'toggle-frame-maximized t)

(add-to-list 'exec-path "/Users/tuomo.virolainen/bin")
(add-to-list 'exec-path "/opt/homebrew/bin")

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(setq global-visual-line-mode t)
(global-auto-revert-mode t)

(add-hook! 'doom-after-reload-hook (doom-load-envvars-file (expand-file-name "env" doom-local-dir) t))

(setq kill-ring-max 1000)

(setq which-key-idle-delay 0.5)

(yank-indent-mode 1)

;; Separate work laptop -specific connection configurations to a separate file.
(let ((sql-config-file "~/.config/doom/sql-connections.el"))
  (print "Looking for a configuration file...")
  (when (file-exists-p sql-config-file)
    (print "Configuration file found! Loading...")
    (load! sql-config-file)))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/org")

(setq projectile-project-search-path '("~/dev"))

(setq mac-option-modifier 'nil
      mac-command-modifier 'meta
      mac-function-modifier 'super
      select-enable-clipboard t)

;; Always load newest byte code
(setq load-prefer-newer t)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

(setq-default indent-tabs-mode nil        ; Stop using tabs to indent
              tab-always-indent 'complete ; Indent first then try completions
              tab-width 4)                ; Smaller width for tab characters

;; Tree-sitter

(use-package! tree-sitter
  :hook (prog-mode . turn-on-tree-sitter-mode)
  :hook (tree-sitter-after-on . tree-sitter-hl-mode)
  :config (require 'tree-sitter-langs)
  ;; This makes every node a link to a section of code
  (setq tree-sitter-debug-jump-buttons t
        ;; and this highlights the entire sub tree in your code
        tree-sitter-debug-highlight-jump-region t))

(use-package! rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :config (setq rainbow-delimiters-max-face-count 4))

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        ;; (clojure "https://github.com/clojure-emacs/clojure-ts-mode")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(setq major-mode-remap-alist
      '((yaml-mode . yaml-ts-mode)
        (sh-mode . bash-ts-mode)
        ;; (clojure-ts-mode . clojure-mode)
        ;; (clojurescript-ts-mode . clojurescript-mode)
        (js2-mode . js-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (json-mode . json-ts-mode)
        (css-mode . css-ts-mode)
        ;; (markdown-mode . markdown-ts-mode)
        ;; (gfm-mode . markdown-ts-mode)
        ;; (web-mode . html-ts-mode)
        (python-mode . python-ts-mode)))

(defun treesit-install-grammars ()
  (interactive)
  (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist)))

;; https://merrick.luois.me/posts/better-tsx-support-in-doom-emacs
;; (use-package! typescript-mode
;;   :mode ("\\.tsx\\'" . typescript-tsx-tree-sitter-mode)
;;   :config
;;   (setq typescript-indent-level 2)

;;   (define-derived-mode typescript-tsx-tree-sitter-mode typescript-mode "TypeScript TSX"
;;     (setq-local indent-line-function 'rjsx-indent-line))

;;   (add-hook! 'typescript-tsx-tree-sitter-mode-local-vars-hook
;;              #'+javascript-init-lsp-or-tide-maybe-h
;;              #'rjsx-minor-mode)
;;   (map! :map typescript-tsx-tree-sitter-mode-map
;;         "<" 'rjsx-electric-lt
;;         ">" 'rjsx-electric-gt))

(after! tree-sitter
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-tsx-tree-sitter-mode . tsx)))

(setq history-length 25)

(savehist-mode 1)

;; Remember and restore the last cursor location of opened files
(save-place-mode 1)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

(add-hook! dired-mode-hook #'dired-revert)

(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

;; Autocomplete

(add-hook! prog-mode #'company-mode)

(after! company-mode
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 2)
  (setq company-show-quick-access nil)
  (add-hook 'evil-normal-state-entry-hook #'company-abort))

;; LSP

(after! lsp-mode
  (setq +lsp-company-backends
        '(:separate company-capf company-yasnippet company-dabbrev)))

(setq yas-triggers-in-field t)

;; Evil

;; Default to global substitution.
(setq evil-ex-substitute-global t)

(setq doom-scratch-initial-major-mode 'org-mode)

(setq evil-shift-width 2)

(setq evil-undo-system 'undo-redo)

(setq evil-split-window-below t
      evil-vsplit-window-right t)

(setq evil-ex-substitute-global t)

;; When we overwrite text in visual mode, don’t add to the kill ring.
(setq evil-kill-on-visual-paste nil)

;; (setq evil-want-fine-undo nil)

(use-package evil-owl
  :config
  (setq evil-owl-max-string-length 500)
  (setq evil-owl-idle-delay 0.5)
  (add-to-list 'display-buffer-alist
               '("*evil-owl*"
                 (display-buffer-in-side-window)
                 (side . bottom)
                 (window-height . 0.3)))
  (evil-owl-mode))

(map! :g "C-s" #'save-buffer)
(map! :after evil :gnvi "C-å" #'consult-line)

(map! :map dired-mode-map
      :n "h" #'dired-up-directory
      :n "l" #'dired-find-alternate-file)

(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

(map! :after evil-maps
      :gnvi "C-k" #'evil-window-up
      :gnvi "C-j" #'evil-window-down
      :gnvi "C-l" #'evil-window-right
      :gnvi "C-h" #'evil-window-left

      :n "C-u" #'evil-scroll-up
      :n "C-ö" #'split-window-below
      :n "Ö" #'xref-find-definitions
      :n "ä" #'delete-other-windows
      :n "C-ä" #'split-window-right
      :n "ö" #'save-buffer
      :n "Ä" #'+vertico/project-search
      :n "å" #'yank-from-kill-ring
      :n "¨" #'evil-ex-search-forward
      :n "Q" #'kill-buffer-and-window
      :n "SPC z" #'recentf-open-files
      :n "C--" nil
      :gnvi "C-M--" #'ibuffer)

(map! :g "M-n" #'make-frame
      :g "M-q" #'doom/delete-frame-with-prompt
      :g "M-§" #'other-frame
      :g "M-<right>" #'forward-word
      :g "M-<left>" #'backward-word
      :g "´" #'kill-buffer
      :g "C-M-y" #'reverse-transpose-sexps
      :g "C-c C-c M-x" #'execute-extended-command
      :g "C-h" #'evil-window-left
      :g "C-j" #'evil-window-down
      :g "C-k" #'evil-window-up
      :g "C-l" #'evil-window-right)

;; Clojure settings

(map! :mode clojure-mode
      :n "°" #'cider-eval-buffer
      :n "M-§" #'cider-eval-buffer
      :n "§" #'cider-eval-defun-at-point
      :n "Ö" #'cider-find-var
      :n "q" #'cider-popup-buffer-quit
      :n "K" #'cider-doc
      :n "DEL" #'paredit-splice-sexp
      :n "C-DEL" #'paredit-splice-sexp)

(defun cider-custom-test-ns-fn (ns)
  "Recognize namespaces (NS) with suffix -spec or -test as test namespaces."
  (when ns
    (cond ((string-match-p "-spec" ns) ns)
          ((string-match-p "-test" ns) ns)
          (t (concat ns "-test")))))

(setq cider-test-infer-test-ns #'cider-custom-test-ns-fn)

;; (with-eval-after-load 'clojurescript-ts-mode
;;   (add-to-list 'lsp-language-id-configuration '(clojurescript-ts-mode . "clojurescript")))

;; (with-eval-after-load 'clojure-mode
;;   (add-hook 'clojure-ts-mode-hook #'cider-mode)
;;   (add-hook 'clojure-ts-mode-hook #'clojure-mode-variables)
;;   (setq clojure-ts-mode-map clojure-mode-map)
;;   (setq clojurescript-ts-mode-map clojurescript-mode-map))

(defun initialize-kondo ()
  (dolist (checker '(clj-kondo-clj clj-kondo-cljs clj-kondo-cljc clj-kondo-edn))
    (setq flycheck-checkers (cons checker (delq checker flycheck-checkers)))))

(defun my-clojure-mode-hook ()
  (clj-refactor-mode 1)
  (yas-minor-mode 1)        ; for adding require/use/import statements
  ;; This choice of keybinding leaves cider-macroexpand-1 unbound
  (paredit-mode 1)
  (flycheck-mode 1)
  (tree-sitter-hl-mode -1)
  (initialize-kondo))

(defun my-cider-repl-mode-hook ()
  (paredit-mode 1)
  (+word-wrap-mode 1)
  (evil-local-set-key 'insert (kbd "C-<return>") 'paredit-RET)
  (evil-local-set-key 'insert (kbd "RET") 'cider-repl-closing-return)
  (setq cider-repl-buffer-size-limit 20000))

(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)
(add-hook 'cider-repl-mode-hook #'my-cider-repl-mode-hook)
;; (add-hook 'clojure-ts-mode-hook #'my-clojure-mode-hook)

(add-hook 'clojure-mode-hook #'lsp)
(add-hook 'clojurescript-mode-hook #'lsp)
(add-hook 'clojurescript-mode-hook #'locally-defer-font-lock)
;; (add-hook 'clojure-ts-mode-hook #'lsp)
;; (add-hook 'clojurescript-ts-mode-hook #'lsp)

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      cider-font-lock-dynamically nil
      cider-repl-buffer-size-limit 1000
      lsp-lens-enable nil ; Show the "1 references" etc text above definitions.
      lsp-enable-indentation nil ; uncomment to use cider indentation instead of lsp
      lsp-completion-enable nil ; uncomment to use cider completion instead of lsp
      )

;; Suppress the 'starting look process' message from ispell:
;; https://github.com/company-mode/company-mode/issues/912
(advice-add 'ispell-lookup-words :around
            (lambda (orig &rest args)
              (shut-up (apply orig args))))

;; Make indentation work with Compojure Api definitions
;; (lsp indentation can handle them out of the box).
(after! cider
  (define-clojure-indent
   (POST 2)
   (GET 2)
   (PATCH 2)
   (PUT 2)))

;; SPARQL

(add-to-list 'auto-mode-alist '("\\.sparql\\'" . sparql-mode))

;; Python

(add-hook! 'python-ts-mode-hook #'lsp)
(add-hook! 'python-ts-mode-hook #'python-black-on-save-mode)

(setq lsp-pylsp-plugins-flake8-max-line-length 120)

;; Bash

(add-hook! 'bash-ts-mode-hook #'turn-off-smartparens-mode)
(add-hook! 'bash-ts-mode-hook #'lsp)
(add-hook! 'sh-mode-hook #'turn-off-smartparens-mode)
(add-hook! 'sh-mode-hook #'lsp)

;; TypeScript etc.

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-ts-mode))

(defun eslint-fix-file ()
  "Run the buffer through eslint --fix."
  (interactive)
  (message "eslint --fix the file %s" (buffer-file-name))
  (call-process-shell-command
   (concat "yarn eslint --fix " (buffer-file-name))
   nil "*Shell Command Output*" t)
  (revert-buffer t t))

(add-hook 'json-mode-hook
          (lambda ()
            (make-local-variable 'js-indent-level)
            (setq js-indent-level 2)))

;; EWW

;; Disable images
(setq shr-inhibit-images t)
;; (setq shr-use-fonts nil)

(add-hook 'eww-mode-hook #'visual-line-mode)

(map! :map eww-mode-map
      :n "H" #'evil-window-top
      :n "L" #'evil-window-bottom)

;; Terminal

(after! eshell-mode
  (define-key eshell-mode-map (kbd "C-l") #'evil-window-right))

(setq eshell-prefer-lisp-functions t)

;; https://github.com/suonlight/multi-vterm#for-evil-users
(use-package multi-vterm
  :after vterm
  :config (add-hook 'vterm-mode-hook
                    (lambda ()
                      (evil-insert-state)
                      ;; (evil-emacs-state)
                      ))

  (setq vterm-max-scrollback 100000)
  (setq vterm-keymap-exceptions nil)

  (map! :map vterm-mode-map
        :i "<C-backspace>" #'(lambda () (interactive) (vterm-send-key (kbd "C-w")))
        :i "<M-backspace>" #'(lambda () (interactive) (vterm-send-key (kbd "C-w")))
        :i "RET" #'vterm-send-return
        :gnvi "C-k" #'evil-window-up
        :gnvi "C-j" #'evil-window-down
        :gnvi "C-l" #'evil-window-right
        :gnvi "C-h" #'evil-window-left
        :n ",c" #'multi-vterm
        :n ",n" #'multi-vterm-next
        :n ",p" #'multi-vterm-prev
        :n "<return>" #'evil-insert-resume))

(setq vterm-kill-buffer-on-exit t)

(setq vterm-shell "/bin/zsh")

;; Marginalia

(after! marginalia
  (setq marginalia-censor-variables nil)

  (defadvice! +marginalia--anotate-local-file-colorful (cand)
    "Just a more colourful version of `marginalia--anotate-local-file'."
    :override #'marginalia--annotate-local-file
    (when-let (attrs (file-attributes (substitute-in-file-name
                                       (marginalia--full-candidate cand))
                                      'integer))
      (marginalia--fields
       ((marginalia--file-owner attrs)
        :width 12 :face 'marginalia-file-owner)
       ((marginalia--file-modes attrs))
       ((+marginalia-file-size-colorful (file-attribute-size attrs))
        :width 7)
       ((+marginalia--time-colorful (file-attribute-modification-time attrs))
        :width 12))))

  (defun +marginalia--time-colorful (time)
    (let* ((seconds (float-time (time-subtract (current-time) time)))
           (color (doom-blend
                   (face-attribute 'marginalia-date :foreground nil t)
                   (face-attribute 'marginalia-documentation :foreground nil t)
                   (/ 1.0 (log (+ 3 (/ (+ 1 seconds) 345600.0)))))))
      ;; 1 - log(3 + 1/(days + 1)) % grey
      (propertize (marginalia--time time) 'face (list :foreground color))))

  (defun +marginalia-file-size-colorful (size)
    (let* ((size-index (/ (log10 (+ 1 size)) 7.0))
           (color (if (< size-index 10000000) ; 10m
                      (doom-blend 'orange 'green size-index)
                    (doom-blend 'red 'orange (- size-index 1)))))
      (propertize (file-size-human-readable size) 'face (list :foreground color)))))

(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Elfeed

(defun elfeed-mark-all-as-read ()
  "Mark all elfeed items as read."
  (interactive)
  (when (equal 'elfeed-search-mode major-mode)
    (elfeed-untag elfeed-search-entries 'unread)
    (elfeed-search-update :force)))

(defun elfeed-kill-buffers ()
  "Kill elfeed buffer and the elfeed.org feed definition buffer."
  (interactive)
  (let ((buffer (get-buffer "elfeed.org")))
    (kill-buffer buffer)
    (elfeed-kill-buffer)))

;; (add-hook! elfeed-search-mode #'elfeed-update)

(after! elfeed-search
  (set-evil-initial-state! 'elfeed-search-mode 'normal))

(after! elfeed-show-mode
  (set-evil-initial-state! 'elfeed-show-mode 'normal))

(after! evil-snipe
  (push 'elfeed-show-mode   evil-snipe-disabled-modes)
  (push 'elfeed-search-mode evil-snipe-disabled-modes))

(after! elfeed
  (elfeed-org)
  (use-package! elfeed-link)

  (setq elfeed-search-filter "@2-week-ago +unread"
        elfeed-search-print-entry-function '+rss/elfeed-search-print-entry
        elfeed-search-title-min-width 80
        elfeed-show-entry-switch #'pop-to-buffer
        elfeed-show-entry-delete #'+rss/delete-pane
        elfeed-show-refresh-function #'+rss/elfeed-show-refresh--better-style
        shr-max-image-proportion 0.6)

  (add-hook! 'elfeed-show-mode-hook (hide-mode-line-mode 1))
  (add-hook! 'elfeed-search-update-hook #'hide-mode-line-mode)

  (defface elfeed-show-title-face '((t (:weight ultrabold :slant italic :height 1.5)))
    "title face in elfeed show buffer"
    :group 'elfeed)
  (defface elfeed-show-author-face `((t (:weight light)))
    "title face in elfeed show buffer"
    :group 'elfeed)
  (set-face-attribute 'elfeed-search-title-face nil
                      :foreground 'nil
                      :weight 'light)

  (map! :map elfeed-search-mode-map
        :after elfeed-search
        [remap kill-this-buffer] "q"
        [remap kill-buffer] "q"
        :n doom-leader-key nil
        :n "q" #'elfeed-kill-buffers
        :n "e" #'elfeed-update
        :n "r" #'elfeed-search-untag-all-unread
        :n "u" #'elfeed-search-tag-all-unread
        :n "s" #'elfeed-search-live-filter
        :n "RET" #'elfeed-search-show-entry
        :n "+" #'elfeed-search-tag-all
        :n "-" #'elfeed-search-untag-all
        :n "S" #'elfeed-search-set-filter
        :n "b" #'elfeed-search-browse-url
        :n "y" #'elfeed-search-yank)

  (map! :map elfeed-show-mode-map
        :after elfeed-show
        [remap kill-this-buffer] "q"
        [remap kill-buffer] "q"
        :n doom-leader-key nil

        :nm "q" #'+rss/delete-pane
        :nm "o" #'ace-link-elfeed
        :nm "RET" #'org-ref-elfeed-add
        :nm "n" #'elfeed-show-next
        :nm "N" #'elfeed-show-prev
        :nm "p" #'elfeed-show-prev
        :nm "+" #'elfeed-show-tag
        :nm "-" #'elfeed-show-untag
        :nm "s" #'elfeed-show-new-live-search
        :nm "y" #'elfeed-show-yank)

  (defadvice! +rss-elfeed-wrap-h-nicer ()
    "Enhances an elfeed entry's readability by wrapping it to a width of
`fill-column' and centering it with `visual-fill-column-mode'."
    :override #'+rss-elfeed-wrap-h
    (setq-local truncate-lines nil
                shr-width 120
                visual-fill-column-center-text t
                default-text-properties '(line-height 1.1))
    (let ((inhibit-read-only t)
          (inhibit-modification-hooks t))
      (visual-fill-column-mode)
      ;; (setq-local shr-current-font '(:family "Merriweather" :height 1.2))
      (set-buffer-modified-p nil)))

  (defun +rss/elfeed-search-print-entry (entry)
    "Print ENTRY to the buffer."
    (let* ((elfeed-goodies/tag-column-width 40)
           (elfeed-goodies/feed-source-column-width 30)
           (title (or (elfeed-meta entry :title) (elfeed-entry-title entry) ""))
           (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
           (feed (elfeed-entry-feed entry))
           (feed-title
            (when feed
              (or (elfeed-meta feed :title) (elfeed-feed-title feed))))
           (tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
           (tags-str (concat (mapconcat 'identity tags ",")))

           (tag-column (elfeed-format-column
                        tags-str (elfeed-clamp (length tags-str)
                                               elfeed-goodies/tag-column-width
                                               elfeed-goodies/tag-column-width)
                        :left))
           (feed-column (elfeed-format-column
                         feed-title (elfeed-clamp elfeed-goodies/feed-source-column-width
                                                  elfeed-goodies/feed-source-column-width
                                                  elfeed-goodies/feed-source-column-width)
                         :left)))

      (insert (propertize feed-column 'face 'elfeed-search-feed-face) " ")
      (insert (propertize tag-column 'face 'elfeed-search-tag-face) " ")
      (insert (propertize title 'face title-faces 'kbd-help title))
      (setq-local line-spacing 0.2)))

  (defun +rss/elfeed-show-refresh--better-style ()
    "Update the buffer to match the selected entry, using a mail-style."
    (interactive)
    (let* ((inhibit-read-only t)
           (title (elfeed-entry-title elfeed-show-entry))
           (date (seconds-to-time (elfeed-entry-date elfeed-show-entry)))
           (author (elfeed-meta elfeed-show-entry :author))
           (link (elfeed-entry-link elfeed-show-entry))
           (tags (elfeed-entry-tags elfeed-show-entry))
           (tagsstr (mapconcat #'symbol-name tags ", "))
           (nicedate (format-time-string "%a, %e %b %Y %T %Z" date))
           (content (elfeed-deref (elfeed-entry-content elfeed-show-entry)))
           (type (elfeed-entry-content-type elfeed-show-entry))
           (feed (elfeed-entry-feed elfeed-show-entry))
           (feed-title (elfeed-feed-title feed))
           (base (and feed (elfeed-compute-base (elfeed-feed-url feed)))))
      (erase-buffer)
      (insert "\n")
      (insert (format "%s\n\n" (propertize title 'face 'elfeed-show-title-face)))
      (insert (format "%s\t" (propertize feed-title 'face 'elfeed-search-feed-face)))
      (when (and author elfeed-show-entry-author)
        (insert (format "%s\n" (propertize author 'face 'elfeed-show-author-face))))
      (insert (format "%s\n\n" (propertize nicedate 'face 'elfeed-log-date-face)))
      (when tags
        (insert (format "%s\n"
                        (propertize tagsstr 'face 'elfeed-search-tag-face))))
      ;; (insert (propertize "Link: " 'face 'message-header-name))
      ;; (elfeed-insert-link link link)
      ;; (insert "\n")
      (cl-loop for enclosure in (elfeed-entry-enclosures elfeed-show-entry)
               do (insert (propertize "Enclosure: " 'face 'message-header-name))
               do (elfeed-insert-link (car enclosure))
               do (insert "\n"))
      (insert "\n")
      (if content
          (if (eq type 'html)
              (elfeed-insert-html content base)
            (insert content))
        (insert (propertize "(empty)\n" 'face 'italic)))
      (goto-char (point-min)))))

;; Common Lisp settings

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

(map! :mode lisp-mode
      :n "°" #'sly-compile-file
      :n "§" #'sly-compile-defun
      :n "DEL" #'paredit-splice-sexp
      :n "C-DEL" #'paredit-splice-sexp)

(add-hook! 'lisp-mode #'paredit-mode)
(add-hook! 'lisp-mode #'flycheck-mode)

;; Emacs Lisp settings

(add-hook 'emacs-lisp-mode-hook #'paredit-mode)
(add-hook 'emacs-lisp-mode-hook #'flycheck-mode)

(map! :mode emacs-lisp-mode
      :n "°" #'eval-buffer
      :n "§" #'eval-defun
      :n "DEL" #'paredit-splice-sexp
      :n "C-DEL" #'paredit-splice-sexp)

;; Restclient settings

(map! :mode restclient-mode
      :n "§" #'restclient-http-send-current)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((restclient . t)))

;; https://github.com/doomemacs/doomemacs/issues/6073
(after! restclient (require 'gnutls))

;; Org mode

(setq org-default-notes-file (concat org-directory "/inbox.org"))

(setq org-agenda-files (list org-default-notes-file))

(setq org-capture-templates
      '(("f" "Fleeting note" item
         (file+headline org-default-notes-file "Notes")
         "- %?"
         :jump-to-captured t)
        ("t" "New task" entry
         (file+headline org-default-notes-file "Tasks")
         "* TODO %i%?")))

(custom-set-faces!
  '(outline-1 :weight extra-bold)
  '(outline-2 :weight bold)
  '(outline-3 :weight bold)
  '(outline-4 :weight semi-bold)
  '(outline-5 :weight semi-bold)
  '(outline-6 :weight semi-bold)
  '(outline-8 :weight semi-bold)
  '(outline-9 :weight semi-bold))

(setq org-ellipsis " ▾")
(setq org-superstar-headline-bullets-list '("› "))
(setq org-agenda-start-with-log-mode t)
(setq org-log-done 'time)
(setq org-log-into-drawer t)
(setq org-cycle-emulate-tab nil)
(setq org-startup-folded 'nofold)

(custom-set-faces!
  '(org-document-title :height 1.2))

(defadvice! +org-init-appearance-h--no-ligatures-a ()
  :after #'+org-init-appearance-h
  (set-ligatures! 'org-mode nil)
  (set-ligatures! 'org-mode
                  :list_property "::"
                  :em_dash       "---"
                  :ellipsis      "..."
                  :arrow_right   "->"
                  :arrow_left    "<-"
                  :arrow_lr      "<->"
                  :properties    ":PROPERTIES:"
                  :end           ":END:"
                  :priority_a    "[#A]"
                  :priority_b    "[#B]"
                  :priority_c    "[#C]"
                  :priority_d    "[#D]"
                  :priority_e    "[#E]"))

(setq doom-themes-org-fontify-special-tags nil)

(use-package! visual-fill-column
  :custom
  (visual-fill-column-width 300)
  (visual-fill-column-center-text t)
  :hook (org-mode . visual-fill-column-mode))

(add-hook! org-mode #'(lambda ()
                        ;; (visual-line-mode 1) ;; make the lines in the buffer wrap around the edges of the screen.
                        (+word-wrap-mode)
                        (+org-pretty-mode)
                        (setq doom-modeline-enable-word-count t)
                        (org-indent-mode)))

(global-set-key (kbd "C-c c") 'org-capture)

(defun locally-defer-font-lock ()
  "Set jit-lock defer and stealth, when buffer is over a certain size."
  (when (> (buffer-size) 50000)
    (setq-local jit-lock-defer-time 0.05
                jit-lock-stealth-time 1)))

(add-hook! 'org-mode-hook #'locally-defer-font-lock)

(after! org
  (global-org-modern-mode)
  (map! :map org-mode-map
        :ni "M-l" #'org-metaright
        :ni "M-h" #'org-metaleft
        :ni "M-k" #'org-metaup
        :ni "M-j" #'org-metadown
        :ni "M-L" #'org-shiftmetaright
        :ni "M-H" #'org-shiftmetaleft
        :ni "M-K" #'org-shiftmetaup
        :ni "M-J" #'org-shiftmetadown)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "PROJ(p)" "LOOP(r)" "STRT(s)" "WAIT(w)" "HOLD(h)" "IDEA(i)" "DOING(g)" "|" "DONE(d)" "KILL(k)")
          (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)")
          (sequence "|" "OKAY(o)" "YES(y)" "NO(n)"))))

;; These (as many other snippets) are from 'https://github.com/elken/doom'.

(defun elken/org-archive-done-tasks ()
  "Attempt to archive all done tasks in file"
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
   "/DONE" 'file))

(map! :map org-mode-map :desc "Archive tasks marked DONE" "C-c DEL a" #'elken/org-archive-done-tasks)

(defun elken/org-remove-kill-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-cut-subtree)
     (pop kill-ring)
     (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
   "/KILL" 'file))

(map! :map org-mode-map :desc "Remove tasks marked as KILL" "C-c DEL k" #'elken/org-remove-kill-tasks)

(setq org-archive-location "archive/Archive_%s::")

(setq org-log-into-drawer t)

;; SOURCE: https://christiantietze.de/posts/2021/02/emacs-org-todo-doing-done-checkbox-cycling/
(defun org-todo-if-needed (state)
  "Change header state to STATE unless the current item is in STATE already."
  (unless (string-equal (org-get-todo-state) state)
    (org-todo state)))

(defun ct/org-summary-todo-cookie (n-done n-not-done)
  "Switch header state to DONE when all subentries are DONE,
to TODO when none are DONE, and to DOING otherwise"
  (org-todo-if-needed (cond ((= n-done 0)
                             "TODO")
                            ((= n-not-done 0)
                             "DONE")
                            (t
                             "DOING"))))
(add-hook 'org-after-todo-statistics-hook #'ct/org-summary-todo-cookie)

(use-package! org-appear
  :after org
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autoemphasis t
        org-appear-autolinks t
        org-appear-autosubmarkers t))

(defun ct/org-summary-checkbox-cookie ()
  "Switch header state to DONE when all checkboxes are ticked,
to TODO when none are ticked, and to DOING otherwise"
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

(setq org-use-property-inheritance t)

(setq org-roam-completion-everywhere t)

(after! org-roam
  (org-roam-db-sync)
  (setq org-roam-capture-templates
        `(("n" "default note" plain "%?"
           :if-new
           (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                      "#+title: ${title}\n#+date: %t\n#+filetags: \n\n ")
           :unnarrowed t)
          ("b" "book" plain "%?"
           :if-new
           (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                      "#+author: ${author}\n#+title: ${title}\n#+subtitle: \n#+date: %t\n#+origin: ${origin}\n#+category: \n#+filetags: :kirjat:\n\n")
           :unnarrowed t)
          ("p" "project" plain "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: Project")
           :unnarrowed t)
          ("m" "meeting" plain "%?"
           :if-new
           (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                      "#+title: %^{title}\n#+present: %^{present} \n#+date: %t\n#+category: \n#+filetags: :työ:\n\n ")
           :unnarrowed t))))

;; SQL

(setq sql-postgres-login-params nil)

(setq lsp-sqls-workspace-config-path nil)

(defun maybe-highlight-ms-sql-kws ()
  "Highlight MS SQL keywords when it's certain that's the dialect we're
working with."
  (when (string-match "umaija" (buffer-file-name))
    (message "Formatting...")
    (sql-highlight-ms-keywords)))

(add-hook! 'sql-mode-hook #'lsp)

(add-hook! 'sql-mode-hook #'maybe-highlight-ms-sql-kws)

(add-hook! 'sql-mode-hook #'sqlind-minor-mode)

(add-hook! 'sql-mode-hook #'(lambda ()
                              (+word-wrap-mode 1)))

;; Capitalize keywords in SQL mode
(add-hook! 'sql-mode-hook #'sqlup-mode)
;; Capitalize keywords in an interactive session (e.g. psql)
(add-hook! 'sql-interactive-mode-hook #'sqlup-mode)
;; Set a global keyword to use sqlup on a region
(global-set-key (kbd "C-c u") #'sqlup-capitalize-keywords-in-region)

(defun indent-sql-buffer ()
  "Since there's some bug that breaks the indentation (`sqlind-indent-line`
specifically) when running it with `newline-and-indent`, I've resorted
to this hack to run the indentation for the whole buffer."
  (interactive)
  (sqlind-minor-mode)
  (indent-region (point-min) (point-max))
  (setq sqlind-minor-mode nil)
  (progn
    (kill-local-variable 'indent-line-function)
    (kill-local-variable 'align-mode-rules-list)))

;; (map! :mode sql-mode
;;       :n "ö" #'indent-sql-buffer)

;; Prettier

(use-package prettier
  :hook ((typescript-mode . prettier-mode)
         (js-mode . prettier-mode)
         (web-mode . prettier-mode)
         ;; (json-mode . prettier-mode)
         (yaml-mode . prettier-mode)))

(setq +format-on-save-disabled-modes
      '(emacs-lisp-mode  ; elisp's mechanisms are good enough
        sql-mode         ; sqlformat is currently broken
        tex-mode         ; latexindent is broken
        python-ts-mode   ; delegate this to Black instead
        python-mode
        latex-mode))

(setq prettier-inline-errors-flag t)

;; Magit

(defun kill-magit-diff-buffer-in-current-repo (&rest _)
  "Delete the magit-diff buffer related to the current repo."
  (let ((magit-diff-buffer-in-current-repo
         (magit-mode-get-buffer 'magit-diff-mode)))
    (kill-buffer magit-diff-buffer-in-current-repo)))

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

;; https://zck.org/balance-emacs-windows
(seq-doseq (fn (list #'split-window #'delete-window))
  (advice-add fn
              :after
              #'(lambda (&rest _args) (balance-windows))))

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
