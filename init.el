;;; init.el -*- lexical-binding: t; -*-

;; This file controls what Doom modules are enabled and what order they load
;; in. Remember to run 'doom sync' after modifying it!

;; NOTE Press 'SPC h d h' (or 'C-h d h' for non-vim users) to access Doom's
;;      documentation. There you'll find a "Module Index" link where you'll find
;;      a comprehensive list of Doom's modules and what flags they support.

;; NOTE Move your cursor over a module's name (or its flags) and press 'K' (or
;;      'C-c c k' for non-vim users) to view its documentation. This works on
;;      flags as well (those symbols that start with a plus).
;;
;;      Alternatively, press 'gd' (or 'C-c c d') on a module to browse its
;;      directory (for easy access to its source code).

(doom! :input
       ;;chinese
       ;;japanese
       ;;layout

       :completion
       company
       ;;helm
       ;;ido
       ;; ivy
       (vertico +orderless +icons)

       :ui
       ;; deft
       doom
       doom-dashboard
       ;; doom-quit
       ;; (emoji +unicode)
       ;; fill-column
       hl-todo
       ;;hydra
       indent-guides
       ;; ligatures
       ;; minimap
       modeline
       nav-flash
       ;; neotree
       ophints
       ;; (popup +defaults)
       ;;tabs
       treemacs
       unicode
       (vc-gutter +pretty)
       vi-tilde-fringe
       ;;window-select
       workspaces
       zen

       :editor
       (evil +everywhere)
       file-templates
       fold
       (format +onsave)
       ;;god
       ;;lispy
       ;;multiple-cursors
       ;;objed
       ;;parinfer
       rotate-text
       snippets
       word-wrap

       :emacs
       dired
       electric
       ibuffer
       undo
       vc

       :term
       eshell
       ;; shell
       ;; term
       vterm

       :checkers
       ;; syntax
       ;;(spell +flyspell)
       ;;grammar

       :tools
       ;;ansible
       ;;debugger
       direnv
       docker
       editorconfig
       ;;ein
       (eval +overlay)
       ;;gist
       lookup
       lsp
       magit
       ;; make
       ;; pass
       pdf
       ;;prodigy
       rgb
       ;;taskrunner
       terraform
       ;;tmux
       ;;upload

       :os
       (:if IS-MAC macos)
       ;;tty

       :lang
       ;;agda
       ;;beancount
       ;;cc
       clojure
       common-lisp
       ;;coq
       ;;crystal
       ;;csharp
       data
       ;;(dart +flutter)
       ;;elixir
       ;;elm
       (emacs-lisp +tree-sitter +lsp)
       ;;erlang
       ;;ess
       ;;factor
       ;;faust
       ;;fsharp
       ;;fstar
       ;;gdscript
       ;;(go +lsp)
       (haskell +dante +lsp)
       ;;hy
       ;;idris
       json
       ;; (java +meghanada)
       javascript
       ;;julia
       kotlin
       latex
       ;;lean
       ;;ledger
       lua
       markdown
       ;;nim
       ;;nix
       ;; ocaml
       (org +roam2 +pretty +journal +present)
       ;;php
       ;;plantuml
       ;; purescript
       (python +lsp)
       ;;qt
       ;;racket
       ;;raku
       rest
       ;;rst
       ;;(ruby +rails)
                                        ; (rust +lsp)
       ;;scala
       ;;(scheme +guile)
       (sh +lsp)
       ;;sml
       ;;solidity
       ;;swift
       ;;terra
       web
       yaml
       ;;zig

       :email
       ;; (mu4e +gmail)
       ;; notmuch
       ;;(wanderlust +gmail)

       :app
       ;;calendar
       ;;emms
       ;;everywhere
       ;;irc
       (rss +org)
       ;;twitter

       :config
       ;; literate
       (default +bindings +smartparens))
