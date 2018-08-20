(setq editor-packages
  '(company
    json-mode
    rjsx-mode
    js2-mode
    tide
    prettier-js
    editorconfig
    nyan-mode
    all-the-icons
    cider
    clojure-mode
    clj-refactor
    paredit
    smartparens
    clojure-mode-extra-font-locking
    parinfer
    flycheck))

;; clojure
(defun editor/init-clojure-mode ()
  (use-package clojure-mode
    :defer t
    :config
    (require 'clojure-mode-extra-font-locking)
    (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
    (add-hook 'clojure-mode-hook #'clj-refactor-mode)
    (add-hook 'clojure-mode-hook #'subword-mode)
    (add-hook 'clojure-mode-hook #'parinfer-mode)
    (add-hook 'clojure-mode-hook #'paredit-mode)
    ;; foo-bar 或者 :bar 算一个符号
    (add-hook 'clojure-mode-hook
              (lambda ()
                (dolist (c (string-to-list ":_-?!#*"))
                  (modify-syntax-entry c "w" clojure-mode-syntax-table))))))


(defun editor/init-parinfer ()
  (use-package parinfer
    :ensure t
    :defer t
    :config
    (setq parinfer-extensions
          '(defaults       ; should be included.
             pretty-parens  ; different paren styles for different modes.
             ;; evil           ; If you use Evil.
             ;; lispy          ; If you use Lispy. With this extension, you should install Lispy and do not enable lispy-mode directly.
             paredit        ; Introduce some paredit commands.
             smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
             smart-yank))))

(defun editor/init-paredit ()
  (use-package paredit
    :defer t
    :ensure t))

(defun editor/init-cider ()
  (use-package cider
    :defer t
    :config
    (add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
    (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)
    (add-hook 'cider-mode-hook #'eldoc-mode)
    (define-key cider-mode-map (kbd "C-c =") 'cider-format-buffer)
    ;; syntax highlighting
    (setq-default cider-font-lock-dynamically '(macro core function var)
                  cider-auto-select-error-buffer nil
                  cider-prompt-for-symbol nil
                  cider-use-fringe-indicators t
                  cider-use-overlays 'both)))

(defun editor/init-clojure-mode-extra-font-locking ()
  (use-package clojure-mode-extra-font-locking
    :ensure t))

(defun editor/init-clj-refactor ()
  (use-package clj-refactor
    :defer t
    :config
    (clj-refactor-mode 1)
    (yas-minor-mode 1)
    (cljr-add-keybindings-with-prefix "C-c C-m")))

(defun editor/init-clojure-mode-extra-font-locking ()
  (use-package clojure-mode-extra-font-locking
    :defer t))

(defun editor/post-init-smartparens ()
  (use-package smartparens
    :defer t
    :config
    (add-hook 'js2-mode-hook #'smartparens-strict-mode)
    (add-hook 'rjsx-mode-hook #'smartparens-strict-mode)))

(defun editor/init-all-the-icons ()
  (use-package all-the-icons
    :defer t
    :config
    (setq all-the-icons-scale-factor 0.95)))

(defun editor/init-editorconfig ()
  (use-package editorconfig
    :ensure t
    :config
    (editorconfig-mode 1)))

(defun editor/init-nyan-mode ()
  (use-package nyan-mode
    :ensure t
    :config
    (nyan-mode t)))

(defun editor/post-init-company ()
  (use-package company
    :ensure t
    :config
    (add-hook 'after-init-hook 'global-company-mode)
    (setq company-transformers '(company-sort-by-backend-importance))
    (setq company-idle-delay 0)
    (setq company-selection-wrap-around t)
    (setq completion-ignore-case t)
    (setq company-dabbrev-downcase nil)
    (yas-global-mode)
    (global-set-key (kbd "C-M-i") 'company-complete)
    (define-key company-active-map (kbd "C-s") 'company-filter-candidates)
    (define-key company-active-map [tab] 'company-complete-selection)))

(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(defun editor/post-init-flycheck ()
  :ensure t
  :config
  (global-flycheck-mode t)
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint
                          handlebars
                          tsx-tide)))
  ;; customize flycheck temp file prefix
  (setq-default flycheck-temp-prefix ".flycheck")
  ;; disable json-jsonlist checking for json files
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(json-jsonlist)))
  (flycheck-add-mode 'javascript-eslint 'rjsx-mode)
  (flycheck-add-mode 'javascript-eslint 'js2-mode)
  (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  ;; (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1)
  (setq-default js2-mode-show-strict-warnings nil
                js2-mode-show-parse-errors nil)
  (setq company-tooltip-align-annotations t))
  ;; (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append))

(defun editor/init-tide ()
  (use-package tide
    :defer t
    :config))

(defun editor/init-rjsx-mode ()
  (use-package rjsx-mode
    :defer t
    :config
    (add-hook 'rjsx-mode-hook 'prettier-js-mode)
    (add-hook 'rjsx-mode-hook
              (lambda ()
                (set (make-local-variable 'company-backends)
                     '(company-tide
                       company-capf
                       company-react
                       company-dabbrev-code
                       company-keywords
                       company-files
                       company-yasnippet))))
    (add-hook 'rjsx-mode-hook
              (lambda ()
                (when (string-equal "jsx" (file-name-extension buffer-file-name))
                  (setup-tide-mode))))))

;; only js file no react
(defun editor/post-init-js2-mode ()
  (use-package js2-mode
    :defer t
    :config
    (add-hook 'js2-mode-hook 'prettier-js-mode)
    (add-hook 'js2-mode-hook
              (lambda ()
                (set (make-local-variable 'company-backends-js2-mode)
                     '(company-capf
                       company-tide
                       company-dabbrev-code
                       company-keywords
                       company-files
                       company-yasnippet))))
    (add-hook 'js2-mode-hook
              (lambda ()
                (when (string-equal "js" (file-name-extension buffer-file-name))
                  (setup-tide-mode))))))

(defun editor/init-prettier-js ()
  (use-package prettier-js
    :defer t
    :ensure t
    :commands (prettier-js)
    :config
    (setq prettier-js-show-erros (quote echo))
    (add-hook 'js2-mode-hook 'prettier-js-mode)
    (add-hook 'typescript-mode 'prettier-js-mode)
    (add-hook 'rjsx-mode-hook 'prettier-js-mode)
    (add-hook 'css-mode-hook 'prettier-js-mode)))

(defun editor/post-init-json-mode ()
  (use-package json-mode
    :config
    (setq json-reformat:indent-width 2)
    (add-hook 'json-mode-hook 'prettier-js-mode)))
