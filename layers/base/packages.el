;;; packages.el --- base layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Kee <Kee@Kee-MacBook-Pro-4.local>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `base-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `base/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `base/pre-init-PACKAGE' and/or
;;   `base/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst base-packages
  '(js2-mode
    rjsx-mode
    lsp-mode
    lsp-ui
    lsp-python
    lsp-javascript-typescript
    editorconfig
    yasnippet
    company
    company-lsp
    flycheck
    nyan-mode
    prettier-js
    nyamoden
    exec-path-from-shell)
  "The list of Lisp packages required by the base layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(defun base/init-exec-path-from-shell ()
  (use-package exec-path-from-shell
    :ensure t
    :config
    (setq exec-path-from-shell-arguments '("-l"))
    :init
    (when (memq window-system '(mac ns x))
      (exec-path-from-shell-initialize))))

(defun base/post-init-lsp-mode ()
  (use-package lsp-mode
    :defer t
    :config
    (setq  lsp-inhibit-message t
           lsp-print-io nil
           lsp-eldoc-render-all nil
           lsp-highlight-symbol-at-point nil)
    (add-hook 'js2-mode-hook #'lsp-mode)))

(defun base/post-init-lsp-ui ()
  (use-package lsp-ui
    :defer t
    :ensure
    :config
    (require 'lsp-imenu)
    (add-hook 'lsp-after-open-hook 'lsp-enable-imenu)
    (setq-default lsp-ui-sideline-show-symbol nil
                  lsp-ui-sideline-ignore-duplicate t
                  lsp-ui-doc-include-signature nil
                  lsp-ui-doc-header t
                  lsp-ui-doc-border "#444"
                  lsp-ui-sideline-enable nil)
    (set-face-attribute 'lsp-ui-doc-background nil
                        :background "#F5F5F5")
    ;; bind key
    (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
    (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)))

(defun base/post-init-lsp-javascript-typescript ()
  (use-package lsp-javascript-typescript
    :defer t
    :after lsp-mode
    :ensure t
    :config
    (add-hook 'js2-mode-hook #'lsp-javascript-typescript-enable)
    (add-hook 'rjsx-mode-hook #'lsp-javascript-typescript-enable)))

(defun base/post-init-flycheck ()
  (use-package flycheck
    :defer t
    :init (global-flycheck-mode t)
    :config
    (setq-default flycheck-disabled-checkers
                  (append flycheck-disabled-checkers
                          '()))
    ;; customize flycheck temp file prefix
    (setq-default flycheck-temp-prefix ".flycheck")
    ;; disable json-jsonlist checking for json files
    (setq-default flycheck-disabled-checkers
                  (append flycheck-disabled-checkers
                          '(json-jsonlist)))
    ;; (flycheck-add-mode 'javascript-eslint 'rjsx-mode)
    (flycheck-add-mode 'javascript-eslint 'js2-mode)
    (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)))


(defun base/post-init-lsp-python ()
  (use-package lsp-python
    :defer t
    :after lsp-mode
    :ensure t
    :config
    (add-hook 'python-mode-hook
              #'lsp-python-enable)))

(defun base/post-init-js2-mode ()
  (use-package js2-mode
    :defer t
    :config
    (setq js2-mode-show-parse-errors nil)
    (setq js2-mode-show-strict-warnings nil)
    (setq-local flycheck-check-syntax-automatically nil)
    (setq-default flycheck-checker 'javascript-eslint)
    (add-to-list 'flycheck-checkers 'javascript-eslint)))

(defun base/init-rjsx-mode ()
  (use-package rjsx-mode
    :defer t
    :ensure t))

(defun base/post-init-company-lsp ()
  (use-package company-lsp
    :defer t
    :config
    (push 'company-lsp company-backends)))

(defun base/post-init-company ()
  (use-package company
    :defer t
    :init (global-company-mode)
    :config
    (setq company-idle-delay 0.2
          company-tooltip-align-annotations t
          company-selection-wrap-around t
          completion-ignore-case t
          company-tooltip-limit 10
          company-minimum-prefix-length 1
          company-tooltip-margin 1)
    (global-set-key (kbd "C-M-i") 'company-complete)
    (define-key company-active-map (kbd "C-s") 'company-filter-candidates)
    (define-key company-active-map [tab] 'company-complete-selection)))

(defun base/post-init-eldoc ()
  (use-package eldoc
    :defer t
    :config
    (global-eldoc-mode)))

(defun base/post-init-yasnippet ()
  (use-package yasnippet
    :defer t
    :config
    ;; (spacemacs|diminish yas-minor-mode "yas" "yas")
    (push 'yas-installed-snippets-dir yas-snippet-dirs)
    (setq yas-snippet-dirs '("~/.emacs.d/snippets"
                             "~/.spacemacs.d/snippets"))
    (add-hook 'prog-mode-hook #'yas-minor-mode)
    ;; show yasnippet panel
    (global-set-key (kbd "C-M-p") 'company-yasnippet)))

(defun base/init-nyan-mode ()
  (use-package nyan-mode
    :ensure t
    :defer t
    :config (nyan-mode t)))

(defun base/post-init-editorconfig ()
  (use-package editorconfig
    :defer t
    :ensure t
    :config
    (editorconfig-mode 1)))

;;; packages.el ends here
