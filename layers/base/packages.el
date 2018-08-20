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
    helm-mode
    prettier-js
    nyan-mode
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
    (add-hook 'js2-mode-hook #'lsp-mode)
    (add-hook 'rjsx-mode-hook #'lsp-mode)))

(defun base/post-init-lsp-ui ()
  (use-package lsp-ui
    :defer t
    :ensure t
    :config
    (setq-default lsp-ui-sideline-enable nil
                  lsp-ui-sideline-show-symbol t
                  lsp-ui-sideline-ignore-duplicate t
                  lsp-ui-doc-header t
                  lsp-ui-doc-border "#444"
                  lsp-ui-doc-include-signature t)
    (setq lsp-ui-doc-frame-parameters '((left . -1)
                                        (no-accept-focus . t)
                                        (no-focus-on-map . t)
                                        (min-width  . 0)
                                        (width  . 0)
                                        (min-height  . 0)
                                        (height  . 0)
                                        (internal-border-width . 1)
                                        (vertical-scroll-bars . nil)
                                        (horizontal-scroll-bars . nil)
                                        (right-fringe . 0)
                                        (left-fringe . 0)
                                        (menu-bar-lines . 0)
                                        (tool-bar-lines . 0)
                                        (line-spacing . 0)
                                        (unsplittable . t)
                                        (undecorated . t)
                                        (top . -1)
                                        (visibility . nil)
                                        (mouse-wheel-frame . nil)
                                        (no-other-frame . t)
                                        (cursor-type . nil)
                                        (inhibit-double-buffering . t)
                                        (drag-internal-border . t)
                                        (no-special-glyphs . t)
                                        (desktop-dont-save . t)))
    (set-face-attribute 'lsp-ui-doc-background nil
                        :background "#F5F5F5")))

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
    :ensure t
    :init (global-flycheck-mode t)
    :config
    (setq-default flycheck-disabled-checkers
                  (append flycheck-disabled-checkers
                          '(lsp-ui)))
    ;; customize flycheck temp file prefix
    (setq-default flycheck-temp-prefix ".flycheck"
                  flycheck-check-syntax-automatically nil)
    ;; disable json-jsonlist checking for json files
    (flycheck-add-mode 'javascript-eslint 'js2-mode)
    (flycheck-add-mode 'javascript-eslint 'rjsx-mode)
    (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)))

(defun base/post-init-lsp-python ()
  (use-package lsp-python
    :defer t
    :after lsp-mode
    :ensure t
    :config
    (add-hook 'python-mode-hook #'lsp-python-enable)))

(defun base/post-init-js2-mode ()
  (use-package js2-mode
    :defer t
    :config
    (setq js2-mode-show-parse-errors nil)
    (setq js2-mode-show-strict-warnings nil)
    (setq-default flycheck-checker 'javascript-eslint)
    (define-key js2-mode-map (kbd "M-,") #'lsp-ui-peek-jump-backward)
    (define-key js2-mode-map (kbd "M-?") #'lsp-ui-peek-find-references)
    (define-key js2-mode-map (kbd "M-.") #'lsp-ui-peek-find-definitions)))

(defun base/init-rjsx-mode ()
  (use-package rjsx-mode
    :ensure t
    :config
    (setq-default flycheck-checker 'javascript-eslint)
    (add-hook 'rjsx-mode-hook #'flycheck-mode)
    (define-key rjsx-mode-map (kbd "M-,") #'xref-pop-marker-stack)
    (define-key rjsx-mode-map (kbd "M-?") #'lsp-ui-peek-find-references)
    (define-key rjsx-mode-map (kbd "M-.") #'lsp-ui-peek-find-definitions)))

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
    (global-eldoc-mode -1)))

(defun base/post-init-yasnippet ()
  (use-package yasnippet
    :defer t
    :config
    (setq yas-snippet-dirs '("~/.emacs.d/snippets"
                             "~/.spacemacs.d/snippets"))
    (add-hook 'prog-mode-hook #'yas-minor-mode)
    ;; show yasnippet panel
    (global-set-key (kbd "C-M-p") 'company-yasnippet)))

(defun base/init-nyan-mode ()
  (use-package nyan-mode
    :ensure
    :config
    (nyan-mode t)))

(defun base/init-helm-mode ()
  (use-package helm-mode
    :defer t
    :config
    (global-set-key (kbd "M-s i") 'helm-imenu)))

(defun base/init-prettier-js ()
  (use-package prettier-js
    :ensure t
    :config
    (setq prettier-js-command "prettier-eslint")
    (spacemacs|diminish prettier-js-mode "⚡" "⚡")
    (add-hook 'scss-mode-hook #'(lambda ()
                                  (enable-minor-mode
                                   '("\\.scss\\'" . prettier-js-mode))))
    (add-hook 'css-mode-hook #'(lambda ()
                                 (enable-minor-mode
                                  '("\\.css\\'" . prettier-js-mode))))
    (add-hook 'js2-mode-hook #'(lambda ()
                                 (enable-minor-mode
                                  '("\\.jsx?\\'" . prettier-js-mode))))
    (add-hook 'json-mode-hook #'(lambda ()
                                  (enable-minor-mode
                                   '("\\.json\\'" . prettier-js-mode))))))

(defun base/post-init-editorconfig ()
  (use-package editorconfig
    :defer t
    :ensure t
    :config
    (editorconfig-mode 1)))

;;; packages.el ends here

