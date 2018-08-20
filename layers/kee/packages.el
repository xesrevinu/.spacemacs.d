(defconst kee-packages
  '(lsp-mode
    lsp-ui
    js2-mode
    lsp-javascript-typescript
    company-lsp
    company))


(defun kee/init-lsp-mode ()
  (use-package lsp-mode
    :defer t
    :ensure t
    :config
    (add-hook 'prog-mode-hook #'lsp-mode)))

(defun kee/init-lsp-ui ()
  (use-package lsp-ui
    :config
    (add-hook 'lsp-mode-hook #'lsp-ui-mode)))


(defun kee/init-company-lsp ()
  (use-package company-lsp
    :after company
    :init
    (add-to-list 'company-backends #'company-lsp)
    (setq company-lsp-enable-snippet nil
          company-lsp-cache-candidates t)))

(defun kee/init-lsp-javascript-typescript ()
  (use-package lsp-javascript-typescript
    :defer t
    :ensure t
    :init (require 'lsp-javascript-typescript)
    :config
    (add-hook 'js-mode-hook #'lsp-javascript-typescript-enable)
    (add-hook 'typescript-mode-hook #'lsp-javascript-typescript-enable)))


(defun kee/post-init-js2-mode ()
  (use-package js2-mode
    :defer t
    :config
    (setq-default js2-mode-show-parse-errors nil
                  js2-mode-show-strict-warnings nil)))

(defun kee/post-init-company ()
  (use-package company
    :diminish company-mode
    :defer t
    :config
    (setf company-backends '((company-files
                              company-keywords
                              company-capf)
                             (company-abbrev company-dabbrev)))
    (setq company-tooltip-limit 15)
    (setq company-minimum-prefix-length 1)
    (setq company-echo-delay 0)
    (setq company-auto-complete nil)
    (setq company--idle-delay 0)
    (add-hook 'after-init-hook #'global-company-mode)))
