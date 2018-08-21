;;; packages.el --- ivy-layer layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
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
;; added to `ivy-layer-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `ivy-layer/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `ivy-layer/pre-init-PACKAGE' and/or
;;   `ivy-layer/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst ivy-layer-packages
  '(ivy-rich
    ivy)
  "The list of Lisp packages required by the ivy-layer layer.

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

(defun ivy-layer/post-init-ivy-rich ()
  (use-package ivy-rich
    :defer t
    :config
    (setq ivy-rich-switch-buffer-align-virtual-buffer t
          ivy-rich-path-style 'abbrev)
    (ivy-rich-mode 1)))


(defun ivy-layer/post-init-ivy ()
  (use-package ivy
    :bind
    (:map ivy-mode-map
          ("C-'" . ivy-avy))
    :config
    (setq-default ivy-virtual-abbreviate 'full
                  ivy-use-virtual-buffers t
                  ivy-initial-inputs-alist nil
                  ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
    (add-hook 'after-init-hook #'ivy-mode)))

;;; packages.el ends here
