;; init.el --- Emacs configuration
;; INSTALL PACKAGES
;; --------------------------------------

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))


(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


(org-babel-load-file (expand-file-name "~/.emacs.d/myinit.org"))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("b7133876a11eb2ded01b4b144b45d9e7457f80dd5900c332241881ab261c50f4" default)))
 '(package-selected-packages
   (quote
    (doom-themes doom-modeline counsel-projectile projectile yasnippet-snippets ggtags iedit aggressive-indent hungry-delete beacon undo-tree subatomic-theme which-key use-package try smartparens py-autopep8 org-bullets multiple-cursors material-theme lsp-julia flycheck-julia elpy counsel company-auctex color-theme-sanityinc-tomorrow better-defaults avy atom-one-dark-theme)))
 '(subatomic-more-visible-comment-delimiters t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)
