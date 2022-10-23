;; init.el --- Emacs configuration
;; INSTALL PACKAGES
;; --------------------------------------

;;; Code:
(require 'package)
;; (setq package-archives '(("melpa" . "https://melpa.org/packages/")
;; 			 ("org" . "https://orgmode.org/elpa/")
;; 			 ("elpa" . "https://elpa.gnu.org/packages")))

(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)
;; (add-to-list 'package-archives
;; 	     '("org" . "http://orgmode.org/elpa/") t)

(package-initialize)
(unless package-archive-contents
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
 '(cdlatex-math-symbol-prefix 180)
 '(custom-safe-themes
   '("b7133876a11eb2ded01b4b144b45d9e7457f80dd5900c332241881ab261c50f4" default))
 '(org-agenda-files '("/home/jonas/Research/Masterthesis/master.org"))
 '(org-file-apps
   '((auto-mode . emacs)
     (directory . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . default)
     ("\\.pdf\\'" . "okular %s")))
 '(package-selected-packages
   '(company-tabnine keychain-enviroment keychain-environment crux eglot-jl org-re-reveal org-mode org-cdlatex company-jedi emacs-reveal oer-reveal ox-reveal spacemacs-theme spacemacs-themes zenburn-theme pylint lsp-python-ms pyvenv lsp-pyright org org-plus-contrib frecency cdlatex company-prescient expand-region expand-line flycheck pkg-info rainbow-delimiters magit eglot f90-interface-browser counsel-tramp ivy-rich all-the-icons-dired helpful evil-nerd-commenter ctrlf selectrum frecentf ivy-prescient selectrum-prescient anki-editor anki-connect company-lsp lsp-ui ccls org-pdfview pdf-tools doom-themes doom-modeline counsel-projectile projectile yasnippet py-yapf dashboard iedit aggressive-indent hungry-delete beacon undo-tree subatomic-theme which-key use-package try smartparens py-autopep8 org-bullets multiple-cursors material-theme lsp-julia flycheck-julia counsel company-auctex color-theme-sanityinc-tomorrow better-defaults avy atom-one-dark-theme auctex))
 '(python-indent-offset 4)
 '(require-final-newline nil)
 '(safe-local-variable-values '((reftex-default-bibliography "../Literatur/Master.bib")))
 '(subatomic-more-visible-comment-delimiters t)
 '(warning-suppress-log-types
   '(((yasnippet backquote-change))
     ((yasnippet backquote-change))
     (comp)))
 '(warning-suppress-types '((use-package) (yasnippet backquote-change) (comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)
