;; init.el --- Emacs configuration

;; INSTALL PACKAGES
;; --------------------------------------
(add-to-list 'load-path "~/.emacs.d/elisp")

(require 'package)

(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar myPackages
  '(better-defaults
    elpy
    flycheck
    material-theme
    py-autopep8
    company))

(mapc #'(lambda (package)
	  (unless (package-installed-p package)
	    (package-install package)))
      myPackages)

;; BASIC CUSTOMIZATION
;; --------------------------------------

(defalias 'yes-or-no-p 'y-or-n-p)
(load-theme 'material t) ;; theme
(setq inhibit-startup-message t) ;; hide the startup message
(global-linum-mode t) ;; enable line numbers globally
(tool-bar-mode -1) ;;disable toolbar
(menu-bar-mode -1) ;;disable menu bar

(require 'smartparens-config)
(show-smartparens-global-mode +1)
(smartparens-global-mode 1)

;;ivy mode(minibuffer)
(ivy-mode 1)


;; COMPANY
;; ----------------------------
(add-hook 'after-init-hook 'global-company-mode)
(setq company-dabbrev-downcase 0)
(setq company-idle-delay 0)
(setq company-require-match nil)


;; PYTHON-ELPY
;; ---------------------------------------
(add-hook 'python-mode-hook (lambda() (flyspell-prog-mode))) ;; flyspell for mi baad inglisch
(elpy-enable)
(setq elpy-rpc-python-command "python3") ;;use python3
(setq python-shell-interpreter "python3");;use python3
(when (require 'flycheck nil t)
  (setq elpy-modules(delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

(global-set-key (kbd "M-n") 'elpy-nav-forward-block)   ;;move bock down
(global-set-key (kbd "M-p") 'elpy-nav-backward-block)  ;;move bock up
(global-set-key (kbd "M-,") 'pop-tag-mark) ;; go to last place where M-. was used (go-to-definition)


;; IPython and Jupyter
;; -------------------------
(global-set-key (kbd "C-c C-n l") 'ein:notebooklist-login)
(global-set-key (kbd "C-c C-n o") 'ein:notebooklist-open)

(require' ein-connect)
(setq ein:completion-backend 'ein:use-ac-jedi-backend)

;; yasnippet
;; -----------------------------
(require 'yasnippet)
(yas-global-mode 1)


;; Latex
;; ---------------------------

(setq ispell-program-name "aspell")
(setq ispell-dictionary "german")
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-buffer)

(load "auctex.el" nil t t)

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-save-query nil)

(setq TeX-PDF-mode t)

;; Make okular work
(setq TeX-source-correlate-mode t
      TeX-source-correlate-start-server t)
(eval-after-load "tex"
  '(setcar (cdr (assoc 'output-pdf TeX-view-program-selection)) "Okular"))


;; init.el ends here
