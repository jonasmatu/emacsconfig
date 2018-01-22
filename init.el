;; init.el --- Emacs configuration


<<<<<<< HEAD

;; a change in master

=======

>>>>>>> experimental
;; INSTALL PACKAGES
;; --------------------------------------

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
    py-autopep8))

(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      myPackages)

;; BASIC CUSTOMIZATION
;; --------------------------------------
(defalias 'yes-or-no-p 'y-or-n-p)
(setq auto-save-default nil)
(setq inhibit-startup-message t) ;; hide the startup message
(load-theme 'material t) ;; load material theme
(global-linum-mode t) ;; enable line numbers globally
(tool-bar-mode -1) ;;disable toolbar
(menu-bar-mode -1) ;;disable menu bar

;; )) for smart parenthesis 
(require 'smartparens-config)
(show-smartparens-global-mode +1)
(smartparens-global-mode 1)
(sp-pair "(" ")" :wrap "C-(")

(ivy-mode 1)

;; autocomplete mit company
(add-hook 'after-init-hook 'global-company-mode)
(setq company-dabbrev-downcase 0)
(setq company-idle-delay 0)
(setq company-require-match nil)


;; Python
;; ----------------------------------
(add-hook 'python-mode-hook (lambda() (flyspell-prog-mode))) ;; flyspell for mi baad inglisch
(elpy-enable)
(setq elpy-rpc-python-command "python3") ;;use python3
(setq python-shell-interpreter "python3");;use python3
(when (require 'flycheck nil t)
  (setq elpy-modules(delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
;; elpy keybindings
(global-set-key (kbd "M-n") 'elpy-nav-forward-block)   ;;move bock down
(global-set-key (kbd "M-p") 'elpy-nav-backward-block)  ;;move bock up
(global-set-key (kbd "M-,") 'pop-tag-mark) ;; go to last place where M-. was used (go-to-definition)
