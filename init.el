;; init.el --- Emacs configuration


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
(setq elpy-rpc-backend "jedi")
(when (require 'flycheck nil t)
  (setq elpy-modules(delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
;; elpy keybindings
(global-set-key (kbd "M-n") 'elpy-nav-forward-block)   ;;move bock down
(global-set-key (kbd "M-p") 'elpy-nav-backward-block)  ;;move bock up
(global-set-key (kbd "M-,") 'pop-tag-mark) ;; go to last place where M-. was used (go-to-definition)

;; C++
;; --------------------------
(setq c-default-style "linux"
      c-basic-offset 4
	  tab-width 4
	  indent-tabs-mode t)
(modern-c++-font-lock-global-mode t)
;; flycheck
(require 'flycheck)
(add-hook 'c++-mode-hook
	  (lambda () (setq flycheck-clang-language-standard "c++11")))
(add-hook 'c++-mode-hook 'flycheck-mode)
;; rtags for references and shit
(add-hook 'c-mode-hook 'rtags-start-process-unless-running)
(add-hook 'c++-mode-hook 'rtags-start-process-unless-running)

(defun my-goto-symbol ()
  (interactive)
  (deactivate-mark)
  (ring-insert find-tag-marker-ring (point-marker))
  (or (and (require 'rtags nil t)
	   (rtags-find-symbol-at-point))))

(rtags-enable-standard-keybindings)

(define-key global-map (kbd "C-c f") 'rtags-find-file)

(define-key global-map (kbd "M-.") 'my-goto-symbol)
(define-key global-map (kbd "M-,") 'pop-tag-mark)


;; cmake-ide
(require 'cmake-ide)
(cmake-ide-setup)
(setq cmake-ide-flags-c++ (append '("std=c++11")))
(global-set-key (kbd "C-c m") 'cmake-ide-compile)

;; yasnippet
(require 'yasnippet)
(yas-reload-all)
(add-hook 'c++-mode-hook #'yas-minor-mode)


;; irony for completion
(require 'irony)
(require 'company-irony-c-headers)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'c++-mode-hook 'irony-mode)

(defun my-irony-mode-hook ()
  (define-key irony-mode-map
    [remap completion-at-point] 'irony-completion-at-point)
  (define-key irony-mode-map
    [remap complete-symbol] 'irony-completion-at-point))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
(eval-after-load 'company '(add-to-list 'company-backends '(company-irony-c-headers
							    company-irony company-yasnippet
							    company-clang))) 
