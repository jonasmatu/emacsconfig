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

(require 'doom-themes)
(load-theme 'doom-one t)

(defalias 'yes-or-no-p 'y-or-n-p)
;; (load-theme 'material t) ;; theme
(setq inhibit-startup-message t) ;; hide the startup message
;;(load-theme 'material t) ;; load material theme
(load-theme 'doom-one t)
(global-linum-mode t) ;; enable line numbers globally
(tool-bar-mode -1) ;;disable toolbar
(menu-bar-mode -1) ;;disable menu bar


;; for smart parenthesis 
(require 'smartparens-config)
(show-smartparens-global-mode +1)
(smartparens-global-mode 1)

;;ivy mode(minibuffer)
(ivy-mode 1)

;; COMPANY
;; ----------------------------
(require)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-dabbrev-downcase 0)
(setq company-idle-delay 0)
(setq company-require-match nil)

;; yasnippet
;; ------------------------------------
(require 'yasnippet)
(yas-reload-all)
(add-hook 'c++-mode-hook #'yas-minor-mode)
(yas-global-mode 1)



;; PYTHON-ELPY
;; ---------------------------------------
(add-hook 'python-mode-hook (lambda() (flyspell-prog-mode))) ;; flyspell for mi baad inglisch
(require 'elpy)
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
(require 'ein-connect)
(global-set-key (kbd "C-c C-n l") 'ein:notebooklist-login)
(global-set-key (kbd "C-c C-n o") 'ein:notebooklist-open)

(setq ein:completion-backend 'ein:use-ac-jedi-backend)
(setq ein:use-auto-complete-superpack t)

;; C++
;; --------------------------
(setq c-default-style "linux"
      c-basic-offset 2
	  tab-width 2
	  indent-tabs-mode t)
;;(modern-c++-font-lock-global-mode t)
;; flycheck and goolge's cpplint checkstyle

(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-c/c++googlelint-executable "/usr/local/bin/cpplint.py")
 '(flycheck-googlelint-linelength "80")
 '(flycheck-googlelint-root ".")
 '(flycheck-googlelint-verbose "0")
 '(package-selected-packages
   ;; (quote
   ;;  (yasnippet-snippets smartparens rtags py-autopep8 material-theme jedi google-c-style flycheck elpy ein doom-themes company-math company-irony-c-headers company-irony company-auctex cmake-mode cmake-ide better-defaults))))
(require 'flycheck)
(eval-after-load 'flycheck
  '(progn
     (require 'flycheck-google-cpplint)
     (flycheck-add-next-checker 'c/c++-clang
				'(warning . c/c++-googlelint))))

(add-hook 'c++-mode-hook
	  (lambda () (setq flycheck-clang-language-standard "c++11")))
(add-hook 'c++-mode-hook 'flycheck-mode)
(add-hook 'c-mode-hook 'flycheck-mode)
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


(company-auctex-init)
(setq company-math-disallow-unicode-symbols-in-face nil)
(append '((company-math-symbols-latex company-math-symbols-unicode
              company-auctex-macros company-auctex-environments))
                      company-backends)

;; auto-complete
;;(defadvice auto-complete-mode (around disable-auto-complete-for-python)
;;  (unless (eq major-mode 'python-mode) ad-do-it))

;;(ad-activate 'auto-complete-mode)
;;(require 'auto-complete)
;;(add-to-list 'ac-modes 'latex-mode) ; beware of using 'LaTeX-mode instead
;;(require 'ac-math) ; package should be installed first 
;; (defun my-ac-latex-mode () ; add ac-sources for latex
;;   (setq ac-sources
;;         (append '(;;ac-source-math-unicode
;; 	  ac-source-math-latex
;;           ac-source-latex-commands)
;;                 ac-sources)))
;; (add-hook 'LaTeX-mode-hook 'my-ac-latex-mode)
;; ;;(setq ac-math-unicode-in-math-p t)
;; (ac-flyspell-workaround) ; fixes a known bug of delay due to flyspell (if it is there)

;; (require 'auto-complete-config) ; should be after add-to-list 'ac-modes and hooks
;; (ac-config-default)
;; (setq ac-auto-show-menu t)
;; ;; (global-auto-complete-mode t) 
;; (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)

;; init.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
