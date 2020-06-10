;; init.el --- Emacs configuration
;; INSTALL PACKAGES
;; --------------------------------------

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)


(setq gc-cons-threshold 100000000)
(package-initialize)
;; (when (not package-archive-contents)
;;   (package-refresh-contents))

(defvar myPackages
  '(better-defaults
    atom-one-dark-theme
    elpy
    flycheck
    material-theme
    py-autopep8
    company
    lsp-julia
    ivy
    auctex
    multiple-cursors
    smartparens
    company-auctex))

(mapc #'(lambda (package)
	  (unless (package-installed-p package)
	    (package-install package)))
      myPackages)


;; BASIC CUSTOMIZATION
;; --------------------------------------
;; (load-theme 'atom-one-dark t)
;; (require 'color-theme-sanityinc-tomorrow)
;; (load-theme 'sanityinc-tomorrow-eighties t)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq inhibit-startup-message t) ;; hide the startup message
(global-linum-mode t) ;; enable line numbers globally
(tool-bar-mode -1) ;;disable toolbar
(menu-bar-mode -1) ;;disable menu bar
(scroll-bar-mode -1) ;; disable scroll bar
(global-set-key (kbd "C-c C-f") 'ff-find-other-file)
(global-set-key (kbd "C-x C-b") 'ibuffer)
;; (setq backup-directory-alist `(("." . ,(expand-file-name
;;                                     (concat dotfiles-dir "saves")))))


;; flyspell
;; --------------------------
;; switch between german and english
(defun fd-switch-dictionary()
      (interactive)
      (let* ((dic ispell-current-dictionary)
    	 (change (if (string= dic "deutsch8") "british" "deutsch8")))
        (ispell-change-dictionary change)
        (message "Dictionary switched from %s to %s" dic change)
        ))



;; for smart parenthesis 
(require 'smartparens-config)
(show-smartparens-global-mode +1)
(smartparens-global-mode 1)

;; ivy mode
(ivy-mode 1)

;; yasnippet
;; ------------------------------------
(require 'yasnippet)
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(yas-reload-all)
(add-hook 'c++-mode-hook #'yas-minor-mode)
(yas-global-mode 1)


;; COMPANY
;; ----------------------------
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-dabbrev-downcase 0)
(setq company-idle-delay 0)
(setq company-require-match nil)

;; company yasnippet fix
;; (defun company-yasnippet-or-completion ()
;;   (interactive)
;;   (let ((yas-fallback-behavior nil))
;;     (unless (yas-expand)
;;       (call-interactively #'company-complete-common))))
;; (add-hook 'company-mode-hook (lambda ()
;;   (substitute-key-definition 'company-complete-common
;;                              'company-yasnippet-or-completion
;;                              company-active-map)))


;; multiple-cursors
;; --------------------------------------
(require 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)

;; Python ROOT
;; (setenv "ROOTSYS" "/usr/local/root")
;; (setenv "PYTHONDIR" "/usr/local/root:$PYTONDIR")
;; (setenv "PYTHONPATH" "/usr/local/root/lib::/usr/local/root/bindings/pyroot:$PYTHONPATH")
;; ;; (setenv "PATH" "/usr/local/root/bin")
;; (setenv "LD_LIBRARY_PATH"
;; 	"/usr/local/root/lib:/usr/local/root/lib:/usr/local/root/bindings/pyroot:$LD_LIBRARY_PATH")


;; JULIA
(require 'lsp-julia)
(setq lsp-julia-package-dir nil)
(require 'julia-mode)
(add-hook 'julia-mode-hook #'lsp-mode)


;; PYTHON-ELPY
;; ---------------------------------------
(add-hook 'python-mode-hook (lambda() (flyspell-prog-mode)))
;; (add-hook 'python-mode-hook (lambda() (setq indent-tabs-mode t)
;; 			      (setq tab-width 4)
;; 			      (setq python-indent-offset 4)))
(require 'elpy)
(elpy-enable)
(setq elpy-rpc-python-command "python3") ;;use python3
(setq python-shell-interpreter "python3");;use python3
(setq elpy-shell-echo-input nil)
(setq python-shell-completion-native-enable nil)
(when (require 'flycheck nil t)
  (setq elpy-modules(delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
(setq flycheck-flake8-maximum-line-length 100)

(global-set-key (kbd "M-n") 'elpy-nav-forward-block)   ;;move bock down
(global-set-key (kbd "M-p") 'elpy-nav-backward-block)  ;;move bock up
(global-set-key (kbd "M-,") 'pop-tag-mark) ;; go to last place where M-. was used (go-to-definition)

;; conda
(setenv "WORKON_HOME" "/home/jonas/.miniconda3/envs")


;; C++
;; --------------------------
;; (add-hook 'c-mode-common-hook (lambda() (flyspell-prog-mode)))
;; (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; (setq c-default-style "linux"
;;       c-basic-offset 2
;; 	  tab-width 2
;; 	  indent-tabs-mode t)
;; ;;(modern-c++-font-lock-global-mode t)
;; ;; flycheck and goolge's cpplint checkstyle

;; (add-hook 'c-mode-common-hook 'google-set-c-style)
;; (add-hook 'c-mode-common-hook 'google-make-newline-indent)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#1d1f21" "#cc6666" "#b5bd68" "#f0c674" "#81a2be" "#b294bb" "#8abeb7" "#c5c8c6"))
 '(beacon-color "#cc6666")
 '(custom-enabled-themes (quote (tango-dark)))
 '(custom-safe-themes
   (quote
    ("1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "3380a2766cf0590d50d6366c5a91e976bdc3c413df963a0ab9952314b4577299" "8c1dd3d6fdfb2bee6b8f05d13d167f200befe1712d0abfdc47bb6d3b706c3434" "669e02142a56f63861288cc585bee81643ded48a19e36bfdf02b66d745bcc626" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" default)))
 '(eglot-connect-timeout 600)
 '(fci-rule-color "#373b41")
 '(flycheck-c/c++googlelint-executable "/usr/local/bin/cpplint.py")
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(flycheck-googlelint-linelength "80")
 '(flycheck-googlelint-root ".")
 '(flycheck-googlelint-verbose "0")
 '(frame-background-mode (quote dark))
 '(package-selected-packages
   (quote
    (color-theme-sanityinc-tomorrow lsp-mode flycheck-julia julia-mode atom-one-dark-theme docker ein benchmark-init jedi-core virtualenv multiple-cursors smartparens rtags py-autopep8 material-theme jedi google-c-style flycheck elpy company-math company-irony-c-headers company-irony company-auctex cmake-mode cmake-ide better-defaults)))
 '(tetris-x-colors
   [[229 192 123]
    [97 175 239]
    [209 154 102]
    [224 108 117]
    [152 195 121]
    [198 120 221]
    [86 182 194]])
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#cc6666")
     (40 . "#de935f")
     (60 . "#f0c674")
     (80 . "#b5bd68")
     (100 . "#8abeb7")
     (120 . "#81a2be")
     (140 . "#b294bb")
     (160 . "#cc6666")
     (180 . "#de935f")
     (200 . "#f0c674")
     (220 . "#b5bd68")
     (240 . "#8abeb7")
     (260 . "#81a2be")
     (280 . "#b294bb")
     (300 . "#cc6666")
     (320 . "#de935f")
     (340 . "#f0c674")
     (360 . "#b5bd68"))))
 '(vc-annotate-very-old-color nil)
 '(window-divider-mode nil))
(require 'flycheck)
(eval-after-load 'flycheck
  '(progn
     (require 'flycheck-google-cpplint)
     (flycheck-add-next-checker 'c/c++-clang
				'(warning . c/c++-googlelint))))

;; (add-hook 'c++-mode-hook
;; 	  (lambda () (setq flycheck-clang-language-standard "c++11")))
;; (add-hook 'c++-mode-hook 'flycheck-mode)
;; (add-hook 'c-mode-hook 'flycheck-mode)
;; ;; rtags for references and shit
;; (add-hook 'c-mode-hook 'rtags-start-process-unless-running)
;; (add-hook 'c++-mode-hook 'rtags-start-process-unless-running)

;; (defun my-goto-symbol ()
;;   (interactive)
;;   (deactivate-mark)
;;   (ring-insert find-tag-marker-ring (point-marker))
;;   (or (and (require 'rtags nil t)
;; 	   (rtags-find-symbol-at-point))))

;; (rtags-enable-standard-keybindings)

;; (define-key global-map (kbd "C-c f") 'rtags-find-file)

;; (define-key c-mode-base-map (kbd "M-.") 'my-goto-symbol)
;; (define-key c-mode-base-map (kbd "M-,") 'pop-tag-mark)

;; cmake-ide
;; (require 'cmake-ide)
;; (cmake-ide-setup)
;; (setq cmake-ide-flags-c++ (append '("std=c++11")))
;; (global-set-key (kbd "C-c m") 'cmake-ide-compile)



;; irony for completion
;; (require 'irony)
;; (require 'company-irony-c-headers)
;; (add-hook 'c-mode-hook 'irony-mode)
;; (add-hook 'c++-mode-hook 'irony-mode)

;; (defun my-irony-mode-hook ()
;;   (define-key irony-mode-map
;;     [remap completion-at-point] 'irony-completion-at-point)
;;   (define-key irony-mode-map
;;     [remap complete-symbol] 'irony-completion-at-point))
;; (add-hook 'irony-mode-hook 'my-irony-mode-hook)
;; (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)


;; (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
;; (eval-after-load 'company '(add-to-list 'company-backends '(company-irony-c-headers
;; 							    company-irony ;; company-yasnippet
;; 							    company-clang)))

;; Add yasnippet support for all company backends
;; https://github.com/syl20bnr/spacemacs/pull/179
(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")

(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

;; Latex
;; ---------------------------
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-buffer)


(load "auctex.el" nil t t)

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-save-query nil)

(setq TeX-PDF-mode t)


(require 'tex-site)
(autoload 'reftex-mode "reftex" "RefTeX Minor Mode" t)
(autoload 'turn-on-reftex "reftex" "RefTeX Minor Mode" nil)
(autoload 'reftex-citation "reftex-cite" "Make citation" nil)
(autoload 'reftex-index-phrase-mode "reftex-index" "Phrase Mode" t)
(add-hook 'latex-mode-hook 'turn-on-reftex) ; with Emacs latex mode
;; (add-hook 'reftex-load-hook 'imenu-add-menubar-index)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

(setq LaTeX-eqnarray-label "eq"
LaTeX-equation-label "eq"
LaTeX-figure-label "fig"
LaTeX-table-label "tab"
LaTeX-myChapter-label "chap"
TeX-auto-save t
TeX-newline-function 'reindent-then-newline-and-indent
TeX-parse-self t
;; TeX-style-path
;; '("style/" "auto/"
;; "/usr/share/emacs21/site-lisp/auctex/style/"
;; "/var/lib/auctex/emacs21/"
;; "/usr/local/share/emacs/site-lisp/auctex/style/")
LaTeX-section-hook
'(LaTeX-section-heading
LaTeX-section-title
LaTeX-section-toc
LaTeX-section-section
LaTeX-section-label))


;; Fix latex item indent
(defun LaTeX-indent-item ()
  "Provide proper indentation for LaTeX \"itemize\",\"enumerate\", and
\"description\" environments.

  \"\\item\" is indented `LaTeX-indent-level' spaces relative to
  the the beginning of the environment.

  Continuation lines are indented either twice
  `LaTeX-indent-level', or `LaTeX-indent-level-item-continuation'
  if the latter is bound."
  (save-match-data
    (let* ((offset LaTeX-indent-level)
           (contin (or (and (boundp 'LaTeX-indent-level-item-continuation)
                            LaTeX-indent-level-item-continuation)
                       (* 2 LaTeX-indent-level)))
           (re-beg "\\\\begin{")
           (re-end "\\\\end{")
           (re-env "\\(itemize\\|\\enumerate\\|description\\)")
           (indent (save-excursion
                     (when (looking-at (concat re-beg re-env "}"))
                       (end-of-line))
                     (LaTeX-find-matching-begin)
                     (current-column))))
      (cond ((looking-at (concat re-beg re-env "}"))
             (or (save-excursion
                   (beginning-of-line)
                   (ignore-errors
                     (LaTeX-find-matching-begin)
                     (+ (current-column)
                        (if (looking-at (concat re-beg re-env "}"))
                            contin
                          offset))))
                 indent))
             ((looking-at (concat re-end re-env "}"))
              indent)
            ((looking-at "\\\\item")
             (+ offset indent))
            (t
             (+ contin indent))))))

(defcustom LaTeX-indent-level-item-continuation 4
  "*Indentation of continuation lines for items in itemize-like
environments."
  :group 'LaTeX-indentation
  :type 'integer)

(eval-after-load "latex"
  '(setq LaTeX-indent-environment-list
         (nconc '(("itemize" LaTeX-indent-item)
                  ("enumerate" LaTeX-indent-item)
                  ("description" LaTeX-indent-item))
                LaTeX-indent-environment-list)))


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

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq gc-cons-threshold 800000)
