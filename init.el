;;;; Coding system: utf-8 as default
(setq buffer-file-coding-system 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)

;;;; ELPA settings
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(defun require-package (package &optional min-version no-refresh)
  "Ask ELPA to install given PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

;; use-package is always needed
(require-package 'use-package)
(require 'use-package)

;;;; UI
;; Fonts
(defun set-font (english chinese size-pair)
  (when (display-graphic-p)
    (use-package font-utils :ensure t)
    (set-face-attribute 'default nil :font
                        (format "%s:pixelsize=%d" english (car size-pair)))
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font) charset
                        (font-spec :family chinese :size (cdr size-pair))))))

(defun emacs-step-font-size (step)
  "Increase/Decrease emacs's font size. Need to define emacs-english-font emacs-chinese-font before use"
  (let ((scale-steps emacs-font-size-pair-list))
    (if (< step 0) (setq scale-steps (reverse scale-steps)))
    (setq emacs-font-size-pair
          (or (cadr (member emacs-font-size-pair scale-steps))
              emacs-font-size-pair))
    (when emacs-font-size-pair
      (message "emacs font size set to %.1f" (car emacs-font-size-pair))
      (set-font emacs-english-font emacs-chinese-font emacs-font-size-pair))))

(defun increase-emacs-font-size ()
  "Decrease emacs's font-size acording emacs-font-size-pair-list."
  (interactive) (emacs-step-font-size 1))

(defun decrease-emacs-font-size ()
  "Increase emacs's font-size acording emacs-font-size-pair-list."
  (interactive) (emacs-step-font-size -1))

(global-set-key (kbd "C-=") 'increase-emacs-font-size)
(global-set-key (kbd "C--") 'decrease-emacs-font-size)

;;;; Show TOC
;; https://github.com/avar/dotemacs/blob/master/.emacs

(defun show-dot-emacs-structure ()
  "Show the outline-mode structure"
  (interactive)
  (occur "^;;;;+"))

;; Show .emacs structure on C-x ?
(add-hook
 'emacs-lisp-mode-hook
 (lambda ()
   (bind-key "C-x ?" 'show-dot-emacs-structure)))

;;;; Emacs tweak
;; Start server
(server-start)
;; Prevent the cursor from blinking
(blink-cursor-mode 0)
;; Don't use messages that you don't read
(setq initial-scratch-message "")
(setq inhibit-startup-message t)
;; Don't let Emacs hurt your ears
;;(setq visible-bell t)
;; Who use the bar to scroll?
(scroll-bar-mode 0)
;(tool-bar-mode 0)
(menu-bar-mode 1)
;; Show keystroke
(setq echo-keystrokes 0.1)
;; Answer briefly
(fset 'yes-or-no-p 'y-or-n-p)
;; backup policies
(setq make-backup-files t)
(setq version-control t)
(setq kept-old-versions 2)
(setq kept-new-versions 5)
(setq delete-old-versions t)
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;;;; Tramp
(setq tramp-default-method "scp")

;;;; Session
(use-package session
  :ensure t
  :init (add-hook 'after-init-hook 'session-initialize))

;;;; Dired

;; load dired-x
(add-hook 'dired-load-hook
          (lambda ()
            (load "dired-x")
            ;; no dot files
            (setq dired-omit-files
                  (concat dired-omit-files "\\|^\\..+$"))
            ;; (setq dired-guess-shell-gnutar "gtar")
            ;; (setq dired-x-hands-off-my-keys nil)
            ))
(add-hook 'dired-mode-hook
          (lambda ()
            ;; Set dired-x buffer-local variables here.  For example:
            (dired-omit-mode 1)
            ))

;; no line-wrap in dired
(defun my-dired-long-lines ()
  (toggle-truncate-lines 1))
(add-hook 'dired-after-readin-hook 'my-dired-long-lines)

;; no extra buffers when entering different dirs
(use-package dired-single
  :ensure t
  :config
  (add-hook 'dired-mode-hook
            (lambda ()
              (define-key dired-mode-map (kbd "RET") 'dired-single-buffer)
              (define-key dired-mode-map (kbd "<mouse-1>") 'dired-single-buffer-mouse)
              (define-key dired-mode-map (kbd "^")
                (lambda ()
                  (interactive)
                  (dired-single-buffer "..")))))
)

;;;; Editing

;;;;; Ace jump
(use-package ace-jump-mode
  :ensure t
  :commands ace-jump-mode
  :init
  (bind-key "C-c SPC" 'ace-jump-mode))

;;;;; Word wrap
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;;;;; Show matched parens
(show-paren-mode)
(electric-pair-mode)

;;;;; Indention
(bind-keys ("RET" . newline-and-indent)
           ("C-j" . newline))

;;; Prefer space indent
(setq-default tab-width 4
              indent-tabs-mode nil)

;;; Highlight indentation
(use-package highlight-indentation
  :ensure t)

;;; Highlight tabs and trailing spaces
(setq whitespace-style '(face
                         indentation
                         trailing
                         empty))
(global-whitespace-mode)

;;;; Dev

;;;;; Align Repeat
(defun align-repeat (start end regexp)
  "Repeat alignment with respect to
     the given regular expression."
  (interactive "r\nsAlign regexp: ")
  (align-regexp start end
                (concat "\\(\\s-*\\)" regexp) 1 1 t))

;;;;; Rainbow for color visualization
(use-package rainbow-mode
  :ensure t
  :defer t
  :init
  (progn
    (add-hook 'css-mode-hook 'rainbow-mode)
    (add-hook 'scss-mode-hook 'rainbow-mode)))

;;;;; Web-mode
(use-package web-mode
  :ensure t
  :mode "\\.js\\|\\.tpl\\|\\.erb\\|\\.html?\\|\\.jinja2\\'"
  :config
  (progn
    (setq web-mode-content-types-alist '(("jsx"  . "\\.js[x]?\\'")))
    (setq web-mode-enable-auto-quoting nil)
    (add-hook 'web-mode-hook 'hs-minor-mode)
    )
)

;;;;; Emmet auto-complete html tags
(use-package emmet-mode
  :ensure t
  :defer t
  :init
  (add-hook 'web-mode-hook 'emmet-mode))

;;;;; Enhanced javascript mode
(use-package js2-mode
  :ensure t
  :disabled t
  :mode "\\.js\\'"
  :config
  (setq js2-basic-offset 2))

;;;;; Perl
(defalias 'perl-mode 'cperl-mode)
(setq cperl-invalid-face nil)

;;;;; xcscope.el
(use-package xcscope
  :ensure t
  :init
  (cscope-setup))

;;;; Auto-completion

;;;;; Yasnippet
(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode)
  :config
  (defcustom commit-prefix-alist
    '(("fix" "Fix: ") ("ad" "Android: "))
    "Prefix yasnippets when writing commit message."
    :type '(alist :value-type (group string))
    :group 'yasnippet
    :set (lambda (symbol value)
           (set-default symbol value)
           (yas-define-snippets 'text-mode value))
    ))

;;;;; Auto-complete
(use-package auto-complete-config
  :ensure auto-complete
  :disabled t
  :init
  (progn
    (ac-config-default)
    (global-auto-complete-mode)))

;;;;; Company-mode
(use-package company
  :ensure t
  :init
  :config
  (progn
    (add-hook 'after-init-hook 'global-company-mode)
    (define-key company-active-map (kbd "<tab>") nil)))

;; Python backend
(use-package company-anaconda
  :ensure t
;  :disabled t
  :config
  (progn
    (add-to-list 'company-backends 'company-anaconda)
    (add-hook 'python-mode-hook 'anaconda-mode)))

(defun my/use-flow-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (flow (and root
                    (expand-file-name "node_modules/flow-bin/vendor/flow"
                                      root))))
    (when (and flow (file-executable-p flow))
      (setq-local company-flow-executable flow))))

;; Flow backend
(use-package company-flow
  :ensure t
  :config
  (progn
    (add-hook 'company-mode-hook #'my/use-flow-from-node-modules)
    (add-to-list 'company-backends 'company-flow)))

(use-package ac-js2
  :ensure t
  :disabled
  :config
  (add-hook 'js2-mode-hook 'ac-js2-mode)
  )

;(add-to-list 'company-backends 'company-gtags)

;;;; Folding
(autoload 'folding-mode          "folding" "Folding mode" t)
(autoload 'turn-off-folding-mode "folding" "Folding mode" t)
(autoload 'turn-on-folding-mode  "folding" "Folding mode" t)

;;;; Woman
(setq woman-manpath (concat (getenv "CYGWIN_HOME") "/usr/share/man"))

;;;; Buffers

;;; Make buffer name unique
(use-package uniquify
  :init
  (setq uniquify-buffer-name-style 'forward))

;;; Auto reload buffers
(global-auto-revert-mode)

;;;;; ibuffer
;; ibuffer groups
(use-package ibuffer
  :ensure t
  :bind ("C-x C-b" . ibuffer)
  :config
  (progn
    (setq ibuffer-show-empty-filter-groups nil)
    (setq ibuffer-saved-filter-groups
          (quote (("default"
                   ("Org" ;; all org-related buffers
                    (mode . org-mode))
                   ("rust-clippy"
                    (filename . "/rust-clippy/"))
                   ("remacs"
                    (filename . "/remacs/"))
                   ("HearthStone Bot"
                    (filename . "/HearthstoneBot/"))
                   ;; Rice Inc
                   ("Sophie Server"
                    (filename . "/sophie/server"))
                   ("Sophie Web"
                    (filename . "/sophie/web"))
                   ("Sophie Docker"
                    (filename . "/docker/"))
                   ("Leela Zero"
                    (filename . "/mypro/leela-zero/"))
                   ;; Glowing Inc. starts
                   ("emma"
                    (filename . "/projects/emma/"))
                   ("kaylee"
                    (filename . "/projects/kaylee/"))
                   ("bryo"
                    (filename . "/projects/bryo/"))
                   ("suso"
                    (filename . "/projects/suso/"))
                   ("lexie"
                    (filename . "/projects/lexie/"))
                   ("noah"
                    (filename . "/projects/noah/"))
                   ("prime"
                    (filename . "/projects/prime/"))
                   ("forum"
                    (filename . "/projects/glow_forum/"))
                   ("ansible"
                    (filename . "/projects/glow_ansible/"))
                   ("BaseAndroid"
                    (filename . "/projects/Base-Android/"))
                   ("CommunityAndroid"
                    (filename . "/projects/Community-Android/"))
                   ("mia-server"
                    (filename . "/projects/mia/server/"))
                   ("mia-web"
                    (filename . "/projects/mia/web/"))
                   ;; Glowing Inc. ends
                   ("Programming" ;; prog stuff not already in MyProjectX
                    (or
                     (mode . c-mode)
                     (mode . perl-mode)
                     (mode . python-mode)
                     (mode . emacs-lisp-mode)
                     ;; etc
                     ))
                   ))))
    (add-hook 'ibuffer-mode-hook
              (lambda ()
                (ibuffer-switch-to-saved-filter-groups "default")))
    )
  )

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  )

(use-package helm
  :ensure t
  :config
  (progn
    (helm-mode 1)
    (global-set-key (kbd "M-x") #'helm-M-x)
    (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
    (global-set-key (kbd "C-x C-f") #'helm-find-files)
    ))

(use-package helm-projectile
  :ensure t
  :config
  (progn
    (projectile-global-mode)
    (setq projectile-completion-system 'helm)
    (helm-projectile-on)
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))
  )

(use-package yafolding
  :ensure t
  :config
  (add-hook 'prog-mode-hook
          (lambda () (yafolding-mode))))

(use-package helm-gtags
  :ensure t
  :config
  (progn
    (define-key helm-gtags-mode-map (kbd "C-c s t") 'helm-gtags-find-tag)
    (define-key helm-gtags-mode-map (kbd "C-c s r") 'helm-gtags-find-rtag)
    (define-key helm-gtags-mode-map (kbd "C-c s p") 'helm-gtags-find-pattern)
    (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
    (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
    (define-key helm-gtags-mode-map (kbd "C-c s u") 'helm-gtags-pop-stack)
    (add-hook 'prog-mode-hook
          (lambda () (helm-gtags-mode)))
    ))

(use-package rust-mode
  :ensure t
  :config
  (add-hook 'rust-mode-hook
            (lambda ()
              (set (make-local-variable 'compile-command)
                   "cargo run")))
  )

(use-package yaml-mode
  :ensure t
  )

(use-package markdown-mode
  :ensure t)

(use-package csharp-mode
  :ensure t)

(use-package lua-mode
  :ensure t)

(use-package groovy-mode
  :ensure t)

(use-package gist
  :ensure t)

(use-package flycheck-flow
  :ensure t)

(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode)
  :config
  (progn
    (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)
    (flycheck-add-mode 'javascript-flow 'web-mode)
    (flycheck-add-mode 'javascript-eslint 'web-mode)
    (flycheck-add-next-checker 'javascript-flow 'javascript-eslint)))

(use-package flycheck-pyflakes
  :ensure t
  :disabled t)

(use-package emojify
  :ensure t
  :init
  (global-emojify-mode))

(use-package pyvenv
  :ensure t)

(defun set-icon-fonts (CODE-FONT-ALIST)
    "Utility to associate many unicode points with specified fonts."
    (--each CODE-FONT-ALIST
      (-let (((font . codes) it))
        (--each codes
          (set-fontset-font t `(,it . ,it) font)))))

(use-package all-the-icons
  :ensure t
  :config
  (set-icon-fonts
   '(("fontawesome"
      ;;                               :android:
      #xf07c #xf0c9 #xf0c4 #xf0cb #xf017 #xf101 #xf17b)

     ("all-the-icons"
      ;;    
      #xe907 #xe928)

     ("github-octicons"
      ;;                          
      #xf091 #xf059 #xf076 #xf075 #xe192  #xf016)

     ("material icons"
      ;;        
      #xe871 #xe918 #xe3e7 #xe859))))

(use-package all-the-icons-dired
  :ensure t
  :config
  (add-hook 'dired-mode-hook
            (lambda ()
              (all-the-icons-dired-mode 1)
              )))

(global-set-key (kbd "C-x C-b") 'ibuffer)

;;;; Remote shortcuts
(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'drkm-fav)

;;;; HideShow keybinding
(setq hs-minor-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-c <left>")   'hs-hide-block)
        (define-key map (kbd "C-c <right>")   'hs-show-block)
        (define-key map (kbd "C-c <up>")	  'hs-hide-all)
        (define-key map (kbd "C-c <down>")	  'hs-show-all)
        (define-key map (kbd "C-c M-<left>")   'hs-hide-level)
        (define-key map [(shift mouse-2)] 'hs-mouse-toggle-hiding)
        map))

;;;; Pressing d will copy both A and B to buffer C
(defun ediff-copy-both-to-C ()
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
(defun add-d-to-ediff-mode-map () (define-key ediff-mode-map "d" 'ediff-copy-both-to-C))
(add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map)

;;;; Re-read dir-locals of current buffer
(defun my-reload-dir-locals-for-current-buffer ()
  "reload dir locals for the current buffer"
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))

(tool-bar-mode -1)
(setq column-number-mode t)

;;;; Machine Specific
(if (string-equal system-type "windows-nt")
    (setq custom-file "~/.emacs.d/custom.win.el")
  )

(if (string-equal system-type "darwin")
    (setq custom-file "~/.emacs.d/custom.mac.el")
  )

(if (string-equal system-type "gnu/linux")
    (setq custom-file "~/.emacs.d/custom.linux.el")   )

;; Customize File
(if custom-file
    (load custom-file 'noerror))

;;;; Set matching fonts (defined in custom.*.el)
(set-font emacs-english-font emacs-chinese-font emacs-font-size-pair)
