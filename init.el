;;;; Coding system: utf-8 as default
(setq buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;;;; Paths
(add-to-list 'exec-path (concat (getenv "CYGWIN_HOME") "/bin"))
(add-to-list 'exec-path "d:/MinGW/bin")
(add-to-list 'exec-path "d:/Tools/putty")
(add-to-list 'exec-path "d:/Tools/clang/bin")

;;;; ELPA settings
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
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
(when (display-graphic-p)
  (use-package font-utils :ensure t)
  (setq fonts '("Consolas" "Source Code Pro" "Menlo" "Monaco" "DejaVu Sans Mono")
        zh-fonts '("Noto Sans S Chinese" "Hiragino Sans GB" "STHeiti" "Microsoft Yahei" "WenQuanYi Zen Hei"))

  (set-face-attribute 'default nil :font
                      (format "%s:pixelsize=%d" (font-utils-first-existing-font fonts) 15))
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font) charset
                      (font-spec :family (font-utils-first-existing-font zh-fonts)))))

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
;; Basic Emacs config

;; Start server
(server-start)
;; Prevent the cursor from blinking
(blink-cursor-mode 0)
;; Don't use messages that you don't read
(setq initial-scratch-message "")
(setq inhibit-startup-message t)
;; Don't let Emacs hurt your ears
(setq visible-bell t)
;; Who use the bar to scroll?
(scroll-bar-mode 0)
;(tool-bar-mode 0)
(menu-bar-mode 0)
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
(setq tramp-default-method "ftp")

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
(use-package dired-single :ensure t)
(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map (kbd "RET") 'joc-dired-single-buffer)
            (define-key dired-mode-map (kbd "<mouse-1>") 'joc-dired-single-buffer-mouse)
            (define-key dired-mode-map (kbd "^")
              (lambda ()
                (interactive)
                (joc-dired-single-buffer "..")))))

;;;; Favourite Directories
;; TODO: submit to MELPA
(use-package drkm-fav)
(setq drkm-userhost "qianhaib@tucsdwb32.tucson.ibm.com")
(setq drkm-hurgsa "qianhaib@hurgsa.ibm.com")
(setq drkm-tucgsa "qianhaib@tucgsa.ibm.com")
(setq drkm-xenium "qianhaib@xenium.ssd.hursley.ibm.com")
(setq drkm-cobalt "pscp:qianhaib@cobalt.ssd.hursley.ibm.com")
(setq drkm-a5build "pscp:qianhaib@a5build.shanghai.cn.ibm.com")
(setq drkm-teststand "pscp:root@panda1.shanghai.cn.ibm.com")
(setq drkm-qundong-root "/pscp:root@218.244.137.221:")
(setq drkm-root (concat "/" (concat drkm-userhost ":")))
(setq drkm-hurgsa-root (concat "/" (concat drkm-hurgsa ":")))
(setq drkm-tucgsa-root (concat "/" (concat drkm-tucgsa ":")))
(setq drkm-xenium-root (concat "/" (concat drkm-xenium ":")))
(setq drkm-cobalt-root (concat "/" (concat drkm-cobalt ":")))
(setq drkm-a5build-root (concat "/" (concat drkm-a5build ":")))
(setq drkm-teststand-root (concat "/" (concat drkm-teststand ":")))
(setq drkm-fav:favourite-directories-alist
  (list (cons "home"  (concat drkm-root "~"))
        (cons "tucgsa"  (concat drkm-tucgsa-root "/gsa/tucgsa/home/q/i/qianhaib"))
        (cons "hurgsa"  (concat drkm-hurgsa-root "/gsa/hurgsa/home/q/i/qianhaib"))
        (cons "root"  drkm-root)
        (cons "mmbuild" (concat drkm-root "/gsa/tucgsa/projects/m/mmbuild"))
        (cons "_0c" (concat drkm-root "/mc/R0.CPSS/cur/cmvc"))
        (cons "_0" (concat drkm-root "~/dev2000/mc/R0.CPSS/usr/cmvc"))
        (cons "usrstg" (concat drkm-root "~/dev2000/mc"))
        (cons "qundong" (concat drkm-qundong-root "/var/www/i_family_health"))
        (cons "a4sb" (concat drkm-xenium-root "/work/qianhaib/sb"))
        (cons "a5sb" (concat drkm-a5build-root "/work/qianhaib/sb"))
        (cons "teststand" (concat drkm-teststand-root "/wideopen/qianhaib"))
        (cons "a5sb-hursley" (concat drkm-cobalt-root "/work/qianhaib/sb"))
        (cons "wideopen" (concat drkm-a5build-root "/wideopen/qianhaib"))
        (cons "wideopen-hursley" (concat drkm-xenium-root "/wideopen/qianhaib"))
        (cons "linuxhome" "/qianhaib@9.125.91.76:~")))

;;;; Editing

;;;;; Word wrap
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;;;;; Show matched parens
(show-paren-mode)
(electric-pair-mode)

;;;;; Show relative line numbers
(use-package relative-line-numbers
  :ensure t)

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

;;;;; Enhanced javascript mode
(use-package js2-mode
  :ensure t
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
  (yas-global-mode))

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
  (progn
    (add-hook 'after-init-hook 'global-company-mode)))

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
                   ("150.4F - Arrowhead 4.3"
                    (filename . "/20101500004F/"))
                   ("F.07 - 7.4"
                    (filename . "/281000F00007"))
                   ("B.42 - 4K"
                    (filename . "/281000B00042/"))
                   ("EM-210"
                    (filename . "/lodestone210/"))
                   ("i_family_health"
                    (filename . "/i_family_health/"))
                   ("i_family_health Backend"
                    (filename . "/i_family_health_backend/"))
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

(global-set-key (kbd "C-x C-b") 'ibuffer)

;;;; Machine Specific
(load "~/.emacs.d/custom.el" 'noerror)

;;;; Customized Variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ange-ftp-ftp-program-name "~/ftp.exe")
 '(c-basic-offset 4)
 '(compile-command "mingw32-make -k ")
 '(default-frame-alist (quote ((tool-bar-lines . 1) (width . 125) (height . 45) (menu-bar-lines . 1))))
 '(diff-command "tkdiff")
 '(diff-switches "")
 '(ediff-custom-diff-options "-cb")
 '(g-curl-common-options "-k --http1.0 --compressed --silent --location --location-trusted")
 '(ibuffer-saved-filter-groups nil)
 '(ibuffer-saved-filters (quote (("emacs-config" ((filename . "/.emacs.d/"))) ("i_family_health" ((filename . "projects/i_family_health/"))) ("B.42" ((filename . "/281000B00042/"))) ("gnus" ((or (mode . message-mode) (mode . mail-mode) (mode . gnus-group-mode) (mode . gnus-summary-mode) (mode . gnus-article-mode)))) ("programming" ((or (mode . emacs-lisp-mode) (mode . cperl-mode) (mode . c-mode) (mode . java-mode) (mode . idl-mode) (mode . lisp-mode)))))))
 '(indent-tabs-mode nil)
 '(org-agenda-files (quote ("~/org/todo/misc.org" "~/org/todo/work.org")))
 '(org-agenda-include-diary nil)
 '(org-mobile-directory "~/org/mobile-publish")
 '(password-cache-expiry 86400)
 '(tab-width 4)
 '(tramp-remote-path (quote (tramp-own-remote-path tramp-default-remote-path "/usr/sbin" "/usr/local/bin" "/local/bin" "/local/freeware/bin" "/local/gnu/bin" "/usr/freeware/bin" "/usr/pkg/bin" "/usr/contrib/bin")))
 '(transient-mark-mode t)
 '(user-full-name "Bood")
 '(user-mail-address "boodweb@gmail.com"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
