;;;; Coding system: utf-8 as default
(setq buffer-file-coding-system 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)

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

(set-background-color "black")
(set-foreground-color "wheat")
(add-to-list 'default-frame-alist '(height . 45))
(add-to-list 'default-frame-alist '(width . 125))

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
;; Customize File
(setq custom-file "~/.emacs.d/custom.el")
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
  :mode "\\.tpl\\|\\.erb\\|\\.html?\\|\\.jinja2\\'")

;;;;; Emmet auto-complete html tags
(use-package emmet-mode
  :ensure t
  :defer t
  :init
  (add-hook 'web-mode-hook 'emmet-mode))

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

;; Python backend
(use-package company-anaconda
  :ensure t
  :disabled t
  :init
  (progn
    (add-to-list 'company-backends 'company-anaconda)))

(use-package ac-js2
  :ensure t
  :disabled
  :init
  (add-hook 'js2-mode-hook 'ac-js2-mode)
  )

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

;;;; Remote shortcuts
(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'drkm-fav)

;;;; Machine Specific
(load "~/.emacs.d/custom.el" 'noerror)