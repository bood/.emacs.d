(add-to-list 'exec-path '"/usr/local/bin")

(setenv "PATH"
        (concat
         "/usr/local/bin:"
         (getenv "PATH")))

(setenv "PATH"
        (concat
         "/Users/Bood/.cargo/bin:"
         (getenv "PATH")))

(add-hook 'java-mode-hook (lambda ()
                            (setq c-basic-offset 2)))

(setenv "PYTHONPATH"
        (concat
         "/Users/Bood/pythonpath:"
         (getenv "PYTHONPATH")))

(pyvenv-activate "/Users/Bood/virtualenv/devbox")

;; Matching fontset
(defvar emacs-english-font "Menlo")
(defvar emacs-chinese-font "Hiragino Sans GB")
(defvar emacs-font-size-pair '(15 . 18)
  "Default font size pair for (english . chinese)")

(defvar emacs-font-size-pair-list
  '(( 5 .  6) (10 . 12)
    (13 . 16) (15 . 18) (17 . 20)
    (19 . 22) (20 . 24) (21 . 26)
    (24 . 28) (26 . 32) (28 . 34)
    (30 . 36) (34 . 40) (36 . 44))
  "This list is used to store matching (englis . chinese) font-size.")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(commit-prefix-alist
   (quote
    (("fix" "Fix: ")
     ("adr" "Android: ")
     ("ios" "iOS: ")
     ("pa" "Patient App: ")
     ("ca" "Coach App: "))))
 '(company-dabbrev-downcase nil)
 '(company-idle-delay 0)
 '(company-minimum-prefix-length 2)
 '(default-frame-alist
    (quote
     ((vertical-scroll-bars)
      (height . 40)
      (width . 120)
      (foreground-color . "wheat")
      (background-color . "gray10"))))
 '(flycheck-flake8-maximum-line-length 200)
 '(org-src-fontify-natively t)
 '(package-selected-packages
   (quote
    (all-the-icons-dired all-the-icons groovy-mode flycheck-flow pyvenv emojify counsel magit-popup yasnippet yaml-mode yafolding xcscope web-mode use-package session relative-line-numbers rainbow-mode python-docstring pyenv-mode markdown-mode magit lua-mode json-rpc js2-mode highlight-indentation helm-projectile helm-gtags gist font-utils flycheck-pyflakes emmet-mode dired-single csharp-mode company-anaconda ace-jump-mode)))
 '(safe-local-variable-values (quote ((web-mode-enable-auto-indentation))))
 '(session-use-package t nil (session))
 '(user-mail-address "bood@glowing.com"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
