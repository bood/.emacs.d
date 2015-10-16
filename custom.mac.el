(setq exec-path (append exec-path '("/usr/local/bin")))
(add-hook 'java-mode-hook (lambda ()
                            (setq c-basic-offset 2)))

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
 '(default-frame-alist
    (quote
     ((vertical-scroll-bars)
      (height . 35)
      (width . 100)
      (foreground-color . "wheat")
      (background-color . "black"))))
 '(org-src-fontify-natively t)
 '(session-use-package t nil (session))
 '(user-mail-address "bood@glowing.com"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
