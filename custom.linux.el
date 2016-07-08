(add-hook 'java-mode-hook (lambda ()
                            (setq c-basic-offset 2)))

;; Matching fontset
(defvar emacs-english-font "Monaco")
(defvar emacs-chinese-font "Microsoft Yahei")
(defvar emacs-font-size-pair '(14 . 16)
  "Default font size pair for (english . chinese)")

(defvar emacs-font-size-pair-list
  '((11 . 12) (12 . 14)
    (14 . 16) (16 . 18) (18 . 20))
  "This list is used to store matching (englis . chinese) font-size.")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default-frame-alist
    (quote
     ((vertical-scroll-bars)
      (height . 45)
      (width . 125)
      (foreground-color . "wheat")
      (background-color . "black"))))
 '(org-src-fontify-natively t)
 '(package-selected-packages
   (quote
    (font-utils yasnippet yaml-mode yafolding xcscope web-mode use-package session rust-mode relative-line-numbers rainbow-mode markdown-mode magit lua-mode js2-mode highlight-indentation helm-projectile helm-gtags gist flycheck-pyflakes emmet-mode dired-single csharp-mode company-anaconda ace-jump-mode)))
 '(session-use-package t nil (session))
 '(user-mail-address "boodweb@gmail.com"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
