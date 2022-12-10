;; lsp
;; (when (file-exists-p "~/.emacs.d/site-lisp/lsp-bridge")
;;   (add-to-list 'load-path "~/.emacs.d/site-lisp/lsp-bridge")
;;   (require 'yasnippet)
;;   (yas-global-mode 1)
;;   (require 'lsp-bridge)
;;   (setq acm-enable-search-words nil)
;;   (setq lsp-bridge-enable-search-words nil)
;;   (setq lsp-bridge-default-mode-hooks
;;         (remove 'org-mode-hook lsp-bridge-default-mode-hooks))
;;   (global-lsp-bridge-mode)
;;   (global-set-key (kbd "C-c C-d") 'lsp-bridge-find-def)
;;   (global-set-key (kbd "C-c C-r") 'lsp-bridge-find-references)
;;   (global-set-key (kbd "C-c C-r") 'lsp-bridge-code-format)

;;   (set-face-attribute 'lsp-bridge-alive-mode-line nil
;;                       :foreground "#61649f"
;;                       :weight 'normal)
;;   )

(straight-use-package
 '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"))

(use-package lsp-bridge
  :init
    (add-to-list 'load-path "~/.emacs.d/site-lisp/lsp-bridge")
    (add-to-list 'load-path "~/.emacs.d/site-lisp/lsp-bridge/acm")
  (require 'yasnippet)
  (yas-global-mode 1)
  (require 'lsp-bridge)
  :config
  (setq acm-enable-search-words nil)
  (setq lsp-bridge-enable-search-words nil)
  (setq lsp-bridge-default-mode-hooks
        (remove 'org-mode-hook lsp-bridge-default-mode-hooks))
  (global-lsp-bridge-mode)
  (global-set-key (kbd "C-c C-d") 'lsp-bridge-find-def)
  (global-set-key (kbd "C-c C-r") 'lsp-bridge-find-references)
  (global-set-key (kbd "C-c C-r") 'lsp-bridge-code-format)

  (set-face-attribute 'lsp-bridge-alive-mode-line nil
                      :foreground "#61649f"
                      :weight 'normal)
  )

(provide 'init-lsp)
