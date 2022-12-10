(straight-use-package
 '(auto-save :type git :host github :repo "manateelazycat/auto-save"))

;; (add-to-list 'load-path "/home/weland/.emacs.d/site-lisp/auto-save") ; add auto-save to your load-path
(require 'auto-save)
(auto-save-enable)

(setq auto-save-silent t)   ; quietly save
(setq auto-save-delete-trailing-whitespace t)  ; automatically delete spaces at the end of the line when saving

;;; custom predicates if you don't want auto save.
;;; disable auto save mode when current filetype is an gpg file.
(setq auto-save-disable-predicates
      '((lambda ()
          (string-suffix-p
           "gpg"
           (file-name-extension (buffer-name)) t))))

(straight-use-package 'tree-sitter)
(straight-use-package 'tree-sitter-langs)

;; (straight-use-package
;;  '(grammatical-edit :type git :host github :repo "manateelazycat/grammatical-edit"))


    (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
    (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
    (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
    (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
    (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
    (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
    (add-hook 'scheme-mode-hook           #'enable-paredit-mode)



;; (require 'grammatical-edit)
;; (dolist (hook (list
;;                'org-mode-hook
;;                'latex-mode-hook
;;                'tex-mode-hook
;;                'c-mode-common-hook
;;                'c-mode-hook
;;                'c++-mode-hook
;;                'java-mode-hook
;;                'haskell-mode-hook
;;                'emacs-lisp-mode-hook
;;                'lisp-interaction-mode-hook
;;                'lisp-mode-hook
;;                'maxima-mode-hook
;;                'ielm-mode-hook
;;                'sh-mode-hook
;;                'makefile-gmake-mode-hook
;;                'php-mode-hook
;;                'python-mode-hook
;;                'js-mode-hook
;;                'go-mode-hook
;;                'qml-mode-hook
;;                'jade-mode-hook
;;                'css-mode-hook
;;                'ruby-mode-hook
;;                'coffee-mode-hook
;;                'rust-mode-hook
;;                'qmake-mode-hook
;;                'lua-mode-hook
;;                'swift-mode-hook
;;                'minibuffer-inactive-mode-hook
;;                'typescript-mode-hook
;;                ))
;;   (add-hook hook '(lambda () (grammatical-edit-mode 1))))


;; (define-key grammatical-edit-mode-map (kbd "(") 'grammatical-edit-open-round)
;; (define-key grammatical-edit-mode-map (kbd "[") 'grammatical-edit-open-bracket)
;; (define-key grammatical-edit-mode-map (kbd "{") 'grammatical-edit-open-curly)
;; (define-key grammatical-edit-mode-map (kbd ")") 'grammatical-edit-close-round)
;; (define-key grammatical-edit-mode-map (kbd "]") 'grammatical-edit-close-bracket)
;; (define-key grammatical-edit-mode-map (kbd "}") 'grammatical-edit-close-curly)
;; (define-key grammatical-edit-mode-map (kbd "=") 'grammatical-edit-equal)

;; (define-key grammatical-edit-mode-map (kbd "%") 'grammatical-edit-match-paren)
;; (define-key grammatical-edit-mode-map (kbd "\"") 'grammatical-edit-double-quote)
;; (define-key grammatical-edit-mode-map (kbd "'") 'grammatical-edit-single-quote)

;; (define-key grammatical-edit-mode-map (kbd "SPC") 'grammatical-edit-space)
;; (define-key grammatical-edit-mode-map (kbd "RET") 'grammatical-edit-newline)

;; (define-key grammatical-edit-mode-map (kbd "M-o") 'grammatical-edit-backward-delete)
;; (define-key grammatical-edit-mode-map (kbd "C-d") 'grammatical-edit-forward-delete)
;; (define-key grammatical-edit-mode-map (kbd "C-k") 'grammatical-edit-kill)

;; (define-key grammatical-edit-mode-map (kbd "M-\"") 'grammatical-edit-wrap-double-quote)
;; (define-key grammatical-edit-mode-map (kbd "M-'") 'grammatical-edit-wrap-single-quote)
;; (define-key grammatical-edit-mode-map (kbd "M-[") 'grammatical-edit-wrap-bracket)
;; (define-key grammatical-edit-mode-map (kbd "M-{") 'grammatical-edit-wrap-curly)
;; (define-key grammatical-edit-mode-map (kbd "M-(") 'grammatical-edit-wrap-round)
;; (define-key grammatical-edit-mode-map (kbd "M-)") 'grammatical-edit-unwrap)

;; (define-key grammatical-edit-mode-map (kbd "M-p") 'grammatical-edit-jump-right)
;; (define-key grammatical-edit-mode-map (kbd "M-n") 'grammatical-edit-jump-left)
;; (define-key grammatical-edit-mode-map (kbd "M-:") 'grammatical-edit-jump-out-pair-and-newline)

;; (define-key grammatical-edit-mode-map (kbd "C-j") 'grammatical-edit-jump-up)


(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/auto-scroll"))
(require 'auto-scroll)


(provide 'init-editor)
