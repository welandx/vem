;; chinese input
(use-package rime
  :custom
  (default-input-method "rime")
  :config
  (define-key rime-mode-map (kbd "C-i") 'rime-force-enable)
  (setq rime-user-data-dir "~/.config/erime")
  (setq rime-show-candidate 'posframe)
  (setq rime-disable-predicates
        '(
          rime-predicate-space-after-cc-p
          ;; rime-predicate-tex-math-or-command-p
          rime-predicate-current-input-punctuation-p
          rime-predicate-ace-window-p
          rime-predicate-after-alphabet-char-p
          meow-normal-mode-p
          rime-predicate-current-uppercase-letter-p
          )))

;; modeline
;; (use-package doom-modeline
;;   :ensure t
;;   :init (doom-modeline-mode 1))

;; org
(use-package org-roam
  :ensure t
  :init
  (defvar org-roam-capture-templates
    '(("d" "default" plain "%?"
       :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                          "#+title: ${title}\n#+filetags: ")
       :unnarrowed t)) )
  :custom
  (org-roam-directory (file-truename "~/org/roam"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n a" . org-agenda)
         ("C-c n o" . org-roam-ui-open)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-goto-today))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:16}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-direction)
                 (direction . right)
                 (window-width . 0.33)
                 (window-height . fit-window-to-buffer))))

(use-package org-roam-ui
  :ensure t
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t)
  (setq org-roam-ui-custom-theme 'plain)
  )

(with-eval-after-load 'org
  (setq-default org-image-actual-width 400)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.8))
  ;; (define-key org-mode-map (kbd "C-c e") 'org-edit-latex-fragment)
(setq org-agenda-files '("~/org/roam/daily"))
  (setq
   ;; Edit settings
   org-auto-align-tags nil
   org-tags-column 0
   org-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   ;; Org styling, hide markup etc.
   org-hide-emphasis-markers t
   org-pretty-entities t

   ;; Agenda styling
   org-agenda-tags-column 0
   org-agenda-block-separator ?─
   org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
   org-agenda-current-time-string
   "⭠ now ─────────────────────────────────────────────────")

  (global-org-modern-mode))

  ;; cn markers without space
  ;; (font-lock-add-keywords 'org-mode
  ;;                         '(("\\cc\\( \\)[/+*_=~][^a-zA-Z0-9]*?[/+*_=~]\\( \\)?\\cc?"
  ;;                            (1 (prog1 () (compose-region (match-beginning 1) (match-end 1) ""))))
  ;;                           ("\\cc?\\( \\)?[/+*_=~][^a-zA-Z0-9]*?[/+*_=~]\\( \\)\\cc"
  ;;                            (2 (prog1 () (compose-region (match-beginning 2) (match-end 2) "")))))
  ;;                         'append)
  ;; (define-key org-mode-map (kbd "C-c v") 'visible-mode)
  ;; (global-set-key (kbd "C-c p") 'org-latex-preview))

;; export without space
;; (with-eval-after-load 'ox
;;   (defun eli-strip-ws-maybe (text _backend _info)
;;     (let* ((text (replace-regexp-in-string
;;                   "\\(\\cc\\) *\n *\\(\\cc\\)"
;;                   "\\1\\2" text));; remove whitespace from line break
;;            ;; remove whitespace from `org-emphasis-alist'
;;            (text (replace-regexp-in-string "\\(\\cc\\) \\(.*?\\) \\(\\cc\\)"
;;                                            "\\1\\2\\3" text))
;;            ;; restore whitespace between English words and Chinese words
;;            (text (replace-regexp-in-string "\\(\\cc\\)\\(\\(?:<[^>]+>\\)?[a-z0-9A-Z-]+\\(?:<[^>]+>\\)?\\)\\(\\cc\\)"
;;                                            "\\1 \\2 \\3" text)))
;;       text))
;;   (add-to-list 'org-export-filter-paragraph-functions #'eli-strip-ws-maybe))

(with-eval-after-load 'org-roam
  (defvar org-roam-capture-templates
    '(("d" "default" plain "%?"
       :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                          "#+title: ${title}\n#+filetags:")
       :unnarrowed t)) )
  )
;; display in center
(use-package olivetti
  :ensure t
  :defer t
  :hook
  (org-mode . olivetti-mode))

;; scratch message
(setq initial-scratch-message ";; Happy hacking weland!

")

;; translate
;; (setq gts-translate-list '(("en" "zh")))
;;(global-set-key (kbd "F1") 'gts-do-translate)


;; Configure Elfeed
(use-package elfeed
  :ensure t
  :defer t
  :config
  (setq elfeed-db-directory (expand-file-name "elfeed" user-emacs-directory)
        elfeed-show-entry-switch 'display-buffer)
  (pixel-scroll-precision-mode) ;; Only available on 29
  :bind
  ("C-x w" . elfeed ))

;; Configure Elfeed with org mode
(use-package elfeed-org
  :ensure t
  :after elfeed
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/.emacs.d/elfeed.org")))


;; Latex auto activating snippets
(straight-use-package 'laas)
(use-package laas
  :hook
  (LaTeX-mode . laas-mode)
  (org-mode . laas-mode)
  :config ; do whatever here
  (aas-set-snippets 'laas-mode
                    ;; set condition!
                    :cond #'texmathp ; expand only while in math
                    "supp" "\\supp"
                    "On" "O(n)"
                    "O1" "O(1)"
                    "Olog" "O(\\log n)"
                    "Olon" "O(n \\log n)"
                    "ali" "aligned"
                    ;; bind to functions!
                    "int" (lambda () (interactive)
                            (yas-expand-snippet "\\int_{$1}^{$2} $0"))
                    "Sum" (lambda () (interactive)
                            (yas-expand-snippet "\\sum\\limits_{$1}^{$2} $0"))
                    "lim" (lambda () (interactive)
                            (yas-expand-snippet "\\lim\\limits_{$1}^{$2} $0"))
                    "Span" (lambda () (interactive)
                             (yas-expand-snippet "\\Span($1)$0"))
                    "beg" (lambda () (interactive)
                            (yas-expand-snippet "\\begin{$1}\n $0 \n \\end{$1}"))
                    "{" (lambda () (interactive)
                            (yas-expand-snippet "\{$1\} $0"))
                    "(" (lambda () (interactive)
                            (yas-expand-snippet "\($1\) $0"))
                    "bce" (lambda () (interactive)
                            (yas-expand-snippet "\\lbrace $1 \\rbrace"))
                    "pen" (lambda () (interactive)
                            (yas-expand-snippet "\\lparen $1 \\rparen"))
                    "bck" (lambda () (interactive)
                            (yas-expand-snippet "\\lbrack $1 \\rbrack"))
                    "pro" (lambda () (interactive)
                            (yas-expand-snippet "\\prod\\limits_{$1}^{$2} $0"))
                    ;; add accent snippets
                    :cond #'laas-object-on-left-condition
                    "qq" (lambda () (interactive) (laas-wrap-previous-object "sqrt"))))



;; LaTex
;; (straight-use-package 'org-fragtog)
;; (use-package org-fragtog
;;   :hook (org-mode . org-fragtog-mode))

;; html css
(setq org-html-coding-system 'utf-8)
(setq org-html-doctype "html5")
(setq org-html-head
      "<link rel='stylesheet' type='text/css' href='https://gongzhitaao.org/orgcss/org.css'/> ")


(use-package format-all
  :ensure t
  :defer t
  :bind
  ("C-c f" . format-all-buffer)
  )
(use-package consult-org-roam
  :ensure t
  :init
  (require 'consult-org-roam)
  ;; Activate the minor-mode
  (consult-org-roam-mode 1)
  :custom
  (consult-org-roam-grep-func #'consult-ripgrep)
  :config
  ;; Eventually suppress previewing for certain functions
  (consult-customize
   consult-org-roam-forward-links
   :preview-key (kbd "M-."))
  :bind
  ("C-c n r" . consult-org-roam-search))

(straight-use-package 'websocket)

(straight-use-package
 '(deno-bridge :type git :host github :repo "manateelazycat/deno-bridge"))

(straight-use-package
 '(insert-translated-name :type git :host github :repo "manateelazycat/insert-translated-name"))

(defun weland/test-websocket ()
  (interactive)

  (add-to-list 'load-path (expand-file-name "~/.emacs.d/straight/repos/deno-bridge"))
  (add-to-list 'load-path (expand-file-name "~/.emacs.d/straight/repos/insert-translated-name"))
  (add-to-list 'load-path (expand-file-name "~/.emacs.d/straight/repos/emacs-websocket"))

  (require 'deno-bridge)
  (require 'insert-translated-name)
  (use-package insert-translated-name
    :bind
    ("C-c s t" . insert-translated-name-insert))
  )

;; corfu-english-helper
(straight-use-package 'corfu)
(straight-use-package
 '(corfu-english-helper :type git :host github :repo "manateelazycat/corfu-english-helper"))

(with-eval-after-load 'org-roam
  ;; (defun my/enable-english-helper ()
  ;;   (interactive)
  ;;   (company-mode 0)
  ;;   (require 'corfu)
  ;;   (require 'corfu-english-helper)
  ;;   (corfu-mode 1)
  ;;   (toggle-corfu-english-helper))

  (defun my/delete-corfu ()
    (interactive)
    (require 'posframe)
    (corfu-mode 0)
    (company-mode 1)
    (posframe-delete " *corfu*")))


;; org-media & org-download
(straight-use-package 'pretty-hydra)
(straight-use-package
 '(org-media-note :type git :host github :repo "yuchen-lea/org-media-note"))
(use-package org-media-note
  :hook (org-mode .  org-media-note-mode)
  :bind (
         ("C-c h" . org-media-note-hydra/body))  ;; 主功能入口
  :config
  (setq org-media-note-screenshot-image-dir "~/org/imgs/")  ;; 用于存储视频截图的目录
  )


;; (with-eval-after-load 'org
;; (setq org-fontify-quote-and-verse-blocks t)
;;   (setq org-list-demote-modify-bullet
;;         '(("+" . "-") ("-" . "+") ("*" . "+") ("1." . "a.")))
;; (set-face-attribute 'outline-1 nil
;;                     :weight 'extra-bold :height 222)
;; (set-face-attribute 'outline-2 nil
;;                     :weight 'bold :height 1.15)
;; (set-face-attribute 'outline-3 nil
;;                     :weight 'bold :height 1.12)
;; (set-face-attribute 'outline-4 nil
;;                     :weight 'semi-bold :height 1.09)
;; (set-face-attribute 'org-document-title nil
;;                     :height 1.2)
;; )


(provide 'init-org)
