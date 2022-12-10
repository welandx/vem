
;; (let ((normal-gc-cons-threshold (* 20 1024 1024))
;;       (init-gc-cons-threshold (* 128 1024 1024)))
;;   (setq gc-cons-threshold init-gc-cons-threshold)
;;   (add-hook 'emacs-startup-hook
;;             (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(defun open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(global-set-key (kbd "C-c i") 'open-init-file)

(add-to-list 'load-path "~/.emacs.d/lisp/")

(straight-use-package 'exec-path-from-shell)

(straight-use-package 'ef-themes)


;; idle load
(defun leo/load-file (file)
  "Load an elisp file from .emacs.d"
  (load (concat "~/.emacs.d/lisp/" file)))
(defun leo/load-idle (sec file)
  "Load a file after N idle seconds"
  (run-with-idle-timer sec nil 'leo/load-file file))


;; Write auto-saves and backups to separate directory.
(setq make-backup-files nil)
;; (make-directory "~/.tmp/emacs/auto-save/" t)
;; (setq auto-save-file-name-transforms '((".*" "~/.tmp/emacs/auto-save/" t)))
;; (setq backup-directory-alist '(("." . "~/.tmp/emacs/backup/")))

(setq auto-save-default nil)
;; Do not move the current file while creating backup.
(setq backup-by-copying t)

;; Disable lockfiles.
(setq create-lockfiles nil)

;; Workaround for https://debbugs.gnu.org/34341 in GNU Emacs <= 26.3.
(when (and (version< emacs-version "26.3") (>= libgnutls-version 30603))
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

;; Write customizations to a separate file instead of this file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t)


(require 'init-package)

(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)


(require 'init-default)
(require 'init-ui)

(defun weland/html2org-clipboard-wayland ()
  "Convert clipboard contents from HTML to Org and then paste (yank)."
  (interactive)
  (kill-new (shell-command-to-string "wl-paste -t text/html | pandoc -f html -t json | pandoc -f json -t org
"))
  (yank))

(require 'init-keymap)
(require 'init-org)
(require 'init-lsp)
;; Start server.
;; (require 'server)
;; (unless (server-running-p)
;;   (server-start))

(straight-use-package 'vertico)
(use-package vertico
  :config
  (vertico-mode))

(straight-use-package 'orderless)
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))


;; Configure directory extension.
(use-package vertico-directory
  :after vertico
  :load-path "straight/build/vertico/extensions"
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(with-eval-after-load 'vertico
  (require 'pyim-cregexp-utils)
  (defun my-orderless-regexp (orig-func component)
    (let ((result (funcall orig-func component)))
      (pyim-cregexp-build result)))

  (advice-add 'orderless-regexp :around #'my-orderless-regexp))
;; Init Frame?
;; (set-face-attribute 'default nil :height 180)

;; (set-face-attribute 'bold nil
;;                     :font (font-spec :family "WenQuanYi Zen Hei")
;;                     :weight 'bold
;;                     :background nil)

;; Switch buffer and search in buffer
;; (with-eval-after-load 'consult
;;   (global-set-key (kbd "C-c b") 'consult-buffer)
;;   (global-set-key (kbd "C-s") 'consult-line))
;; (use-package consult
;;   :config
;;   (global-set-key (kbd "C-c b") 'consult-buffer)
;;   (global-set-key (kbd "C-s") 'consult-line))




;; window jump
(global-set-key (kbd "C-c w") 'other-window)
(global-set-key (kbd "C-c b") 'switch-to-buffer)

;; (use-package dirvish
;;   :ensure t
;;   :config
;;   (dirvish-override-dired-mode))

(require 'init-editor)

(require 'init-app)

;; (require 'init-timer)

;; =================================================
;; ===== The following has not been archived =======
;; =================================================

(defun weland/config-linums ()
  (interactive)
  (message (concat (shell-command-to-string "wc -l ~/.emacs.d/init.el ~/.emacs.d/lisp/* | tail -1 | awk '{print $1}' | tr -s '\n' ' '") "lines"))
  )

(defun weland/delete-posframe (&optional subtracttime)
  (interactive)
  (posframe-delete " *acm-doc-buffer*")
  (posframe-delete " *rime-posframe*")
  (posframe-delete " *acm-buffer*")
  (posframe-delete " *pyim-page--posframe-buffer*")
  (posframe-delete " *corfu*")
  (posframe-delete " *Minibuf-1*")
  (posframe-delete-all))

(with-eval-after-load 'buffer-timer
  (advice-add 'buffer-timer-go-idle :before #'weland/delete-posframe))


(with-eval-after-load 'lsp-bridge
  (setq mode-line-misc-info
        '(
          ;; (lsp-bridge-mode
          ;;  (" " lsp-bridge--mode-line-format " "))
          ;; (global-mode-string
          ;;  ("" global-mode-string))
          )))

(straight-use-package 'magit)


(defun weland/enable-english-helper ()
  (interactive)
  (corfu-mode 1)
  (toggle-corfu-english-helper))


(defun suppress-message-advice-around (fun &rest args)
  (let (message-log-max)
    (with-temp-message (or (current-message) "")
      (apply fun args))))
;; example: suppress any messages from `save-buffer'
(advice-add 'save-buffer :around 'suppress-message-advice-around)
;; undo:
;;(advice-remove 'save-buffer 'suppress-message-advice-around)

;; (defmacro with-suppressed-message (&rest body)
;;   "Suppress new messages temporarily in the echo area and the `*Messages*' buffer while BODY is evaluated."
;;   (declare (indent 0))
;;   (let ((message-log-max nil))
;;     `(with-temp-message (or (current-message) "") ,@body)))

;; (with-suppressed-message (save-buffer))
;; (leo/load-idle 3 "init-timer.el")
(require 'init-timer)

(straight-use-package 'ox-hugo)
(with-eval-after-load 'ox
  (require 'ox-hugo)
  (setq org-hugo-base-dir "~/Documents/welandx.github.io/")
  (setq org-hugo-default-section-directory "posts")


  ;; this function will result poor performence
  (defun weland/link-for-hugo ()
    "expand org id, manually call it due to leistung"
    (interactive)
    (require 'find-lisp)
    ;; (let ((org-id-extra-files (find-lisp-find-files org-roam-directory "\.org$")))
    ;;     (org-hugo-export-wim-to-md))
    (setq org-id-extra-files (find-lisp-find-files org-roam-directory "\.org$")))

  (defun benmezger/org-roam-export-all (publish-dir)
    "Re-exports all Org-roam files to Hugo markdown."
    (interactive)
    ;; (dolist (f (org-roam-list-files))
    (dolist (f  (directory-files publish-dir t ".org"))
      (with-current-buffer (find-file f)
        (org-hugo-export-wim-to-md))))

  (defun weland/org-roam-export ()
    "export: politics math cs roam-root"
    (interactive)
    (benmezger/org-roam-export-all "/home/weland/org/roam/politics")
    (benmezger/org-roam-export-all "/home/weland/org/roam/math")
    (benmezger/org-roam-export-all "/home/weland/org/roam/computer-science")
    (benmezger/org-roam-export-all "/home/weland/org/roam"))
  )

;; (straight-use-package 'esup)
;; (straight-use-package 'frimacs)


(with-eval-after-load 'telega
  (defun get-key-combo (key)
    "Just return the key combo entered by the user"
    (interactive "kKey combo: ")
    key)

  (defun keymap-unset-key (key keymap)
    "Remove binding of KEY in a keymap KEY is a string or vector
representing a sequence of keystrokes."
    (interactive
     (list (call-interactively #'get-key-combo)
	       (completing-read "Which map: " minor-mode-map-alist nil t)))
    (let ((map (rest (assoc (intern keymap) minor-mode-map-alist))))
      (when map
        (define-key map key nil)
        (message  "%s unbound for %s" key keymap))))

  (keymap-unset-key "	" "rime-mode"))


;; display time on modeline
(setq display-time-format "%I:%M:%S")

(setq display-time-interval 1)


;; if use zenburn-theme ==============================
;; use variable-pitch fonts for some headings and titles
(setq zenburn-use-variable-pitch t)

;; scale headings in org-mode
(setq zenburn-scale-org-headlines t)

;; scale headings in outline-mode
(setq zenburn-scale-outline-headlines t)

;; fi use zenburn-theme ===============================

(setq diary-file
      (concat org-directory "/private/standard-diary.org"))

;; (setq org-agenda-include-diary nil)

(straight-use-package 'vterm)

(use-package vterm
  :bind
  ("C-c p t" . vterm))

(with-eval-after-load 'org
  (setq org-todo-keywords
        '((sequence "TODO"  "DONE"  "READLATER" "KILL" "WAIT")))
  (setq org-todo-keyword-faces
        '(("WAIT" . "orange") ("READLATER" . "magenta") ("KILL" . "grey") ("DONE" . "green"))))

(use-package consult
  :bind
  (("C-c s r" . consult-ripgrep)
   ("C-c s b" . consult-buffer)))

;; (straight-use-package 'apheleia)

(straight-use-package 'vundo)


(setq org-capture-templates `(
	                          ("p" "Protocol" entry (file+headline ,(concat org-directory "notes.org") "Inbox")
                               "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
	                          ("L" "Protocol Link" entry (file+headline ,(concat org-directory "notes.org") "Inbox")
                               "* %? [[%:link][%:description]] \nCaptured On: %U")
                              ))


(add-hook 'elfeed-show-mode-hook 'auto-scroll-mode)
(add-hook 'elfeed-new-entry-hook 'auto-scroll-mode)

(add-hook 'elfeed-show-mode-hook 'iscroll-mode)
(add-hook 'elfeed-new-entry-hook 'iscroll-mode)

;; (defadvice elfeed-search-show-entry (around elfeed-search-show-entry-around activate)
;;   "Open entries in a new buffer below."
;;   (ic/mark-current-as-read)
;;   (ic/elfeed-delete-show-windows)
;;   (ic/split-and-follow-vertically)
;;   ad-do-it)


;; (defun ic/mark-current-as-read ()
;;   (interactive)
;;   "Mark current entry as read."
;;   (let ((current (elfeed-search-selected :ignore-region)))
;;     (elfeed-untag current 'unread)
;;     (elfeed-search-update-entry current)
;;     (elfeed-db-save-safe)))

(setq dired-mouse-drag-files t)

;; (straight-use-package
;;  '(awesome-tray :type git :host github :repo "manateelazycat/awesome-tray"))


;; (require 'awesome-tray)
;; (awesome-tray-mode 1)

;; (set-face-attribute 'mode-line nil
;;                     :foreground "#999"
;;                     :background "#999")

;; (when (powerline-selected-window-active)
;; (set-face-attribute 'mode-line-inactive nil
;;                     :underline t
;;                     :background (face-background 'default))
;;           )


;; change modeline color by meow&ime state
(defconst my-default-color (cons (face-background 'mode-line)
                                 (face-foreground 'mode-line)))

(defun my-show-meow-state ()
  "Change modeline color to notify user current ime & meow state."
  (let ((color (cond
                ((minibufferp)
                 my-default-color)
                ;; 因为各种状态有重叠，以下顺序影响最终效果，重要的条目往前排
                (current-input-method
                 '("#d1d2d3" . "#ffffff"))
                ((meow-normal-mode-p)
                 my-default-color)
                ((meow-insert-mode-p)
                 '("#aa2021" . "#ffffff"))
                ((meow-motion-mode-p)
                 '("#444488" . "#ffffff"))
                ;; ((meow-beacon-mode-p)
                ;;  my-default-color)
                ;; ((meow-keypad-mode-p)
                ;;  my-default-color)
                ;; ((buffer-modified-p)
                ;;  '("#006fa0" . "#ffffff"))
                (t
                 my-default-color))))
    (set-face-background 'mode-line (car color))
    (set-face-foreground 'mode-line (cdr color))))

;; (add-hook 'post-command-hook #'my-show-meow-state)
;; 可能有用的hook，根据需要启用:
;; 1. 若开启了老猫auto-save或者super-save，则用after-save-hook触发更新(buffer-modified-p)
;; (add-hook 'after-save-hook #'my-show-meow-state)
;; 2. 根据窗口变动
;; (add-hook 'window-configuration-change-hook #'my-show-meow-state)
;; 3. 根据聚焦窗口变动
;; (add-function :after after-focus-change-function #'my-show-meow-state)

(defun weland/deno-jieba ()
  (interactive)
  (add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/deno-bridge-jieba"))
  (require 'deno-bridge-jieba)
  (deno-bridge-jieba-start)
  (require 'epc)
  (meow-normal-define-key
   '("e" . deno-bridge-jieba-forward-word)
   '("b" . deno-bridge-jieba-backward-word))
  (advice-add 'forward-word :around #'deno-bridge-jieba-forward-word)
  (advice-remove 'forward-word 'deno-bridge-jieba-forward-word))

(defun weland/cnhl ()
  (interactive)
  (add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/cnhl"))
  (require 'cnhl)
  (setq cnhl-thulac-module-path
	    (expand-file-name "~/Downloads/cppthulac/models"))
  (setq cnhl-nlp-selected "thulac")
  (cnhl-use-dependency 'hl))

(defun weland/forward-cn (&optional ARG)
  (denote-bridge-jieba-forward-word))

;; (advice-add 'forward-word :override #'weland/forward-cn)
;; (setq cnhl-nlp-selected "thulac")

(require 'org-download)

(use-package wolfram
  :config
  (setq wolfram-alpha-app-id "HUR2VQ-K8UE7267R9"))

(use-package tab-bar
  :custom-face
  (tab-bar ((t (:background "#LightGreen"))))
  (tab-bar-tab-inactive ((t (:background "#555"))))
  )

(straight-use-package 'tab-bar-groups)

(use-package tab-bar-groups
  :demand
  :config
  (tab-bar-groups-activate)
  :bind (:map tab-prefix-map
              ("g0" . tab-bar-groups-close-group-tabs)
              ("g2" . tab-bar-groups-new-tab)
              ("ga" . tab-bar-groups-change-tab-group)
              ("gg" . tab-bar-groups-regroup-tabs)
              ("gd" . tab-bar-groups-duplicate-tab)
              ("gk" . tab-bar-groups-eject-tab)
              ("gr" . tab-bar-groups-rename-group)))

;; (straight-use-package 'desktop)

;; font weight
(use-package emacs
  :custom-face
  (default ((t (:weight normal)))))


;; webp
(use-package image
  :custom
  (image-use-external-converter t))

(use-package popweb
  :load-path "./site-lisp/popweb")
(use-package popweb-org-roam-link
  :after popweb
  :load-path "./site-lisp/popweb/extension/org-roam")
(use-package popweb-url
  :after popweb
  :load-path "./site-lisp/popweb/extension/url-preview")


;; ?
(setq scroll-preserve-screen-position t
      scroll-margin 2
      scroll-conservatively 0)

;; color
(use-package sky-color-clock
  :load-path "./site-lisp/sky-color-clock"
  :config
  (sky-color-clock-initialize 40)
  ;; (setq global-mode-string (append global-mode-string (list " ")))
  (setq global-mode-string (append global-mode-string (list '(:eval (sky-color-clock)))))
  )
;; (append '(:eval (sky-color-clock)) (default-value 'global-mode-string))
(use-package iscroll
  :hook
  (org-mode-hook . iscroll-mode))
(defun weland/remove-sky ()
  (setq global-mode-string
        (cl-remove '(:eval (sky-color-clock)) global-mode-string
                   :test 'equal)))

(with-eval-after-load 'meow
  (defun weland/meow-setup-indicator ()
    "Setup indicator appending the return of function
`meow-indicator' to the modeline.

This function should be called after you setup other parts of the mode-line
 and will work well for most cases.

If this function is not enough for your requirements,
use `meow-indicator' to get the raw text for indicator
and put it anywhere you want."
    (unless (cl-find '(:eval (meow-indicator)) global-mode-string :test 'equal)
      (setq global-mode-string (append '((:eval (meow-indicator))) global-mode-string))))

  (advice-add 'meow-setup-indicator :around #'weland/meow-setup-indicator)

  (defun weland/meow--remove-modeline-indicator ()
    (setq global-mode-string
          (cl-remove '(:eval (meow-indicator)) global-mode-string
                     :test 'equal)))

  ;; (remove-function 'default 'meow-setup-indicator)
  (advice-add 'meow--remove-modeline-indicator :around #'weland/meow--remove-modeline-indicator))

(straight-use-package
 '(hammy :type git :host github :repo "alphapapa/hammy.el"))

(use-package hammy
  :config
  (hammy-mode))

(use-package tab-bar
  :bind
  ("C-<tab>" . tab-next)
  :config
  (setf mode-line-misc-info
        ;; When the tab-bar is active, don't show global-mode-string
        ;; in mode-line-misc-info, because we now show that in the
        ;; tab-bar using `tab-bar-format-align-right' and
        ;; `tab-bar-format-global'.
        (remove '(global-mode-string ("" global-mode-string))
                mode-line-misc-info))
  (unless (member 'tab-bar-format-global tab-bar-format)
    ;; Show `global-mode-string' in the tab bar.
    (setf tab-bar-format (append tab-bar-format '(tab-bar-format-align-right tab-bar-format-global)))))


(straight-use-package 'pdf-tools)
(use-package pdf-tools
  :defer 10)

(straight-use-package 'citre)
(use-package citre
  :defer 5
  :bind
  ("C-x c j" . citre-jump)
  ("C-x c J" . citre-jump-back)
  ("C-x c p" . citre-ace-peek)
  :init
  (require 'citre-config))

(straight-use-package 'projectile)
(use-package projectile
  :defer 5
  :config
  (setq projectile-generic-command "fd . -0 --type f --color=never"))

;; (straight-use-package 'exwm)
;; (require 'exwm)
;; (require 'exwm-config)
;; (exwm-config-example)

;; (setq exwm-input-global-keys `(,(kbd "s-d") .
;;                                (lambda (command)
;;                                  (interactive (list (read-shell-command "$ ")))
;;                                  (start-process-shell-command command nil command))))


;; (add-hook 'after-init-hook '(lambda () (w32-send-sys-command #xf030)))

;; (straight-use-package
;;  '(blink-search :type git :host github :repo "manateelazycat/blink-search"))
;; (require 'blink-search)

(straight-use-package 'vertico-posframe)
;; (use-package vertico-posframe
;;   :config
;;   (vertico-posframe-mode 1))


(use-package clue
  :load-path "~/.emacs.d/site-lisp/clue"
  :defer t)

(setq help-window-select t)


(use-package toggle-one-window
  :load-path "~/.emacs.d/site-lisp/toggle-one-window"
  :bind
  ("C-c o" . toggle-one-window))

(use-package recentf
  :config
  (recentf-mode))


(setq split-width-threshold 30)
(setq split-height-threshold 10)


(defun my-sensible-window-split (&optional window)
  (cond
   ((and (> (window-width window)
            (window-height window))
         (window-splittable-p window 'horizontal))
    (with-selected-window window
      (split-window-right)))
   ((window-splittable-p window)
    (with-selected-window window
      (split-window-below)))))

(setq split-window-preferred-function #'my-sensible-window-split)


(straight-use-package 'fancy-battery)
(use-package fancy-battery
  :config
  (setq fancy-battery-show-percentage t)
  (add-hook 'after-init-hook #'fancy-battery-mode))

(straight-use-package
 '(tokyo-theme :type git :host github :repo "rawleyfowler/tokyo-theme.el"))

;; (straight-use-package 'dashboard)
;; (use-package dashboard
;;   :config
;;   (dashboard-setup-startup-hook)
;;   (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*"))))

(with-eval-after-load 'recentf
  (add-to-list 'recentf-exclude
               (recentf-expand-file-name "~/.buffer-timer-log"))
  (add-to-list 'recentf-exclude
               (recentf-expand-file-name "~/.log/*")))

(straight-use-package 'embark)
(straight-use-package 'embark-consult)
(use-package embark
  :bind
  ("C-'" . embark-act)
  ("C-." . embark-dwim)
  )
(global-set-key (kbd "C-;") 'embark-act)
(setq prefix-help-command 'embark-prefix-help-command)


(defun eaf-play-bili-mpv ()
  (interactive)
  (setq bilipath (eaf-get-path-or-url))
  (string-match "\\(https://www.bilibili.com/video/BV[0-9a-z]+/\\)"  bilipath)
  (mpv-play-url (match-string 1 bilipath)))

(defun eaf-get-bili-path ()
  (interactive)
  (setq bilipath (eaf-get-path-or-url))
  (string-match "\\(https://www.bilibili.com/video/BV[0-9a-z]+/\\)"  bilipath)
  (kill-new (match-string 1 bilipath)))

(straight-use-package 'ripgrep)


(straight-use-package 'resize-window)

(straight-use-package 'xenops)

(use-package xenops
  :hook
  (org-mode . xenops-mode)
  :config
  (setq xenops-math-image-scale-factor 1.8))


(straight-use-package 'workgroups2)
(workgroups-mode 1)


(defun posframe-suit-wg (name)
  (interactive (list (wg-read-new-workgroup-name)))
  (posframe-delete " *acm-doc-buffer*")
  (posframe-delete " *rime-posframe*")
  (posframe-delete " *pyim-page--posframe-buffer*")
  (posframe-delete " *acm-buffer*"))

(advice-add 'wg-create-workgroup :before #'posframe-suit-wg)

(defun buffer-mode (buffer-or-string)
  "Returns the major mode associated with a buffer."
  (with-current-buffer buffer-or-string
    major-mode))

(straight-use-package 'plain-theme)

(defun get-pkg-reqs-alist ()
  (cl-loop for pkg-and-desc in package-alist
           for pkg = (car pkg-and-desc)
           for desc = (cadr pkg-and-desc)
           for req-names = (cl-loop for it in (package-desc-reqs desc) collect (car it))
           collect (cons pkg req-names)))
(defun weland/package-info ()
  (setq info (get-pkg-reqs-alist))
  (with-temp-file "/tmp/g.dot"
    (insert "digraph G {")
    (insert (mapconcat #'identity
                       (cl-loop for pkg-reqs in info
                                for pkg = (car pkg-reqs)
                                for reqs = (cdr pkg-reqs)
                                nconcing (cl-loop for req in reqs
                                                  collect (format "\"%s\" -> \"%s\";\n" pkg req))) " "))
    (insert "}")))


;; (straight-use-package 'flycheck-aspell)
;; ;; Because Aspell does not support Org syntax, the user has
;; ;; to define a checker with the desired flags themselves.
;; (require 'flycheck-aspell)
;; (flycheck-aspell-define-checker "org"
;;   "Org" ("--add-filter" "url")
;;   (org-mode))
;; (add-to-list 'flycheck-checkers 'org-aspell-dynamic)

;; (setq ispell-dictionary "your_default_dictionary")
(setq ispell-program-name "aspell")
(setq ispell-silently-savep t)

(straight-use-package
 '(screenshot :type git :host github :repo "tecosaur/screenshot"))

(straight-use-package 'wucuo)

(add-hook 'prog-mode-hook #'wucuo-start)
(add-hook 'text-mode-hook #'wucuo-start)

;; (straight-use-package 'corfu)
(use-package corfu
  :straight t
  :defer t
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  :hook ((org-mode . corfu-mode)
         )

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  )


(straight-use-package
 '(corfu-english-helper :type git :host github :repo "manateelazycat/corfu-english-helper"))


(straight-use-package 'websocket)

(add-to-list 'load-path "/home/weland/.emacs.d/site-lisp/websocket-bridge")
(require 'websocket-bridge)
(websocket-bridge-server-start)
(add-to-list 'load-path "~/.emacs.d/site-lisp/dictionary-overlay/")
(require 'dictionary-overlay)
(setq dictionary-overlay-just-unknown-words t)
;; (setq dictionary-overlay-translate-engine 'deepl)


(global-set-key (kbd "C-c l") 'toggle-one-window)

;; (straight-use-package 'ace-window)
(use-package ace-window
  :straight t
  :bind
  ("C-c a" . ace-window))


;; (with-eval-after-load 'workgroups2
;;   ;; provide major mode, package to require, and functions
;;   (wg-support 'ivy-occur-grep-mode 'ivy
;;               `((serialize . ,(lambda (_buffer)
;;                                 (list (base64-encode-string (buffer-string) t))))
;;                 (deserialize . ,(lambda (buffer _vars)
;;                                   (switch-to-buffer (wg-buf-name buffer))
;;                                   (insert (base64-decode-string (nth 0 _vars)))
;;                                   ;; easier than `ivy-occur-grep-mode' to set up
;;                                   (grep-mode)
;;                                   ;; need return current buffer at the end of function
;;                                   (current-buffer))))))

(with-eval-after-load 'workgroups2
  ;; provide major mode, package to require, and functions
  (wg-support 'telega-root-mode 'telega
    `((deserialize . ,(lambda (_buffer vars)
                        (ignore _buffer vars)
                        (telega)
                        (wg-switch-to-buffer "*Telega Root*"))))
    )
  (wg-support 'eaf-mode 'eaf
    `((serialize . ,(lambda (_buffer)
                      (list (eaf-get-path-or-url))
                      ))
      (deserialize . ,(lambda (_buffer vars)
                        (ignore _buffer)
                        (wg-dbind (url) vars
                          (eaf-open-browser url)
                          (wg-switch-to-buffer url))
                        )))
    ))


;; (setq wl-copy-process nil)
;; (defun wl-copy (text)
;;   (setq wl-copy-process (make-process :name "wl-copy"
;;                                       :buffer nil
;;                                       :command '("wl-copy" "-f" "-n")
;;                                       :connection-type 'pipe))
;;   (process-send-string wl-copy-process text)
;;   (process-send-eof wl-copy-process))
;; (defun wl-paste ()
;;   (if (and wl-copy-process (process-live-p wl-copy-process))
;;       nil ; should return nil if we're the current paste owner
;;     (shell-command-to-string "wl-paste -n | tr -d \r")))
;; (setq interprogram-cut-function 'wl-copy)
;; (setq interprogram-paste-function 'wl-paste)

(straight-use-package 'vertico-prescient)

(use-package org-latex-impatient
  :straight t
  :defer t
  ;; :hook (org-mode . org-latex-impatient-mode)
  ;; (tex-mode . org-latex-impatient-mode)
  :init
  (setq org-latex-impatient-tex2svg-bin
        ;; location of tex2svg executable
        "~/node_modules/mathjax-node-cli/bin/tex2svg"))


(with-eval-after-load 'org-roam
(defun gsgx/org-roam-create-note-from-headline ()
  "Create an Org-roam note from the current headline if it doesn't
exist without jumping to it"
  (let* ((title (nth 4 (org-heading-components)))
         ;; Read in the name of the node, with the title filled in
         ;; TODO: How can I just use the title without user input?
         (node (org-roam-node-read title)))
    ;; Skip the node if it already exists
    (if (org-roam-node-file node)
        (message "Skipping %s, node already exists" title)
      ;; Without this the subsequent kills seem to be grouped together, not
      ;; sure why
      (kill-new "")
      ;; Cut the subtree from the original file
      (org-cut-subtree)
      ;; Create the new capture file
      (org-roam-capture- :node node)
      ;; Paste in the subtree
      (org-paste-subtree)
      ;; Removing the heading from new node
      (kill-whole-line)
      ;; Finalizing the capture will save and close the capture buffer
      (org-capture-finalize nil)
      ;; Because we've deleted a subtree, we need the following line to make the
      ;; `org-map-entries' call continue from the right place
      (setq org-map-continue-from
            (org-element-property :begin (org-element-at-point))))))

(defun gsgx/org-roam-create-note-from-headlines ()
  (interactive)
  (if (region-active-p)
      ;; `region-start-level' means we'll map over only headlines that are at
      ;; the same level as the first headline in the region. This may or may not
      ;; be what you want
      (org-map-entries
       'gsgx/org-roam-create-note-from-headline t 'region-start-level)
    ;; If no region was selected, just create the note from the current headline
    (gsgx/org-roam-create-note-from-headline))))

(straight-use-package
 '(novel-mode :type git :host github :repo "TLINDEN/novel-mode"))

(straight-use-package 'rg)
(straight-use-package 'golden-ratio)

(straight-use-package 'denote)
(with-eval-after-load 'denote
  (global-set-key (kbd "C-c d") 'denote-open-or-create))
(use-package denote
  :bind
  ("C-c d o" . denote-open-or-create))

