;; (set-face-attribute 'default nil :font (font-spec :family "BlexMono Nerd Font" :size 18))
;; (set-fontset-font t 'han (font-spec :family "霞鹜文楷" :size 18))
;; (set-face-font 'variable-pitch "Overpass")
;; (set-fontset-font t 'variable-pitch (font-spec :family "overpass" :size 18))
;; frame drawer
(setq default-frame-alist (append default-frame-alist '((alpha-background . 85))))

;; (display-time-mode)
(setq tab-bar-format '(tab-bar-format-global)
      tab-bar-mode t)
(setq tab-bar-format
      '(tab-bar-format-history
        tab-bar-format-tabs
        tab-bar-separator
        ;; tab-bar-format-align-right
        tab-bar-format-global
        ))


(add-to-list 'load-path "~/.emacs.d/site-lisp")
(require 'init-frame-hooks)

(defun weland/frame-init ()
  (menu-bar-mode 0)
  (when (display-graphic-p)
    (tool-bar-mode 0)
    (scroll-bar-mode 0))
  (setq inhibit-startup-screen t)
  (column-number-mode)
  (weland/load-font)
  (meomacs-load-theme)
  (set-face-attribute 'default nil
                      :weight 'normal)
  ;; (load-theme 'ef-cyprus t)
  ;; font set
  ;; (set-face-attribute 'default nil :font (font-spec :family "IBM Plex Mono" :size 22))
  ;; (set-fontset-font t 'han (font-spec :family "霞鹜文楷"))
  )

(add-hook 'after-make-window-system-frame-hooks 'weland/frame-init)
(add-hook 'after-init-hook 'weland/frame-init)



(defun weland/load-font ()

(when (member "MonoLisa Nasy" (font-family-list))
    (set-frame-font "MonoLisa Nasy-16" t t))
  ;; (when (member "BlexMono Nerd Font" (font-family-list))
  ;;   (set-frame-font "BlexMono Nerd Font-15" t t))

  (set-fontset-font
   t
   'symbol
   (cond
    ((string-equal system-type "windows-nt")
     (cond
      ((member "Segoe UI Symbol" (font-family-list)) "Segoe UI Symbol")))
    ((string-equal system-type "darwin")
     (cond
      ((member "Apple Symbols" (font-family-list)) "Apple Symbols")))
    ((string-equal system-type "gnu/linux")
     (cond
      ((member "Symbola" (font-family-list)) "Symbola")))))
  (progn
    ;; set font for emoji (if before emacs 28, should come after setting symbols. emacs 28 now has 'emoji . before, emoji is part of 'symbol)
    (set-fontset-font
     t
     (if (version< emacs-version "28.1")
         '(#x1f300 . #x1fad0)
       'emoji
       )
     (cond
      ((member "Apple Color Emoji" (font-family-list)) "Apple Color Emoji")
      ((member "Noto Color Emoji" (font-family-list)) "Noto Color Emoji")
      ((member "Noto Emoji" (font-family-list)) "Noto Emoji")
      ((member "Segoe UI Emoji" (font-family-list)) "Segoe UI Emoji")
      ((member "Symbola" (font-family-list)) "Symbola"))))

  (set-fontset-font
   t
   'han
   (cond
    ((string-equal system-type "windows-nt")
     (cond
      ((member "Microsoft YaHei" (font-family-list)) "Microsoft YaHei")
      ((member "Microsoft JhengHei" (font-family-list)) "Microsoft JhengHei")
      ((member "SimHei" (font-family-list)) "SimHei")))
    ((string-equal system-type "darwin")
     (cond
      ((member "Hei" (font-family-list)) "Hei")
      ((member "Heiti SC" (font-family-list)) "Heiti SC")
      ((member "Heiti TC" (font-family-list)) "Heiti TC")))
    ((string-equal system-type "gnu/linux")
     (cond
            ;; ((member "FZSuXinShiLiuKaiS\-R\-GB" (font-family-list)) "FZSuXinShiLiuKaiS\-R\-GB")

      ((member "LXGW WenKai" (font-family-list)) "LXGW WenKai")

      ((member "FlyFlowerSong" (font-family-list)) "FlyFlowerSong")
      ((member "WenQuanYi Micro Hei" (font-family-list)) "WenQuanYi Micro Hei")))))
  )


;; Run after startup
(add-hook 'after-init-hook
          (lambda ()
            (when window-system
              (weland/load-font))))
(require 'subr-x)
(require 'cl-lib)

(defvar meomacs-themes '(catppuccin-latte modus-operandi)
  "Themes to use.")

(defun meomacs-load-theme ()
  (when-let ((theme (car meomacs-themes)))
    (message "Load theme: %s" (car meomacs-themes))
    (mapc 'disable-theme custom-enabled-themes)
    (unless (eq theme 'default)
      (load-theme theme t))))

(defun meomacs-next-theme ()
  (interactive)
  (when meomacs-themes
    (setq meomacs-themes (append (cdr meomacs-themes) (list (car meomacs-themes))))
    (meomacs-load-theme)))

(global-set-key (kbd "C-c r") 'meomacs-next-theme)




(setq frame-resize-pixelwise t)

;; But do not resize windows pixelwise, this can cause crashes in some cases
;; when resizing too many windows at once or rapidly.
(setq window-resize-pixelwise nil)


;;
;;; Minibuffer

;; Allow for minibuffer-ception. Sometimes we need another minibuffer command
;; while we're in the minibuffer.
(setq enable-recursive-minibuffers t)

;; Show current key-sequence in minibuffer ala 'set showcmd' in vim. Any
;; feedback after typing is better UX than no feedback at all.
(setq echo-keystrokes 0.02)

;; Expand the minibuffer to fit multi-line text displayed in the echo-area. This
;; doesn't look too great with direnv, however...
(setq resize-mini-windows 'grow-only)

;; Typing yes/no is obnoxious when y/n will do
(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  ;; DEPRECATED: Remove when we drop 27.x support
  (advice-add #'yes-or-no-p :override #'y-or-n-p))




(provide 'init-ui)
