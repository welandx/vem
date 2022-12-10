;;; Social
;;----------------------------
(use-package telega
  :straight t
  :defer t
  :init
  ;; (setq telega-server-libs-prefix "/usr/local/lib")
  :config
  (setq telega-proxies   (list '(:server "127.0.0.1":port 7891 :enable t :type (:@type "proxyTypeSocks5") )))
  (telega-notifications-mode t))
  ;; (telega-mode-line-mode 1))

;; (global-set-key (kbd "d"))
(with-eval-after-load 'telega
  ;; (define-key telega-msg-button-map "C-c C-t" 'telega-sticker-choose-favorite-or-recent)
  ;; (define-key telega-)
  (define-key telega-msg-button-map "k" nil)
  (define-key telega-msg-button-map "l" nil)
  (define-key telega-msg-button-map "h" nil))

(add-hook 'telega-load-hook
	      (lambda ()
	        (define-key global-map (kbd "C-c o") telega-prefix-map)
            (define-key telega-prefix-map (kbd "k") 'telega-sticker-choose-favorite-or-recent)
            ))


;; EAF
;;---------------------------
(use-package eaf
  ;; :straight (emacs-application-framwork :type git :host github :repo "manateelazycat/emacs-application-framework")
  :load-path "~/.emacs.d/site-lisp/emacs-application-framework"
  :custom
  (eaf-browser-continue-where-left-off t)
  (eaf-browser-enable-adblocker t)
  (browse-url-browser-function 'eaf-open-browser)
  (eaf-webengine-default-zoom 1.3)
  (eaf-webengine-default-zoom 1.3)
  (eaf-proxy-type "http")
  (eaf-proxy-host "127.0.0.1")
  (eaf-proxy-port "7890")
  :config
  (defalias 'browse-web #'eaf-open-browser)
  (require 'eaf-browser)
  (require 'eaf-rss-reader)
  (require 'eaf-mindmap)
  (require 'eaf-mpv)
  ;; (require 'eaf-netease-music)
  ;; (require 'eaf-netease-cloud-music)
  (require 'eaf-video-player)
  (require 'eaf-music-player)
  (require 'eaf-pdf-viewer)
  (with-eval-after-load 'TeX
    (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex --synctex=1%(mode)%' %t" TeX-run-TeX nil t))
    (add-to-list 'TeX-view-program-list '("eaf" eaf-pdf-synctex-forward-view))
    (add-to-list 'TeX-view-program-selection '(output-pdf "eaf")))
  (eaf-bind-key eaf-send-key "1" eaf-browser-keybinding)
  (eaf-bind-key eaf-send-key "2" eaf-browser-keybinding)
  )


(provide 'init-app)
