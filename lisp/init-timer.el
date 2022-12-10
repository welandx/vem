(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/elisp-buffer-timer"))
;; (leo/load-idle 3 "elisp-buffer-timer/buffer-timer.el")
(require 'buffer-timer)
(with-eval-after-load 'buffer-timer
  (setq buffer-timer-idle-limit 2000)
  ;; below should be set pravitely
  ;;(setq buffer-timer-output-file (concat (getenv "HOME") "/.log/buffer-timer-%Y-%m-%d"))
  ;; Example list of titles and regexps to group by.  This
  (setq buffer-timer-regexp-master-list
        '(
          ("idle" .
           (("generic" .			  "^\\*idle\\*")
            ("generic2" .			  "^\\*idle-2\\*")
            ("minibuf" .                        "^ \\*Minibuf-.*")))
          ("personal" .
           (("reading" .                        "lib/ebooks/")
            ("daily" .                        "daily")
            ("Surf" .
             (("game" . "178")
              ("Emacs" . "Emacs China")
              ))
            ("study" .
             (("English" .  "扇贝")
              ("math" .                   "org/roam/math")
              ("CS"  .                     "org/roam/computer-science")
              ("politic" .                 "org/roam/politic")
              ("generic" .                 "org/roam")
              ))
            ("Social" .
             (("generic" .                "Telega")
              ("hp" .                      "Guang")
              ("vim" . "vimzh")
              ("emacs" .                   "emacs_zh")))
            ("config" .                    "conf")))
          ("work" .
           (("python" . "code/python")
            ("c++" . "code/cpp")
            )))))

(provide 'init-timer)
