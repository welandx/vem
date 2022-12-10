;; Enable installation of packages from MELPA.
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Install packages.
(dolist (package '(markdown-mode paredit rainbow-delimiters ivy meow posframe yasnippet consult rime use-package kaolin-themes org-modern  org-roam-ui pyim))
  (unless (package-installed-p package)
    (package-install package)))

(provide 'init-package)
