(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line

(require 'evil)
(evil-mode 1)

(shell)
(sr-speedbar-open)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-linum-mode t)
(desktop-save-mode 1)
(load-theme 'spacemacs-dark t)

(setq redisplay-dont-pause t
  scroll-margin 1
  scroll-step 1
  scroll-conservatively 10000
  scroll-preserve-screen-position 1)

(elscreen-start)


(require 'virtualenvwrapper)
(venv-initialize-interactive-shells) ;; if you want interactive shell support
(setq venv-location "C:/Users/CGuillemot/Envs")

(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)


(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this-word)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this-word)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this-word)

(global-set-key (kbd "C-c C-q") 'comint-quit-subjob)

(defun toggle-window-dedicated ()
  "Control whether or not Emacs is allowed to display another
buffer in current window."
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window (not (window-dedicated-p window))))
       "%s: Can't touch this!"
     "%s is up for grabs.")
   (current-buffer)))

(global-set-key (kbd "C-c t") 'toggle-window-dedicated)


(global-set-key (kbd "C-c g") 'jedi:goto-definition)

(defun kill-other-buffers ()
    "Kill all other buffers."
    (interactive)
    (mapc 'kill-buffer 
          (delq (current-buffer) 
                (remove-if-not 'buffer-file-name (buffer-list)))))

(add-hook 'after-init-hook #'global-flycheck-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" default)))
 '(package-selected-packages
   (quote
    (flycheck virtualenvwrapper helm magit psvn diff-hl elscreen multiple-cursors evil spacemacs-theme jedi solarized-theme)))
 '(safe-local-variable-values (quote ((project-venv-name . "pikuli")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
