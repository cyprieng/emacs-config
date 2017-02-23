(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line

;; OPEN SHELL AND FILE EXPLORER
(shell)
(sr-speedbar-open)

;; HELM
(require 'helm-config)
(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)

;; SMART PARENTHESIS
(require 'smartparens-config)
(add-hook 'python-mode-hook #'smartparens-mode)
(add-hook 'php-mode-hook #'smartparens-mode)

;; TABBAR
(require 'tabbar)
(tabbar-mode t)

;; BASIC CONFIG & THEME
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-linum-mode t)
(desktop-save-mode 1)
(load-theme 'spacemacs-dark t)

;; SCROLL OPTION
(setq redisplay-dont-pause t
  scroll-margin 1
  scroll-step 1
  scroll-conservatively 10000
  scroll-preserve-screen-position 1)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; SCREEN
(elscreen-start)

;; ACE WINDOW
(global-set-key (kbd "M-w") 'ace-window)

;; MULTIPLE CURSORS
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this-word)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this-word)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this-word)

;; KILL SHELL SHORTCUT
(global-set-key (kbd "C-c C-q") 'comint-quit-subjob)

;; DIDICATED WINDOW
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

;; KILL OTHER BUFFERS COMMAND
(defun kill-other-buffers ()
    "Kill all other buffers."
    (interactive)
    (mapc 'kill-buffer
          (delq (current-buffer)
                (remove-if-not 'buffer-file-name (buffer-list)))))

;; SVN DIFF HIGHLIGHTER
(global-diff-hl-mode)
(defadvice svn-status-update-modeline (after svn-update-diff-hl activate)
    (diff-hl-update))

;; DELETE WHITESPACE BEFORE SAVE
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; FLY CHECK
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-check-syntax-automatically '(mode-enabled save))

;; DELETE WORD OR WHITESPACE
(defun kill-whitespace-or-word ()
  (interactive)
  (if (looking-at "[ \t\n]")
      (let ((p (point)))
        (re-search-forward "[^ \t\n]" nil :no-error)
        (backward-char)
        (kill-region p (point)))
    (kill-word 1)))

(defun kill-whitespace-or-word-backward ()
  (interactive)
  (if (looking-back "[ \t\n]")
      (let ((p (point)))
        (re-search-backward "[^ \t\n]" nil :no-error)
        (forward-char)
        (kill-region p (point)))
    (kill-word 1)))

(global-set-key  [C-delete] 'kill-whitespace-or-word)
(global-set-key  [C-backspace] 'kill-whitespace-or-word-backward)

;; PYTHON===============================================================================
;; COMMAND INSERT DEBUG LINE
(eval-after-load 'python
                 '(define-key python-mode-map (kbd "C-M-d") (lambda () (interactive) (insert "import pdb;pdb.set_trace()"))))

;; JEDI CONFIG
(require 'virtualenvwrapper)
(venv-initialize-interactive-shells) ;; if you want interactive shell support
(setq venv-location "C:/Users/CGuillemot/Envs")
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
(eval-after-load 'python
		 '(define-key python-mode-map (kbd "C-c g") 'jedi:goto-definition))

;; PHP===============================================================================
(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))

;; COMMAND INSERT DEBUG LINE
(eval-after-load 'php-mode
                 '(define-key php-mode-map (kbd "C-M-d") (lambda () (interactive) (insert "eval(\\Psy\\sh());"))))

;; AC PHP
(add-hook 'php-mode-hook
            '(lambda ()
               (auto-complete-mode t)
               (require 'ac-php)
               (setq ac-sources  '(ac-source-php ) )
               (yas-global-mode 1)
               (define-key php-mode-map  (kbd "C-c g") 'ac-php-find-symbol-at-point)   ;goto define
               (define-key php-mode-map  (kbd "C-t") 'ac-php-location-stack-back   ) ;go back
               ))

;; JAVASCRIPT===============================================================================
(add-hook 'js-mode-hook
	  '(lambda ()
		(tern-mode t)
		(define-key js-mode-map  (kbd "C-c g") 'tern-find-definition)   ;goto define
	  ))

(eval-after-load 'tern
   '(progn
      (auto-complete-mode t)
      (require 'tern-auto-complete)
      (tern-ac-setup)))

;;===============================================================================

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
    (hungry-delete ac-php smartparens ace-window tabbar flycheck virtualenvwrapper helm magit psvn diff-hl elscreen multiple-cursors evil spacemacs-theme jedi solarized-theme)))
 '(safe-local-variable-values (quote ((project-venv-name . "pikuli")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
