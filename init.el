(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line

;; FIRA CODE
(set-face-attribute 'default nil :font "Fira Mono" )
(set-frame-font "Fira Mono" nil t)

;; This works when using emacs --daemon + emacsclient
(add-hook 'after-make-frame-functions (lambda (frame) (set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code")))
;; This works when using emacs without server/client
(set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code")
;; I haven't found one statement that makes both of the above situations work, so I use both for now

(defconst fira-code-font-lock-keywords-alist
  (mapcar (lambda (regex-char-pair)
            `(,(car regex-char-pair)
              (0 (prog1 ()
                   (compose-region (match-beginning 1)
                                   (match-end 1)
                                   ;; The first argument to concat is a string containing a literal tab
                                   ,(concat "	" (list (decode-char 'ucs (cadr regex-char-pair)))))))))
          '(("\\(www\\)"                   #Xe100)
            ("[^/]\\(\\*\\*\\)[^/]"        #Xe101)
            ("\\(\\*\\*\\*\\)"             #Xe102)
            ("\\(\\*\\*/\\)"               #Xe103)
            ("\\(\\*>\\)"                  #Xe104)
            ("[^*]\\(\\*/\\)"              #Xe105)
            ("\\(\\\\\\\\\\)"              #Xe106)
            ("\\(\\\\\\\\\\\\\\)"          #Xe107)
            ("\\({-\\)"                    #Xe108)
            ;;("\\(\\[\\]\\)"                #Xe109)
            ("\\(::\\)"                    #Xe10a)
            ("\\(:::\\)"                   #Xe10b)
            ("[^=]\\(:=\\)"                #Xe10c)
            ("\\(!!\\)"                    #Xe10d)
            ("\\(!=\\)"                    #Xe10e)
            ("\\(!==\\)"                   #Xe10f)
            ("\\(-}\\)"                    #Xe110)
            ("\\(--\\)"                    #Xe111)
            ("\\(---\\)"                   #Xe112)
            ("\\(-->\\)"                   #Xe113)
            ("[^-]\\(->\\)"                #Xe114)
            ("\\(->>\\)"                   #Xe115)
            ("\\(-<\\)"                    #Xe116)
            ("\\(-<<\\)"                   #Xe117)
            ("\\(-~\\)"                    #Xe118)
            ("\\(#{\\)"                    #Xe119)
            ("\\(#\\[\\)"                  #Xe11a)
            ("\\(##\\)"                    #Xe11b)
            ("\\(###\\)"                   #Xe11c)
            ("\\(####\\)"                  #Xe11d)
            ("\\(#(\\)"                    #Xe11e)
            ("\\(#\\?\\)"                  #Xe11f)
            ("\\(#_\\)"                    #Xe120)
            ("\\(#_(\\)"                   #Xe121)
            ("\\(\\.-\\)"                  #Xe122)
            ("\\(\\.=\\)"                  #Xe123)
            ("\\(\\.\\.\\)"                #Xe124)
            ("\\(\\.\\.<\\)"               #Xe125)
            ("\\(\\.\\.\\.\\)"             #Xe126)
            ("\\(\\?=\\)"                  #Xe127)
            ("\\(\\?\\?\\)"                #Xe128)
            ("\\(;;\\)"                    #Xe129)
            ("\\(/\\*\\)"                  #Xe12a)
            ("\\(/\\*\\*\\)"               #Xe12b)
            ("\\(/=\\)"                    #Xe12c)
            ("\\(/==\\)"                   #Xe12d)
            ("\\(/>\\)"                    #Xe12e)
            ("\\(//\\)"                    #Xe12f)
            ("\\(///\\)"                   #Xe130)
            ("\\(&&\\)"                    #Xe131)
            ("\\(||\\)"                    #Xe132)
            ("\\(||=\\)"                   #Xe133)
            ("[^|]\\(|=\\)"                #Xe134)
            ("\\(|>\\)"                    #Xe135)
            ("\\(\\^=\\)"                  #Xe136)
            ("\\(\\$>\\)"                  #Xe137)
            ;;("\\(\\+\\+\\)"                #Xe138)
            ;;("\\(\\+\\+\\+\\)"             #Xe139)
            ("\\(\\+>\\)"                  #Xe13a)
            ("\\(=:=\\)"                   #Xe13b)
            ("[^!/]\\(==\\)[^>]"           #Xe13c)
            ("\\(===\\)"                   #Xe13d)
            ("\\(==>\\)"                   #Xe13e)
            ("[^=]\\(=>\\)"                #Xe13f)
            ("\\(=>>\\)"                   #Xe140)
            ("\\(<=\\)"                    #Xe141)
            ("\\(=<<\\)"                   #Xe142)
            ("\\(=/=\\)"                   #Xe143)
            ("\\(>-\\)"                    #Xe144)
            ("\\(>=\\)"                    #Xe145)
            ("\\(>=>\\)"                   #Xe146)
            ("[^-=]\\(>>\\)"               #Xe147)
            ("\\(>>-\\)"                   #Xe148)
            ("\\(>>=\\)"                   #Xe149)
            ("\\(>>>\\)"                   #Xe14a)
            ("\\(<\\*\\)"                  #Xe14b)
            ("\\(<\\*>\\)"                 #Xe14c)
            ("\\(<|\\)"                    #Xe14d)
            ("\\(<|>\\)"                   #Xe14e)
            ("\\(<\\$\\)"                  #Xe14f)
            ("\\(<\\$>\\)"                 #Xe150)
            ("\\(<!--\\)"                  #Xe151)
            ("\\(<-\\)"                    #Xe152)
            ("\\(<--\\)"                   #Xe153)
            ("\\(<->\\)"                   #Xe154)
            ("\\(<\\+\\)"                  #Xe155)
            ("\\(<\\+>\\)"                 #Xe156)
            ("\\(<=\\)"                    #Xe157)
            ("\\(<==\\)"                   #Xe158)
            ("\\(<=>\\)"                   #Xe159)
            ("\\(<=<\\)"                   #Xe15a)
            ("\\(<>\\)"                    #Xe15b)
            ("[^-=]\\(<<\\)"               #Xe15c)
            ("\\(<<-\\)"                   #Xe15d)
            ("\\(<<=\\)"                   #Xe15e)
            ("\\(<<<\\)"                   #Xe15f)
            ("\\(<~\\)"                    #Xe160)
            ("\\(<~~\\)"                   #Xe161)
            ("\\(</\\)"                    #Xe162)
            ("\\(</>\\)"                   #Xe163)
            ("\\(~@\\)"                    #Xe164)
            ("\\(~-\\)"                    #Xe165)
            ("\\(~=\\)"                    #Xe166)
            ("\\(~>\\)"                    #Xe167)
            ("[^<]\\(~~\\)"                #Xe168)
            ("\\(~~>\\)"                   #Xe169)
            ("\\(%%\\)"                    #Xe16a)
           ;; ("\\(x\\)"                   #Xe16b) This ended up being hard to do properly so i'm leaving it out.
            ("[^:=]\\(:\\)[^:=]"           #Xe16c)
            ("[^\\+<>]\\(\\+\\)[^\\+<>]"   #Xe16d)
            ("[^\\*/<>]\\(\\*\\)[^\\*/<>]" #Xe16f))))

(defun add-fira-code-symbol-keywords ()
  (font-lock-add-keywords nil fira-code-font-lock-keywords-alist))

(add-hook 'prog-mode-hook
          #'add-fira-code-symbol-keywords)

;;MACOS
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
(setq mac-option-modifier nil
      mac-command-modifier 'meta
      x-select-enable-clipboard t)

(define-key global-map [home] 'beginning-of-line)
(define-key global-map [end] 'end-of-line)

(defun pbcopy ()
  (interactive)
  (call-process-region (point) (mark) "pbcopy")
  (setq deactivate-mark t))

(defun pbpaste ()
  (interactive)
  (call-process-region (point) (if mark-active (mark) (point)) "pbpaste" t t))

(defun pbcut ()
  (interactive)
  (pbcopy)
  (delete-region (region-beginning) (region-end)))

(global-set-key (kbd "C-c c") 'pbcopy)
(global-set-key (kbd "C-c v") 'pbpaste)
(global-set-key (kbd "C-c x") 'pbcut)

;; OPEN ESHELL
(eshell)
(setq eshell-prompt-function
  (lambda ()
    " $ "))

;; NEOTREE
(require 'all-the-icons)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
(setq neo-window-width 40)
(add-hook 'after-init-hook #'neotree-toggle)

;; HELM
(require 'helm-config)
(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)

;; SMART PARENTHESIS
(require 'smartparens-config)
(add-hook 'python-mode-hook #'smartparens-mode)
(add-hook 'php-mode-hook #'smartparens-mode)
(show-paren-mode 1)

;; ORIGAMI
(require 'origami)
(global-origami-mode t)
(global-set-key (kbd "C-c C-g") 'origami-toggle-node)

;; INSERT RAW CHAR
(global-set-key (kbd "C-v") 'quoted-insert)

;; GUESS INDENT
(dtrt-indent-mode t)

;; TABBAR
(require 'tabbar)
(tabbar-mode t)
(set-face-attribute
 'tabbar-unselected nil
 :background "gray30"
 :foreground "white"
 :box '(:line-width 5 :color "gray30" :style nil))
(defun make-xpm-bar (color height width)
  "Create an XPM bitmap of a bar."
  (propertize
   " " 'display
   (let ((data (make-list height (make-list width 1)))
         (color (or color "None")))
     (create-image
      (concat
       (format "/* XPM */\nstatic char * percent[] = {\n\"%i %i 2 1\",\n\". c %s\",\n\"  c %s\","
               (length (car data))
               (length data)
               color
               color)
       (apply #'concat
              (cl-loop with idx = 0
                       with len = (length data)
                       for dl in data
                       do (cl-incf idx)
                       collect
                       (concat "\""
                               (cl-loop for d in dl
                                        if (= d 0) collect (string-to-char " ")
                                        else collect (string-to-char "."))
                               (if (eq idx len) "\"};" "\",\n")))))
      'xpm t :ascent 'center))))

(defun my-tabbar-display-tab (tab)
    (let ((label (if tabbar--buffer-show-groups
                     (format "[%s]" (tabbar-tab-tabset tab))
                   (format "%s" (tabbar-tab-value tab))))
          (bar-color "#51afef")
          (bar-height 25)
          (bar-width 3)
          (selected-p (eq tab (tabbar-selected-tab (tabbar-current-tabset)))))
      (concat (when (and (display-graphic-p) selected-p)
                (make-xpm-bar bar-color bar-height bar-width))
              " "
              (if tabbar-auto-scroll-flag
                  label
                (tabbar-shorten
                 label (max 1 (/ (window-width)
                                 (length (tabbar-view
                                          (tabbar-current-tabset)))))))
              " ")))

(setq tabbar-tab-label-function #'my-tabbar-display-tab)

(defun my-tabbar-buffer-groups () ;; customize to show all normal files in one group
   "Returns the name of the tab group names the current buffer belongs to.
 There are two groups: Emacs buffers (those whose name starts with '*', plus
 dired buffers), and the rest.  This works at least with Emacs v24.2 using
 tabbar.el v1.7."
   (list (cond ((string-equal "*" (substring (buffer-name) 0 1)) "emacs")
               ((eq major-mode 'dired-mode) "emacs")
               (t "user"))))
 (setq tabbar-buffer-groups-function 'my-tabbar-buffer-groups)

;; BASIC CONFIG & THEME
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-linum-mode t)
(desktop-save-mode 1)

(require 'doom-themes)

;; THEME
(setq doom-themes-enable-bold t
      doom-themes-enable-italic t)
(load-theme 'doom-one t)
(doom-themes-visual-bell-config)
(doom-themes-neotree-config)
(doom-themes-org-config)
(add-to-list 'default-frame-alist '(background-color . "#282C34"))

;; SCROLL OPTION
(setq redisplay-dont-pause t
  scroll-margin 1
  scroll-step 1
  scroll-conservatively 10000
  scroll-preserve-screen-position 1)
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; ACE WINDOW
(global-set-key (kbd "M-a") 'ace-window)

;; MULTIPLE CURSORS
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this-word)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this-word)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this-word)

;; KILL SHELL SHORTCUT
(global-set-key (kbd "C-c C-q") 'comint-quit-subjob)

;; KILL CURRENT BUFFER
(global-set-key (kbd "C-q") 'kill-this-buffer)

;; DEDICATED WINDOW
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

;; AVOID SPLIT WINDOW
(defun toggle-window-unsplittable ()
  "Control whether or not Emacs is allowed to split window."
  (interactive)
         (set-frame-parameter nil 'unsplittable t)
   (current-buffer))
(global-set-key (kbd "C-c C-t") 'toggle-window-unsplittable)

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

;; DUCK DUCK GO
(defun duckduckgo-search (text)
  "Search DuckDuckGo from Emacs."
  (interactive "sSearch: ")
  (browse-url
   (concat "https://duckduckgo.com/?q="
           (replace-regexp-in-string " " "+" text))))

(global-set-key  (kbd "C-c s") 'duckduckgo-search)

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
    (backward-kill-word 1)))

(global-set-key  [C-delete] 'kill-whitespace-or-word)
(global-set-key  [C-backspace] 'kill-whitespace-or-word-backward)

;; SWITCH TO BUFFER -> EXCLUDE INTERNAL BUFFERS
(defun my-switch-to-buffer ()
  (interactive)
  (let ((completion-regexp-list '("\\`\\([^*]\\|\*eshell\*\\|\*scratch\*\\)"
                                  "\\`\\([^T]\\|T\\($\\|[^A]\\|A\\($\\|[^G]\\|G\\($\\|[^S]\\|S.\\)\\)\\)\\).*")))
    (call-interactively 'switch-to-buffer)))
(global-set-key [C-tab] 'my-switch-to-buffer)

;; PYTHON===============================================================================
;; COMMAND INSERT DEBUG LINE
(eval-after-load 'python
                 '(define-key python-mode-map (kbd "C-M-d") (lambda () (interactive) (insert "import pdb;pdb.set_trace()"))))

;; JEDI CONFIG
(require 'virtualenvwrapper)
(venv-initialize-interactive-shells) ;; if you want interactive shell support
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
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))
(add-hook 'js-mode-hook
	  '(lambda ()
    (auto-complete-mode t)
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
    (term+ multi-term origami highlight-indent-guides all-the-icons-dired projectile doom-themes dracula-theme treemacs vimish-fold jedi vue-mode dtrt-indent neotree hungry-delete ac-php smartparens ace-window tabbar flycheck virtualenvwrapper helm magit psvn diff-hl multiple-cursors evil spacemacs-theme solarized-theme)))
 '(safe-local-variable-values (quote ((project-venv-name . "pikuli")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
