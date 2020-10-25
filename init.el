;; Emacs initialization 
(setq inhibit-startup-message nil)
(setq inhibit-startup-message t)

(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(menu-bar-mode -1)          ; Disable the menu bar
(scroll-bar-mode -1)        ; Disable visible scrollbar

;; Startup performance
  ;; Reducing the frequency of garbage collection
(setq gc-cons-threshold (* 50 1000 1000))

  ;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Improve scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1)

;; Set frame tansparency and maximize windows by default
(set-frame-parameter (selected-frame) 'alpha '(90 . 90))
  (add-to-list 'default-frame-alist '(alpha . (90 . 90)))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
  (add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Keep transient cruft out of ~/.emacs.d/
(setq user-emacs-directory "~/.cache/emacs/"
      backup-directory-alist `(("." . ,(expand-file-name "backups" user-emacs-directory)))
      url-history-file (expand-file-name "url/history" user-emacs-directory)
      auto-save-list-file-prefix (expand-file-name "auto-save-list/.saves-" user-emacs-directory)
      projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" user-emacs-directory))

;; Keep customization settings in a temporary file
(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("mepla-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))
 
;; Emacs theme
(load-theme 'Sholum t) 

;; Use-package configuration
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; Set up the visible bell
(setq visible-bell t)

;; Font
  ;; Enable proper Unicode glyph support
(defun dw/replace-unicode-font-mapping (block-name old-font new-font)
  (let* ((block-idx (cl-position-if
                         (lambda (i) (string-equal (car i) block-name))
                         unicode-fonts-block-font-mapping))
         (block-fonts (cadr (nth block-idx unicode-fonts-block-font-mapping)))
         (updated-block (cl-substitute new-font old-font block-fonts :test 'string-equal)))
    (setf (cdr (nth block-idx unicode-fonts-block-font-mapping))
          `(,updated-block)))) 
          
;; EXWM configuration (TODO)
(setq dw/exwm-enabled (and (eq window-system 'x)
                           (seq-contains command-line-args "--use-exwm")))
  
;; Cursor configuration
(setq-default cursor-type 'bar)
(blink-cursor-mode 0)

;; Stop creating those #auto-save# files
(setq auto-save-default nil)

;; Disable line numbers for some modes
(require 'display-line-numbers)
(defcustom display-line-numbers-exempt-modes '(vterm-mode eshell-mode shell-mode term-mode ansi-term-mode pdf-view-mode doc-view-mode which-key-mode)
  "Major modes on which to disable the linum mode, exempts them from global requirement"
  :group 'display-line-numbers
  :type 'list
  :version "green")

(defun display-line-numbers--turn-on ()
  "turn on line numbers but excempting certain majore modes defined in `display-line-numbers-exempt-modes'"
  (if (and
       (not (member major-mode display-line-numbers-exempt-modes))
       (not (minibufferp)))
      (display-line-numbers-mode)))

(global-display-line-numbers-mode)

;; Simplify Leader Bindings
(use-package general
  :config
  (general-evil-setup t)

  (general-create-definer dw/leader-key-def
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (general-create-definer dw/ctrl-c-keys
    :prefix "C-c"))

  ;; Which-key configuration
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))
  
  ;; ESC cancels all
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Evil Mode
(defun dw/evil-hook ()
  (dolist (mode '(custom-mode
                  eshell-mode
                  git-rebase-mode
                  erc-mode
                  circe-server-mode
                  circe-chat-mode
                  circe-query-mode
                  sauron-mode
                  term-mode))
  (add-to-list 'evil-emacs-state-modes mode)))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-respect-visual-line-mode t)
  (setq evil-move-beyond-eol t)
  :config
  (add-hook 'evil-mode-hook 'dw/evil-hook)
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))
  
  ;; Evil collection
(use-package evil-collection
  :after evil
  :custom
  (evil-collection-outline-bind-tab-p nil)
  :config
  (evil-collection-init))
  
;; Hydra
(use-package hydra
  :defer 1)

;; Better completions
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :init
  (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-wrap t)
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t)

  ;; Use different regex strategies per completion command
(push '(swiper . ivy--regex-ignore-order) ivy-re-builders-alist)
(push '(counsel-M-x . ivy--regex-ignore-order) ivy-re-builders-alist)

  ;; Set minibuffer height for different commands
(setf (alist-get 'swiper ivy-height-alist) 15)
(setf (alist-get 'counsel-switch-buffer ivy-height-alist) 7))

(use-package ivy-hydra
  :defer t
  :after hydra)

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil)) ;; Don't start searches with ^

(use-package ivy-rich
  :init
  (ivy-rich-mode 1)
  :config
  (setq ivy-format-function #'ivy-format-function-line)
  (setq ivy-rich--display-transformers-list
        (plist-put ivy-rich-display-transformers-list
                   'ivy-switch-buffer
                   '(:columns
                     ((ivy-rich-candidate (:width 40))
                      (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right)); return the buffer indicators
                      (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))          ; return the major mode info
                      (ivy-rich-switch-buffer-project (:width 15 :face success))             ; return project name using `projectile'
                      (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))  ; return file path relative to project root or `default-directory' if project is nil
                     :predicate
                     (lambda (cand)
                       (if-let ((buffer (get-buffer cand)))
                           ;; Don't mess with EXWM buffers
                           (with-current-buffer buffer
                             (not (derived-mode-p 'exwm-mode)))))))))

(use-package flx  ;; Improves sorting for fuzzy-matched results
  :defer t
  :init
  (setq ivy-flx-limit 10000))

(use-package smex ;; Adds M-x recent command sorting for counsel-M-x
  :defer 1
  :after counsel)

(use-package wgrep)

(dw/leader-key-def
  "r"   '(ivy-resume :which-key "ivy resume")
  "f"   '(:ignore t :which-key "files")
  "ff"  '(counsel-find-file :which-key "open file")
  "fr"  '(counsel-recentf :which-key "recent files")
  "fR"  '(revert-buffer :which-key "revert file")
  "fl"  '(load-file :which-key "load file")
  "fs"  '(save-buffer :which-key "save file"))
  
;; Jumping with Avy
(use-package avy
  :commands (avy-goto-char avy-goto-word-0 avy-goto-line))

(dw/leader-key-def
  "j"   '(:ignore t :which-key "jump")
  "jj"  '(avy-goto-char :which-key "jump to char")
  "jw"  '(avy-goto-word-0 :which-key "jump to word")
  "jl"  '(avy-goto-line :which-key "jump to line"))

;; Change theme / White space mode  
(dw/leader-key-def
  "t"  '(:ignore t :which-key "toggles")
  "tw" 'whitespace-mode
  "tt" '(counsel-load-theme :which-key "choose theme"))
  
;; Highlight Matching Braces
(use-package paren
  :config
  (set-face-attribute 'show-paren-match-expression nil :background "#363e4a")
  (show-paren-mode 1))
  
;; Smart Parens
(use-package smartparens
  :hook ((prog-mode . smartparens-mode)
         (org-mode . smartparens-mode)))
  
;; Pinentry
(use-package pinentry
  :config
  (setq epa-pinentry-mode 'loopback))
(pinentry-start)

;; Rainbow delimiters
(use-package rainbow-delimiters
  :hook ((prog-mode . rainbow-delimiters-mode)
         (org-mode . rainbow-delimiters-mode)))  

;; Rainbow mode
(use-package rainbow-mode
  :defer t
  :hook (org-mode))  

;; Flycheck configuration
(use-package flycheck
  :defer t
  :hook (lsp-mode . flycheck-mode)
        (org-mode . flycheck-mode))

;; TRAMP
  ;; Set default connection mode to SSH 
(setq tramp-default-method "ssh")

;; Commenting lines
(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))
  
;; Automatically clean whitespace
(use-package ws-butler
  :hook (((text-mode . ws-butler-mode)
         (prog-mode . ws-butler-mode)
         (org-mode . ws-butler-mode))))
         
;; Helpers
(defun dw/org-file-jump-to-heading (org-file heading-title)
  (interactive)
  (find-file (expand-file-name org-file))
  (goto-char (point-min))
  (search-forward (concat "* " heading-title))
  (org-overview)
  (org-reveal)
  (org-show-subtree)
  (forward-line))

(defun dw/org-file-show-headings (org-file)
  (interactive)
  (find-file (expand-file-name org-file))
  (counsel-org-goto)
  (org-overview)
  (org-reveal)
  (org-show-subtree)
  (forward-line))         

;; Displaying World Time
(setq display-time-world-list
  '(("America/Sao_Paulo" "Brasilia")
    ("America/Los_Angeles" "Seattle")
    ("America/New_York" "New York")
    ("Europe/Athens" "Athens")
    ("Pacific/Auckland" "Auckland")
    ("Asia/Shanghai" "Shanghai")))
(setq display-time-world-time-format "%a, %d %b %I:%M %p %Z")

;; Mode Line
  ;; Basic
(setq display-time-format "%H:%M %d%p %b %y"
      displat-time-default-load-average nil)
(display-time-mode)
      
(use-package diminish)

  ;; Smart Mode Line
(use-package smart-mode-line
  :config
  (setq sml/no-confirm-load-theme t)
  (sml/setup)
  (sml/apply-theme 'respectful) ; Respect the theme colors
  (setq sml/mode-width 'right
      sml/name-width 60)

  (setq-default mode-line-format
  `("%e"
      ,(when dw/exwm-enabled
          '(:eval (format "[%d] " exwm-workspace-current-index)))
      mode-line-front-space
      evil-mode-line-tag
      mode-line-mule-info
      mode-line-client
      mode-line-modified
      mode-line-remote
      mode-line-frame-identification
      mode-line-buffer-identification
      sml/pos-id-separator
      (vc-mode vc-mode)
      " "
      ;mode-line-position
      sml/pre-modes-separator
      mode-line-modes
      " "
      mode-line-misc-info))

  (setq rm-excluded-modes
    (mapconcat
      'identity
      ; These names must start with a space!
      '(" GitGutter" " MRev" " company"
      " Helm" " Undo-Tree" " Projectile.*" " Z" " Ind"
      " Org-Agenda.*" " ElDoc" " SP/s" " cider.*")
      "\\|")))
      
  ;; Doom Modeline
(use-package all-the-icons)      
(use-package minions
  :hook (doom-modeline-mode . minions-mode)
  :custom
  (minions-mode-line-lighter ""))

(use-package doom-modeline
  :hook (after-init . doom-modeline-init)
  :custom
  (doom-modeline-height 20)
  (doom-modeline-bar-width 6)
  (doom-modeline-lsp t)
  (doom-modeline-github nil)
  (doom-modeline-mu4e nil)
  (doom-modeline-irc nil)
  (doom-modeline-minor-modes t)
  (doom-modeline-persp-name nil)
  (doom-modeline-buffer-file-name-style 'truncate-except-project)
  (doom-modeline-major-mode-icon nil))
  (doom-modeline-mode 1)
  
;; Keychord
(use-package use-package-chords
  :disabled
  :config (key-chord-mode 1))    
    
;; Kdeconnect configuration
(use-package kdeconnect
  :config
  (setq kdeconnect-active-device "9180bcdf671688e2")
  (dw/leader-key-def
  "k"  '(:ignore t :which-key "kdeconnect")
  "ks" '(:ignore t :which-key "send")
  "kp" '(:ignore t :which-key "ping")
  "kpp" '(kdeconnect-ping :which-key "ping")
  "kpm" '(kdeconnect-ping-msg :which-key "msg")
  "ksf" '(kdeconnect-send-file :which-key "file")
  "kss" '(kdeconnect-send-sms :which-key "sms")
  "kd" '(:ignore t :which-key "devices")
  "kdl" '(kdeconnect-list-devices :which-key "list")
  "kdg" '(kdeconnect-get-active-device :which-key "get active device")
  "kds" '(kdeconnect-select-active-device :which-key "select active device")
  "kr" '(kdeconnect-ring :which-key "ring")))
  
;; Notifications configuration
(use-package alert
  :commands alert
  :config
  (setq alert-default-style 'notifications))

;; Discord configuration
(use-package elcord
  :custom
  (elcord-display-burffer-details nil)
  :config
  (elcord-mode))
  
;; Window configuration
  ;; Zooming
  ;; The keybindings for this are C-M-- and C-M-=
(use-package default-text-scale
  :defer 1
  :config
  (default-text-scale-mode))
  ;; Window selection
(use-package ace-window
  :bind (("M-o" . ace-window))
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))) 
  
  ;; Window history
(winner-mode)
(define-key evil-window-map "u" 'winner-undo)
  
  ;; Split/Delete
(dw/leader-key-def
  "w"   '(:ignore t :which-key "window")
  "ws"   '(:ignore t :which-key "split")
  "wsj" '(split-window-below :which-key "split window below")
  "wsl" '(split-window-right :which-key "split window right")
  "wd"   '(:ignore t :which-key "delete")
  "wdd" '(delete-window :which-key "current window")
  "wdo" '(delete-other-windows :which-key "other windows"))
  
;; Google translate configuration
(use-package popup)
(use-package google-translate
  :after popup
  :config
  (require 'google-translate-default-ui
  (dw/leader-key-def
    "/"  '(:ignore t :which-key "translate")
    "/a" '(google-translate-at-point :which-key "at point")
    "/q" '(google-translate-query-translate :which-key "query"))))
    
;; Yasnippet configuration
(use-package yasnippet-snippets)
(use-package yasnippet
  :hook ((prog-mode . yas-minor-mode)
         (org-mode . yas-minor-mode))
  :config
  (yas-reload-all))    
    
;; Auto complete configuration
(use-package fuzzy)
(use-package auto-complete
  :after popup fuzzy yasnippet) 
  (ac-config-default)
  (setq ac-use-menu-map t)
  (define-key ac-menu-map "\C-j" 'ac-next)
  (define-key ac-menu-map "\C-k" 'ac-previous)
  (auto-complete-mode 1)

;; Buffer configuration
(defun dw/ignore-non-vimb-buffers (buffer-name)
  (if-let ((buf (get-buffer buffer-name)))
    (when buf
      (with-current-buffer buf
        (not (and (derived-mode-p 'exwm-mode)
                  (string-equal exwm-class-name "Vimb")))))))

(defun dw/switch-to-browser-buffer ()
  (interactive)
  (let ((ivy-use-virtual-buffers nil)
        (ivy-ignore-buffers (append ivy-ignore-buffers '(dw/ignore-non-vimb-buffers))))
    (counsel-switch-buffer)))

(global-set-key (kbd "C-M-j") 'counsel-switch-buffer)
(global-set-key (kbd "C-M-k") 'dw/switch-to-browser-buffer)

(dw/leader-key-def
  "b"   '(:ignore t :which-key "buffers")
  "bb"  '(counsel-switch-buffer :which-key "switch buffer")
  "bn"  '(bury-buffer :which-key "bury buffer")
  "bd"  '(:ignore t :which-key "kill buffer")
  "bdd" '(kill-buffer-and-window :which-key "current buffer")
  "bdo" '(kill-buffer :which-key "other buffer"))
   
;; Expand region configuration
(use-package expand-region
  :bind (("M-[" . er/expand-region)
         ("M-{" . er/mark-outside-pairs)))  

;; Dired configuration
  ;; Omit-mode
(use-package dired
  :ensure nil
  :defer 1
  :commands (dired dired-jump)
  :config
  (setq dired-listing-switches "-agho --group-directories-first"
        dired-omit-files "^\\.[^.].*"
        dired-omit-verbose nil))

(autoload 'dired-omit-mode "dired-x")       
          
(add-hook 'dired-mode-hook
    (lambda ()
    (interactive)
    (dired-omit-mode 1)))

  ;; Dired design configuration
(add-hook 'dired-mode-hook
  (lambda ()
  (interactive)
  (all-the-icons-dired-mode 1)
  (hl-line-mode 1)))
    
(add-hook 'dired-load-hook
  (lambda ()
  (interactive)
  (dired-collapse)))

(use-package dired-single
  :defer t)

(use-package dired-ranger
  :defer t)

(use-package dired-collapse
  :defer t) 
  
  ;; Key bindings
(evil-collection-define-key 'normal 'dired-mode-map
  "h" 'dired-single-up-directory
  "H" 'dired-omit-mode
  "l" 'dired-single-buffer
  "y" 'dired-ranger-copy
  "n" 'dired-ranger-move
  "p" 'dired-ranger-paste)    

(defun dw/dired-link (path)
  (lexical-let ((target path))
    (lambda () (interactive) (message "Path: %s" target) (dired target))))

(dw/leader-key-def
  "d"   '(:ignore t :which-key "dired")
  "dd"  '(dired :which-key "here")
  "dh"  `(,(dw/dired-link "~") :which-key "home")
  "do"  `(,(dw/dired-link "~/Downloads") :which-key "downloads")
  "dp"  `(,(dw/dired-link "~/Pictures") :which-key "pictures")
  "dv"  `(,(dw/dired-link "~/Videos") :which-key "videos")
  "da"  `(,(dw/dired-link "~/Documents") :which-key "documents")
  "ds"  `(,(dw/dired-link "~/Songs") :which-key "songs"))
  
;; Opening Files Externally
(use-package openwith
  :config
  (setq openwith-associations
    (list
      (list (openwith-make-extension-regexp
             '("mpg" "mpeg" "mp3" "mp4"
               "avi" "wmv" "wav" "mov" "flv"
               "ogm" "ogg" "mkv"))
             "stremio"
             '(file))
      (list (openwith-make-extension-regexp
             '("xbm" "pbm" "pgm" "ppm" "pnm"
               "png" "gif" "bmp" "tif" "jpeg"
               "jpg")) 
             "gthumb"
             '(file))
       (list (openwith-make-extension-regexp
              '("pdf"))
              "okular"
              '(file))))
   (openwith-mode 1))
   
