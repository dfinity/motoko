;; Motoko Emacs IDE
;; ===
;;
;; A minimal set of packages + configuration that lets you use the
;; Motoko LSP support from within Emacs without having to change
;; your personal configuration.
;;
;; Requirements:
;; 1. Emacs 26 or newer
;; 2. mo-ide binary (Can be generated with `make mo-ide` in the
;;    compiler repo)
;;
;; How to use:
;;
;; 1. Change the settings below to match your setup and preferences
;;
;; 2. Start Emacs from your command line like so:
;;
;; `emacs -q --load ~/motoko/emacs/init_eglot.el`
;;
;; (adjust to match the path to this file)
;;
;; 3. After opening a Motoko file, run M-x eglot and select your entry point

;;======== SETTINGS ==========
;; Change this to point to the directory where you cloned the
;; motoko repo
(defvar mo/installation-directory "~/motoko")

;; Change this to the command that starts the mo-ide binary on your machine.
;; If you want to customize further or pass additional arguments, check out
;; mo/lsp-startup.
(defvar mo/lsp-command "mo-ide")

;; Change this to point to dfx's cache if you don't want to work against the
;; stdlib version in your compiler checkout. If you don't want the std library
;; or manage it through vessel set this to `nil`
(defvar mo/stdlib-args
  `("--package" "stdlib" ,(concat mo/installation-directory "/stdlib/src")))

;; Change nil to t if you want to use vim bindings for editing and
;; navigation
(defvar mo/use-evil nil)
;;============================

(defun mo/vessel-args ()
  "Run vessel sources."
  (when (file-exists-p "vessel.json")
    (let ((output (split-string (shell-command-to-string "vessel sources") " " t)))
      (when (string-equal (car output) "--package") output))))

(defun mo/lsp-startup (_interactive?)
  "Asks for an entry-point and constructs the LS command."
  (let* ((entry-point (read-file-name "Entry Point: "))
         (command `(,mo/lsp-command
                    "--canister-main" ,(expand-file-name entry-point)
                    ,@mo/stdlib-args
                    ,@(mo/vessel-args)
                    ;; DON'T CHANGE THIS
                    "--port" :autoport)))
    command))

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(use-package swift-mode :ensure t)
(eval-and-compile
  (defun mo/mo-mode-load-path ()
    (format "%s/emacs" mo/installation-directory)))
(use-package motoko-mode
  :load-path (lambda () (list (mo/mo-mode-load-path)))
  :config
  (add-hook 'motoko-mode-hook 'company-mode)
  (add-hook 'motoko-mode-hook 'eglot))

(use-package company :ensure t)

(use-package eglot
  :ensure t
  :config
  (add-to-list 'eglot-server-programs
               `(motoko-mode . mo/lsp-startup)))

(when mo/use-evil
  (use-package evil
    :ensure t
    :init
    (progn
      (setq evil-want-C-u-scroll t)
      (evil-mode 1)
      (evil-declare-change-repeat 'company-complete))))
