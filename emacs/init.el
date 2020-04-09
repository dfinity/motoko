;; Motoko Emacs IDE
;; ===
;;
;; A minimal set of packages + configuration that lets you use the
;; Motoko LSP support from within Emacs without having to change
;; your personal configuration.
;;
;; Requirements:
;; 1. Emacs 26 or newer
;; 2. dfx
;;   OR
;; 2. mo-ide binary (Can be generated with `make mo-ide` in the
;;    compiler repo)
;;
;; How to use:
;;
;; 1. Change the settings below to match your setup and preferences
;;
;; 2. Start Emacs from your command line like so:
;;
;; `emacs -q --load ~/code/motoko/emacs/init.el`
;;
;; (adjust to match the path to this file)
;;
;; 3. Open a Motoko source file in a Motoko project and select the
;; project root from the menu

;;======== SETTINGS ==========
;; Change this to the command that starts the language server on your
;; machine. In a dfx project with dfx on your path that is just `dfx
;; ide`. If you have a more complicated setup you have to pass the
;; proper entry point and library paths to the `mo-ide` binary
;; yourself.
(setq mo/lsp-command '("dfx" "_language-service"))

;; Change this to point to the directory where you cloned the
;; motoko repo
(setq mo/installation-directory "~/code/motoko")

;; Change nil to t if you want to use vim bindings for editing and
;; navigation
(setq mo/use-evil nil)
;;============================

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

;; We use ivy to make the popup asking for the project root
;; non-obnoxious. It's not necessarily required for the LSP to work,
;; so if you're working this into your existing config helm would work
;; just as well.
(use-package ivy
  :ensure t
  :config (ivy-mode 1))

(use-package swift-mode :ensure t)
(eval-and-compile
  (defun mo/mo-mode-load-path ()
    (format "%s/emacs" mo/installation-directory)))
(use-package motoko-mode
  :load-path (lambda () (list (mo/mo-mode-load-path)))
  :config
  (add-hook 'motoko-mode-hook 'flycheck-mode)
  (add-hook 'motoko-mode-hook 'lsp)
  (add-hook 'motoko-mode-hook 'company-mode))

(use-package lsp-mode :ensure t
  :config
  (setq lsp-prefer-flymake nil)
  (setq lsp-auto-configure t)
  (add-to-list 'lsp-language-id-configuration '(motoko-mode . "motoko"))
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection mo/lsp-command)
    :major-modes '(motoko-mode)
    :server-id 'mo-lsp)))

(use-package lsp-ui :ensure t :commands lsp-ui-mode
  :config (setq lsp-ui-sideline-enable nil))
(use-package company :ensure t)
(use-package company-lsp :ensure t :commands company-lsp)
(use-package yasnippet :ensure t)
(use-package flycheck :ensure t)

(when mo/use-evil
  (use-package evil
    :ensure t
    :init
    (progn
      (setq evil-want-C-u-scroll t)
      (evil-mode 1)
      (evil-declare-change-repeat 'company-complete))))
