;; ActorScript Emacs IDE
;; ===
;;
;; A minimal set of packages + configuration that lets you use the
;; ActorScript LSP support from within Emacs without having to change
;; your personal configuration.
;;
;; Requirements:
;; - Emacs 26 or newer
;; - `as-ide` binary (Can be generated with `make as-ide` in the
;;   compiler repo)
;;
;; How to use:
;;
;; 1. Change the settings below to match your setup and preferences
;;
;; 2. Start Emacs from your command line like so:
;;
;; `emacs -q --load ~/code/actorscript/emacs/init.el`
;;
;; (adjust to match the path to this file)
;;
;; 3. Open an AS source file in an AS project and select the project
;; root from the menu

;;======== SETTINGS ==========
;; Change this to point to your `as-ide` binary, or leave it if the
;; binary can be found on your PATH
(setq as/ide-binary "as-ide")

;; Change this to point to the directory where you cloned the
;; actorscript repo
(setq as/installation-directory "~/code/actorscript")

;; Change nil to t if you want to use vim bindings for editing and
;; navigation
(setq as/use-evil nil)
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
  (defun as/as-mode-load-path ()
    (format "%s/emacs" as/installation-directory)))
(use-package actorscript-mode
  :load-path (lambda () (list (as/as-mode-load-path)))
  :config
  (add-hook 'actorscript-mode-hook 'flycheck-mode)
  (add-hook 'actorscript-mode-hook 'lsp)
  (add-hook 'actorscript-mode-hook 'company-mode))

(use-package lsp-mode :ensure t
  :config
  (setq lsp-prefer-flymake nil)
  (setq lsp-auto-configure t)
  (add-to-list 'lsp-language-id-configuration '(actorscript-mode . "actorscript"))
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection as/ide-binary)
    :major-modes '(actorscript-mode)
    :server-id 'asls)))

(use-package lsp-ui :ensure t :commands lsp-ui-mode)
(use-package company :ensure t)
(use-package company-lsp :ensure t :commands company-lsp)
(use-package yasnippet :ensure t)
(use-package flycheck :ensure t)

(when as/use-evil
  (use-package evil
    :ensure t
    :init
    (progn
      (setq evil-want-C-u-scroll t)
      (evil-mode 1)
      (evil-declare-change-repeat 'company-complete))))
