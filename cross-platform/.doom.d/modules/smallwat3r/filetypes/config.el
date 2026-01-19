;;; smallwat3r/filetypes/config.el -*- lexical-binding: t; -*-

;; Applescript
;; doc: https://github.com/emacsorphanage/applescript-mode
(use-package! applescript-mode
  :mode (("\\.scpt\\'" . applescript-mode)
         ("\\.applescript\\'" . applescript-mode))
  :interpreter ("osascript" . applescript-mode))

;; Logs
;; doc: https://github.com/doublep/logview
(use-package! logview
  :mode ("\\.log.*" . logview-mode))

;; Nginx
;; doc: https://github.com/ajc/nginx-mode
(use-package! nginx-mode
  :mode (("/nginx/conf.d/.*" . nginx-mode)
         ("/nginx/.*\\.conf\\'" . nginx-mode)
         ("/nginx/sites-\\(?:available\\|enabled\\)/" . nginx-mode)))

;; SQL
;; doc: https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#SQL-Mode
(use-package! sql
  :mode ("\\.\\(m\\|my\\)?sql\\'" . sql-mode)
  :custom
  ;; mostly used for local development only so disable SSL mode
  ;; by default to ease connectivity from localhost
  (sql-mysql-options '("--ssl-mode=DISABLED"))
  (sql-mysql-login-params '((user :default "root")
                            password
                            database
                            (server :default "127.0.0.1")
                            (port :default 3306)))
  (sql-postgres-login-params '((user :default "postgres")
                               password
                               database
                               (server :default "127.0.0.1")
                               (port :default 5432))))

;; Makefile
(use-package! makefile-mode
  :mode ("Makefile.*" . makefile-mode))

;; TOML
(add-to-list 'auto-mode-alist '("poetry\\.lock\\'" . conf-toml-mode))

;; ROS launch files
(add-to-list 'auto-mode-alist '("\\.launch\\'" . xml-mode))

;; Lua
(add-to-list 'auto-mode-alist '("conky\\.conf\\'" . lua-mode))
