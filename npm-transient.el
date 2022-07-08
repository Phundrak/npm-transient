;;; npm-transient.el --- transient menu for working with npm projects

;; Version: 0.6.0
;; Author: Allen Gooch <allen.gooch@gmail.com>
;; Maintainer: Lucien Cartier-Tilet <lucien@phundrak.com>
;; Url: https://github.com/Phundrak/npm-transient
;; Keywords: convenience, project, javascript, node, npm
;; Package-Requires: ((emacs "25.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This package allows you to easily work with npm projects.  It
;; provides a transient menu for interacting with your package
;; manager.  To launch it, simply run `M-x npm-transient'.

;;; Credit:

;; This package began as a fork of the npm-mode package, and its repository history has been preserved.

;;; Code:

(require 'json)
(require 'transient)
(require 'tabulated-list)


;;; Group, custom variables, variables, and constants

(defgroup npm-transient nil
  "Customization group for `npm-transient'."
  :prefix "npm-transient-"
  :group 'tools
  :group 'convenience
  :link '(url-link :tag "Github" "https://github.com/Phundrak/eshell-info-banner.el"))

(defcustom npm-transient-comint t
  "Colorize NPM’s buffer.
See the argument COMINT in function `compile'."
  :group 'npm-transient
  :type 'boolean
  :safe #'booleanp)

(defcustom npm-transient-command-prefix "C-c n"
  "Prefix for npm-transient."
  :group 'npm-transient
  :type 'string)

(defvar npm-transient--project-file-name "package.json"
  "The name of npm project files.")


;;; Internal transient
(eval-when-compile
  (defmacro npm-transient--define-infix (key name description type default &rest reader)
    "Define infix and its corresponding variable at once.
The variable is named npm-transient--NAME, is of type TYPE, and has a
docstring DESCRIPTION.
The KEY and READER are for the infix declaration."
    (let ((var-name (concat "npm-transient--" name)))
      `(progn
         (defcustom ,(intern var-name) ,default
           ,(concat description "
Internal variable for transient menus.")
           :type ,type
           :group 'npm-transient)
         (transient-define-infix ,(intern (concat "npm-transient--set-" name)) ()
           ,(format "Set %s option." var-name)
           :class 'transient-lisp-variable
           :variable ',(intern var-name)
           :key ,key
           :description ,description
           :argument ,(concat "--" name)
           :reader (lambda (&rest _) ,@reader)))))

  (npm-transient--define-infix
   "-p" "package-manager-arguments" "Package manager arguments"
   'string ""
   (read-string "Package manager arguments: "))

  (npm-transient--define-infix
   "-c" "command-arguments" "Package manager’s subcommand arguments"
   'string ""
   (read-string "Package manager’s subcommand arguments: "))

  (npm-transient--define-infix
   "-d" "dep-dest" "How to install the dependency"
   'symbol 'regular
   (intern (completing-read "How to install the dependency: "
                            '(regular dev peer bundle optional))))

  (npm-transient--define-infix
   "-D" "list-depth" "Depth when listing dependencies (unused for now)"
   'integer 0
   (round (read-number "Depth for listing dependencies: "))))


;;; Internal functions

(defun npm-transient--ensure-npm-module ()
  "Assert the current directory is inside an npm module."
  (npm-transient--project-file))

(defun npm-transient--project-file ()
  "Return path to the project file, or nil.
If project file exists in the current working directory, or a
parent directory recursively, return its path.  Otherwise, return
nil."
  (let ((dir (locate-dominating-file default-directory npm-transient--project-file-name)))
    (unless dir
      (error (concat "Error: cannot find " npm-transient--project-file-name)))
    (concat dir npm-transient--project-file-name)))

(defun npm-transient--get-project-property (prop &optional suffix)
  "Get the given PROP from the current project file.
If SUFFIX is non-nil, then it gets suffixed to the car of each
value if the result is a list."
  (let* ((project-file (npm-transient--project-file))
         (json-object-type 'hash-table)
         (json-contents (with-temp-buffer
                          (insert-file-contents project-file)
                          (buffer-string)))
         (json-hash (json-read-from-string json-contents))
         (value (gethash prop json-hash))
         values)
    (if (not (hash-table-p value))
        value
      (maphash (lambda (key value)
                 (setq values
                       (append values
                               `((,(if suffix (concat key " " suffix) key) . ,key)))))
               value)
      values)))

(defun npm-transient--get-project-scripts ()
  "Get a list of project scripts."
  (npm-transient--get-project-property "scripts"))

(defun npm-transient--get-project-dependencies ()
  "Get a list of project dependencies."
  (append (npm-transient--get-project-property "dependencies")
          (npm-transient--get-project-property "devDependencies" "(dev)")
          (npm-transient--get-project-property "peerDependencies" "(peer)")
          (npm-transient--get-project-property "bundledDependencies" "(bundled)")
          (npm-transient--get-project-property "optionalDependencies" "(optional)")))

(cl-defun npm-transient--make-command (&key (command "")    (arguments "")
                                            (extra-args "") double-dash-args)
  "Generate a package manager command.
The function will automatically add package manager arguments
from `npm-transient--package-manager-arguments'.

It will then add the COMMAND, ARGUMENTS to the command,
`npm-transient--command-arguments' and EXTRA-ARGS as additional
arguments to the command.

If DOUBLE-DASH-ARGS is non-nil, it will add a double dash
followed by the content of the variable."
  (replace-regexp-in-string
   " +" " "
   (concat "npm "
           (or npm-transient--package-manager-arguments "")
           " "
           command
           " "
           arguments
           " "
           (or npm-transient--command-arguments "")
           " "
           extra-args
           (if double-dash-args (concat " -- " double-dash-args) ""))))

(defun npm-transient--exec-process (cmd)
  "Execute a process running CMD."
  (let ((compilation-buffer-name-function
         (lambda (mode)
           (format "*npm:%s - %s*"
                   (npm-transient--get-project-property "name") cmd))))
    (message (concat "Running " cmd))
    (compile cmd npm-transient-comint)))

(cl-defun npm-transient--exec-command (&key (command "")    (arguments "")
                                            (extra-args "") double-dash-args)
  "Exec package manager COMMAND.
Pass `npm-transient--package-manager-arguments' as arguments to the
package manager, and `npm-transient--command-arguments' to the
command.
Optionally pass ARGUMENTS to the command such as \\='is-odd\\='
in \\='npm install is-odd\\='.
Optionally add EXTRA-ARGS as extra arguments for the command.
Optionally, pass DOUBLE-DASH-ARGS as arguments after a double
dash."
  (npm-transient--exec-process
   (npm-transient--make-command :command command       :arguments arguments
                           :extra-args extra-args :double-dash-args double-dash-args)))

(defun npm-transient--uninstall-dep (dep)
  "Uninstall dependency DEP."
  (interactive (list
                (let* ((deps (npm-transient--get-project-dependencies))
                       (dep-read (completing-read "Uninstall dependency: " deps)))
                  (cdr (assoc-string dep-read deps)))))
  (npm-transient--exec-command :command "uninstall" :arguments dep))

(defun npm-transient--install-dep (dep)
  "Install DEP with the package manager.
Also see `npm-transient--dep-dest',
`npm-transient--package-manager-arguments', and
`npm-transient--command-arguments'."
  (interactive "sDependency name: ")
  (let ((save-argument (pcase npm-transient--dep-dest
                           ('regular  "")
                           ('dev      "--save-dev")
                           ('peer     "--save-peer")
                           ('bundle   "--save-bundle")
                           ('optional "--save-optional")
                           (otherwise (error "Unknown option in npm-transient--install-deps: %s"
                                             otherwise)))))
    (npm-transient--exec-command :command "install"
                            :arguments dep
                            :extra-args save-argument)))

(defun npm-transient--install-deps ()
  "Install all dependencies from package.json."
  (interactive)
  (npm-transient--exec-command :command "install"))

(defun npm-transient--list-dependencies ()
  "List dependencies of the current project.
The dependency depth shown depends on `npm-transient--list-depth'
which can be set in the dependencies transient menu."
  (interactive)
  (let ((cmd (npm-transient--make-command :command "list"
                                     :extra-args "--depth=0"))
        (packages))
    (with-temp-buffer
      (shell-command cmd (current-buffer) (current-buffer))
      (goto-char (point-min))
      (re-search-forward "$")
      (goto-char (1+ (point)))
      (delete-region (point) (point-min))
      (goto-char (point-min))
      (save-match-data
        (while (re-search-forward (rx (group (+ (not space)))
                                      "@"
                                      (group (+ (not space)))
                                      (* (not "@"))
                                      eol)
                                  nil t)
          (add-to-list 'packages
                       (list nil (vector (match-string 1) (match-string 2)))
                       t))))
    (switch-to-buffer "*npm dependencies*")
    (setq tabulated-list-format [("Dependency" 40) ("Version" 8)])
    (setq tabulated-list-entries packages)
    (message "entries: %S" tabulated-list-entries)
    (tabulated-list-init-header)
    (tabulated-list-print)))

(defun npm-transient--clean-project-root ()
  "Delete node_modules directory in root project."
  (interactive)
  (let ((dir (concat (file-name-directory (npm-transient--ensure-npm-module)) "node_modules")))
    (if (file-directory-p dir)
        (when (yes-or-no-p (format "Are you sure you wish to delete %s?" dir))
          (delete-directory dir t nil)
          (when (yes-or-no-p (format "Remove lock file?"))
            (delete-file "package-lock.json")))
      (message (format "%s has already been cleaned" dir)))))

(defun npm-transient--init-project ()
  "Initialize an npm project."
  (interactive)
  (npm-transient--exec-command :command "init"))

(defun npm-transient-run--read-command ()
  "Read command from user."
  (completing-read "Run script: " (npm-transient--get-project-scripts)))

(defun npm-transient--run-script (script)
  "Run an npm SCRIPT."
  (interactive
   (list (completing-read "Run script: " (npm-transient--get-project-scripts))))
  (npm-transient--exec-command :comand "run" :arguments script))

(defun npm-transient--visit-project-file ()
  "Visit the project file."
  (interactive)
  (find-file (npm-transient--project-file)))


;;; User interface


;;; Transient menus
(transient-define-prefix npm-transient-manage-dependencies ()
  ["Options"
   (npm-transient--set-package-manager-arguments)
   (npm-transient--set-command-arguments)
   (npm-transient--set-dep-dest)
   (npm-transient--set-list-depth)]
  ["Commands"
   ("a" "Add dependency" npm-transient--install-dep)
   ("i" "Install project dependencies" npm-transient--install-deps)
   ("u" "Uninstall depedency" npm-transient--uninstall-dep)
   ("l" "List dependencies" npm-transient--list-dependencies)])

(transient-define-prefix npm-transient ()
  ["Options"
   (npm-transient--set-package-manager-arguments)
   (npm-transient--set-command-arguments)]
  ["Actions"
   ("c" "Clean project root" npm-transient--clean-project-root)
   ("d" "Manage dependencies" npm-transient-manage-dependencies)
   ("i" "Init project" npm-transient--init-project)
   ("r" "Run script" npm-transient--run-script)
   ("v" "Visit package.json" npm-transient--visit-project-file)
   ("q" "Quit" (lambda () (interactive) nil))])

(provide 'npm-transient)
;;; npm-transient.el ends here
