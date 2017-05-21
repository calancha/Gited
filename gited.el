;;; gited.el --- Operate on Git branches like dired  -*- lexical-binding:t -*-
;;
;; Filename: gited.el
;; Description: Operate on Git branches like dired
;;
;; Author: Tino Calancha <tino.calancha@gmail.com>
;; Maintainer: Tino Calancha <tino.calancha@gmail.com>
;; Copyright (C) 2016-2017, Tino Calancha, all rights reserved.
;; Created: Wed Oct 26 01:28:54 JST 2016
;; Compatibility: GNU Emacs: 24.4
;; Version: 0.1
;; Package-Requires: ((emacs "24.4") (cl-lib "0.5"))
;; Last-Updated: Sun May 21 14:43:41 JST 2017
;;           By: calancha
;;     Update #: 597
;;
;; Features that might be required by this library:
;;
;;   `vc-git', `cl-lib', `dired', `tabulated-list',
;;   `find-func'.
;;
;; Keywords: vc, convenience
;;
;;

;;; Commentary:
;;
;; This library lists the branches in a Git repository.  Then you can
;; operate on them with a dired-like interface.
;;
;; The command `gited-list-branches' prompts for the kind of branch
;; (local branches, remote branches or tags) and lists them.
;; This command is used quite often, thus it might be convenient
;; to give it a key binding.  For instance, if `gited.el' is in
;; your `load-path', then you can bind it to `C-x C-g' in Dired buffers
;; by adding the following lines into your .emacs file:
;;
;; (require 'gited)
;; (define-key dired-mode-map "\C-x\C-g" 'gited-list-branches)
;; 
;; If you are familiar with Dired, then you already know how to use
;; Gited; that's because most of the Gited commands with a Dired equivalent
;; share same keybindings.
;; For instance `gited-rename-branch' is bound to `R' as `dired-do-rename'.
;; Similarly, `gited-mark' is bound to `m' as `dired-mark'.
;;
;;
;;  Internal variables defined here:
;;
;;   `gited--hide-details-set', `gited--op',
;;   `gited--running-async-op', `gited-actual-switches',
;;   `gited-after-change-hook', `gited-author-face',
;;   `gited-author-idx', `gited-bisect-buf-name',
;;   `gited-bisect-buffer', `gited-bisect-buffer',
;;   `gited-bisect-output-name', `gited-branch-after-op',
;;   `gited-branch-alist', `gited-branch-idx',
;;   `gited-branch-name-face', `gited-buffer',
;;   `gited-buffer-name', `gited-commit-idx',
;;   `gited-commit-msg-face', `gited-current-branch',
;;   `gited-date-idx', `gited-date-regexp',
;;   `gited-date-time-face', `gited-del-char',
;;   `gited-deletion-branch-face', `gited-deletion-face',
;;   `gited-flag-mark-face', `gited-flag-mark-line-face',
;;   `gited-header', `gited-list-format',
;;   `gited-list-refs-format-command', `gited-log-buffer',
;;   `gited-mark-face', `gited-mark-idx',
;;   `gited-marker-char', `gited-mode',
;;   `gited-mode-map', `gited-modified-branch',
;;   `gited-new-or-deleted-files-re', `gited-op-string',
;;   `gited-output-buffer', `gited-output-buffer-name',
;;   `gited-re-mark', `gited-ref-kind',
;;   `gited-section-highlight-face', `gited-toplevel-dir'.
;;
;;  Coustom variables defined here:
;;
;;   `gited-current-branch-face', `gited-date-format',
;;   `gited-expert', `gited-patch-options',
;;   `gited-patch-program', `gited-protected-branches',
;;   `gited-reset-mode', `gited-short-log-cmd',
;;   `gited-switches', `gited-use-header-line',
;;   `gited-verbose'.
;;
;;  Macros defined here:
;;
;;   `gited-map-over-marks', `gited-mark-if',
;;   `gited-with-current-branch'.
;;
;;  Commands defined here:
;;
;;   `gited--mark-merged-branches-spec', `gited--mark-unmerged-branches-spec',
;;   `gited-add-patched-files', `gited-apply-add-and-commit-patch',
;;   `gited-apply-patch', `gited-async-operation',
;;   `gited-bisect', `gited-branch-clear',
;;   `gited-checkout-branch', `gited-commit',
;;   `gited-copy-branch', `gited-copy-branchname-as-kill',
;;   `gited-delete-branch', `gited-diff-with-branch',
;;   `gited-do-delete', `gited-do-flagged-delete',
;;   `gited-do-kill-lines', `gited-extract-patches',
;;   `gited-flag-branch-deletion', `gited-goto-branch',
;;   `gited-goto-first-branch', `gited-goto-last-branch',
;;   `gited-kill-line', `gited-list-branches',
;;   `gited-log', `gited-log-last-n-commits',
;;   `gited-mark', `gited-mark-branches-containing-commit',
;;   `gited-mark-branches-containing-regexp', `gited-mark-branches-regexp',
;;   `gited-mark-merged-branches', `gited-mark-unmerged-branches',
;;   `gited-merge-branch', `gited-move-to-author',
;;   `gited-move-to-branchname', `gited-move-to-date',
;;   `gited-move-to-end-of-author', `gited-move-to-end-of-branchname',
;;   `gited-move-to-end-of-date', `gited-next-line',
;;   `gited-next-marked-branch', `gited-number-marked',
;;   `gited-origin', `gited-prev-line',
;;   `gited-prev-marked-branch', `gited-pull',
;;   `gited-push', `gited-rename-branch',
;;   `gited-reset-branch', `gited-set-branch-upstream',
;;   `gited-show-commit', `gited-stash',
;;   `gited-stash-apply', `gited-stash-branch',
;;   `gited-stash-drop', `gited-stash-pop',
;;   `gited-status', `gited-summary',
;;   `gited-sync-with-trunk', `gited-toggle-marks',
;;   `gited-unmark', `gited-unmark-all-branches',
;;   `gited-unmark-all-marks', `gited-unmark-backward',
;;   `gited-update', `gited-visit-branch-sources',
;;   `gited-why'.
;;
;;  Non-interactive functions defined here:
;;
;;   `gited--bisect-after-run', `gited--bisect-executable-p',
;;   `gited--case-ref-kind', `gited--check-unmerged-marked-branches',
;;   `gited--clean-previous-patches', `gited--fill-branch-alist',
;;   `gited--fontify-current-row', `gited--get-branches-from-command',
;;   `gited--get-column', `gited--get-merged-branches',
;;   `gited--get-patch-or-commit-buffers', `gited--get-unmerged-branches',
;;   `gited--goto-column', `gited--goto-first-branch',
;;   `gited--handle-new-or-delete-files', `gited--list-format-init',
;;   `gited--mark-branches-in-region',
;;   `gited--mark-merged-or-unmerged-branches',
;;   `gited--mark-merged-or-unmerged-branches-spec', `gited--merged-branch-p',
;;   `gited--move-to-end-of-column', `gited--output-buffer',
;;   `gited--patch-or-commit-buffer', `gited--set-output-buffer-mode',
;;   `gited--stash-branch', `gited--update-padding',
;;   `gited--valid-ref-p', `gited-all-branches',
;;   `gited-async-operation-sentinel', `gited-at-header-line-p',
;;   `gited-bisecting-p', `gited-branch-exists-p',
;;   `gited-buffer-p', `gited-current-branch',
;;   `gited-current-branches-with-marks', `gited-current-state-list',
;;   `gited-date-string-to-time', `gited-dir-under-Git-control-p',
;;   `gited-fontify-current-branch', `gited-fontify-marked-branch-name',
;;   `gited-format-columns-of-files', `gited-get-branchname',
;;   `gited-get-commit', `gited-get-date',
;;   `gited-get-element-in-row', `gited-get-last-commit-time',
;;   `gited-get-mark', `gited-get-marked-branches',
;;   `gited-git-command', `gited-git-command-on-region',
;;   `gited-hide-details-update-invisibility-spec',
;;   `gited-insert-marker-char', `gited-internal-do-deletions',
;;   `gited-last-commit-title', `gited-listed-branches',
;;   `gited-log-msg', `gited-log-summary',
;;   `gited-map-lines', `gited-mark-pop-up',
;;   `gited-mark-remembered', `gited-modified-files',
;;   `gited-modified-files-p', `gited-next-branch',
;;   `gited-number-of-commits', `gited-prev-branch',
;;   `gited-print-entry', `gited-remember-marks',
;;   `gited-remote-repository-p', `gited-repeat-over-lines',
;;   `gited-stashes', `gited-tabulated-list-entries',
;;   `gited-trunk-branches', `gited-untracked-files'.
;;
;;  Faces defined here:
;;
;;   `gited-author', `gited-branch-name',
;;   `gited-commit-msg', `gited-date-time',
;;   `gited-deletion', `gited-deletion-branch',
;;   `gited-flag-mark', `gited-flag-mark-line',
;;   `gited-header', `gited-mark',
;;   `gited-modified-branch', `gited-section-highlight',
;;   `gited-status-branch-local', `gited-status-tag'.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This file is NOT part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:


(require 'cl-lib)
(require 'tabulated-list)
(require 'dired)
(require 'vc-git)
(require 'find-func) ; `find-library-name'

(defgroup gited nil
  "Git branch editing."
  :version "26.1"
  :group 'vc)

(defvar gited-mode nil
  "Variable saving the status of `gited-mode'.")
(make-variable-buffer-local 'gited-mode)
(put 'gited-mode 'permanent-local t)

(defvar gited-current-branch nil
  "The branch currently checked out.")
(make-variable-buffer-local 'gited-current-branch)
(put 'gited-current-branch 'permanent-local t)

(defvar gited-toplevel-dir nil
  "Absolute path of the top-level directory for the current repository.")
(make-variable-buffer-local 'gited-toplevel-dir)
(put 'gited-toplevel-dir 'permanent-local t)

;; Stolen from ediff-ptch.el
(defcustom gited-patch-program "patch"
  "Name of the program that applies patches.
It is recommended to use GNU-compatible versions."
  :type 'string
  :group 'gited)

;; Stolen from ediff-ptch.el
(defcustom gited-patch-options "-f"
  "Options to pass to `gited-patch-program'.

It is recommended to pass the `-f' option to the patch program, so it won't ask
questions.  However, some implementations don't accept this option, in which
case the default value for this variable should be changed."
  :type 'string
  :group 'gited)

(defcustom gited-use-header-line t
  "If non-nil, use the header line to display Gited column titles."
  :type 'boolean
  :group 'gited)

(defcustom gited-verbose nil
  "If non-nil, show the author name and commit date."
  :type 'boolean
  :group 'gited)

(defvar gited--hide-details-set nil)
(make-variable-buffer-local 'gited--hide-details-set)

(defvar gited-ref-kind nil
  "The kind of Git reference displayed in `gited-buffer'.
It must be `local', `remote' or `tags'.")
(make-variable-buffer-local 'gited-ref-kind)
(put 'gited-ref-kind 'permanent-local t)

(defvar gited-mark-idx 0
  "Position of marker char in array `tabulated-list-entries'.")

(defvar gited-author-idx 1
  "Position of author in array `tabulated-list-entries'.")

(defvar gited-date-idx 2
  "Position of date in array `tabulated-list-entries'.")

(defvar gited-branch-idx 3
  "Position of branch name in array `tabulated-list-entries'.")

(defvar gited-commit-idx 4
  "Position of newest commit tittle in array `tabulated-list-entries'.")

(defvar gited-after-change-hook nil
  "Hook run after make a change in the list of branches.")

(defvar gited-branch-after-op nil
  "Branch where to set point after an asynchronous operation.")

(defvar gited-op-string nil
  "String to describe the actual operation.")

(defvar gited-marker-char ?*
  "In gited, the current mark character.
This is what the do-commands look for, and what the mark-commands store.")

(defvar gited-del-char ?D
  "Character used to flag branches for deletion.")

;; "Regexp matching a marked line.
(defvar gited-re-mark "^[^ \n]")

(defvar gited-branch-alist nil
  "Alist with elements (INDEX MARK TIME BRANCH-NAME AUTHOR-NAME TITLE).
INDEX identify each row in the table.
MARK is the mark character shown in the table for that row.
TIME is the time of the last commit in that branch.
BRANCH-NAME is the name of the branch.
AUTHOR-NAME is the author of the last commit in that branch.
TITLE is the title of the last commit.")
(make-variable-buffer-local 'gited-branch-alist)

(defvar gited-buffer-name "*gited*"
  "Name of the buffer where to list the repository branches.")

(defvar gited-buffer nil
  "Buffer where to list the repository branches.")
(make-variable-buffer-local 'gited-buffer)
(put 'gited-buffer 'permanent-local t)

(defvar gited-output-buffer-name "*gited-output*"
  "Name of the buffer where to show output from Git commands.")

(defvar gited-output-buffer nil
  "Output buffer for Git commands.")
(make-variable-buffer-local 'gited-output-buffer)
(put 'gited-output-buffer 'permanent-local t)

(defvar gited-bisect-buf-name "*gited-bisect*"
  "Name of the buffer where to show bisect output.")

(defvar gited-bisect-buffer nil
  "Output buffer for Git bisects.")
(make-variable-buffer-local 'gited-bisect-buffer)
(put 'gited-bisect-buffer 'permanent-local t)

(defvar gited-bisect-output-name "*gited-bisect*"
  "Name of the output buffer for Git bisects.")

(defvar gited-bisect-buffer nil
  "Output buffer for Git bisects.")
(make-variable-buffer-local 'gited-bisect-buffer)
(put 'gited-bisect-buffer 'permanent-local t)

(defvar gited-list-refs-format-command
  '("for-each-ref" "--format='(%(authordate:raw) \
\"%(refname:short)\" \"%(authorname)\")'" "refs/%s")
  "Format strings to build a Git command to list references.")

(defvar gited-date-regexp (concat "\\("
                                  (substring
                                   directory-listing-before-filename-regexp
                                   (length "\\([0-9][BkKMGTPEZY]? ")))
  "Regular expression to match a date in `gited-buffer' buffer.")

(defcustom gited-switches "-g"
  "Control how to sort `gited-branch-alist'.
Option -r reverse order while sorting.
Option -g do not show the author name."
  :type '(choice
          (const :tag "Unset" nil) string)
  :group 'gited)

(defvar gited-actual-switches gited-switches
  "Switches used on this buffer.")
(make-variable-buffer-local 'gited-actual-switches)
(put 'gited-actual-switches 'permanent-local t)

(defvar gited-list-format nil
  "Format of the columns in the branch list.")
(make-variable-buffer-local 'gited-list-format)

(defcustom gited-reset-mode "mixed"
  "Default mode of a Git reset."
  :type '(choice
          (const :tag "soft" "soft")
          (const :tag "mixed" "mixed")
          (const :tag "hard" "hard")
          (const :tag "merge" "merge")
          (const :tag "keep" "keep"))
  :group 'gited)

(defun gited--list-format-init (&optional col-names col-sizes)
  "Initialize `gited-list-format'.
Optional arguments COL-NAMES and COL-SIZES are the column names
and sizes."
  (setq gited-actual-switches gited-switches)
  (setq gited-list-format
        (vector `(,(if col-names (nth 0 col-names) "M")
                  ,(if col-sizes (nth 0 col-sizes) 2) t)
                `(,(if col-names (nth 1 col-names) "Authors")
                  ,(if col-sizes (nth 1 col-sizes) 16) t)
                `(,(if col-names (nth 2 col-names) "Date")
                  ,(if col-sizes (nth 2 col-sizes) 17)
                  (lambda (row1 row2)
                    (let* ((reverse-order
                            (member "-r" (split-string gited-actual-switches)))
                           (t1 (aref (cadr row1) gited-date-idx))
                           (t2 (aref (cadr row2) gited-date-idx))
                           (earlierp
                            (time-less-p
                             (apply #'encode-time (parse-time-string t1))
                             (apply #'encode-time (parse-time-string t2)))))
                      (if reverse-order
                          earlierp
                        (not earlierp)))))
                `(,(if col-names (nth 3 col-names) "Branches")
                  ,(if col-sizes (nth 3 col-sizes) 50) t)
                `(,(if col-names (nth 4 col-names) "Last Commit")
                  ,(if col-sizes (nth 4 col-sizes) 65) t))))

(setq gited-list-format (gited--list-format-init))

(defcustom gited-expert nil
  "If non-nil, don't ask for confirmation for some operations on branches."
  :type 'boolean
  :group 'gited)

(defcustom gited-date-format "%F %R"
  "Format to display the date in `gited-buffer'."
  :type 'string
  :group 'gited)

(defcustom gited-current-branch-face 'font-lock-keyword-face
  "Face used for displaying current checkout branch."
  :type 'face
  :group 'gited)

(defface gited-section-highlight
  '((((class color) (background light)) :background "grey95")
    (((class color) (background  dark)) :background "grey20"))
  "Face for highlighting the current branch."
  :group 'gited)
(defvar gited-section-highlight-face 'gited-section-highlight)

(defface gited-flag-mark-line
  '((((background dark)) (:background "#787831311414")) ; ~ dark red brown
    (t                   (:background "Skyblue")))
  "Face used for flagged and marked lines in Gited buffers."
  :group 'gited :group 'font-lock-highlighting-faces)
(defvar gited-flag-mark-line-face 'gited-flag-mark-line)

(defface gited-flag-mark
  '((((background dark))
     (:foreground "Blue" :background "#7575D4D41D1D")) ; ~ olive green
    (t                   (:foreground "Yellow" :background "Blueviolet")))
  "Face used for flags and marks (except D) in Gited buffers."
  :group 'gited :group 'font-lock-highlighting-faces)
(defvar gited-flag-mark-face 'gited-flag-mark)

(defface gited-deletion-branch
  '((t (:foreground "Red")))
  "Face used for branches flagged for deletion in Gited buffers."
  :group 'gited :group 'font-lock-highlighting-faces)
(defvar gited-deletion-branch-face 'gited-deletion-branch)

(defface gited-deletion
  '((t (:foreground "Yellow" :background "Red")))
  "Face used for deletion flags (D) in Gited buffers."
  :group 'gited :group 'font-lock-highlighting-faces)
(defvar gited-deletion-face 'gited-deletion)

(defface gited-header
  '((t (:inherit font-lock-type-face)))
  "Face used used for header when listing Git branches."
  :group 'gited)

(defface gited-status-branch-local ; Same as magit-branch-local.
  '((((class color) (background light)) :foreground "SkyBlue4")
    (((class color) (background  dark)) :foreground "LightSkyBlue1"))
  "Face for local branches in status."
  :group 'gited)

(defface gited-status-tag ; Same as gited-tag.
  '((((class color) (background light)) :foreground "Goldenrod4")
    (((class color) (background  dark)) :foreground "LightGoldenrod2"))
  "Face for tag labels shown in log buffer."
  :group 'gited)

(defcustom gited-protected-branches nil
  "Name of protected branches.
These branches cannot be deleted or renamed."
  :type '(repeat (string :tag "Branch name"))
  :group 'gited)

(defcustom gited-short-log-cmd
  '("log" "--pretty=format:'%h %ad | %s%d [%an]'" "--graph" "--date=short")
  "Default short format for Git log."
  :type 'string
  :group 'gited)

(defvar gited-header 'gited-header
  "Face name used for header when listing Git branches.")

(defface gited-modified-branch
  '((((background dark)) (:foreground "green1"))
    (t                   (:foreground "red")))
  "*Face used for branches with unstaged/uncommitted changes."
  :group 'gited :group 'font-lock-highlighting-faces)
(defvar gited-modified-branch 'gited-modified-branch)

(defface gited-mark
  '((t (:inherit font-lock-constant-face)))
  "Face used for Gited marks."
  :group 'gited-faces)
(defvar gited-mark-face 'gited-mark
  "Face name used for Gited marks.")

(defface gited-author
  '((((background dark)) (:foreground "orange"))
    (t                   (:foreground "black")))
  "*Face used for AUTHOR in Gited buffers."
  :group 'gited :group 'font-lock-highlighting-faces)
(defvar gited-author-face 'gited-author)

(defface gited-date-time
  '((((background dark)) (:foreground "#74749A9AF7F7")) ; ~ med blue
    (t                   (:foreground "DarkGoldenrod4")))
  "Face used for date and time in Gited buffers."
  :group 'gited :group 'font-lock-highlighting-faces)
(defvar gited-date-time-face 'gited-date-time)

(defface gited-branch-name
  '((((background dark)) (:foreground "Yellow"))
    (t                   (:foreground "Blue")))
  "Face used for branch names  in Gited buffers."
  :group 'gited :group 'font-lock-highlighting-faces)
(defvar gited-branch-name-face 'gited-branch-name)

(defface gited-commit-msg ; Same as magit-branch-remote.
  '((((class color) (background light)) :foreground "DarkOliveGreen4")
    (((class color) (background  dark)) :foreground "DarkSeaGreen2"))
  "Face used for commit-msg  in Gited buffers."
  :group 'gited :group 'font-lock-highlighting-faces)
(defvar gited-commit-msg-face 'gited-commit-msg)



;;; Macros.

(defmacro gited-with-current-branch (branch &rest body)
  "Set BRANCH temporarily current and execute forms in BODY.
BRANCH must be the name of an existing branch.
The value returned is the value of the last form in BODY."
  (declare (indent 1) (debug t))
  (let ((cur-branch (make-symbol "cur-branch")))
    `(let ((,cur-branch gited-current-branch))
       (unwind-protect
           (progn
             (vc-git-checkout nil ,branch)
             (setq gited-current-branch ,branch)
             ,@body)
         ;; Restore original current branch.
         (vc-git-checkout nil ,cur-branch)
         (setq gited-current-branch ,cur-branch)))))

;;; Map over marks.
(defmacro gited-map-over-marks (body arg)
  "Eval BODY with point on each marked line.  Return a list of BODY's results.
If no marked branch could be found, execute BODY on the current
line.  ARG, if non-nil, specifies the branches to use instead of the
marked branches.

If ARG is an integer, use the next ARG (or previous -ARG, if
ARG<0) files.  In that case, point is dragged along.  This is so
that commands on the next ARG (instead of the marked) files can
be chained easily.
For any other non-nil value of ARG, use the current file.

Search starts at the beginning of the buffer, thus the car of the
list corresponds to the line nearest to the buffer's bottom.
This is also true for (positive and negative) integer values of
ARG.

BODY should not be too long as it is expanded four times.

If DISTINGUISH-ONE-MARKED is non-nil, then if we find just one
marked file, return (t FILENAME) instead of (FILENAME)."
  `(prog1
       (let ((inhibit-read-only t) case-fold-search found results)
         (if ,arg
             (if (integerp ,arg)
                 (progn ;; no save-excursion, want to move point.
                   (gited-repeat-over-lines
                    ,arg
                    (lambda ()
                      (setq results (cons ,body results))))
                   (if (< ,arg 0)
                       (nreverse results)
                     results))
               ;; non-nil, non-integer ARG means use current file:
               (list ,body))
           (let ((regexp
                  (concat "^"
                          (regexp-quote
                           (char-to-string gited-marker-char))))
                 next-position)
             (save-excursion
               (goto-char (point-min))
               ;; remember position of next marked file before BODY
               ;; can insert lines before the just found file,
               ;; confusing us by finding the same marked file again
               ;; and again and...
               (setq next-position (and (re-search-forward regexp nil t)
                                        (point-marker))
                     found (not (null next-position)))
               (while next-position
                 (goto-char next-position)
                 (setq results (cons ,body results))
                 ;; move after last match
                 (goto-char next-position)
                 (forward-line)
                 (set-marker next-position nil)
                 (setq next-position (and (re-search-forward regexp nil t)
                                          (point-marker)))))
             (if found
                 results
               (list ,body)))))
     ;; save-excursion loses, again
     (gited-move-to-branchname)))

(defmacro gited-mark-if (predicate msg)
  "Mark all branches for which PREDICATE eval to non-nil.
PREDICATE is evaluated on each line, with point at beginning of line.
MSG is a noun phrase for the type of branches being marked.
It should end with a noun that can be pluralized by adding `s'.
Return value is the number of files marked, or nil if none were marked."
  `(let ((inhibit-read-only t) count)
     (save-excursion
       (setq count 0)
       (when ,msg
         (message "%s %ss%s..."
                  (cond ((eq gited-marker-char ?\s) "Unmarking")
                        ((eq gited-marker-char gited-del-char)
                         "Flagging")
                        (t "Marking"))
                  ,msg
                  (if (eq gited-marker-char gited-del-char)
                      " for deletion"
                    "")))
       (gited--goto-first-branch)
       (while (not (eobp))
         (if ,predicate
             (progn
               (gited-insert-marker-char)
               (gited-fontify-marked-branch-name)
               (cl-incf count)))
         (forward-line))
       (if ,msg (message "%s %s%s %s%s."
                         count
                         ,msg
                         (gited-plural-s count)
                         (if (eq gited-marker-char ?\s) "un" "")
                         (if (eq gited-marker-char gited-del-char)
                             "flagged" "marked"))))
     (and (> count 0) count)))


;;; Convenience functions.
;; Following functions are stolen from Dired.
;; To avoid code duplication we just bind `dired-log-buffer' to
;; `gited-log-buffer' and use the original definitions in dired.el.
(defvar gited-log-buffer "*Gited log*")

(defalias 'gited-plural-s 'dired-plural-s)

(defun gited-why ()
  "Pop up a buffer with error log output from Gited.
A group of errors from a single command ends with a formfeed.
Thus, use \\[backward-page] to find the beginning of a group of errors."
  (interactive)
  (let ((dired-log-buffer gited-log-buffer))
    (dired-why)))

(defun gited-summary ()
  "Summarize basic Gited commands and show recent Gited errors."
  (interactive)
  (gited-why)
  (message
   "d-elete, u-ndelete, x-punge, f-ind, o-rigin, R-ename, C-opy, h-elp"))

(defun gited-log-msg (log &rest args)
  (let ((dired-log-buffer gited-log-buffer))
    (dired-log log args)))

(defun gited-log-summary (string failures)
  (let ((dired-log-buffer gited-log-buffer))
    (dired-log-summary string failures)))

(defun gited-format-columns-of-files (branches)
  (let ((beg (point)))
    (completion--insert-strings branches)
    (put-text-property beg (point) 'mouse-face nil)))

(defun gited-next-branch ()
  "Return name of next branch."
  (save-excursion
    (forward-line)
    (ignore-errors
      (gited-get-branchname))))

(defun gited-prev-branch ()
  "Return name of previous branch."
  (save-excursion
    (forward-line -1)
    (ignore-errors
      (gited-get-branchname))))

(defun gited-current-branch ()
  "Return name of current branch."
  (car (vc-git-branches)))

(defun gited-get-last-commit-time (branch)
  "Return last commit time of BRANCH."
  (apply #'encode-time
         (parse-time-string
          (cadr
           (cdr
            (nth
             (cl-position-if
              (lambda (x) (equal branch (nth 3 x))) gited-branch-alist)
             gited-branch-alist))))))

(defun gited--get-column (col)
  (mapcar (lambda (x)
            (elt (cadr x) col))
          tabulated-list-entries))

(defun gited-listed-branches ()
  "Return a list with all listed branches/tags names."
  (gited--get-column gited-branch-idx))

(defalias 'gited-get-branches 'gited-listed-branches)

(defun gited-git-command (args &optional buffer display unquote)
  "Execute a Git command with arguments ARGS.
Optional arg BUFFER is the output buffer.
Optional arg DISPLAY means redisplay buffer as output is inserted.
Optional arg UNQUOTE removes single quotes from the output."
  (prog1
      (apply #'call-process vc-git-program nil buffer display args)
    (when (and unquote buffer (buffer-live-p buffer))
      (with-current-buffer buffer
        ;; Drop single quotes.
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward "^'\\(.*\\)'$" nil t)
            (replace-match "\\1")))))))

(defun gited-git-command-on-region (args &optional buffer display)
  "Execute a Git command with arguments ARGS and region as input.
Optional arg BUFFER is the output buffer.
Optional arg DISPLAY means redisplay buffer as output is inserted."
  (apply #'call-process-region nil nil vc-git-program nil buffer display args))

(defun gited-all-branches ()
  "Return a list with all (local and remote) branches and tags."
  (let ((args '("for-each-ref" "--format='%(refname:short)'" "refs/heads"
                "refs/remotes" "refs/tags")))
    (with-temp-buffer
      (gited-git-command args (current-buffer) nil 'unquote)
      (split-string (buffer-string) "\n" 'omit-nulls))))

(defun gited-copy-branchname-as-kill ()
  "Copy names of marked branches into the kill ring.
The names are separated by a space.

If on a subdir headerline, use absolute subdirname instead;
prefix arg and marked files are ignored in this case.

You can then feed the file name(s) to other commands with \\[yank]."
  (interactive)
  (let ((string
         (mapconcat (function identity)
                    (or (gited-get-marked-branches)
                        (list (gited-get-branchname)))
                    " ")))
    (unless (string= string "")
      (if (eq last-command 'kill-region)
          (kill-append string nil)
        (kill-new string))
      (message "%s" string))))

(defun gited--output-buffer (&optional buf-name)
  (unless buf-name
    (setq buf-name gited-output-buffer-name))
  (let* ((buf
          (cond ((equal buf-name gited-bisect-buf-name)
                 (and (buffer-live-p gited-bisect-buffer) gited-bisect-buffer))
                (t
                 (and (buffer-live-p gited-output-buffer) gited-output-buffer))))
         (res (or (and (buffer-live-p buf)
                       (equal default-directory
                              (buffer-local-value 'default-directory buf))
                       buf)
                  (generate-new-buffer buf-name))))
    (if (equal buf-name gited-bisect-buf-name)
        (setq gited-bisect-buffer res)
      (setq gited-output-buffer res))))


;;; Predicates.

(defun gited-modified-files ()
  "Return a list with all unstaged files."
  (let ((regexp "^[[:blank:]]*[MAU]+[[:blank:]]*\\(.+\\)")
        (case-fold-search)
        res)
    (with-temp-buffer
      (gited-git-command '("status" "--porcelain") (current-buffer))
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (push (match-string 1) res)))
    (nreverse res)))

(defun gited-modified-files-p ()
  "Return non-nil if there are unstaged changes."
  (and (gited-modified-files) t))

(defun gited-dir-under-Git-control-p ()
  "Return non-nil if current directory is under Git version control."
  (zerop (gited-git-command '("status"))))

(defun gited-branch-exists-p (branch)
  "Return non-nil if BRANCH exists."
  (member branch (gited-listed-branches)))

(defun gited-buffer-p ()
  "Return non-nil if current buffer is a gited buffer."
  (string-prefix-p gited-buffer-name
                   (buffer-name (current-buffer))))

(defun gited-at-header-line-p ()
  "Return non-nil if point is at header line."
  (and (not gited-use-header-line)
       (= 1 (line-number-at-pos (point)))))

(defun gited-remote-repository-p ()
  "Return non-nil if current repository is remote."
  (let ((regexp "^remote.origin"))
    (with-temp-buffer
      (gited-git-command  '("config" "--local" "--list") (current-buffer))
      (goto-char 1)
      (and (re-search-forward regexp nil t) t))))


;;; Operations on branches (copy, merge, ...).

(defun gited--get-branches-from-command (cmd)
  (with-temp-buffer
    (gited-git-command cmd (current-buffer) nil 'unquote)
    (goto-char (point-min))
    (while (re-search-forward "^\\(  \\|\\* \\)" nil t)
      (replace-match ""))
    (split-string (buffer-string) "\n" 'omit-nulls)))

(defun gited-trunk-branches ()
  "Return a list with branch names tracked from a remote repository."
  (let ((regexp "^branch\.\\([^.]+\\)\.merge=")
        res)
    (with-temp-buffer
      (gited-git-command  '("config" "--local" "--list") (current-buffer))
      (goto-char 1)
      (while (re-search-forward regexp nil t)
        (push (match-string 1) res))
      (nreverse res))))

(defun gited--get-unmerged-branches ()
  (let ((args `("branch" "--no-merged" ,(car (gited-trunk-branches)))))
    (gited--get-branches-from-command args)))

(defun gited--get-merged-branches ()
  (let ((args `("branch" "--merged" ,(car (gited-trunk-branches)))))
    (gited--get-branches-from-command args)))

(defun gited--check-unmerged-marked-branches ()
  (let ((marked-branches (or (gited-get-marked-branches)
                             (list (gited-get-branchname)))))
    (dolist (b marked-branches)
      (let ((unmerged (gited--get-unmerged-branches)))
        (dolist (x unmerged)
          (when (string= b x)
            (error "Cannot delete unmerged branches.  Try C-u %s"
                   (substitute-command-keys (this-command-keys)))))))))

(defun gited--merged-branch-p (branch)
  (and (member branch (gited--get-merged-branches))
       t))

(defun gited-untracked-files ()
  "Return a list with all untracked files."
  (let ((args '("status" "--porcelain"))
        (regexp "^[?]\\{2\\}[[:blank:]]+\\(.+\\)")
        res)
    (with-temp-buffer
      (gited-git-command args (current-buffer))
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (push (match-string 1) res)))
    (nreverse res)))

(defun gited-stashes ()
  "Return a list with all the stashes."
  (let ((args '("stash" "list"))
        res)
    (with-temp-buffer
      (gited-git-command args (current-buffer))
      (goto-char (point-min))
      (while (not (eobp))
        (push (buffer-substring (point-at-bol) (point-at-eol)) res)
        (forward-line)))
    (nreverse res)))

(defun gited-last-commit-title ()
  "Return title of the last commit."
  (let ((args '("log" "--pretty=format:'%s'" "-n1")))
    (with-temp-buffer
      (gited-git-command args (current-buffer) nil 'unquote)
      (buffer-string))))

;; Non-nil while running an asynchronous Gited subprocess.
(defvar gited--running-async-op nil)
(make-variable-buffer-local 'gited--running-async-op)
(defun gited-async-operation (command &optional remote-op-p buffer)
  "Run COMMAND asynchronously.
COMMAND perform a branch operation, i.e., rename or delete a branch.
Optional arg REMOTE-OP-P, means the operation modify the remote
repository.  Otherwise, the operation just change local branches.
Optional arg BUFFER is the output buffer for the operation.  Otherwise,
use `gited-output-buffer'."
  (interactive)
  (if gited--running-async-op
      (error "Cannot run 2 Gited async process in parallel")
    (let* ((gited-buf (current-buffer))
           (out-buf (or buffer (gited--output-buffer)))
           (directory default-directory)
           proc)
      (with-current-buffer out-buf
        ;; Always display out-buf for remote operations.  This is to prompt
        ;; for Git user/password.
        (when remote-op-p
          (display-buffer out-buf '(nil (allow-no-window . t))))
        (setq default-directory directory
              proc (start-process-shell-command
                    "*gited-async-operation*" out-buf command)
              mode-line-process '(":%s"))
        (with-current-buffer gited-buf
          (setq gited--running-async-op t))
        (with-no-warnings
          (require 'shell) (shell-mode))
        (set-process-sentinel proc 'gited-async-operation-sentinel)
        (set-process-filter proc 'comint-output-filter)
        ;; Associate out-buf with gited-buf; this is used in the sentinel.
        (setq gited-buffer gited-buf)))))

(defun gited-async-operation-sentinel (proc state)
  "Sentinel for asynchronous operations on branches.
PROC is the process.
STATE is the state of process PROC."
  (let* ((buf (process-buffer proc))
         (gited-buf (and (buffer-live-p buf)
                         (buffer-local-value 'gited-buffer buf)))
         (op-string gited-op-string)
         (inhibit-read-only t))
    (ignore state)
    (when (memq (process-status proc) '(exit signal))
      (with-current-buffer gited-buf
        (setq gited--running-async-op nil)
        (when (gited-bisecting-p)
          (gited--bisect-after-run (process-buffer proc)))))
    (when (buffer-live-p gited-buf)
      (set-buffer gited-buf)
      (run-hooks 'gited-after-change-hook)
      (when gited-branch-after-op
        (gited-goto-branch gited-branch-after-op)
        (setq gited-branch-after-op nil
              gited-op-string nil))
      (message "%s done!" op-string))))

(define-minor-mode gited-hide-details-mode
  "Toggle visibility of detailed information in current Gited buffer.
When this minor mode is enabled, details such as last commit author and
date are hidden from view."
  :group 'gited
  (unless (derived-mode-p 'gited-mode)
    (error "Not a Gited buffer"))
  (gited-hide-details-update-invisibility-spec))

(put 'gited-hide-details-mode 'permanent-local t)

(defun gited-update ()
  "Update `gited-branch-alist' and redisplay the list of branches."
  (interactive)
  (unless (derived-mode-p major-mode 'gited-mode)
    (error "Gited mode cannot be enabled in this buffer"))
  (let ((target-br (ignore-errors (gited-get-branchname)))
        (at-headr-p (gited-at-header-line-p))
        (hide-details gited-hide-details-mode))
    (gited-list-branches gited-ref-kind nil 'update)
    (gited-hide-details-mode (if hide-details 1 0))
    (if (not at-headr-p)
        (gited-goto-branch target-br)
      (gited-goto-branch gited-current-branch))))

(add-hook 'gited-after-change-hook 'gited-update)

(defun gited-rename-branch (old-name new-name)
  "Rename branch OLD-NAME to NEW-NAME."
  (interactive
   (let* ((old (gited-get-branchname))
          (new (read-string
                (format "Rename %s to: " old)
                nil nil old)))
     (list old new)))
  (when (member old-name gited-protected-branches)
    (error "Cannot rename a protected branch"))
  (let ((buf (gited--output-buffer))
        (inhibit-read-only t) remote-op-p)
    (setq gited-output-buffer buf)
    (with-current-buffer buf (erase-buffer))
    (if (and (not gited-expert)
             (not (y-or-n-p
                   (format "Rename branch '%s' to '%s'? "
                           old-name new-name))))
        (message "OK, rename canceled")
      (if (gited-async-operation
           (pcase gited-ref-kind
             ("remote"
              (let ((old (substring old-name (length "origin/")))
                    (new (if (string-prefix-p "origin/" new-name)
                             (substring new-name (length "origin/"))
                           new-name)))
                (setq remote-op-p t)
                (format "%s push origin origin/%s:refs/heads/%s :%s"
                        vc-git-program old new old)))
             ("local" (format "%s branch --move %s %s"
                              vc-git-program old-name new-name))
             ("tags" (error "Rename tags not implemented!"))
             (_ (error "Unsupported gited-ref-kind: must be \
local, remote or tags")))
           remote-op-p)
          (progn
            (setq gited-branch-after-op new-name
                  gited-op-string
                  (format "Rename branch '%s' to '%s'" old-name new-name)))
        (error "Cannot rename branch '%s' to '%s'" old-name new-name)))))

(defun gited-merge-branch (branch)
  "Merge BRANCH with another one.
That means, changes from another branch are added into BRANCH."
  (interactive
   (list (gited-get-branchname)))
  (let ((branch-new
         (completing-read
          (format "Merge %s with: " branch)
          (gited-listed-branches)
          nil t)))
    (gited-with-current-branch branch
      (if (zerop (gited-git-command `("merge" ,branch-new)))
          (message "Merged %s into %s!" branch-new branch)
        (error "Cannot merge '%s' into '%s'" branch-new branch)))))

(defun gited-reset-branch (commit &optional mode)
  "Reset current branch to an earlier state.

COMMIT is a SHA1 string or HEAD~N, to reset BRANCH to that commit.
Interactively prompt for the limit commit: 0 means HEAD,
 1 means HEAD~, and so on.
Interactively with a prefix argument prompts for the reset mode.
 Defaults to `gited-reset-mode'."
  (interactive
   (let* ((alist
           '(("s" . "soft") ("m" . "mixed") ("h" . "hard")
             ("g" . "merged") ("k" . "keep")))
          (mode
           (if current-prefix-arg
               (cdr (assoc
                     (completing-read
                      "Reset mode (s = soft, m = mixed, h = hard, \
g = merged, k = keep): "
                      '("s" "m" "h" "g" "k")
                      nil 'mustmatch nil nil "h")
                     alist))
             gited-reset-mode))
          (input
           (read-string
            (format "Reset --%s to commit (0 = HEAD, \
1 = HEAD~1, ... or SHA1): " mode)
            nil nil "0")))
     (list (if (gited--valid-ref-p input)
               input
             (concat "HEAD~" input))
           mode)))
  (unless mode (setq mode gited-reset-mode))
  (let ((branch (gited-current-branch))
        (args `("reset" ,(concat "--" mode) ,commit)))
    (if (not (y-or-n-p
              (format "Reset --%s '%s' to '%s'? " mode branch commit)))
        (message "OK, reset canceled")
      (if (zerop (gited-git-command args))
          (message "Reseted --%s '%s' to '%s'!" mode branch commit)
        (error "Cannot reset --%s '%s' to '%s'" mode branch commit)))))

(defun gited-delete-branch (branch &optional force)
  "Delete branch BRANCH.
BRANCH default to the branch at current line.
Optional arg FORCE, if non-nil then delete non-fully merged branches
as well."
  (interactive
   (list (gited-get-branchname) current-prefix-arg))
  (let ((br-after (or (gited-next-branch)
                      (gited-prev-branch)
                      (gited-current-branch)))
        (buf (gited--output-buffer))
        (inhibit-read-only t))
    (setq gited-output-buffer buf)
    (with-current-buffer buf (erase-buffer))
    (when (string= branch gited-current-branch)
      (error "Cannot delete the current branch"))
    (when (member branch gited-protected-branches)
      (error "Cannot delete a protected branch"))
    (if (and (not gited-expert)
             (not (y-or-n-p (format "Delete branch '%s'? " branch))))
        (message "OK, deletion canceled")
      (pcase gited-ref-kind
        ("tags" (error "Delete tags not implemented!"))
        ("local"
         (if (zerop (gited-git-command
                     ;; --delete --force as shortcut of -D doesn't exist
                     ;; in old Git versions.
                     `("branch"
                       ,(if force "-D" "--delete")
                       ,branch)
                     buf))
             (progn
               (gited-goto-branch br-after)
               (message "Delete branch '%s'!" branch))
           (error "Cannot delete unmerged branch '%s'.  Try C-u %s"
                  branch
                  (substitute-command-keys "\\[gited-do-flagged-delete\]"))))
        ("remote"
         (gited-async-operation
          (format "%s push origin :%s"
                  vc-git-program
                  (substring branch (length "origin/"))) 'remote-op-p)
         (setq gited-branch-after-op br-after
               gited-op-string (format "Delete branch '%s'" branch)))
        (_ (error "Unsupported gited-ref-kind: must be \
local, remote or tags"))))))

(defun gited-do-delete (&optional arg force)
  "Delete all marked (or next ARG) branches.
Optional arg FORCE, if non-nil then delete non-fully merged branches
as well."
  (interactive
   (let ((prefix current-prefix-arg))
     (list prefix (equal prefix '(4)))))
  (unless force
    (gited--check-unmerged-marked-branches))
  (gited-internal-do-deletions
   ;; this may move point if ARG is an integer
   (gited-map-over-marks (cons (or (gited-get-branchname)
                                   (error "No branch on this line"))
                               (point))
                         arg)
   arg force)
  (run-hooks 'gited-after-change-hook))

(defun gited-mark-pop-up (buffer-or-name op-symbol branches function &rest args)
  "Return FUNCTION's result on ARGS after showing which branches are marked.
Displays the branch names in a window showing a buffer named
BUFFER-OR-NAME; the default name being \" *Marked Branches*\".  The
window is not shown if there is just one branch, `gited-no-confirm'
is t, or OP-SYMBOL is a member of the list in `gited-no-confirm'.

By default, Gited shrinks the display buffer to fit the marked branches.
To disable this, use the Customization interface to add a new rule
to `display-buffer-alist' where condition regexp is
\"^ \\*Marked Branches\\*$\",
action argument symbol is `window-height' and its value is nil.

BRANCHES is the list of marked branches.  It can also be (t BRANCHNAME)
in the case of one marked branch, to distinguish that from using
just the current branch.

FUNCTION should not manipulate branches, just read input (an
argument or confirmation)."
  (if (= (length branches) 1)
      (apply function args)
    (let ((buffer (get-buffer-create (or buffer-or-name " *Marked Branches*")))
          ;; Mark *Marked Branches* window as softly-dedicated, to prevent
          ;; other buffers e.g. *Completions* from reusing it (bug#17554).
          (display-buffer-mark-dedicated 'soft))
      (ignore op-symbol) ; ignore unused symbol.
      (with-current-buffer buffer
        (with-current-buffer-window
         buffer
         (cons 'display-buffer-below-selected
               '((window-height . fit-window-to-buffer)))
         #'(lambda (window _value)
             (with-selected-window window
               (unwind-protect
                   (apply function args)
                 (when (window-live-p window)
                   (quit-restore-window window 'kill)))))
         (gited-format-columns-of-files
          branches)
         (remove-text-properties (point-min) (point-max)
                                 '(mouse-face nil help-echo nil)))))))

(defun gited-internal-do-deletions (l arg &optional force)
  ;; L is an alist of branches to delete, with their buffer positions.
  ;; ARG is the prefix arg.
  ;; FORCE, if non-nil then delete non-fully merged branches as well.
  ;; (car L) *must* be the *last* (bottommost) branch in the gited buffer.
  ;; That way as changes are made in the buffer they do not shift the
  ;; lines still to be changed, so the (point) values in L stay valid.
  ;; Also, for subdirs in natural order, a subdir's branches are deleted
  ;; before the subdir itself - the other way around would not work.
  (let* ((branches (mapcar (function car) l))
         (count (length l))
         (succ 0))
    ;; canonicalize branch list for pop up
    (if (gited-mark-pop-up
         " *Deletions*" 'delete branches 'y-or-n-p
         (format "%s %s "
                 "Delete"
                 (replace-regexp-in-string "files"
                                           "branches"
                                           (dired-mark-prompt arg branches))))
        (save-excursion
          (let ((progress-reporter
                 (make-progress-reporter
                  "Deleting..."
                  succ count))
                failures) ;; branches better be in reverse order for this loop!
            (while l
              (goto-char (cdar l))
              (let ((inhibit-read-only t))
                (condition-case err
                    (let ((fn (caar l))
                          (gited-expert t)) ;; Don't ask confirmation again.
                      (gited-delete-branch fn force)
                      ;; if we get here, removing worked
                      (cl-incf succ)
                      (progress-reporter-update progress-reporter succ))
                  (error ;; catch errors from failed deletions
                   (gited-log-msg "%s\n" err)
                   (setq failures (cons (caar l) failures)))))
              (setq l (cdr l)))
            (if (not failures)
                (progress-reporter-done progress-reporter)
              (gited-log-summary
               (format "%d of %d deletion%s failed"
                       (length failures) count
                       (gited-plural-s count))
               failures))))
      (message "(No deletions performed)")))
  (gited-goto-branch gited-current-branch))

(defun gited-do-flagged-delete (&optional force)
  "In Gited, delete the branches flagged for deletion.
Optional arg FORCE, if non-nil then delete non-fully merged branches
as well."
  (interactive "P")
  (let* ((gited-marker-char gited-del-char)
         (regexp (concat "^" (regexp-quote (char-to-string gited-marker-char))))
         case-fold-search)
    (unless force
      (gited--check-unmerged-marked-branches))
    (if (save-excursion (goto-char (point-min))
                        (re-search-forward regexp nil t))
        (progn
          (gited-internal-do-deletions
           ;; this can't move point since ARG is nil
           (gited-map-over-marks (cons (gited-get-branchname) (point))
                                 nil)
           nil force)
          (run-hooks 'gited-after-change-hook))
      (message "(No deletions requested)"))))

(defun gited-copy-branch (old new &optional commit)
  "Copy branch OLD to branch NEW.
OLD default to branch at current line.

Optional arg COMMIT, if non-nil then is a SHA1 string or
HEAD~N, to copy OLD until that commit, inclusive.
When called with a prefix, prompt for the limit commit: 0 means HEAD,
1 means HEAD~, and so on."
  (interactive
   (let* ((old-br (gited-get-branchname))
          (new-br (read-string
                   (format "Copy %s to: " old-br)
                   nil nil old-br))
          (ask current-prefix-arg)
          (num-or-sha1
           (if ask
               (read-string "Show commit (0 = HEAD, 1 = HEAD~1, ... or SHA1): "
                            nil nil "0")
             "HEAD")))
     (list old-br new-br num-or-sha1)))
  (unless commit (setq commit "HEAD"))
  (unless (gited--valid-ref-p commit)
    (setq commit (concat old "~" commit)))
  (let ((cmd1 `("checkout" ,old))
        (cmd2 `("checkout" "-b" ,new ,commit))
        (cmd3 `("checkout" ,gited-current-branch)))
    (if (and (zerop (gited-git-command cmd1))
             (zerop (gited-git-command cmd2))
             (zerop (gited-git-command cmd3)))
        (progn
          (message "Copied %s to %s!" old new)
          (run-hooks 'gited-after-change-hook)
          (gited-goto-branch new))
      (error "Cannot copy '%s' to '%s'" old new))))


;;; Checkout/visit sources.

(defun gited-visit-branch-sources (&optional other-window)
  "Visit source code for current revision.
If optional arg OTHER-WINDOW is non-nil, then use another window."
  (interactive "P")
  (when (and (gited-modified-files-p)
             (not (equal gited-current-branch (gited-get-branchname))))
    (error "Cannot checkout a new branch: there are modified files"))
  (let* ((branch (gited-get-branchname))
         (visit-sources
          (y-or-n-p (format "Visit '%s' branch sources? " branch))))
    (if (not visit-sources)
        (error "OK, canceled")
      (let ((gited-expert visit-sources))
        (gited-checkout-branch branch)
        (if other-window
            (dired-other-window default-directory)
          (dired default-directory))
        (dired-revert)))))

(defun gited--fontify-current-row ()
  (remove-text-properties
   (point-at-bol) (point-at-eol) '(face))
  (let ((inhibit-read-only t) pos)
    (save-excursion
      (gited-move-to-author)
      (setq pos (point))
      (gited-move-to-end-of-author)
      (put-text-property
       pos (point) 'face gited-author-face)
      (gited-move-to-date)
      (setq pos (point))
      (gited-move-to-end-of-date)
      (put-text-property
       pos (point) 'face gited-date-time-face)
      (gited-move-to-branchname)
      (setq pos (point))
      (gited-move-to-end-of-branchname)
      (put-text-property
       pos (point) 'face gited-branch-name-face)
      (setq pos (point))
      (put-text-property
       pos (point-at-eol) 'face gited-commit-msg-face))))

(defun gited-checkout-branch (branch)
  "Checkout BRANCH."
  (interactive
   (list (completing-read "Checkout branch: "
                          (gited-all-branches)
                          nil 'mustmatch (gited-get-branchname) nil)))
  (when (and (gited-modified-files-p)
             (not (equal gited-current-branch (gited-get-branchname))))
    (error "Cannot checkout a new branch: there are modified files"))
  (let* ((cur-br gited-current-branch)
         (new-branch-p (and (equal gited-ref-kind "local")
                            (not (member branch (gited-get-branches)))))
         (inhibit-read-only t)
         (local-branch (if new-branch-p
                           (read-string
                            "Checkout in local branch: "
                            nil nil (file-name-nondirectory branch))
                         branch)))
    (save-excursion
      (gited-goto-branch cur-br)
      (gited--fontify-current-row)
      (if (not new-branch-p)
          (vc-git-checkout nil branch)
        (gited-git-command `("checkout" "-b" ,local-branch ,branch))
        (run-hooks 'gited-after-change-hook))
      (setq gited-current-branch local-branch))
    (gited-fontify-current-branch)
    (when new-branch-p
      (gited-goto-branch gited-current-branch)))
  (message "Current branch is %s" gited-current-branch))



;;; Apply patches.

(defun gited--patch-or-commit-buffer (&optional commit)
  (let ((regexp
         (if commit
             "\\`\\*gited-commit-\\([0-9]+\\)\\*\\'"
           "\\`\\*gited-patch-\\([0-9]+\\)\\*\\'")))
    (get-buffer
     (completing-read
      (if commit
          "Use commit message in buffer: "
        "Apply patch from buffer: ")
      (cl-delete-if-not
       (lambda (x) (string-match regexp x))
       (mapcar #'buffer-name (buffer-list)))))))

(defconst gited-new-or-deleted-files-re
  (format "^\\(%s\\|%s\\|%s\\|%s\\|%s\\|%s\\)"
          "deleted file mode"
          "new file mode"
          "copy from"
          "copy to"
          "rename from"
          "rename to")
  "Regexp to match new or deleted files in a Git diff.")

;; Currently just handle new files.
(defun gited--handle-new-or-delete-files (patch-buf)
  (let ((new-files))
    (goto-char 1)
    (with-current-buffer patch-buf
      (while (re-search-forward gited-new-or-deleted-files-re nil t)
        (unless (string= "new file mode" (match-string-no-properties 0))
          (error "Only creation of files is implementing: %s"
                 (match-string-no-properties 0)))
        (let* ((str (buffer-substring-no-properties
                     (point-at-bol 0) (point-at-eol 0)))
               (file
                (progn
                  (string-match "diff --git a/\\(.*\\) b/.*" str)
                  (match-string-no-properties 1 str))))
          (push file new-files))))
    (if (zerop (gited-git-command (nconc '("add") new-files)))
        (message "Sucessfully staged new files: %s"
                 (mapconcat #'shell-quote-argument new-files " "))
      (error "Cannot stage some new files.  Please check"))))

(defun gited-apply-patch (buf-patch &optional update)
  "Apply patch at BUF-PATCH into current branch.
If optional arg UPDATE is non-nil, then call `gited-update'
after checkout."
  (interactive
   (list (gited--patch-or-commit-buffer)
         current-prefix-arg))
  (let ((toplevel gited-toplevel-dir)
        create-or-del-files-p)
    (with-temp-buffer
      ;; Apply patches from top-level dir.
      (setq default-directory (file-name-as-directory toplevel))
      (insert-buffer-substring-no-properties buf-patch)
      (goto-char 1)
      (setq create-or-del-files-p
            (re-search-forward gited-new-or-deleted-files-re nil t))
      (if (not (zerop (gited-git-command-on-region '("apply" "--check"))))
          (error "Cannot apply patch at '%s'.  Please check"
                 (buffer-name buf-patch))
        (gited-git-command-on-region '("apply"))
        (when create-or-del-files-p
          (gited--handle-new-or-delete-files (current-buffer)))
        (and update (gited-update))
        (message "Patch applied successfully!")))))

(defun gited-add-patched-files (files &optional ask)
  "Stage FILES for next commit.
If ASK is non-nil, then prompt the user before to add every hunk
and display the output buffer in other window."
  (interactive
   (list (gited-modified-files) current-prefix-arg))
  (unless files (error "No modified files"))
  (let ((buf (gited--output-buffer)))
    (cond (ask
           ;; Output buffer must be editable.
           (with-current-buffer buf
             (setq buffer-read-only nil)
             (erase-buffer))
           (gited-async-operation (format "%s add --patch" vc-git-program)
                                  nil buf)
           (setq gited-op-string "add --patch")
           (display-buffer buf))
          (t
           (let ((toplevel gited-toplevel-dir))
             (with-temp-buffer
               ;; Add files from top-level dir.
               (setq default-directory (file-name-as-directory toplevel))
               (if (not (zerop (gited-git-command (nconc '("add") files))))
                   (error "Cannot add files.  Please check")
                 (message "Successfully added files %s"
                          (mapconcat #'shell-quote-argument files " ")))))))))

(defun gited-commit (comment &optional author)
  "Commit latest changes using COMMENT as the message.
Optional argument AUTHOR is the author of the commit.
A prefix argument prompts for AUTHOR."
  (interactive
   (let ((_files (or (gited-modified-files) (error "No changes to commit")))
         (name (and current-prefix-arg (read-string "Author: ")))
         (msg (read-string "Message: ")))
     (list msg name)))
  (or (gited-modified-files) (error "No changes to commit"))
  (let* ((buf (generate-new-buffer "*git-commit*"))
         (args
          (delete ""
                  `("commit" ,(if author (format "--author=\"%s\"" author) "")
                    "-m"
                    ,comment))))
    (unless (zerop (gited-git-command args buf))
      (display-buffer buf)
      (error "Commit failt: please check %s" (buffer-name buf)))
    (with-current-buffer buf
      (insert (format "\nCommit successfully with message:\n\n\"%s\"" comment)))
    (gited--set-output-buffer-mode buf)
    (display-buffer buf)))

(defun gited-apply-add-and-commit-patch (buf-patch buf-commit)
  "Apply patch at BUF-PATCH, stage it and commit it with message BUF-COMMIT."
  (interactive
   (list
    (gited--patch-or-commit-buffer)
    (gited--patch-or-commit-buffer 'commit)))
  (if (null (ignore-errors (gited-apply-patch buf-patch)))
      (error "Cannot apply patch at %s" (buffer-name buf-patch))
    (let ((cur-buf (current-buffer)))
      ;; Stage changes.
      (gited-add-patched-files (gited-modified-files)) ; Switch to another buf.
      (switch-to-buffer cur-buf)
      (let* ((commit-msg
              (with-current-buffer buf-commit
                (buffer-string)))
             (args `("commit" "-m" ,commit-msg)))
        (if (zerop (gited-git-command args))
            (message "Patch applied and committed successfully!")
          (error "Cannot commit patch at %s" (buffer-name buf-patch)))))))


;;; Common operations on Git repositiores: pull, diff, log, etc.

(defun gited-number-of-commits ()
  "Return number of Git commits in current buffer."
  (let ((regexp "^commit[:]? \\([[:xdigit:]]+\\)"))
    (save-excursion
      (goto-char (point-min))
      (let ((count 0))
        (while (re-search-forward regexp nil t)
          (cl-incf count))
        (if (called-interactively-p 'interactive)
            (message "%d commits in current buffer" count)
          count)))))

(defun gited--case-ref-kind ()
  (pcase gited-ref-kind
    ("remote" "remotes/")
    ("local" "heads/")
    ("tags" "tags/")
    (_ (error "Unsupported gited-ref-kind: must be local, remote or tags"))))

(defun gited--set-output-buffer-mode (buffer &optional mode editable)
  (let ((win (get-buffer-window buffer)))
    (when win (set-window-point win (point-min))))
  (with-current-buffer buffer
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (or editable (setq buffer-read-only t))
    (pcase mode
      (`diff (diff-mode))
      (`outline (outline-mode))
      (_ (fundamental-mode)))))

(defun gited-diff-with-branch (branch)
  "Show diff of BRANCH with a second branch.
BRANCH default to the branch at current line.

The actual diff run is:
diff BRANCH SECOND-BRANCH."
  (interactive
   (list (gited-get-branchname)))
  (let ((branch-new
         (completing-read (format "Diff %s with: " branch)
                          (gited-listed-branches)))
        (buf (gited--output-buffer)))
    (setq gited-output-buffer buf)
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (gited-git-command `("diff" ,branch ,branch-new)
                           (current-buffer)))
      (display-buffer buf))
    (gited--set-output-buffer-mode buf 'diff)))

(defun gited--valid-ref-p (str)
  (let ((args `("rev-parse" ,str)))
    (zerop (gited-git-command args))))

(defun gited-show-commit (branch &optional commit)
  "Show a commit of BRANCH.
BRANCH default to the branch at current line.

Optional arg COMMIT, if non-nil then is a SHA1 string or
HEAD~N, to indicate which commit to display.
Interactively prompt for the limit commit: 0 means HEAD,
1 means HEAD~, and so on."
  (interactive
   (let* ((branch (gited-get-branchname))
          (num-or-sha1
           (read-string "Show commit (0 = HEAD, 1 = HEAD~1, ... or SHA1): "
                        nil nil "0")))
     (list branch num-or-sha1)))
  (let ((buf (gited--output-buffer))
        (args (if (gited--valid-ref-p commit)
                  `("show" ,commit)
                (list "show" (concat (gited--case-ref-kind)
                                     branch "~" commit)))))
    (setq gited-output-buffer buf)
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (gited-git-command args (current-buffer)))
      (display-buffer buf))
    (gited--set-output-buffer-mode buf 'diff)))

(defun gited-status ()
  "Show repository status.
If `magit' is installed, then this calls `magit-status'.  Otherwise,
show similar info as that command."
  (interactive)
  (let ((buf (gited--output-buffer))
        (branch gited-current-branch)
        (gited-buf gited-buffer))
    (setq gited-output-buffer buf)
    (if (ignore-errors (find-library-name "magit"))
        (with-no-warnings (magit-status-internal default-directory))
      (with-current-buffer buf
        (let ((inhibit-read-only t)
              (untracked (gited-untracked-files))
              (unstaged (gited-modified-files))
              (bisectingp (gited-bisecting-p))
              (stashes (gited-stashes)))
          (erase-buffer)
          ;; (outline-mode)
          (insert (format "Head:     %s %s\n"
                          (propertize branch
                                      'font-lock-face 'gited-status-branch-local)
                          (gited-last-commit-title)))
          ;; (insert (format "Tag:     %s (%s)\n" tag tag-id))
          (when bisectingp
            (insert (format "\n%s\nCall C-u C-u %s to reset\n"
                            (propertize
                             "You are bisecting:"
                             'font-lock-face 'gited-status-tag)
                            (with-current-buffer gited-buf
                              (substitute-command-keys "\\[gited-bisect]"))))
            (let ((nentries -1)
                  bad-commit res)
              (with-temp-buffer
                (gited-git-command '("bisect" "log") (current-buffer))
                (goto-char (point-min))
                (save-excursion
                  (when (re-search-forward
                         "# first bad commit: \\[\\([[:xdigit:]]+\\)\\]"
                         nil t)
                    (setq bad-commit (match-string 1))))
                (while (re-search-forward "^[^#]+$" nil t)
                  (push (buffer-substring-no-properties (point-at-bol) (point))
                        res))
                (setq res (nreverse res) nentries (length res)))
              (insert (format "\n%s (%d)\n"
                              (propertize
                               "Bisect Log:"
                               'font-lock-face 'gited-status-tag)
                              (1+ nentries)))
              (while res
                (insert (concat (pop res) "\n")))
              (when bad-commit
                (insert (concat bad-commit " is the first bad commit\n")))))
          (when untracked
            (insert (format "\n%s (%d)\n"
                            (propertize "Untracked files"
                                        'font-lock-face 'gited-status-tag)
                            (length untracked)))
            (insert (mapconcat 'identity untracked "\n")))
          (when unstaged
            (insert (format "\n\n%s (%d)\n"
                            (propertize "Unstaged changes"
                                        'font-lock-face 'gited-status-tag)
                            (length unstaged)))
            (insert (mapconcat 'identity unstaged "\n")))
          (when stashes
            (insert (format "\n\n%s (%d)\n"
                            (propertize "Stashes"
                                        'font-lock-face 'gited-status-tag)
                            (length stashes)))
            (insert (mapconcat 'identity stashes "\n")))
          (display-buffer buf)
          (gited--set-output-buffer-mode buf 'outline)))
      )))

(defun gited-pull ()
  "Run git pull in current branch."
  (interactive)
  (let ((branch gited-current-branch))
    (if (not (or gited-expert
                 (y-or-n-p (format "Pull on '%s' branch? " branch))))
        (message "OK, pull canceled")
      (let ((buf (gited--output-buffer))
            (cmd (format "%s pull" vc-git-program))
            (inhibit-read-only t))
        (setq gited-output-buffer buf
              gited-op-string cmd)
        (with-current-buffer buf (erase-buffer))
        (gited-async-operation cmd 'remote-op-p)))))

(defun gited-push ()
  "Run git push in current branch."
  (interactive)
  (if (not (or gited-expert
               (y-or-n-p (format "Push '%s' branch? "
                                 gited-current-branch))))
      (message "OK, push canceled")
    (let ((buf (gited--output-buffer))
          (cmd (format "%s push" vc-git-program)))
      (setq gited-output-buffer buf
            gited-op-string cmd)
      (with-current-buffer buf
        (setq buffer-read-only nil) ; Editable, they can ask username.
        (erase-buffer))
      (gited-async-operation cmd 'remote-op-p))))

(defun gited-set-branch-upstream (branch)
  "Push a local BRANCH to origin."
  (interactive
   (list (gited-get-branchname)))
  (unless (string= gited-ref-kind "local")
    (error "Gited should be listing local branches"))
  (if (not (or gited-expert
               (y-or-n-p (format "Push '%s' branch up stream? "
                                 branch))))
      (message "OK, push canceled")
    (let ((buf (gited--output-buffer))
          (cmd (format "%s push --set-upstream origin %s"
                       vc-git-program branch))
          (inhibit-read-only t))
      (setq gited-output-buffer buf
            gited-op-string (format "Set branch '%s' upsetream" branch))
      (with-current-buffer buf (erase-buffer))
      (gited-async-operation cmd 'remote-op-p))))

(defun gited-origin (branch &optional no-display)
  "Run git log origin..BRANCH.
BRANCH defaults to the branch at point.
If optional arg NO-DISPLAY is non-nil, then don't display the
output buffer.
Return output buffer.

Note that this command only has sense if you have a remote branch
called origin in your Git configuration.  Otherwise, if you wish to
see the newest N commits then use `\\[gited-log-last-n-commits\]'."
  (interactive
   (list (gited-get-branchname) current-prefix-arg))
  (unless (string= gited-ref-kind "local")
    (error "Not listing local branches"))
  (unless (gited-remote-repository-p)
    (error "Not a remote repository.  Try '%s' or '%s'"
           (substitute-command-keys "\\[gited-log\]")
           (substitute-command-keys "\\[gited-log-last-n-commits\]")))
  (let ((buf (gited--output-buffer))
        (args (list "log" (concat "origin.." (gited--case-ref-kind) branch)))
        (gited-buf gited-buffer) count)
    (setq gited-output-buffer buf)
    (with-current-buffer buf
      (let ((inhibit-read-only t) res)
        (erase-buffer)
        (setq res (gited-git-command args (current-buffer)))
        (unless (zerop res)
          (with-current-buffer gited-buf
            (error "Command 'git log origin..BRANCH' fails.  Try '%s' or '%s'"
                   (substitute-command-keys "\\[gited-log\]")
                   (substitute-command-keys "\\[gited-log-last-n-commits\]"))))
        (goto-char (point-min)))
      (setq count (gited-number-of-commits))
      (or no-display (display-buffer buf)))
    (gited--set-output-buffer-mode buf 'diff)
    (message "%d commit%s in '%s'"
             count (if (> count 1) "s" "") (buffer-name buf))
    buf))

(defun gited-log (branch start-date end-date &optional short)
  "Show Git log for BRANCH between START-DATE and END-DATE.
If optional arg SHORT is non-nil, then use a short format.

Interactively, prompt for START-DATE and END-DATE."
  (interactive
   (let* ((branch (gited-get-branchname))
          (last-commit-time
           (gited-get-last-commit-time branch))
          (default
            (format-time-string
             "%F"
             (time-subtract
              last-commit-time
              (days-to-time 7))))
          (start   (read-string "start-date: " default nil nil nil))
          (default (format-time-string "%F"))
          (end     (read-string "end-date: " default nil nil nil)))
     (list branch start end current-prefix-arg)))
  (let ((buf (gited--output-buffer))
        (args (append (if short gited-short-log-cmd '("log"))
                      (list (concat (gited--case-ref-kind) branch)))))
    (setq gited-output-buffer buf)
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq args (append args
                           (list (format "--after=\"%s\"" start-date))
                           (list (format "--before=\"%s\"" end-date))))
        (gited-git-command args buf))
      (display-buffer buf))
    (gited--set-output-buffer-mode buf 'outline)))

(defun gited-log-last-n-commits (branch n &optional short)
  "Show newest N log entries for BRANCH.
When N is of the form N1-N2, then skip the N1 newest log entries
and show the remaining newest N2 entries.
The actual command run in this case is as follows:
git-log --skip=N1 --max-count=N2.
If optional arg SHORT is non-nil use a short format."
  (interactive
   (list (gited-get-branchname)
         (read-string "Show newest N commits, or those in (N1, N1 + N2]: "
                      nil nil "1")
         current-prefix-arg))
  (let ((buf (gited--output-buffer)))
    (setq gited-output-buffer buf)
    (with-current-buffer buf
      (let* ((inhibit-read-only t)
             (skip (and (string-match "\\([0-9]+\\)-\\([0-9]+\\)" n)
                        (string-to-number (match-string 1 n))))
             (max (string-to-number (if skip (match-string 2 n) n)))
             (args (append (if short gited-short-log-cmd '("log"))
                           (and skip (list (format "--skip=%d" skip)))
                           (list (format "--max-count=%d" max))
                           (list branch))))
        (erase-buffer)
        (gited-git-command args buf))
      (display-buffer buf))
    (gited--set-output-buffer-mode buf 'outline)))

;;; Extract patches

(defun gited--clean-previous-patches ()
  (mapc (lambda (x)
          (when (buffer-live-p x)
            (kill-buffer x)))
        (nconc
         (gited--get-patch-or-commit-buffers)
         (gited--get-patch-or-commit-buffers 'commit))))

(defun gited-extract-patches (n &optional origin write-file)
  "Extract the patches from the N newest commits.
Optional arg ORIGIN, means extract the patches from all commits accesible
from the trunk, and not being in the trunk.
Optional arg WRITE-FILE if non-nil, then write the patches to disk."
  (interactive
   (let* ((prefix current-prefix-arg)
          (num (unless prefix
                 (read-string "Extract N newest patches: "
                              nil nil "1")))
          (from-origin (and prefix (equal prefix '(4))))
          (write (and prefix (equal prefix '(16)))))
     (list num from-origin write)))
  (let* ((branch (gited-get-branchname))
         (buffer (if origin
                     (gited-origin branch 'no-display)
                   (gited-log-last-n-commits branch n)
                   gited-output-buffer))
         num-commits count)
    (with-current-buffer buffer
      (if (zerop (buffer-size))
          (error "No new patches")
        ;; Previous patch buffers must be deleted.
        (gited--clean-previous-patches)
        (save-excursion
          (goto-char (point-min))
          (setq num-commits (gited-number-of-commits)
                count num-commits))))
    ;; Following form must be evalled with branch temporary current.
    (gited-with-current-branch branch
      (dotimes (i num-commits)
        (let ((buf-patch (get-buffer-create (format "*gited-patch-%d*" count)))
              (buf-commit (get-buffer-create
                           (format "*gited-commit-%d*" count))))
          (with-current-buffer buf-patch
            (gited-git-command
             `("format-patch" "-1" ,(format "HEAD~%d" i) "--stdout")
                               (current-buffer))
            (gited--set-output-buffer-mode (current-buffer) 'diff 'editable))
          (with-current-buffer buf-commit
            (gited-git-command `("show" "-s" "--format=%B" ,(format "HEAD~%d" i))
                               (current-buffer))
            (while (looking-at "^$") ; Delete empty lines.
              (delete-char -1)))
          (when write-file
            (with-temp-file (expand-file-name
                             (substring (buffer-name buf-patch) 1 -1)
                             temporary-file-directory)
              (insert
               (with-current-buffer buf-patch
                 (buffer-string)))))
          (cl-decf count))))
    (if write-file
        (message "Extracted %d patches and saved in %s"
                 num-commits temporary-file-directory)
      (message "Extracted %d patches" num-commits))) t)

(defun gited--get-patch-or-commit-buffers (&optional commit)
  (let ((regexp
         (if commit
             "\\`\\*gited-commit-\\([0-9]+\\)\\*\\'"
           "\\`\\*gited-patch-\\([0-9]+\\)\\*\\'")))
    (sort
     (cl-delete-if-not
      (lambda (x)
        (string-match regexp (buffer-name x)))
      (buffer-list))
     (lambda (x y)
       (string< (buffer-name x) (buffer-name y)))
     )))

(defun gited-sync-with-trunk (branch-target)
  "Extract latest patches in branch at point and apply then into BRANCH-TARGET.
BRANCH-TARGET is a new branch copied from (car (gited-trunk-branches)).
The effect is similar than merge the branch at point with the trunk;
one difference is that we don't modify the trunk, instead we copy it;
another difference that we don't get a 'Merge branch...' commit in the log.
this command set BRANCH-TARGET current."
  (interactive
   (let* ((br (gited-get-branchname))
          (prompt
           (format "Syncronized '%s' into new branch: " br))
          (def (if (string-match "-new\\([0-9]*\\)\\'" br)
                   (format "%s%d" (substring br 0 (match-beginning 1))
                           (1+ (string-to-number (match-string 1 br))))
                 (concat br "-new1"))))
     (list
      (completing-read prompt
                       (gited-listed-branches)
                       nil nil def))))
  ;; Previous patch buffers must be deleted.
  (gited--clean-previous-patches)
  (unless (gited-remote-repository-p)
    (error "This command only works for repositories \
tracking a remote repository"))
  (if (null (ignore-errors (gited-extract-patches nil t)))
      (error "No new patches to apply")
    ;; If branch-target doesn't exists create it as copy of master.
    (unless (member branch-target (gited-listed-branches))
      (cond ((gited-trunk-branches)
             (gited-copy-branch (car (gited-trunk-branches)) branch-target))
            (t (error "I don't know what is your master branch"))))
    (let (num-commits)
      (gited-with-current-branch branch-target
        (let* ((buf-patches
                (gited--get-patch-or-commit-buffers))
               (buf-commits
                (gited--get-patch-or-commit-buffers 'commit)))
          (setq num-commits (length buf-patches))
          (while buf-patches
            (gited-apply-add-and-commit-patch (car buf-patches)
                                              (car buf-commits))
            (setq buf-patches (cdr buf-patches)
                  buf-commits (cdr buf-commits)))))
      (gited-checkout-branch branch-target)
      (gited-update)
      (message "Successfully applied and committed %d commits!"
               num-commits))))

(defun gited-bisecting-p ()
  "Return non-nil if a Git bisect is on process."
  (zerop (gited-git-command '("bisect" "log"))))

(defun gited--bisect-executable-p (command)
  (let ((file (car (split-string command))))
    (unless (file-executable-p file)
      (error "File '%s' not executable" file))))

(defun gited--bisect-after-run (buffer)
  (let ((regexp "^[[:xdigit:]]+ is the first bad commit")
        pos window)
    (gited--set-output-buffer-mode buffer)
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward regexp nil t)
          (setq pos (match-beginning 0))
          (put-text-property (match-beginning 0)
                             (match-end 0)
                             'font-lock-face 'gited-status-tag))))
    (setq window (display-buffer buffer))
    (when pos (set-window-point window pos))))

(defun gited-bisect (&optional script reset)
  "Execute a Git bisect.
Optional arg SCRIPT if non-nil, then is a script to pass to
git bisect run.
Optional arg RESET if non-nil, then means abort the current bisect.
Interactively, a prefix 'C-u' prompts for SCRIPT; a prefix 'C-u C-u'
set RESET non-nil."
  (interactive
   (let ((prefix current-prefix-arg))
     (list (equal prefix '(4))
           (equal prefix '(16)))))
  (let ((bisectingp (gited-bisecting-p))
        (obuf (gited--output-buffer gited-bisect-buf-name))
        (branch (gited-get-branchname)))
    ;; Ensure output buffer is editable.
    (with-current-buffer obuf (setq buffer-read-only nil))
    (cond (reset
           (if (not bisectingp)
               (message "Not bisecting.  Nothing to do")
             (setq gited--running-async-op nil)
             (gited-git-command '("bisect" "reset") obuf)
             (message "Successfully reseted git bisect!")))
          ((not bisectingp)
           (with-current-buffer obuf
             (erase-buffer))
           (let ((bad (read-string "Start bisect with bad/new revision: "
                                   nil nil branch))
                 (good (read-string "Good/Old revision: " nil nil branch))
                 (cmd (and script
                           (read-shell-command "Bisect shell command: "))))
             (and cmd
                  (gited--bisect-executable-p cmd)) ; File must be executable.
             (gited-git-command `("bisect" "start" ,bad ,good) obuf)
             (when cmd
               ;; (when (zerop (gited-git-command `("bisect" "run" ,cmd) obuf))
               ;;   (gited--bisect-after-run obuf))
               (gited-async-operation
                (format "%s bisect run %s" vc-git-program cmd) nil obuf)
               (setq gited-op-string "bisect run"))
             (display-buffer obuf)))
          ((and bisectingp script)
           (let ((cmd
                  (and script (read-shell-command "Bisect shell command: "))))
             (and cmd
                  (gited--bisect-executable-p cmd)) ; File must be executable.
             ;; (when (zerop (gited-git-command `("bisect" "run" ,cmd) obuf))
             ;;   (gited--bisect-after-run obuf))
             (gited-async-operation
              (format "%s bisect run %s" vc-git-program cmd) nil obuf)
             (setq gited-op-string "bisect run"))
           (display-buffer obuf))
          (t
           (let* ((is-badp (y-or-n-p "Is current revision Bad/New? "))
                  (is-goodp (and (not is-badp)
                                 (y-or-n-p "Is current revision Good/Old? ")))
                  (skip (and (not (or is-badp is-goodp))
                             (y-or-n-p "Do you want to skip this commit? ")))
                  (args
                   (cond (is-badp '("bisect" "bad"))
                         (is-goodp '("bisect" "good"))
                         (skip '("bisect" "skip"))
                         (t (error "Commit should be either bad, \
good or skip")))))
             (gited-git-command args obuf)
             (display-buffer obuf))))))


;;; Git Stash commands.
(defun gited-stash (&optional untracked)
  "Stash the changes in a dirty working directory away.
If called with prefix argument, then include untracked files.  With two
prefix arguments includes the ignored files as well."
  (interactive
   (let ((prefix current-prefix-arg))
     (list (cond ((and prefix (equal prefix '(4))) "--include-untracked")
                 ((and prefix (equal prefix '(16))) "--all")))))
  (let* ((msg (read-string
               "Stash message: "
               (format "WIP on %s: " (gited-current-branch))))
         (args (append '("stash") '("save") `(,msg)
                       (and untracked `(,untracked)))))
    (gited-git-command args)))

(defun gited-stash-apply ()
  "Apply a stash to the working tree."
  (interactive)
  (if (null (gited-stashes))
      (message "Empty stash list")
    (let* ((stash
            (read-string "Apply stash: " nil nil "stash@{0}"))
           (args `("stash" "apply" ,stash)))
      (gited-git-command args))))

(defun gited-stash-pop ()
  "Apply a stash to the working tree and remove it from stash list."
  (interactive)
  (if (null (gited-stashes))
      (message "Empty stash list")
    (let* ((stash
            (read-string "Apply pop: " nil nil "stash@{0}"))
           (args `("stash" "pop" ,stash)))
      (gited-git-command args))))

(defun gited--stash-branch ()
  (cond ((null (gited-stashes))
         (error "Empty stash list"))
        ((gited-modified-files)
         (error "Commit your local changes before you switch branches"))
        (t)))
  
(defun gited-stash-branch (branch stash)
  "Create and checkout a new BRANCH from STASH."
  (interactive
   (let ((stash
          (and (gited--stash-branch)
               (read-string "Branch stash: " nil nil "stash@{0}")))
         (br (read-string "Branch name: ")))
     (list br stash)))
  (when (gited--stash-branch)
    (let ((args `("stash" "branch" ,branch ,stash)))
      (gited-git-command args))))

(defun gited-stash-drop ()
  "Remove a stash from the stash list."
  (interactive)
  (if (null (gited-stashes))
      (message "Empty stash list")
    (let* ((stash
            (read-string "Drop stash: " nil nil "stash@{0}"))
           (args `("stash" "drop" ,stash)))
      (gited-git-command args))))

(defun gited-branch-clear ()
  "Remove all stashes from the stash list."
  (interactive)
  (if (null (gited-stashes))
      (message "Empty stash list")
    (if (y-or-n-p "Remove all stashes? ")
        (gited-git-command '("stash" "clear"))
      (error "OK, canceled"))))

(defalias 'gited-delete-all-stashes 'gited-branch-clear)



;;; Moving around.

(defun gited-next-line (&optional arg)
  "Go to start of branch name in next ARG lines."
  (interactive "p")
  (forward-line arg)
  (condition-case nil
      (gited-move-to-branchname)
    (error
     (forward-line -1)
     (gited-move-to-branchname)
     (message "At last branch!")
     (ding)
     (sit-for 1)
     (message nil))))

(defun gited-prev-line (&optional arg)
  "Go to start of branch name in previous ARG lines."
  (interactive "p")
  (let ((oline (line-number-at-pos))
        nb-line)
    (when (natnump arg) (setq arg (- arg)))
    (forward-line arg)
    (gited-move-to-branchname)
    (setq nb-line (line-number-at-pos))
    (when (or (= oline nb-line)
              (< (- oline (abs arg)) nb-line))
      (message "At first branch!")
      (ding)
      (sit-for 1)
      (message nil))))

(defun gited--goto-first-branch ()
  (goto-char (point-min))
  (when (overlays-at (point-min))
    (forward-line)))

(defun gited-goto-first-branch ()
  "Go to first branch in current Gited buffer."
  (interactive)
  (gited--goto-first-branch)
  (gited-move-to-branchname))

(defun gited-goto-last-branch ()
  "Go to last branch in current Gited buffer."
  (interactive)
  (goto-char (point-max))
  (forward-line -1)
  (gited-move-to-branchname))

(defun gited--goto-column (col)
  (forward-line 0)
  (dotimes (_ (1- col))
    (goto-char
     (next-single-property-change
      (point)
      'tabulated-list-column-name))))

(defun gited-goto-branch (branch)
  "Go to line describing BRANCH in this Gited buffer.
Return buffer position on success, else nil."
  (interactive
   (let ((cur-branch gited-current-branch))
     (list (completing-read "Jump to branch: "
                            (gited-listed-branches)
                            nil t nil nil cur-branch))))
  (let ((row (cl-position branch (gited-listed-branches) :test #'equal)))
    (goto-char (point-min))
    (forward-line (if (overlays-at (point-min)) (1+ row) row))
    (gited--goto-column (1+ gited-branch-idx))))

(defun gited-next-marked-branch (arg &optional wrap opoint)
  "Move to the next ARG marked branch.
If WRAP is non-nil, wrap around to the beginning of the buffer if
we reach the end."
  (interactive "p\np")
  (or opoint (setq opoint (point)));; return to where interactively started
  (if (if (> arg 0)
          (re-search-forward gited-re-mark nil t arg)
        (beginning-of-line)
        (re-search-backward gited-re-mark nil t (- arg)))
      (gited-move-to-branchname)
    (if (null wrap)
        (progn
          (goto-char opoint)
          (error "No next marked branch"))
      (message "(Wraparound for next marked branch)")
      (goto-char (if (> arg 0) (point-min) (point-max)))
      (gited-next-marked-branch arg nil opoint))))

(defun gited-prev-marked-branch (arg &optional wrap)
  "Move to the previous ARG marked branch.
If WRAP is non-nil, wrap around to the end of the buffer if we
reach the beginning of the buffer."
  (interactive "p\np")
  (gited-next-marked-branch (- arg) wrap))


;; Fill and return `gited-branch-alist'.

(defun gited-get-element-in-row (idx)
  (let ((row (tabulated-list-get-entry)))
    (if row
        (aref row idx)
      (error "No row at point"))))

(defun gited-get-branchname ()
  (gited-get-element-in-row gited-branch-idx))

(defun gited-get-mark ()
  (gited-get-element-in-row gited-mark-idx))

(defun gited-get-date ()
  (gited-get-element-in-row gited-date-idx))

(defun gited-get-commit ()
  (gited-get-element-in-row gited-commit-idx))

(defalias 'gited--move-to-column #'gited--goto-column)

(defun gited--move-to-end-of-column (col)
  (gited--move-to-column col)
  (goto-char (next-single-property-change (point) 'tabulated-list-column-name)))

(defun gited--fill-branch-alist (&optional pattern)
  (let* ((args (append (butlast gited-list-refs-format-command)
                       (list (format (car (last gited-list-refs-format-command))
                                     (if pattern
                                         (pcase pattern
                                           ("local" "heads")
                                           ("remote" "remotes")
                                           (_ pattern))
                                       "heads")))))
         (alist
          (with-temp-buffer
            (insert "(\n")
            (unless (zerop (gited-git-command args (current-buffer)
                                              nil 'unquote))
              (error "No Git repository in current directory"))
            (insert ")")
            (mapcar (lambda (x)
                      (when (stringp (car x)) ; No time: set it to beginning
                                              ; of epoch.
                        (push 0 x))
                      (when (= (length x) 4) ; Drop time zone.
                        (setf (cdr x) (cddr x)))
                      (when (and (stringp (car (last x))) ; If no Author, set
                                                          ; it Unknown.
                                 (string= "" (car (last x))))
                        (setf (car (last x)) "Unknown"))
                      x)
                    (car (read-from-string (buffer-string))))))
         (prep
          (make-progress-reporter
           "Collecting brach info..."
           0 (length alist))))
    (cl-flet ((format-time-fn (time-secs)
                              (format-time-string
                               gited-date-format
                               (apply #'encode-time
                                      (decode-time
                                       (if (< emacs-major-version 25)
                                           (seconds-to-time time-secs)
                                         time-secs)))))
              (get-mark-fn (x)
                           (let ((table
                                  (save-excursion
                                    (gited--goto-first-branch)
                                    (ignore-errors (gited-get-mark)))))
                             (if (and table (ignore-errors
                                              (gited-goto-branch (nth 1 x))))
                                 (cons (gited-get-mark) nil)
                               '(" ")))))
      ;; Get title of first commit for each listed branch.
      (setq gited-branch-alist
            (cl-loop for entry the elements of alist using (index idx) collect
                     (progn
                       (progress-reporter-update prep idx)
                       (let* ((args (list "log"
                                          "--pretty=format:%h | %s"
                                          (cadr entry) "-n1" "--"))
                              (str (with-temp-buffer
                                     (gited-git-command args
                                                        (current-buffer) nil)
                                     (buffer-string))))
                         ;; Format time in seconds as `gited-date-format'.
                         (setf (car entry) (format-time-fn (car entry)))
                         (append `(,(1+ idx)) (get-mark-fn entry)
                                 entry `(,str)))))))
    (progress-reporter-done prep)
    gited-branch-alist))


;;; Toggles.

(defun gited-hide-details-update-invisibility-spec ()
  (let ((col-names
         (if gited-hide-details-mode
             '("M" "Branches" "" "" "Last Commit")
           '("M" "Authors" "Date" "Branches" "Last Commit")))
        (col-sizes
         (if gited-hide-details-mode
             '(2 50 -1 -1 65)
           '(2 16 17 50 65))))
    (gited--list-format-init col-names col-sizes)
    (setq tabulated-list-format gited-list-format)
    (funcall (if gited-hide-details-mode
                 'add-to-invisibility-spec
               'remove-from-invisibility-spec)
             'gited-hide-details-author)
    (funcall (if gited-hide-details-mode
                 'add-to-invisibility-spec
               'remove-from-invisibility-spec)
             'gited-hide-details-date)
    (tabulated-list-init-header)
    (gited--update-padding (not gited-hide-details-mode))))

(defun gited--update-padding (undo)
  "Update columns padding after `gited-hide-details-mode'."
  (let ((inhibit-read-only t))
    (save-excursion
      (gited-goto-first-branch)
      (while (not (eobp))
        (gited-move-to-end-of-branchname)
        (skip-chars-backward " \t")
        (forward-char 1)
        (when (equal (get-text-property (point) 'display)
                     `(space :align-to ,(if undo 54 89)))
          (put-text-property (point) (1+ (point))
                             'display `(space :align-to ,(if undo 89 54)))
          (delete-region (1+ (point))
                         (gited-move-to-end-of-branchname)))
        (forward-line 1))) nil))


;;; Marked branches.
(defun gited-map-lines (fn)
  "Map FN on each Gited line."
  (let (br-name mark)
    (save-excursion
      (gited--goto-first-branch)
      (while (ignore-errors
               (setq br-name (gited-get-branchname)
                     mark  (gited-get-mark)))
        (funcall fn br-name mark)
        (forward-line)))))

(defun gited-get-marked-branches ()
  "Return a list of branches currently marked."
  (delq nil
        (mapcar (lambda (e)
                  (when (equal (string-to-char (cdr e))
                               gited-marker-char)
                    (car e)))
                (gited-current-state-list))))

(defun gited-current-state-list (&optional pos)
  "Return a list like (BRANCH . MARK) of all branches in an Gited buffer.
If POS is non-nil, return a list like (BRANCH MARK POINT), where POINT is
the value of point at the beginning of the line for that buffer."
  (let ((gited-current-state-list-tmp '()))
    (if pos
        (gited-map-lines
         (lambda (branch mark)
           (push (list branch mark (point))
                 gited-current-state-list-tmp)))
      (gited-map-lines
       (lambda (branch mark)
         (push (cons branch mark) gited-current-state-list-tmp))))
    (nreverse gited-current-state-list-tmp)))

(defun gited-current-branches-with-marks ()
  "Return a list like (BRANCH . MARK) of all listed branches."
  (let ((branches (gited-current-state-list)))
    (mapcar (lambda (x)
              (let ((e (assoc x branches)))
                (if e
                    e
                  (cons x ?\s))))
            (gited-listed-branches))))


 ;;; Mark/unmark.

(defun gited-remember-marks (beg end)
  "Return alist of branches and their marks, from BEG to END."
  (if selective-display         ; must unhide to make this work.
      (let ((inhibit-read-only t))
        (subst-char-in-region beg end ?\r ?\n)))
  (let (branch chr alist)
    (save-excursion
      (goto-char beg)
      (while (re-search-forward gited-re-mark end t)
        (when (setq branch (gited-get-branchname))
          (setq chr (preceding-char)
                alist (cons (cons branch chr) alist)))))
    alist))

(defun gited-mark-remembered (alist)
  "Mark all branches remembered in ALIST.
Each element of ALIST looks like (BRANCH . MARKERCHAR)."
  (let (elt branch chr)
    (save-excursion
      (while alist
        (setq elt (car alist)
              alist (cdr alist)
              branch (car elt)
              chr (cdr elt))
        (when (gited-goto-branch branch)
          (beginning-of-line)
          (delete-char 1)
          (insert chr))))))

(defun gited-fontify-current-branch ()
  "Set font for current branch."
  (let ((inhibit-read-only t))
    (save-excursion
      (gited-goto-branch gited-current-branch)
      (when (string= " " (gited-get-mark))
        (remove-text-properties
         (point-at-bol) (point-at-eol) '(face))
        (put-text-property
         (point)
         (gited-move-to-end-of-branchname)
         'face gited-current-branch-face)
        (put-text-property
         (point-at-bol)
         (point-at-eol)
         'face gited-section-highlight-face)))))

(defun gited-fontify-marked-branch-name (&optional mark)
  "Set font for a marked branch."
  (let ((marker (or mark (string-to-char (gited-get-mark))))
        (inhibit-read-only t))
    (gited-move-to-branchname)
    (remove-text-properties
     (point-at-bol) (point-at-eol) '(face))
    (cond ((eq marker ?\s)
           (if (string= (gited-get-branchname)
                        gited-current-branch)
               (put-text-property
                (point-at-bol)
                (point-at-eol)
                'face gited-section-highlight-face)
             (gited--fontify-current-row)))
          ((eq marker gited-marker-char)
           (put-text-property
            (point-at-bol) (1+ (point-at-bol)) 'face gited-flag-mark-face)
           (put-text-property
            (1+ (point-at-bol))
            (point-at-eol)
            'face gited-flag-mark-line-face))
          ((eq marker gited-del-char)
           (put-text-property
            (point-at-bol) (1+ (point-at-bol)) 'face gited-deletion-face)
           (put-text-property
            (1+ (point-at-bol))
            (point-at-eol)
            'face gited-deletion-branch-face)))))

(defun gited-insert-marker-char (&optional marker)
  (tabulated-list-set-col gited-mark-idx
                          (char-to-string (or marker gited-marker-char))
                          'change))

(defun gited-flag-branch-deletion (arg &optional interactive)
  "In Gited, flag the branch at current line (or next ARG) for deletion.
If the region is active, flag all branches in the region.
Otherwise, with a prefix arg, flag branches on the next ARG lines.

If the region is active in Transient Mark mode, flag all branches
in the active region."
  (interactive (list current-prefix-arg t))
  (gited-mark arg gited-del-char interactive))

(defun gited-toggle-marks ()
  "Toggle marks: marked branches become unmarked, and vice versa.
Branches marked with other flags (such as `D') are not affected.
As always, hidden subdirs are not affected."
  (interactive)
  (save-excursion
    (gited--goto-first-branch)
    (while (not (eobp))
      (let* ((mark (string-to-char (gited-get-mark)))
             (flag
              (cond ((eq ?\s mark) gited-marker-char)
                    ((eq gited-marker-char mark) ?\s)
                    (t nil))))
        (when flag
          (gited-insert-marker-char flag)
          (gited-fontify-marked-branch-name flag)))
      (forward-line))))

(defun gited-kill-line (&optional arg)
  "Kill the current line or next ARG lines (not the branches).
With a prefix argument, kill that many lines starting with the current line.
\(A negative argument kills backward.)"
  (interactive "P")
  (setq arg (prefix-numeric-value arg))
  (let (buffer-read-only branch)
    (while (/= 0 arg)
      (setq branch (gited-get-branchname))
      (if (not branch)
          (error "Can only kill branch lines")
        (setq tabulated-list-entries
              (assq-delete-all
               (car (tabulated-list-delete-entry))
               tabulated-list-entries))
        (if (> arg 0)
            (cl-decf arg)
          (cl-incf arg)
          (forward-line -1))))
    (gited-move-to-branchname)))

(defun gited-do-kill-lines (&optional arg fmt)
  "Kill all marked lines (not the branches).
With a prefix argument, kill that many lines starting with the current line.
\(A negative argument kills backward.)"
  ;; Returns count of killed lines.  FMT="" suppresses message.
  (interactive "P")
  (if arg
      (gited-kill-line arg)
    (save-excursion
      (gited--goto-first-branch)
      (let (buffer-read-only
            (count 0)
            (regexp
             (concat "^" (regexp-quote (char-to-string gited-marker-char)))))
        (while (and (not (eobp))
                    (re-search-forward regexp nil t))
          (cl-incf count)
          (setq tabulated-list-entries
                (assq-delete-all
                 (car (tabulated-list-delete-entry))
                 tabulated-list-entries)))
        (or (equal "" fmt)
            (message (or fmt "Killed %d line%s.") count (gited-plural-s count)))
        count))))

(defun gited-mark-branches-regexp (regexp &optional marker-char)
  "Mark all files matching REGEXP for use in later commands.
A prefix argument means to unmark them instead.

REGEXP is an Emacs regexp, not a shell wildcard.  Thus, use `\\.o$' for
object files--just `.o' will mark more than you might think."
  (interactive
   (list (read-regexp (concat (if current-prefix-arg "Unmark" "Mark")
                              " branches (regexp): ")
                      nil)
         (and current-prefix-arg ?\s)))
  (let ((gited-marker-char (or marker-char gited-marker-char)))
    (gited-mark-if
     (and (not (eolp))          ; empty line
          (let ((fn (gited-get-branchname)))
            (and fn (string-match-p regexp fn))))
     "matching branch")))

(defun gited-date-string-to-time (str)
  (apply #'encode-time
	 (parse-time-string str)))

(defun gited-mark-branches-containing-regexp (regexp &optional marker-char days)
  "Mark all branches containing REGEXP in some commit message.
A prefix argument means to unmark them instead.

Optional arg DAYS, if non-nil then limit the search to DAYS before the
newest commit.  Otherwise, limit the search to commits until 1 month earlier
than the newest commit.
In interactive calls, a prefix C-u C-u prompts for DAYS."
  (interactive
   (let ((def current-prefix-arg)
         regex marker interval)
     (pcase def
       (`(16) (setq interval
                    (string-to-number
                     (read-string "Number of days before newest commit: "
                                  nil nil "30"))
                    marker (pcase (read-number "Mark (0) or Unmark (1): " 0)
                             (0 gited-marker-char)
                             (1 ?\s)
                             (_ gited-marker-char))
                    regex (read-string
                           (format "%s branches containing regexp: "
                                   (if (char-equal marker gited-marker-char)
                                       "Mark" "UNmark")))))
       (_ (setq interval 30
                marker (if current-prefix-arg
                           ?\s
                         gited-marker-char)
                regex (read-string
                       (format "%s branches containing regexp: "
                               (if (char-equal marker gited-marker-char)
                                   "Mark" "UNmark"))))))
     (list regex marker interval)))
  (let ((gited-marker-char (or marker-char gited-marker-char)))
    (gited-mark-if
     (and (not (eolp))
          (gited-get-branchname)
          (let* ((fn (gited-get-branchname))
                 (time-max
		  (gited-date-string-to-time
		   (car
		    (cddr
		     (cl-find-if
		      (lambda (x)
			(string= (nth 3 x) fn)) gited-branch-alist)))))
                 (time-min (time-subtract time-max (days-to-time (or days 30))))
                 (args (list "log"
                             (format "--after=%s"
                                     (format-time-string "%F" time-min))
                             (format "--before=%s"
                                     (format-time-string "%F" time-max))
                             (format "--grep=%s" regexp)
                             fn "--")))
            (with-temp-buffer
              (gited-git-command args (current-buffer))
              (not (string= "" (buffer-string))))))
     "matching branch")))

(defun gited-mark-branches-containing-commit (commit &optional marker-char)
  "Mark all branches containing COMMIT.
A prefix argument means to unmark them instead.
COMMIT is the sha1 of the commit."
  (interactive
   (list (read-string (format "%s branches containing sha1 commit: "
                              (if current-prefix-arg "UNmark" "Mark")))
         (and current-prefix-arg ?\s)))
  (let* ((args `("branch" ,(concat "--contains=" commit)))
         (branches (gited--get-branches-from-command args))
         (gited-marker-char (or marker-char gited-marker-char)))
    (gited-mark-if
     (and (not (eolp))
          (let ((fn (gited-get-branchname)))
            (and fn (member fn branches))))
     "matching branch")))

(defvar gited--op nil)
(defun gited--mark-merged-or-unmerged-branches-spec (op-val)
  (setq gited--op op-val)
  (read-string (format "%s branches %s with branch: "
                       (if current-prefix-arg "UNmark" "Mark")
                       (pcase gited--op
                         (`merged "merged")
                         (_ "unmerged")))
               nil nil gited-current-branch))

(defun gited--mark-merged-branches-spec (branch prefix)
  (interactive "i\nP")
  (ignore branch prefix)
  (list
   (gited--mark-merged-or-unmerged-branches-spec 'merged)
   (if current-prefix-arg ?\s gited-marker-char)))

(defun gited--mark-unmerged-branches-spec (branch prefix)
  (interactive "i\nP")
  (ignore branch prefix)
  (list
   (gited--mark-merged-or-unmerged-branches-spec 'unmerged)
   (if current-prefix-arg ?\s gited-marker-char)))

(defun gited--mark-merged-or-unmerged-branches (branch op marker-char)
  (let* ((args
          (list "branch"
                (if (string= op "merged") "--merged" "--no-merged")
                branch))
         (branches (gited--get-branches-from-command args))
         (gited-marker-char (or marker-char gited-marker-char)))
    (gited-mark-if
     (and (not (eolp))
          (let ((fn (gited-get-branchname)))
            (and fn (member fn branches))))
     "matching branch")))

(defun gited-mark-merged-branches (branch &optional marker-char)
  "Mark all merged branches with BRANCH.
A prefix argument means to unmark them instead."
  (interactive
   (call-interactively #'gited--mark-merged-branches-spec))
  (gited--mark-merged-or-unmerged-branches branch "merged" marker-char))

(defun gited-mark-unmerged-branches (branch &optional marker-char)
  "Mark all unmerged branches with BRANCH.
A prefix argument means to unmark them instead."
  (interactive
   (call-interactively #'gited--mark-unmerged-branches-spec))
  (gited--mark-merged-or-unmerged-branches branch "unmerged" marker-char))

(defun gited-repeat-over-lines (arg function)
  ;; This version skips non-file lines.
  (let ((pos (make-marker)))
    (beginning-of-line)
    (while (and (> arg 0) (not (eobp)))
      (cl-decf arg)
      (beginning-of-line)
      (when (gited-at-header-line-p) (forward-line))
      (save-excursion
        (forward-line)
        (move-marker pos (1+ (point))))
      (save-excursion (funcall function))
      ;; Advance to the next line--actually, to the line that *was* next.
      ;; (If FUNCTION inserted some new lines in between, skip them.)
      (goto-char pos))
    (while (and (< arg 0) (not (bobp)))
      (cl-incf arg)
      (forward-line -1)
      (while (not (gited-at-header-line-p)) (forward-line -1))
      (beginning-of-line)
      (save-excursion (funcall function)))
    (move-marker pos nil)
    (ignore-errors (gited-move-to-branchname))
    (when (eobp)
      (forward-line -1)
      (ignore-errors (gited-move-to-branchname)))))

(defun gited-mark (arg mark &optional interactive)
  "Mark the branch at point in the Gited buffer with MARK.
If the region is active, mark all branches in the region.
Otherwise, with a prefix arg, mark branches on the next ARG lines.

Use \\[gited-unmark-all-branches] to remove all marks
and \\[gited-unmark] on a subdir to remove the marks in
this subdir."
  (interactive (list current-prefix-arg gited-marker-char t))
  (cond
   ;; Mark branches in the active region.
   ((and interactive (use-region-p))
    (save-excursion
      (let ((beg (region-beginning))
            (end (region-end)))
        (gited--mark-branches-in-region
         (progn (goto-char beg) (point-at-bol))
         (progn (goto-char end) (point-at-bol))
         mark))))
   ;; Mark the current (or next ARG) branches.
   (t
    (let ((inhibit-read-only t))
      (gited-repeat-over-lines
       (prefix-numeric-value arg)
       (lambda ()
         (when (ignore-errors (gited-get-branchname))
           (gited-insert-marker-char mark)
           (gited-fontify-marked-branch-name mark))))))))

(defun gited--mark-branches-in-region (start end mark)
  (when (> start end)
    (error "Wrong input values: start, end, <"))
  (goto-char start) ; assumed at beginning of line
  (while (< (point) end)
    (when (gited-get-branchname)
      (gited-insert-marker-char mark)
      (gited-fontify-marked-branch-name mark))
    (forward-line)))

(defun gited-unmark-backward ()
  "Unmark the branches in the region, or ARG branches."
  (interactive)
  (forward-line -1)
  (when (gited-get-branchname)
    (gited-insert-marker-char ?\s)
    (gited-fontify-marked-branch-name ?\s)
    (gited-move-to-branchname)))

(defun gited-unmark-all-branches (mark)
  "Remove all branches with flag MARK."
  (interactive "cRemove marks (RET means all):")
  (save-excursion
    (gited--goto-first-branch)
    (while (not (eobp))
      (let ((str (aref (tabulated-list-get-entry)
                       gited-mark-idx)))
        (when (and (gited-get-branchname)
                   (or (and (eq mark ?\r) (not (string= str " ")))
                       (string= (char-to-string mark) str)))
          (gited-insert-marker-char ?\s)
          (gited-fontify-marked-branch-name ?\s))
        (forward-line)))))

(defun gited-unmark-all-marks ()
  "Remove all marks from all marked branches in the Gited buffer."
  (interactive)
  (gited-unmark-all-branches ?\r))

(defun gited-move-to-branchname ()
  (interactive)
  (gited--move-to-column (1+ gited-branch-idx)))

;; Return point.
(defun gited-move-to-end-of-branchname ()
  (interactive)
  (gited--move-to-end-of-column (1+ gited-branch-idx)))

(defun gited-move-to-author ()
  (interactive)
  (gited--move-to-column (1+ gited-author-idx)))

(defun gited-move-to-end-of-author ()
  (interactive)
  (gited--move-to-end-of-column (1+ gited-author-idx)))

(defun gited-move-to-date ()
  (interactive)
  (gited--move-to-column (1+ gited-date-idx)))

(defun gited-move-to-end-of-date ()
  (interactive)
  (gited--move-to-end-of-column (1+ gited-date-idx)))


(defun gited-unmark (arg &optional interactive)
  "Unmark the branch at point in the Gited buffer.
If the region is active, unmark all branches in the region.
Otherwise, with a prefix arg, unmark branches on the next ARG lines.

If the region is active in Transient Mark mode, unmark all branches
in the active region."
  (interactive (list current-prefix-arg t))
  (gited-mark arg ?\s interactive))

(defun gited-number-marked ()
  "Return number of marked files."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((count 0))
      (while (not (eobp))
        (unless (looking-at "^[[:blank:]]")
          (cl-incf count))
        (forward-line))
      (prog1 count
        (if (zerop count)
            (message "No marked branches")
          (message "%d marked %s"
                   count
                   (if (> count 1)
                       "branches" "branch")))))))


;;; Mode map.
(defvar gited-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "t") 'gited-toggle-marks)
    (define-key map (kbd "(") 'gited-hide-details-mode)
    (define-key map (kbd "u") 'gited-unmark)
    (define-key map (kbd "=") 'gited-diff-with-branch)
    (define-key map (kbd "j") 'gited-goto-branch)
    (define-key map (kbd "DEL") 'gited-unmark-backward)
    (define-key map (kbd "U") 'gited-unmark-all-marks)
    (define-key map (kbd "M-DEL") 'gited-unmark-all-branches)
    (define-key map (kbd "x") 'gited-do-flagged-delete)
    ;; moving
    (define-key map (kbd "<") 'gited-goto-first-branch)
    (define-key map (kbd ">") 'gited-goto-last-branch)
    (define-key map (kbd "M-<") 'gited-goto-first-branch)
    (define-key map (kbd "M->") 'gited-goto-last-branch)
    (define-key map (kbd "n") 'gited-next-line)
    (define-key map (kbd "SPC") 'next-line)
    (define-key map (kbd "p") 'gited-prev-line)
    (define-key map (kbd "M-}") 'gited-next-marked-branch)
    (define-key map (kbd "M-{") 'gited-prev-marked-branch)
    ;; immediate operations
    (define-key map (kbd "a") 'gited-apply-patch)
    (define-key map (kbd "A") 'gited-add-patched-files)
    (define-key map (kbd "B") 'gited-bisect)
    (define-key map (kbd "C-c c") 'gited-commit)
    (define-key map (kbd "w") 'gited-copy-branchname-as-kill)
    (define-key map (kbd "e") 'gited-extract-patches)
    (define-key map (kbd "T") 'gited-sync-with-trunk)
    (define-key map (kbd "M") 'gited-merge-branch)
    (define-key map (kbd "c") 'gited-checkout-branch)
    (define-key map (kbd "v") 'gited-visit-branch-sources)
    (define-key map (kbd "f") 'gited-visit-branch-sources)
    (define-key map (kbd "s") 'gited-show-commit)
    (define-key map (kbd "S") 'gited-status)
    (define-key map (kbd "RET") 'gited-visit-branch-sources)
    (define-key map (kbd "g") 'gited-update)
    (define-key map "\C-x\C-g" 'gited-list-branches)
    (define-key map (kbd "k") 'gited-do-kill-lines)
    (define-key map (kbd "P") 'gited-pull)
    (define-key map (kbd "r") 'gited-reset-branch)
    (define-key map (kbd "* p") 'gited-set-branch-upstream)
    (define-key map (kbd "* <") 'gited-pull)
    (define-key map (kbd "* >") 'gited-push)
    (define-key map (kbd "o") 'gited-origin)
    (define-key map (kbd "l") 'gited-log)
    (define-key map (kbd "L") 'gited-log-last-n-commits)
    ;; marking banches
    (define-key map (kbd "m") 'gited-mark)
    (define-key map (kbd "% n") 'gited-mark-branches-regexp)
    (define-key map (kbd "% c") 'gited-mark-branches-containing-commit)
    (define-key map (kbd "% g") 'gited-mark-branches-containing-regexp)
    (define-key map (kbd "% m") 'gited-mark-merged-branches)
    (define-key map (kbd "% M") 'gited-mark-unmerged-branches)
    (define-key map (kbd "d") 'gited-flag-branch-deletion)
    ;; Git stash
    (define-key map (kbd "* s") 'gited-stash)
    (define-key map (kbd "* a") 'gited-stash-apply)
    (define-key map (kbd "* A") 'gited-stash-pop)
    (define-key map (kbd "* b") 'gited-stash-branch)
    (define-key map (kbd "* d") 'gited-stash-drop)
    ;; marked operations
    (define-key map (kbd "* N") 'gited-number-marked)
    (define-key map (kbd "R") 'gited-rename-branch)
    (define-key map (kbd "C") 'gited-copy-branch)
    (define-key map (kbd "D") 'gited-do-delete)
    (define-key map (kbd "?") 'gited-summary)
    map))



;;;###autoload
(defun gited-list-branches (&optional pattern other-window update)
  "List all branches with the time of its last commit."
  (interactive
   (progn
     (unless (gited-dir-under-Git-control-p)
       (error "No Git repository in current directory"))
     (let* ((opts '("local" "remote" "tags"))
            (patt (completing-read
                   "List (local, remote, tags): "
                   opts nil t nil nil "local")))
       (list patt current-prefix-arg nil))))
  (if (and (buffer-live-p gited-buffer)
           (not update)
           (or (not pattern)
               (equal pattern gited-ref-kind)))
      (switch-to-buffer gited-buffer)
    (unless (gited-dir-under-Git-control-p)
      (error "No Git repository in current directory"))
    (let ((buf (or (and (buffer-live-p gited-buffer) gited-buffer)
                   (setq gited-buffer (generate-new-buffer gited-buffer-name)))))
      (unless (equal pattern gited-ref-kind)
        (setq gited-ref-kind pattern))
      (if other-window
          (switch-to-buffer-other-window buf)
        (switch-to-buffer buf))
      (or gited-mode (gited-mode))
      ;; Set `gited-toplevel-dir' if not set yet.
      (unless gited-toplevel-dir
        (setq gited-toplevel-dir
              (with-temp-buffer
                (gited-git-command '("rev-parse" "--show-toplevel")
                                   (current-buffer))
                (file-name-as-directory
                 (buffer-substring 1 (1- (point-max)))))))
      (setq tabulated-list-use-header-line gited-use-header-line
            gited-buffer buf
            gited-ref-kind pattern
            gited-current-branch (gited-current-branch)
            tabulated-list-printer #'gited-print-entry)
      ;; Ignore dired-hide-details-* value of invisible text property by default.
      (when (eq buffer-invisibility-spec t)
        (setq buffer-invisibility-spec (list t)))
      (gited-tabulated-list-entries)
      (tabulated-list-print)
      (gited-goto-branch gited-current-branch)
      (gited-fontify-current-branch)
      (unless gited--hide-details-set
        (or gited-verbose (gited-hide-details-mode 1))
        (setq gited--hide-details-set t)))))

(defun gited-print-entry (id cols)
  "Insert a Gited entry at point.
ID is a Lisp object identifying the entry to print, and COLS is a vector
of column descriptors."
  (let ((beg   (point))
        (x     (max tabulated-list-padding 0))
        (ncols (length tabulated-list-format))
        (inhibit-read-only t))
    (if (> tabulated-list-padding 0)
        (insert (make-string x ?\s)))
    (dotimes (n ncols)
      (let ((pos (point)))
        (setq x (tabulated-list-print-col n (aref cols n) x))
        (cond
         ((= n gited-author-idx)
          (add-text-properties
           pos (point)
           `(invisible gited-hide-details-author
                       font-lock-face ,gited-author-face)))
         ((= n gited-date-idx)
          (add-text-properties
           pos (point)
           `(invisible gited-hide-details-date
                       font-lock-face ,gited-date-time-face)))
         ((= n gited-branch-idx)
          (put-text-property
           pos (point)
           'font-lock-face gited-branch-name-face))
         ((= n gited-commit-idx)
          (put-text-property
           pos (point)
           'font-lock-face gited-commit-msg-face))
         (t nil))
        ))
    (insert ?\n)
    ;; Ever so slightly faster than calling `put-text-property' twice.
    (add-text-properties
     beg (point)
     `(tabulated-list-id ,id tabulated-list-entry ,cols))))

(defun gited-tabulated-list-entries ()
  (let ((alist (gited--fill-branch-alist gited-ref-kind))
        mark author date branch commit id values result)
    (while alist
      (setq id (caar alist)
            values (cdr (car alist))
            mark (nth 0 values)
            author (nth 3 values)
            date (nth 1 values)
            branch (nth 2 values)
            commit (nth 4 values)
            alist (cdr alist))
      (push (list id (vector mark author date branch commit)) result))
    (setq tabulated-list-entries (nreverse result))
    (tabulated-list-init-header)))

;;; Define minor mode.
(define-derived-mode gited-mode tabulated-list-mode "Gited"
  "Toggle gited-mode.
Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.

Mode to edit Git branches as Dired."
  (unless (gited-buffer-p)
    (error "Gited mode cannot be enabled in this buffer"))
  (gited--list-format-init)
  (setq tabulated-list-format gited-list-format)
  (add-hook 'tabulated-list-revert-hook 'gited-tabulated-list-entries nil t)
  (setq tabulated-list-sort-key '("Date")))


(provide 'gited)
;;; gited.el ends here
