;;; askk-user-dict.el --- User dictionary for ASKK -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2024 synrbb
;; SPDX-License-Identifier: Apache-2.0 OR GPL-3.0-or-later

;;; Code:

(require 'askk-jisyo)

(defvar askk-user-dict--alist nil)

;;;###autoload
(defun askk-user-dict-lookup (headword okurigana)
  (when-let* ((entry (assoc headword askk-user-dict--alist)))
    (if okurigana
        (delete-dups
         (append
          (cdr (assoc okurigana (nth 2 entry)))
          (copy-sequence (nth 1 entry))))
      (copy-sequence (nth 1 entry)))))

(defun askk-user-dict--prefix-keys (prefix)
  "PREFIX から始まる見出し語（送り仮名なし）のリストを返す。"
  (let (result)
    (pcase-dolist (`(,headword ,_  ,o-alist) askk-user-dict--alist)
      (when (and (null o-alist) (string-prefix-p prefix headword))
        (push headword result)))
    (nreverse result)))

(defun askk-user-dict--add-entry (headword okurigana candidate)
  (let ((entry (assoc headword askk-user-dict--alist))
        o-entry)
    (if entry
        (setq askk-user-dict--alist (delq entry askk-user-dict--alist))
      (setq entry (list headword nil nil)))
    (setf (nth 1 entry) (cons candidate (delete candidate (nth 1 entry))))
    (push entry askk-user-dict--alist)
    (when okurigana
      (setq o-entry (assoc okurigana (nth 2 entry)))
      (if o-entry
          (setf (cdr o-entry)
                (cons candidate (delete candidate (cdr o-entry))))
        (push (cons okurigana (list candidate)) (nth 2 entry))))))

(defun askk-user-dict--delete-entry (headword okurigana candidate)
  (when-let* ((entry (assoc headword askk-user-dict--alist))
              (delp t))
    (when-let* ((o-entry (and okurigana (assoc okurigana (nth 2 entry)))))
      (unless (setf (cdr o-entry) (delete candidate (cdr o-entry)))
        (setf (nth 2 entry) (delete o-entry (nth 2 entry))))
      (catch 'loop
        (dolist (o-entry (nth 2 entry))
          (when (member candidate (cdr o-entry))
            (throw 'loop (setq delp nil))))))
    (when delp
      (unless (setf (nth 1 entry) (delete candidate (nth 1 entry)))
        (setq askk-user-dict--alist (delete entry askk-user-dict--alist))))))

(defun askk-user-dict-load ()
  (or askk-user-dict--alist
      (setq askk-user-dict--alist
            (and (file-readable-p askk-jisyo-file)
                 (askk-jisyo-load askk-jisyo-file)))))

(defun askk-user-dict-save ()
  (let* ((new (askk-jisyo--encode askk-user-dict--alist))
         (new-size (string-bytes new))
         (old-size (or (file-attribute-size
                        (file-attributes askk-jisyo-file))
                       0)))
    (when (or (>= new-size old-size)
              (and (yes-or-no-p (concat "Save small "
                                        askk-jisyo-file "?"))
                   (prog1 t
                     (copy-file askk-jisyo-file
                                (concat askk-jisyo-file ".bak")
                                t t))))
      (let ((dir (file-name-directory askk-jisyo-file)))
        (unless (file-directory-p dir)
          (make-directory dir t)))
      (with-temp-file askk-jisyo-file
        (insert new)))
    ;; for kill-emacs-query-functions
    t))

(provide 'askk-user-dict)
;;; askk-user-dict.el ends here
