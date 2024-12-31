;;; askk-jisyo.el --- DDSKK jisyo support for ASKK -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2024 synrbb
;; SPDX-License-Identifier: Apache-2.0 OR GPL-3.0-or-later

;;; Code:

(defgroup askk-jisyo nil
  "Options for askk-jisyo.el."
  :group 'askk)

(defcustom askk-jisyo-directory user-emacs-directory
  "SKK 辞書ディレクトリ。"
  :type 'directory)

(defcustom askk-jisyo-file (expand-file-name (if (eq askk-jisyo-directory
                                                     user-emacs-directory)
                                                 "askk-jisyo" "jisyo")
                                             askk-jisyo-directory)
  "DDSKK の個人辞書形式のファイル。"
  :type 'file)

(defcustom askk-jisyo-lookup-file (expand-file-name "SKK-JISYO.L"
                                                    askk-jisyo-directory)
  "Default file name for `askk-jisyo-lookup'."
  :type 'file)

;;;###autoload
(defun askk-jisyo-lookup (headword &optional _ filename)
  (unless filename
    (setq filename askk-jisyo-lookup-file))

  (with-current-buffer
      (get-buffer-create (concat " *askk-jisyo:" filename "*") t)
    (when (= (buffer-size) 0)
      (insert-file-contents filename))
    (goto-char (point-min))

    (while (eq (char-after) ?\;)
      (forward-line))

    (when (re-search-forward
           (concat "^" (regexp-quote headword) " \\(/.+/\\)$") nil t)
      (askk-jisyo--parse-candidates (match-string-no-properties 1)))))

(defun askk-jisyo-load (filename)
  (with-temp-buffer
    (insert-file-contents filename)
    (askk-jisyo--read)))

(defun askk-jisyo--read ()
  (let (result)
    (goto-char (point-min))
    (while (not (eobp))
      (if (= (char-after) ?\;)
          (forward-line)
        (push (askk-jisyo--read-line) result)))
    (nreverse result)))

(defun askk-jisyo--read-line ()
  (let ((beg (point))
        len headword candidates okurigana-alist)
    (setq len (skip-chars-forward "^ "))
    (setq headword (buffer-substring-no-properties beg
                                                   (setq beg (+ beg len))))
    ;; skip space and first /
    (forward-char 2)

    (setq beg (+ beg 2))
    (setq len (skip-chars-forward "^;/"))
    (while (/= (char-after) ?\n)
      (cond
       ((= (char-after beg) ?\[)
        (push (cons (buffer-substring-no-properties (1+ beg)
                                                    (setq beg (+ beg len)))
                    nil)
              okurigana-alist))
       ((and okurigana-alist
             (= (char-after beg) ?\])
             (= len 1))
        (setq beg (+ beg len)))
       (t
        (push (cons (buffer-substring-no-properties beg
                                                    (setq beg (+ beg len)))
                    (when (= (char-after) ?\;)
                      (setq len (skip-chars-forward "^/"))
                      (buffer-substring-no-properties (1+ beg)
                                                      (setq beg (+ beg len)))))
              (if okurigana-alist
                  (cdr (car okurigana-alist))
                candidates))))
      ;; skip /
      (forward-char)
      (setq beg (1+ beg))
      (setq len (skip-chars-forward "^;/\n")))

    ;; skip \n
    (forward-char)

    (list headword
          (nreverse candidates)
          (let (ret)
            (dolist (x okurigana-alist)
              (push (cons (car x) (nreverse (cdr x))) ret))
            ret))))

(defun askk-jisyo--parse-candidates (str)
  (with-temp-buffer
    (save-excursion
      (insert "x " str "\n"))
    (nth 1 (askk-jisyo--read-line))))

(defun askk-jisyo--encode (alist)
  (let ((with (list ";; okuri-ari entries."))
        (without (list ";; okuri-nasi entries.")))
    (dolist (entry alist)
      (push (askk-jisyo--encode-entry entry) (if (nth 2 entry) with without)))
    (mapconcat #'identity
               (nconc (nreverse with) (nreverse without) '(""))
               "\n")))

(defun askk-jisyo--encode-entry (entry)
  (pcase-let ((`(,headword ,candidates ,okurigana-alist) entry))
    (concat headword
            " /"
            (askk-jisyo--encode-candidates candidates)
            "/"
            (and okurigana-alist
                 (concat (askk-jisyo--encode-okurigana-alist okurigana-alist)
                         "/")))))

(defun askk-jisyo--encode-okurigana-alist (alist)
  (let (ret)
    (pcase-dolist (`(,okurigana . ,candidates) alist)
      (push (concat "["
                    okurigana
                    "/"
                    (askk-jisyo--encode-candidates candidates)
                    "/]")
            ret))
    (mapconcat #'identity (nreverse ret) "/")))

(defun askk-jisyo--encode-candidates (candidates)
  (let (ret)
    (pcase-dolist (`(,str . ,annotation) candidates)
      (push (if annotation (concat str ";" annotation) str) ret))
    (mapconcat #'identity (nreverse ret) "/")))

(provide 'askk-jisyo)
;;; askk-jisyo.el ends here
