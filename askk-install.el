;;; askk-install.el --- Installation support for askk -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2024-2025 askk contributors
;; SPDX-License-Identifier: Apache-2.0 OR GPL-3.0-or-later

;;; Code:

(require 'url-handlers)
(require 'askk-jisyo)

(defvar askk-install--jisyo-url
  "https://raw.githubusercontent.com/skk-dev/dict/master/SKK-JISYO.L")

(defun askk-install--to-utf8 ()
  (goto-char (point-min))
  (when (looking-at "^;+.*[ \t;]+coding[ \t]*:[ \t]*\\([^ \t\n;]+\\).*$")
    (let ((coding-system (intern (match-string 1))))
      (delete-region (match-beginning 0) (match-end 0))
      (insert ";;; -*- mode: fundamental; coding: utf-8; -*-")
      (forward-line)
      (decode-coding-region (point) (point-max) coding-system))))

(defun askk-install--make-callback (&rest funcs)
  (lambda (status filename)
    (when-let* ((err (plist-get status :error)))
      (signal (car err) (cdr err)))

    (let ((buffer (current-buffer))
          (dir (file-name-directory filename)))
      (unless (file-directory-p dir)
        (make-directory dir t))
      (with-temp-file filename
        (set-buffer-multibyte nil)
        (url-insert buffer)
        (kill-buffer buffer)
        (mapc #'funcall funcs)))
    (message "Done")))

;;;###autoload
(defun askk-install-jisyo (url filename)
  (interactive
   (let* ((url (read-string "URL: " askk-install--jisyo-url))
          (filename (read-file-name "File: "
                                    askk-jisyo-directory
                                    nil
                                    nil
                                    (url-file-nondirectory url))))
     (list url filename)))
  (url-retrieve url
                (askk-install--make-callback #'askk-install--to-utf8)
                (list filename)))

(defun askk-install--pack-uint32 (n)
  (string
   (logand n #xff)
   (logand (ash n -8) #xff)
   (logand (ash n -16) #xff)
   (logand (ash n -24) #xff)))

(defun askk-install--to-cdb ()
  (goto-char (point-min))
  (while (= (char-after) ?\;)
    (forward-line))
  (delete-region (point-min) (point))

  (let ((buckets (make-vector 256 nil))
        (offset 2048)
        (beg (point))
        mid end
        c h klen vlen)
    (while (not (eobp))
      ;; SKK 辞書の仕様としては ; から始まる見出し語も有効だから
      ;; 本当は ;; okuri-nasi entries. との比較をしないといけない。
      (when (= (char-after) ?\;)
        (forward-line)
        (delete-region beg (point)))

      (setq h 5381)
      (while (/= (setq c (char-after)) ?\s)
        (setq h (logxor (+ (ash h 5) h) c))
        (forward-char))
      (setq h (logand h #xffffffff))

      (setq klen (- (setq mid (point)) beg))
      (delete-char 1)

      (skip-chars-forward "^\n")
      (setq vlen (- (setq end (point)) mid))
      (delete-char 1)

      (goto-char beg)
      (insert (askk-install--pack-uint32 klen)
              (askk-install--pack-uint32 vlen))
      (setq beg (goto-char (+ end 8)))

      (push (cons h offset) (aref buckets (logand h #xff)))
      (setq offset (+ offset 8 klen vlen)))

    (goto-char (point-min))
    (seq-doseq (bucket buckets)
      ;; tinycdb 0.81 と同じバイナリを生成するために 2 倍する。
      ;; cdb の仕様としては 2 倍する必要はないはず。
      (let ((nslots (ash (length bucket) 1)))
        (insert (askk-install--pack-uint32 offset)
                (askk-install--pack-uint32 nslots))
        (setq offset (+ offset (ash nslots 3)))))

    (seq-doseq (bucket buckets)
      (let ((nslots (ash (length bucket) 1))
            i pos)
        (setq beg (goto-char (point-max)))
        (insert (make-string (ash nslots 3) 0))
        ;; tinycdb 0.81 と同じバイナリを生成するために nreverse する。
        ;; cdb の仕様としては nreverse する必要はないはず。
        (dolist (slot (nreverse bucket))
          (setq i (% (ash (car slot) -8) nslots))
          (while (progn
                   (setq pos (+ beg (ash i 3)))
                   (or (> (char-after (+ pos 4)) 0)
                       (> (char-after (+ pos 5)) 0)
                       (> (char-after (+ pos 6)) 0)
                       (> (char-after (+ pos 7)) 0)))
            (setq i (% (1+ i) nslots)))
          (goto-char pos)
          (insert (askk-install--pack-uint32 (car slot))
                  (askk-install--pack-uint32 (cdr slot)))
          (delete-char 8))))))

;;;###autoload
(defun askk-install-cdb-jisyo (url filename)
  (interactive
   (let* ((url (read-string "SKK-JISYO URL: " askk-install--jisyo-url))
          (filename (read-file-name "CDB File: "
                                    askk-jisyo-directory
                                    nil
                                    nil
                                    (concat (url-file-nondirectory url)
                                            ".cdb"))))
     (list url filename)))
  (url-retrieve url
                (askk-install--make-callback #'askk-install--to-utf8
                                             #'askk-install--to-cdb)
                (list filename)))

(provide 'askk-install)
;;; askk-install.el ends here
