;;; askk-cdb.el --- CDB file lookup for ASKK -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2024-2025 askk contributors
;; SPDX-License-Identifier: Apache-2.0 OR GPL-3.0-or-later

;;; Code:

(require 'askk-jisyo)

(defgroup askk-cdb nil
  "Options for askk-cdb.el."
  :group 'askk)

(defcustom askk-cdb-lookup-file (expand-file-name "SKK-JISYO.L.cdb"
                                                  askk-jisyo-directory)
  "Default file name for `askk-cdb-lookup'."
  :type 'file)

;;;###autoload
(defun askk-cdb-lookup (headword &optional _ filename)
  (unless filename
    (setq filename askk-cdb-lookup-file))
  (unless (file-name-absolute-p filename)
    (setq filename (expand-file-name filename askk-jisyo-directory)))

  (and-let* ((key (encode-coding-string headword 'utf-8))
             (val (askk-cdb--get filename key)))
    (askk-jisyo--parse-candidates (decode-coding-string val 'utf-8))))

(defun askk-cdb--get (filename key)
  (with-current-buffer
      (get-buffer-create (concat " *askk-cdb:" filename "*") t)
    (when (= (buffer-size) 0)
      (set-buffer-multibyte nil)
      (insert-file-contents-literally filename))

    (let* ((pos (point-min))
           (klen (length key))
           (h (askk-cdb--hash key))
           (offset (ash (logand h #xff) 3))
           (nslots (askk-cdb--unpack-uint32 (+ offset 4)))
           boffset doffset vlen)
      (catch 'loop
        (when (= nslots 0)
          (throw 'loop nil))
        (setq boffset (askk-cdb--unpack-uint32 offset))
        (dotimes (i nslots)
          (setq offset (+ boffset (ash (% (+ (ash h -8) i) nslots) 3)))
          (setq doffset (askk-cdb--unpack-uint32 (+ offset 4)))
          (when (= doffset 0)
            (throw 'loop nil))
          (when (and (= (askk-cdb--unpack-uint32 offset) h)
                     (= (askk-cdb--unpack-uint32 doffset) klen)
                     (string= (buffer-substring-no-properties
                               (+ pos doffset 8)
                               (+ pos doffset 8 klen))
                              key))
            (setq vlen (askk-cdb--unpack-uint32 (+ doffset 4)))
            (throw 'loop
                   (buffer-substring-no-properties
                    (+ pos doffset 8 klen)
                    (+ pos doffset 8 klen vlen)))))))))

(defun askk-cdb--hash (key)
  (let ((h 5381))
    (seq-doseq (c key)
      (setq h (logxor (+ (ash h 5) h) c)))
    (logand h #xffffffff)))

(defun askk-cdb--unpack-uint32 (offset)
  (let ((pos (point-min)))
    (logior (char-after (+ pos offset))
            (ash (char-after (+ pos offset 1)) 8)
            (ash (char-after (+ pos offset 2)) 16)
            (ash (char-after (+ pos offset 3)) 24))))

(provide 'askk-cdb)
;;; askk-cdb.el ends here
