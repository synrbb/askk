;;; askk-server.el --- SKK server support for ASKK -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2024 synrbb
;; SPDX-License-Identifier: Apache-2.0 OR GPL-3.0-or-later

;;; Code:

(require 'askk-jisyo)

(defgroup askk-server nil
  "Options for askk-server.el."
  :group 'askk)

(defcustom askk-server-host "127.0.0.1"
  "Default host name for `askk-server-lookup'."
  :type 'string)

(defcustom askk-server-port 1178
  "Default port for `askk-server-lookup'."
  :type 'natnum)

(defcustom askk-server-coding-system nil
  "Default coding system for `askk-server-lookup'."
  :type 'coding-system)

(defvar askk-server--process nil)

;;;###autoload
(defun askk-server-lookup (headword &optional _ host port coding-system)
  (unless (and askk-server--process
               (eq (process-status askk-server--process) 'open))
    (setq askk-server--process
          (open-network-stream
           "askk-server"
           " *askk-server*"
           (or host askk-server-host)
           (or port askk-server-port)
           :coding (and-let* ((coding-system (or coding-system
                                                 askk-server-coding-system)))
                     (cons 'utf-8 coding-system))
           :noquery t)))

  (with-current-buffer (process-buffer askk-server--process)
    (delete-region (point-min) (point-max))

    (process-send-string askk-server--process (concat "1" headword " "))
    (while (and (accept-process-output askk-server--process)
                (not (eq (char-before (point-max)) ?\n))))

    (if (eq (char-before (point-max)) ?\n)
        (let ((code (char-after (point-min))))
          (cond
           ((= code ?1)
            (askk-jisyo--parse-candidates
             (buffer-substring-no-properties (1+ (point-min))
                                             (1- (point-max)))))
           ((/= code ?4)
            (error "Unknown response: %s" (buffer-string)))))
      (askk-server-cleanup)
      (error "Server closed connection"))))

;;;###autoload
(defun askk-server-cleanup ()
  (when (and askk-server--process
             (eq (process-status askk-server--process) 'open))
    (process-send-string askk-server--process "0")
    (delete-process askk-server--process))
  (setq askk-server--process nil))

(provide 'askk-server)
;;; askk-server.el ends here
