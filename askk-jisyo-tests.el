;;; askk-jisyo-tests.el --- Tests for askk-jisyo.el -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2024-2025 askk contributors
;; SPDX-License-Identifier: Apache-2.0 OR GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'askk-jisyo)

(defmacro askk-jisyo-tests-with-temp-buffer (str &rest body)
  (declare (indent defun))
  `(with-temp-buffer
     (insert ,str)
     (goto-char (point-min))
     ,@body))

(ert-deftest askk-jisyo-tests-read ()
  (askk-jisyo-tests-with-temp-buffer "\
;; okuri-ari entries.
あi /合/[い/合/]/
;; okuri-nasi entries.
あ /亜/
"
    (goto-char (point-max))
    (should (equal (askk-jisyo--read)
                   '(("あi" (("合")) (("い" ("合"))))
                     ("あ" (("亜")) nil))))))

(ert-deftest askk-jisyo-tests-read-empty ()
  (askk-jisyo-tests-with-temp-buffer "\
;; okuri-ari entries.
;; okuri-nasi entries.
"
    (should-not (askk-jisyo--read))))

(ert-deftest askk-jisyo-tests-read-line ()
  (askk-jisyo-tests-with-temp-buffer "あ /阿/亜/\n"
    (should (equal (askk-jisyo--read-line)
                   '("あ" (("阿") ("亜")) nil)))
    (should (eobp))))

(ert-deftest askk-jisyo-tests-read-line-with-annotation ()
  (askk-jisyo-tests-with-temp-buffer "あ /阿;い/亜;う/\n"
    (should (equal (askk-jisyo--read-line)
                   '("あ" (("阿" . "い") ("亜" . "う")) nil)))))

(ert-deftest askk-jisyo-tests-read-line-with-bracket-annotation ()
  (askk-jisyo-tests-with-temp-buffer "あ /亜;[い]う/\n"
    (should (equal (askk-jisyo--read-line)
                   '("あ" (("亜" . "[い]う")) nil)))))

(ert-deftest askk-jisyo-tests-read-line-with-okurigana ()
  (askk-jisyo-tests-with-temp-buffer "あi /合/[い/合/]/\n"
    (should (equal (askk-jisyo--read-line)
                   '("あi" (("合")) (("い" ("合"))))))))

(ert-deftest askk-jisyo-tests-read-line-complex ()
  (askk-jisyo-tests-with-temp-buffer "\
おs /押;あ/惜;い/[す/押;あ/]/[し/押;あ/惜;い/]/\n"
    (should (equal (askk-jisyo--read-line)
                   '("おs"
                     (("押" . "あ") ("惜" . "い"))
                     (("す" ("押" . "あ"))
                      ("し" ("押" . "あ") ("惜" . "い"))))))))

(ert-deftest askk-jisyo-tests-parse-candidates ()
  (should (equal (askk-jisyo--parse-candidates "/阿;い/亜;う/")
                 '(("阿" . "い") ("亜" . "う")))))

(ert-deftest askk-jisyo-tests-encode ()
  (should (equal (askk-jisyo--encode '(("あi" (("合")) (("い" ("合"))))
                                       ("あ" (("亜")) nil)))
                 "\
;; okuri-ari entries.
あi /合/[い/合/]/
;; okuri-nasi entries.
あ /亜/
")))

(ert-deftest askk-jisyo-tests-encode-empty ()
  (should (equal (askk-jisyo--encode nil)
                 "\
;; okuri-ari entries.
;; okuri-nasi entries.
")))

(ert-deftest askk-jisyo-tests-encode-entry ()
  (should (equal (askk-jisyo--encode-entry '("あ" (("阿") ("亜")) nil))
                 "あ /阿/亜/")))

(ert-deftest askk-jisyo-tests-encode-entry-complex ()
  (should (equal (askk-jisyo--encode-entry
                  '("おs"
                    (("押" . "あ") ("惜" . "い"))
                    (("す" ("押" . "あ"))
                     ("し" ("押" . "あ") ("惜" . "い")))))
                 "おs /押;あ/惜;い/[す/押;あ/]/[し/押;あ/惜;い/]/")))

(ert-deftest askk-jisyo-tests-encode-okurigana ()
  (should (equal (askk-jisyo--encode-okurigana-alist
                  '(("す" ("押" . "あ"))
                    ("し" ("押" . "あ") ("惜" . "い"))))
                 "[す/押;あ/]/[し/押;あ/惜;い/]")))

(ert-deftest askk-jisyo-tests-encode-candidates ()
  (should (equal (askk-jisyo--encode-candidates '(("阿") ("亜")))
                 "阿/亜")))

(ert-deftest askk-jisyo-tests-encode-candidates-with-annotation ()
  (should (equal (askk-jisyo--encode-candidates '(("阿" . "い") ("亜" . "う")))
                 "阿;い/亜;う")))

(provide 'askk-jisyo-tests)
;;; askk-jisyo-tests.el ends here
