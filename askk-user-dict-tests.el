;;; askk-user-dict-tests.el --- Tests for askk-user-dict.el -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2024 synrbb
;; SPDX-License-Identifier: Apache-2.0 OR GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'askk-user-dict)

(ert-deftest askk-user-dict-test-lookup ()
  (let (askk-user-dict--alist)
    (askk-user-dict--add-entry "あい" nil '("愛"))
    (askk-user-dict--add-entry "あい" nil '("亜衣"))
    (should (equal (askk-user-dict-lookup "あい" nil)
                   '(("亜衣") ("愛"))))))

(ert-deftest askk-user-dict-test-lookup-not-found ()
  (let (askk-user-dict--alist)
    (should-not (askk-user-dict-lookup "あい" nil))))

(ert-deftest askk-user-dict-test-lookup-okurigana ()
  (let (askk-user-dict--alist)
    (askk-user-dict--add-entry "あk" "か" '("開"))
    (askk-user-dict--add-entry "あk" "き" '("飽"))
    (askk-user-dict--add-entry "あk" "か" '("空"))
    (should (equal (askk-user-dict-lookup "あk" "か")
                   '(("空") ("開") ("飽"))))))

(ert-deftest askk-user-dict-test-add ()
  (let (askk-user-dict--alist)
    (askk-user-dict--add-entry "あい" nil '("愛"))
    (should (equal askk-user-dict--alist '(("あい" (("愛")) nil))))))

(ert-deftest askk-user-dict-test-add-okurigana ()
  (let (askk-user-dict--alist)
    (askk-user-dict--add-entry "あi" "い" '("合"))
    (should (equal askk-user-dict--alist
                   '(("あi" (("合")) (("い" ("合")))))))))

(ert-deftest askk-user-dict-test-add-order ()
  (let (askk-user-dict--alist)
    (askk-user-dict--add-entry "あい" nil '("愛"))
    (askk-user-dict--add-entry "あい" nil '("亜衣"))
    (should (equal (assoc "あい" askk-user-dict--alist)
                   '("あい" (("亜衣") ("愛")) nil)))
    (askk-user-dict--add-entry "あい" nil '("愛"))
    (should (equal (assoc "あい" askk-user-dict--alist)
                   '("あい" (("愛") ("亜衣")) nil)))))

(ert-deftest askk-user-dict-test-add-order-okurigana ()
  (let (askk-user-dict--alist)
    (askk-user-dict--add-entry "あi" "い" '("合"))
    (askk-user-dict--add-entry "あi" "い" '("会"))
    (should (equal (assoc "あi" askk-user-dict--alist)
                   '("あi" (("会") ("合")) (("い" ("会") ("合"))))))
    (askk-user-dict--add-entry "あi" "い" '("合"))
    (should (equal (assoc "あi" askk-user-dict--alist)
                   '("あi" (("合") ("会")) (("い" ("合") ("会"))))))))

(ert-deftest askk-user-dict-test-add-multi ()
  (let (askk-user-dict--alist)
    (askk-user-dict--add-entry "あい" nil '("愛"))
    (askk-user-dict--add-entry "いえ" nil '("家"))
    (should (equal askk-user-dict--alist '(("いえ" (("家")) nil)
                                           ("あい" (("愛")) nil))))))

(ert-deftest askk-user-dict-test-add-multi-okurigana ()
  (let (askk-user-dict--alist)
    (askk-user-dict--add-entry "あk" "か" '("開"))
    (askk-user-dict--add-entry "あk" "き" '("飽"))
    (let ((entry (assoc "あk" askk-user-dict--alist)))
      (should (equal (nth 1 entry) '(("飽") ("開"))))
      (should (equal (nth 2 entry) '(("き" ("飽"))
                                     ("か" ("開")))))
      (askk-user-dict--add-entry "あk" "か" '("空"))
      (should (equal (nth 1 entry) '(("空") ("飽") ("開"))))
      (should (equal (nth 2 entry) '(("き" ("飽"))
                                     ("か" ("空") ("開"))))))))

(ert-deftest askk-user-dict-test-delete-unknown-not-error ()
  (let (askk-user-dict--alist)
    (askk-user-dict--add-entry "あい" nil '("愛"))
    (askk-user-dict--add-entry "あk" "き" '("飽"))
    (askk-user-dict--delete-entry "あ" nil '("亜"))
    (askk-user-dict--delete-entry "あい" nil '("亜衣"))
    (askk-user-dict--delete-entry "あk" "く" '("開"))))

(ert-deftest askk-user-dict-test-delete-all ()
  (let (askk-user-dict--alist)
    (askk-user-dict--add-entry "あい" nil '("愛"))
    (askk-user-dict--add-entry "いえ" nil '("家"))
    (askk-user-dict--delete-entry "あい" nil '("愛"))
    (should (equal askk-user-dict--alist '(("いえ" (("家")) nil))))))

(ert-deftest askk-user-dict-test-delete-sub ()
  (let (askk-user-dict--alist)
    (askk-user-dict--add-entry "あい" nil '("愛"))
    (askk-user-dict--add-entry "あい" nil '("亜衣"))
    (askk-user-dict--delete-entry "あい" nil '("愛"))
    (should (equal askk-user-dict--alist '(("あい" (("亜衣")) nil))))))

(ert-deftest askk-user-dict-test-delete-all-sub-cands ()
  (let (askk-user-dict--alist)
    (askk-user-dict--add-entry "あk" "か" '("開"))
    (askk-user-dict--add-entry "あk" "き" '("飽"))
    (askk-user-dict--delete-entry "あk" "き" '("飽"))
    (let ((entry (assoc "あk" askk-user-dict--alist)))
      (should (equal (nth 1 entry) '(("開"))))
      (should (equal (nth 2 entry) '(("か" ("開"))))))))

(ert-deftest askk-user-dict-test-delete-part-sub-cands ()
  (let (askk-user-dict--alist)
    (askk-user-dict--add-entry "あk" "か" '("開"))
    (askk-user-dict--add-entry "あk" "き" '("飽"))
    (askk-user-dict--add-entry "あk" "き" '("空"))
    (askk-user-dict--delete-entry "あk" "き" '("飽"))
    (let ((entry (assoc "あk" askk-user-dict--alist)))
      (should (equal (nth 1 entry) '(("空") ("開"))))
      (should (equal (nth 2 entry) '(("き" ("空"))
                                     ("か" ("開"))))))))

(ert-deftest askk-user-dict-test-delete-preserve-cands ()
  (let (askk-user-dict--alist)
    (askk-user-dict--add-entry "あk" "か" '("開"))
    (askk-user-dict--add-entry "あk" "か" '("空"))
    (askk-user-dict--add-entry "あk" "き" '("空"))
    (askk-user-dict--delete-entry "あk" "か" '("空"))
    (let ((entry (assoc "あk" askk-user-dict--alist)))
      (should (equal (nth 1 entry) '(("空") ("開"))))
      (should (equal (nth 2 entry) '(("き" ("空"))
                                     ("か" ("開"))))))))

(ert-deftest askk-user-dict-test-delete-headword ()
  (let (askk-user-dict--alist)
    (askk-user-dict--add-entry "あk" "か" '("空"))
    (askk-user-dict--delete-entry "あk" "か" '("空"))
    (should-not (assoc "あk" askk-user-dict--alist))))

(ert-deftest askk-user-dict-test-prefix-keys ()
  (let ((askk-user-dict--alist '(("あお" (("青")) nil)
                                 ("いえ" (("家")) nil)
                                 ("あい" (("愛")) nil)
                                 ("あk" (("空")) (("き") ("空"))))))
    (should (equal (askk-user-dict--prefix-keys "あ")
                   '("あお" "あい")))))

(provide 'askk-user-dict-tests)
;;; askk-user-dict-tests.el ends here
