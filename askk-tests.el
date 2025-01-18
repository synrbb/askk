;;; askk-tests.el --- Tests for askk.el -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2024-2025 askk contributors
;; SPDX-License-Identifier: Apache-2.0 OR GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'askk)

(ert-deftest askk-tests-trans-tree ()
  (let ((tree (askk-trans--node-make)))
    (askk-trans--node-insert tree "a" '("あ"))
    (should (equal (assq ?a (nth 2 tree))
                   '(?a ("あ") nil)))
    (askk-trans--node-insert tree "ka" '("か"))
    (should (equal (assq ?k (nth 2 tree))
                   '(?k nil ((?a ("か") nil)))))
    (askk-trans--node-insert tree "n" '("ん"))
    (askk-trans--node-insert tree "nn" '("ん"))
    (should (equal (assq ?n (nth 2 tree))
                   '(?n ("ん") ((?n ("ん") nil)))))
    (should (equal tree
                   '(nil nil ((?n ("ん") ((?n ("ん") nil)))
                              (?k nil ((?a ("か") nil)))
                              (?a ("あ") nil)))))))

(ert-deftest askk-tests-trans-customization ()
  (let* ((askk-transliteration-alist '(("a" "ああ")
                                       ("a" "いい")))
         (tree (askk-trans--make)))
    (should (equal (assq ?a (askk-trans--node-children tree))
                   '(?a ("ああ") nil)))))

(ert-deftest askk-tests-output-commit ()
  (let (askk-kana--default-string
        askk-kana--additional-string
        askk-kana--additional-flag)
    (askk--output-commit "あい")
    (should (equal askk-kana--default-string "あい"))
    (askk--output-commit ?う)
    (should (equal askk-kana--default-string "あいう"))
    (askk-katakana-mode)
    (askk--output-commit "え")
    (should (equal askk-kana--default-string "あいうエ"))
    (askk--output-commit ?お)
    (should (equal askk-kana--default-string "あいうエオ"))
    (setq askk-kana--additional-flag t)
    (askk--output-commit ?ん)
    (should (equal askk-kana--default-string "あいうエオ"))
    (should (equal askk-kana--additional-string "ン"))))

(defmacro askk-tests-with-transliteration (alist &rest body)
  (declare (indent defun))
  `(let ((askk-transliteration-base-alist ,alist)
         askk-kana--default-string
         askk-trans--root
         askk-trans--node
         askk-trans--events)
     ,@body))

(ert-deftest askk-tests-transliterate-a ()
  (askk-tests-with-transliteration '(("a" "あ"))
    (askk-trans--transliterate ?a)
    (should (equal askk-kana--default-string "あ"))
    (should-not askk-trans--events)))

(ert-deftest askk-tests-transliterate-ka ()
  (askk-tests-with-transliteration '(("ka" "か"))
    (askk-trans--transliterate ?k)
    (should-not askk-kana--default-string)
    (should (equal askk-trans--events '(?k)))

    (askk-trans--transliterate ?a)
    (should (equal askk-kana--default-string "か"))
    (should-not askk-trans--events)))

(ert-deftest askk-tests-transliterate-kya ()
  (askk-tests-with-transliteration '(("kya" "きゃ"))
    (askk-trans--transliterate ?k)
    (askk-trans--transliterate ?y)
    (should-not askk-kana--default-string)
    (should (equal askk-trans--events '(?y ?k)))

    (askk-trans--transliterate ?a)
    (should (equal askk-kana--default-string "きゃ"))
    (should-not askk-trans--events)))

(ert-deftest askk-tests-transliterate-undefined ()
  (askk-tests-with-transliteration nil
    (askk-trans--transliterate ?a)
    (should (equal askk-kana--default-string "a"))
    (should-not askk-trans--events)

    (should-not (askk-trans--transliterate ?b #'ignore))
    (should (equal askk-kana--default-string "a"))
    (should-not askk-trans--events)))

(ert-deftest askk-tests-transliterate-nn ()
  (askk-tests-with-transliteration '(("n" "ん") ("nn" "ん"))
    (askk-trans--transliterate ?n)
    (should-not askk-kana--default-string)
    (should (equal askk-trans--events '(?n)))

    (askk-trans--transliterate ?n)
    (should (equal askk-kana--default-string "ん"))
    (should-not askk-trans--events)))

(ert-deftest askk-tests-transliterate-na ()
  (askk-tests-with-transliteration '(("n" "ん") ("nn" "ん") ("na" "な"))
    (askk-trans--transliterate ?n)
    (askk-trans--transliterate ?a)
    (should (equal askk-kana--default-string "な"))
    (should-not askk-trans--events)))

(ert-deftest askk-tests-transliterate-nka ()
  (askk-tests-with-transliteration '(("n" "ん") ("nn" "ん") ("ka" "か"))
    (askk-trans--transliterate ?n)
    (askk-trans--transliterate ?k)
    (should (equal askk-kana--default-string "ん"))
    (should (equal askk-trans--events '(?k)))

    (askk-trans--transliterate ?a)
    (should (equal askk-kana--default-string "んか"))
    (should-not askk-trans--events)))

(ert-deftest askk-tests-transliterate-n? ()
  (askk-tests-with-transliteration '(("n" "ん") ("nn" "ん"))
    (askk-trans--transliterate ?n)
    (askk-trans--transliterate ??)
    (should (equal askk-kana--default-string "ん?"))
    (should-not askk-trans--events))

  (askk-tests-with-transliteration '(("n" "ん") ("nn" "ん") ("?" "？"))
    (askk-trans--transliterate ?n)
    (askk-trans--transliterate ??)
    (should (equal askk-kana--default-string "ん？"))
    (should-not askk-trans--events)))

(ert-deftest askk-tests-transliterate-atta ()
  (askk-tests-with-transliteration '(("a" "あ") ("ta" "た") ("tt" "っ" . ?t))
    (askk-trans--transliterate ?a)
    (askk-trans--transliterate ?t)
    (askk-trans--transliterate ?t)
    (should (equal askk-kana--default-string "あっ"))
    (should (equal askk-trans--events '(?t)))

    (askk-trans--transliterate ?a)
    (should (equal askk-kana--default-string "あった"))
    (should-not askk-trans--events)))

(ert-deftest askk-tests-transliterate-zl ()
  (askk-tests-with-transliteration '(("zl" "→"))
    (askk-trans--transliterate ?z)
    (askk-trans--transliterate ?l)
    (should (equal askk-kana--default-string "→"))
    (should-not askk-trans--events)))

(ert-deftest askk-tests-handle-normal-zL ()
  (askk-tests-with-transliteration '(("zL" "⇒"))
    (askk-trans--transliterate ?z)
    (askk-trans--transliterate ?L)
    (should (equal askk-kana--default-string "⇒"))
    (should-not askk-trans--events)))

(ert-deftest askk-tests-transliterate-discard-1 ()
  (askk-tests-with-transliteration '(("ka" "か") ("ta" "た"))
    (askk-trans--transliterate ?k)
    (askk-trans--transliterate ?t)
    (should-not askk-kana--default-string)
    (should (equal askk-trans--events '(?t)))))

(ert-deftest askk-tests-transliterate-discard-2 ()
  (askk-tests-with-transliteration '(("a" "あ") ("xtsu" "っ"))
    (askk-trans--transliterate ?x)
    (askk-trans--transliterate ?t)
    (askk-trans--transliterate ?a)
    (should (equal askk-kana--default-string "あ"))
    (should-not askk-trans--events)))

(ert-deftest askk-tests-transliterate-discard-3 ()
  (askk-tests-with-transliteration '(("ka" "か"))
    (askk-trans--transliterate ?k)
    (askk-trans--transliterate ??)
    (should (equal askk-kana--default-string "?"))
    (should-not askk-trans--events)))

(ert-deftest askk-tests-headword-make ()
  (let ((askk-headword--input-string "\n あ \n イ \n")
        askk-okurigana--event)
    (should (equal (askk-headword--make) "あい"))
    (setq askk-okurigana--event ?k)
    (should (equal (askk-headword--make) "あいk"))))

(defmacro askk-tests-with-lookup (kv &rest body)
  (declare (indent defun))
  `(let ((askk-lookup-sources
          (list (list (lambda (headword &rest _)
                        (should (equal (car ,kv) headword))
                        (list (cdr ,kv)))))))
     ,@body))

(defmacro askk-tests-with-temp-mode (mode &rest body)
  (declare (indent defun))
  `(unwind-protect
       (progn
         (funcall ,mode)
         ,@body)
     (funcall ,mode -1)))

(defmacro askk-tests-with-output-buffer (init-func &rest body)
  (declare (indent defun))
  `(with-temp-buffer
     (askk-tests-with-temp-mode #'askk-mode
       (askk-preedit--setup)
       (unwind-protect
           (progn
             (funcall ,init-func)
             (askk-preedit--update)
             ,@body)
         (askk-preedit--teardown)))))

(defun askk-tests-input-method-handle (events)
  (setq unread-command-events
        (append (listify-key-sequence events) unread-command-events))
  (while unread-command-events
    (seq-doseq (c (askk-input-method (read-event)))
      (setq last-command-event c)
      (self-insert-command 1 c))))

(defun askk-tests-trigger-events (events)
  (setq unread-command-events
        (append (listify-key-sequence events) unread-command-events))
  (while unread-command-events
    (askk--handle-event (read-event))))

(ert-deftest askk-tests-input-method-ascii ()
  (askk-tests-with-output-buffer #'askk-ascii-mode
    (askk-tests-input-method-handle "a1;!.(")
    (should (equal (buffer-string) "a1;!.("))))

(ert-deftest askk-tests-input-method-fullwidth-ascii ()
  (askk-tests-with-output-buffer #'askk-fullwidth-ascii-mode
    (askk-tests-input-method-handle "a1;!.(")
    (should (equal (buffer-string) "ａ１；！．（"))))

(ert-deftest askk-tests-input-method-kana ()
  (askk-tests-with-output-buffer #'askk-kana--normal
    (askk-tests-input-method-handle "attaqatta")
    (should (equal (buffer-string) "あったアッタ"))))

(ert-deftest askk-tests-input-method-conversion-space ()
  (askk-tests-with-output-buffer #'askk-kana--normal
    (askk-tests-with-lookup '("あい" . ("愛"))
      (askk-tests-input-method-handle ";ai \n"))
    (should (equal (buffer-string) "愛"))))

(ert-deftest askk-tests-input-method-conversion-okurigana ()
  (askk-tests-with-output-buffer #'askk-kana--normal
    (askk-tests-with-lookup '("あt" . ("会"))
      (askk-tests-input-method-handle ";a;tta\n"))
    (should (equal (buffer-string) "会った"))))

(ert-deftest askk-tests-input-method-conversion-auto ()
  (askk-tests-with-output-buffer #'askk-kana--normal
    (askk-tests-with-lookup '("へんかん" . ("変換"))
      (askk-tests-input-method-handle ";henkanwo\n"))
    (should (equal (buffer-string) "変換を"))))

(ert-deftest askk-tests-input-method-conversion-prefix ()
  (askk-tests-with-output-buffer #'askk-kana--normal
    (askk-tests-with-lookup '("し>" . ("私"))
      (askk-tests-input-method-handle ";si>\n"))
    (should (equal (buffer-string) "私"))))

(ert-deftest askk-tests-input-method-implicit-commit ()
  (askk-tests-with-output-buffer #'askk-kana--normal
    (askk-tests-with-lookup '("へんかん" . ("変換"))
      (askk-tests-input-method-handle ";henkan wo"))
    (should (equal (buffer-string) "変換を"))))

(ert-deftest askk-tests-input-method-n-and-sticky ()
  (askk-tests-with-output-buffer #'askk-kana--normal
    (askk-tests-with-lookup '("へんかん" . ("変換"))
      (askk-tests-input-method-handle "n;henkan \n"))
    (should (equal (buffer-string) "ん変換"))))

(ert-deftest askk-tests-input-method-n-and-upper ()
  (askk-tests-with-output-buffer #'askk-kana--normal
    (askk-tests-with-lookup '("あい" . ("愛"))
      (askk-tests-input-method-handle "nAi \n"))
    (should (equal (buffer-string) "ん愛"))))

(ert-deftest askk-tests-input-method-n-and-abbrev ()
  (askk-tests-with-output-buffer #'askk-kana--normal
    (askk-tests-with-lookup '("ai" . ("人工知能"))
      (askk-tests-input-method-handle "n/ai \n"))
    (should (equal (buffer-string) "ん人工知能"))))

(ert-deftest askk-tests-input-method-n-and-ascii ()
  (askk-tests-with-output-buffer #'askk-kana--normal
    (askk-tests-input-method-handle "nlabc")
    (should (equal (buffer-string) "んabc"))))

(ert-deftest askk-tests-input-method-n-and-fullwidth-ascii ()
  (askk-tests-with-output-buffer #'askk-kana--normal
    (askk-tests-input-method-handle "nLabc")
    (should (equal (buffer-string) "んａｂｃ"))))

(ert-deftest askk-tests-input-method-n-and-toggle ()
  (askk-tests-with-output-buffer #'askk-kana--normal
    (askk-tests-input-method-handle "nqnn")
    (should (equal (buffer-string) "んン"))))

(ert-deftest askk-tests-fullwidth-ascii-electric-pair ()
  (askk-tests-with-output-buffer #'askk-fullwidth-ascii-mode
    (askk-tests-with-temp-mode #'electric-pair-mode
      (askk-tests-input-method-handle "([{"))
    (should (equal (buffer-string) "（［｛｝］）"))))

(ert-deftest askk-tests-kana-electric-pair ()
  (askk-tests-with-output-buffer #'askk-kana--normal
    (askk-tests-with-transliteration '(("(" "（") ("z(" "（"))
      (askk-tests-with-temp-mode #'electric-pair-mode
        (askk-tests-input-method-handle "(z([{")))
    (should (equal (buffer-string) "（（[{}]））"))))

(ert-deftest askk-tests-handle-normal-sticky ()
  (askk-tests-with-output-buffer #'askk-kana--normal
    (askk-tests-trigger-events ";")
    (should (equal (buffer-string) "▽"))
    (should-not askk-trans--events)
    (should (eq askk--conversion-mode 'composing))))

(ert-deftest askk-tests-handle-normal-sticky-unused ()
  (askk-tests-with-output-buffer #'askk-kana--normal
    (let (askk-sticky-shift)
      (askk-tests-trigger-events ";"))
    (should (equal (buffer-string) ";"))
    (should (eq askk--conversion-mode 'normal))))

(ert-deftest askk-tests-handle-normal-sticky-trans-defined ()
  (askk-tests-with-output-buffer #'askk-kana--normal
    (askk-tests-with-transliteration '((";" "；"))
      (askk-tests-trigger-events ";"))
    (should (equal (buffer-string) "▽"))
    (should (eq askk--conversion-mode 'composing))))

(ert-deftest askk-tests-handle-composing-space-empty ()
  (askk-tests-with-output-buffer #'askk-kana--composing
    (should (equal (buffer-string) "▽"))
    (askk-tests-trigger-events " ")
    (should (equal (buffer-string) ""))
    (should-not askk-trans--events)
    (should (eq askk--conversion-mode 'normal))))

(ert-deftest askk-tests-handle-composing-space-n ()
  (askk-tests-with-output-buffer #'askk-kana--composing
    (askk-tests-with-lookup '("ん" . ("ン"))
      (askk-tests-trigger-events "n "))
    (should (equal (buffer-string) "▼ン"))
    (should-not askk-trans--events)
    (should (eq askk--conversion-mode 'selecting))))

(ert-deftest askk-tests-handle-composing-space-kan ()
  (askk-tests-with-output-buffer #'askk-kana--composing
    (askk-tests-with-lookup '("かん" . ("缶"))
      (askk-tests-trigger-events "kan "))
    (should (equal (buffer-string) "▼缶"))
    (should-not askk-trans--events)
    (should (eq askk--conversion-mode 'selecting))))

(ert-deftest askk-tests-handle-composing-abbrev ()
  (askk-tests-with-output-buffer #'askk-kana--composing
    (setq askk--abbrev-flag t)
    (askk-tests-trigger-events "asdfg/")
    (should (equal (buffer-string) "▽asdfg/"))
    (should-not askk-trans--events)))

(ert-deftest askk-tests-handle-composing-sticky-empty ()
  (askk-tests-with-output-buffer #'askk-kana--composing
    (askk-tests-trigger-events ";")
    (should (equal (buffer-string) ";"))
    (should-not askk-trans--events)
    (should (eq askk--conversion-mode 'normal))))

(ert-deftest askk-tests-handle-composing-sticky-empty-transliterated ()
  (askk-tests-with-output-buffer #'askk-kana--composing
    (askk-tests-with-transliteration '((";" "；"))
      (askk-tests-trigger-events ";"))
    (should (equal (buffer-string) "；"))))

(ert-deftest askk-tests-handle-composing-sticky-n ()
  (askk-tests-with-output-buffer #'askk-kana--composing
    (askk-tests-trigger-events "n;")
    (should (equal (buffer-string) "▽ん*"))
    (should-not askk-trans--events)))

(ert-deftest askk-tests-handle-composing-upper-headword ()
  (askk-tests-with-output-buffer #'askk-kana--composing
    (askk-tests-trigger-events "Ka")
    (should (equal (buffer-string) "▽か"))))

(ert-deftest askk-tests-handle-composing-upper-headword-transliterated ()
  (askk-tests-with-output-buffer #'askk-kana--composing
    (askk-tests-with-transliteration '(("zL" "⇒"))
      (askk-tests-trigger-events "zL"))
    (should (equal (buffer-string) "▽⇒"))))

(ert-deftest askk-tests-handle-composing-upper-okurigana ()
  (askk-tests-with-output-buffer #'askk-kana--composing
    (askk-tests-with-lookup '("あt" . ("会"))
      (askk-tests-trigger-events "a;TTA"))
    (should (equal (buffer-string) "▼会った"))))

(ert-deftest askk-tests-handle-composing-okurigana-event-delayed ()
  (askk-tests-with-output-buffer #'askk-kana--composing
    (askk-tests-trigger-events "a;t")
    (should (eq askk-okurigana--event ?t))))

(ert-deftest askk-tests-handle-composing-nL ()
  (askk-tests-with-output-buffer #'askk-kana--composing
    (askk-tests-trigger-events "nL")
    (should (equal (buffer-string) "ん"))
    (should-not askk-trans--events)
    (should (eq askk--input-mode 'fullwidth-ascii))))

(ert-deftest askk-tests-handle-composing-nQ ()
  :expected-result :failed
  (askk-tests-with-output-buffer #'askk-kana--composing
    (askk-tests-trigger-events "nQ")
    (should (equal (buffer-string) "ん▽"))
    (should-not askk-trans--events)
    (should (eq askk--conversion-mode 'composing))))

(ert-deftest askk-tests-handle-composing-nA ()
  (askk-tests-with-output-buffer #'askk-kana--composing
    (askk-tests-trigger-events "nA")
    (should (equal (buffer-string) "▽な"))
    (should-not askk-trans--events)))

(ert-deftest askk-tests-handle-composing-aKi ()
  (askk-tests-with-output-buffer #'askk-kana--composing
    (askk-tests-trigger-events "aK")
    (should (equal (buffer-string) "▽あ*"))
    (should (equal askk-trans--events '(?k)))

    (askk-tests-with-lookup '("あk" . ("開"))
      (askk-tests-trigger-events "i"))
    (should (equal (buffer-string) "▼開き"))
    (should-not askk-trans--events)
    (should (eq askk--conversion-mode 'selecting))))

(ert-deftest askk-tests-handle-composing-aKI ()
  (askk-tests-with-output-buffer #'askk-kana--composing
    (askk-tests-with-lookup '("あk" . ("開"))
      (askk-tests-trigger-events "aKI"))
    (should (equal (buffer-string) "▼開き"))))

(ert-deftest askk-tests-handle-composing-aI ()
  (askk-tests-with-output-buffer #'askk-kana--composing
    (askk-tests-with-lookup '("あi" . ("合"))
      (askk-tests-trigger-events "aI"))
    (should (equal (buffer-string) "▼合い"))
    (should (eq askk--conversion-mode 'selecting))))

(ert-deftest askk-tests-handle-composing-auto ()
  (askk-tests-with-output-buffer #'askk-kana--composing
    (askk-tests-trigger-events "ai")
    (should (equal (buffer-string) "▽あい"))

    (askk-tests-with-lookup '("あい" . ("愛"))
      (askk-tests-trigger-events "wo"))
    (should (equal (buffer-string) "▼愛を"))
    (should-not askk-trans--events)
    (should (eq askk--conversion-mode 'selecting))))

(ert-deftest askk-tests-handle-composing-auto-empty ()
  (askk-tests-with-output-buffer #'askk-kana--composing
    (askk-tests-trigger-events "wo")
    (should (equal (buffer-string) "▽を"))
    (should-not askk-trans--events)
    (should (eq askk--conversion-mode 'composing))))

(ert-deftest askk-tests-handle-composing-suffix-empty ()
  (askk-tests-with-output-buffer #'askk-kana--composing
    (askk-tests-trigger-events ">")
    (should (equal (buffer-string) "▽>"))
    (should-not askk-trans--events)))

(ert-deftest askk-tests-handle-composing-prefix ()
  (askk-tests-with-output-buffer #'askk-kana--composing
    (askk-tests-with-lookup '("し>" . ("私"))
      (askk-tests-trigger-events "si>"))
    (should (equal (buffer-string) "▼私"))
    (should-not askk-trans--events)
    (should (eq askk--conversion-mode 'selecting))))

(ert-deftest askk-tests-handle-composing-nl ()
  (askk-tests-with-output-buffer #'askk-kana--composing
    (askk-tests-trigger-events "nl")
    (should (equal (buffer-string) "ん"))
    (should-not askk-trans--events)
    (should (eq askk--input-mode 'ascii))))

(ert-deftest askk-tests-handle-composing-undefined ()
  (askk-tests-with-output-buffer #'askk-kana--composing
    (askk-tests-trigger-events "'")
    (should (equal (buffer-string) "▽'"))
    (should-not askk-trans--events)))

(ert-deftest askk-tests-handle-composing-okurigana-nn ()
  (askk-tests-with-output-buffer #'askk-kana--composing
    (askk-tests-with-lookup '("ふくn" . ("含"))
      (askk-tests-trigger-events "fukuNn"))
    (should (equal (buffer-string) "▼含ん"))))

(ert-deftest askk-tests-handle-composing-okurigana-nn-sticky ()
  (askk-tests-with-output-buffer #'askk-kana--composing
    (askk-tests-with-lookup '("ふくn" . ("含"))
      (askk-tests-trigger-events "fuku;nn"))
    (should (equal (buffer-string) "▼含ん"))))

(ert-deftest askk-tests-handle-composing-okurigana-sokuon ()
  (askk-tests-with-output-buffer #'askk-kana--composing
    (askk-tests-with-lookup '("いっs" . ("逸"))
      (askk-tests-trigger-events "isSi"))
    (should (equal (buffer-string) "▼逸し"))))

(ert-deftest askk-tests-handle-composing-okurigana-sokuon-sticky ()
  (askk-tests-with-output-buffer #'askk-kana--composing
    (askk-tests-with-lookup '("いっs" . ("逸"))
      (askk-tests-trigger-events "is;si"))
    (should (equal (buffer-string) "▼逸し"))))

(ert-deftest askk-tests-delete-backward-char-events ()
  (pcase-dolist (`(,n . ,expected) '((1 . (?k)) (2 . nil)))
    (let (askk-kana--default-string
          askk-kana--additional-string
          (askk-trans--events '(?y ?k)))
      (with-temp-buffer
        (askk-delete-backward-char n)
        (should-not askk-kana--default-string)
        (should-not askk-kana--additional-string)
        (should (equal askk-trans--events expected))))))

(ert-deftest askk-tests-delete-backward-char-additional ()
  (pcase-dolist (`(,n . ,expected) '((1 . "っ") (2 . nil)))
    (let ((askk-kana--default-string "しめき")
          (askk-kana--additional-string "って")
          askk-trans--events)
      (with-temp-buffer
        (askk-delete-backward-char n)
        (should (equal askk-kana--default-string "しめき"))
        (should (equal askk-kana--additional-string expected))
        (should-not askk-trans--events)))))

(ert-deftest askk-tests-delete-backward-char-base ()
  (pcase-dolist (`(,n . ,expected) '((1 . "てす") (3 . nil)))
    (let ((askk-kana--default-string "てすと")
          askk-kana--additional-string
          askk-trans--events)
      (with-temp-buffer
        (askk-delete-backward-char n)
        (should (equal askk-kana--default-string expected))
        (should-not askk-kana--additional-string)
        (should-not askk-trans--events)))))

(ert-deftest askk-tests-delete-backward-char-composing-prompt ()
  (pcase-dolist (`(,n . ,expected) '((3 . composing) (4 . normal)))
    (let ((askk-kana--default-string "てすと")
          (askk--conversion-mode 'composing))
      (with-temp-buffer
        (askk-delete-backward-char n)
        (should-not askk-kana--default-string)
        (should (eq askk--conversion-mode expected))))))

(ert-deftest askk-tests-delete-backward-char-leading ()
  (let ((askk-kana--default-string "てすと")
        (askk--conversion-mode 'composing))
    (with-temp-buffer
      (insert "leading")
      (askk-delete-backward-char 7)
      (should-not askk-kana--default-string)
      (should (equal (buffer-string) "lead")))))

(ert-deftest askk-tests-delete-backward-char-okurigana-additional ()
  (let ((askk-kana--default-string "しめき")
        (askk-kana--additional-string "っ")
        (askk-trans--events '(?t))
        (askk-okurigana--prompt-flag t)
        (askk-kana--additional-flag t))
    (with-temp-buffer
      (askk-delete-backward-char 1)
      (should (equal askk-kana--default-string "しめき"))
      (should (equal askk-kana--additional-string "っ"))
      (should-not askk-trans--events)
      (askk-delete-backward-char 1)
      (should-not askk-kana--additional-string)
      (askk-delete-backward-char 1)
      (should-not askk-okurigana--prompt-flag)
      (should-not askk-kana--additional-flag)
      (askk-delete-backward-char 1)
      (should (equal askk-kana--default-string "しめ")))))

(ert-deftest askk-tests-delete-backward-char-okurigana-base ()
  (let ((askk-kana--default-string "い")
        (askk-kana--additional-string nil)
        (askk-trans--events '(?s))
        (askk-okurigana--prompt-flag t)
        (askk-kana--additional-flag nil))
    (with-temp-buffer
      (askk-delete-backward-char 1)
      (should (equal askk-kana--default-string "い"))
      (should-not askk-kana--additional-string)
      (should (equal askk-trans--events '(?s)))
      (should-not askk-okurigana--prompt-flag)
      (askk-delete-backward-char 1)
      (should-not askk-trans--events))))

(provide 'askk-tests)
;;; askk-tests.el ends here
