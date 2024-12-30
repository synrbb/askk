;;; askk-tests.el --- Tests for askk.el -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2024 synrbb
;; SPDX-License-Identifier: Apache-2.0 OR GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'askk)

(ert-deftest askk-test-trans-tree ()
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

(ert-deftest askk-test-output-commit ()
  (let (askk--output)
    (should (equal (askk--output-commit "あい") '("あい")))
    (should (equal (askk--output-commit ?う) '("う" "あい")))
    (askk-katakana-mode)
    (should (equal (askk--output-commit "え") '("エ" "う" "あい")))
    (should (equal (askk--output-commit ?お) '("オ" "エ" "う" "あい")))))

(defmacro with-askk-test-transliteration (alist &rest body)
  (declare (indent defun))
  `(let ((askk-transliteration-alist ,alist)
         askk--output
         askk-trans--root
         askk-trans--node
         askk-trans--events)
     ,@body))

(ert-deftest askk-test-transliterate-a ()
  (with-askk-test-transliteration '(("a" "あ"))
    (askk-trans--transliterate ?a)
    (should (equal askk--output '("あ")))
    (should-not askk-trans--events)))

(ert-deftest askk-test-transliterate-ka ()
  (with-askk-test-transliteration '(("ka" "か"))
    (askk-trans--transliterate ?k)
    (should-not askk--output)
    (should (equal askk-trans--events '(?k)))

    (askk-trans--transliterate ?a)
    (should (equal askk--output '("か")))
    (should-not askk-trans--events)))

(ert-deftest askk-test-transliterate-kya ()
  (with-askk-test-transliteration '(("kya" "きゃ"))
    (askk-trans--transliterate ?k)
    (askk-trans--transliterate ?y)
    (should-not askk--output)
    (should (equal askk-trans--events '(?y ?k)))

    (askk-trans--transliterate ?a)
    (should (equal askk--output '("きゃ")))
    (should-not askk-trans--events)))

(ert-deftest askk-test-transliterate-undefined ()
  (with-askk-test-transliteration nil
    (askk-trans--transliterate ?a)
    (should (equal askk--output '("a")))
    (should-not askk-trans--events)

    (should-not (askk-trans--transliterate ?b #'ignore))
    (should (equal askk--output '("a")))
    (should-not askk-trans--events)))

(ert-deftest askk-test-transliterate-nn ()
  (with-askk-test-transliteration '(("n" "ん") ("nn" "ん"))
    (askk-trans--transliterate ?n)
    (should-not askk--output)
    (should (equal askk-trans--events '(?n)))

    (askk-trans--transliterate ?n)
    (should (equal askk--output '("ん")))
    (should-not askk-trans--events)))

(ert-deftest askk-test-transliterate-na ()
  (with-askk-test-transliteration '(("n" "ん") ("nn" "ん") ("na" "な"))
    (askk-trans--transliterate ?n)
    (askk-trans--transliterate ?a)
    (should (equal askk--output '("な")))
    (should-not askk-trans--events)))

(ert-deftest askk-test-transliterate-nka ()
  (with-askk-test-transliteration '(("n" "ん") ("nn" "ん") ("ka" "か"))
    (askk-trans--transliterate ?n)
    (askk-trans--transliterate ?k)
    (should (equal askk--output '("ん")))
    (should (equal askk-trans--events '(?k)))

    (askk-trans--transliterate ?a)
    (should (equal askk--output '("か" "ん")))
    (should-not askk-trans--events)))

(ert-deftest askk-test-transliterate-n? ()
  (with-askk-test-transliteration '(("n" "ん") ("nn" "ん"))
    (askk-trans--transliterate ?n)
    (askk-trans--transliterate ??)
    (should (equal askk--output '("?" "ん")))
    (should-not askk-trans--events))

  (with-askk-test-transliteration '(("n" "ん") ("nn" "ん") ("?" "？"))
    (askk-trans--transliterate ?n)
    (askk-trans--transliterate ??)
    (should (equal askk--output '("？" "ん")))
    (should-not askk-trans--events)))

(ert-deftest askk-test-transliterate-atta ()
  (with-askk-test-transliteration '(("a" "あ") ("ta" "た") ("tt" "っ" . ?t))
    (askk-trans--transliterate ?a)
    (askk-trans--transliterate ?t)
    (askk-trans--transliterate ?t)
    (should (equal askk--output '("っ" "あ")))
    (should (equal askk-trans--events '(?t)))

    (askk-trans--transliterate ?a)
    (should (equal askk--output '("た" "っ" "あ")))
    (should-not askk-trans--events)))

(ert-deftest askk-test-transliterate-discard-1 ()
  (with-askk-test-transliteration '(("ka" "か") ("ta" "た"))
    (askk-trans--transliterate ?k)
    (askk-trans--transliterate ?t)
    (should-not askk--output)
    (should (equal askk-trans--events '(?t)))))

(ert-deftest askk-test-transliterate-discard-2 ()
  (with-askk-test-transliteration '(("a" "あ") ("xtsu" "っ"))
    (askk-trans--transliterate ?x)
    (askk-trans--transliterate ?t)
    (askk-trans--transliterate ?a)
    (should (equal askk--output '("あ")))
    (should-not askk-trans--events)))

(ert-deftest askk-test-transliterate-discard-3 ()
  (with-askk-test-transliteration '(("ka" "か"))
    (askk-trans--transliterate ?k)
    (askk-trans--transliterate ??)
    (should (equal askk--output '("?")))
    (should-not askk-trans--events)))

(defmacro with-askk-test-lookup (kv &rest body)
  (declare (indent defun))
  `(let ((askk-lookup-sources
          (list (list (lambda (headword &rest _)
                        (should (equal (car ,kv) headword))
                        (list (cdr ,kv)))))))
     ,@body
     (let ((candidate (nth askk-cand--index askk-cand--candidates)))
       (askk-headword--replace (car candidate)))))

(defmacro with-askk-test-output-buffer (init-func &rest body)
  (declare (indent defun))
  `(let (askk--output
         askk-trans--root
         askk-trans--node
         askk-trans--events)
     (with-temp-buffer
       (askk-mode)
       (funcall ,init-func)
       (askk--output-flush)
       ,@body)))

(defun askk-test-trigger-events (events)
  (seq-doseq (event events)
    (setq last-command-event event)
    (setq this-command (keymap-lookup (askk--current-keymap)
                                      (single-key-description event)))
    (command-execute this-command)))

(ert-deftest askk-test-fullwidth-ascii ()
  (with-askk-test-output-buffer #'askk-fullwidth-ascii-mode
    (askk-test-trigger-events "a1;!.(")
    (should (equal (buffer-string) "ａ１；！．（"))))

(ert-deftest askk-test-fullwidth-ascii-electric-pair ()
  (with-askk-test-output-buffer #'askk-fullwidth-ascii-mode
    (electric-pair-mode)
    (askk-test-trigger-events "([{")
    (should (equal (buffer-string) "（［｛｝］）"))))

(ert-deftest askk-test-kana-electric-pair ()
  (with-askk-test-output-buffer #'askk-kana--normal
    (with-askk-test-transliteration '(("(" "（") ("z(" "（"))
      (askk-test-trigger-events "(z([{")
      (should (equal (buffer-string) "（（[{"))
      (erase-buffer)
      (askk-electric-pair-mode)
      (askk-test-trigger-events "(z([{")
      (should (equal (buffer-string) "（（[{}]））")))))

(ert-deftest askk-test-handle-normal-sticky ()
  (with-askk-test-output-buffer #'askk-kana--normal
    (askk-test-trigger-events ";")
    (should (equal (buffer-string) "▽"))
    (should-not askk-trans--events)
    (should (eq askk--conversion-mode 'composing))))

(ert-deftest askk-test-handle-normal-sticky-unused ()
  (with-askk-test-output-buffer #'askk-kana--normal
    (let (askk-sticky-shift)
      (askk-test-trigger-events ";"))
    (should (equal (buffer-string) ";"))
    (should (eq askk--conversion-mode 'normal))))

(ert-deftest askk-test-handle-normal-sticky-trans-defined ()
  (with-askk-test-output-buffer #'askk-kana--normal
    (askk-trans--node-insert (askk-trans--root) ";" '("；"))
    (askk-test-trigger-events ";")
    (should (equal (buffer-string) "▽"))
    (should (eq askk--conversion-mode 'composing))))

(ert-deftest askk-test-handle-normal-nL ()
  (with-askk-test-output-buffer #'askk-kana--normal
    (askk-test-trigger-events "nL")
    (should (equal (buffer-string) "ん"))
    (should-not askk-trans--events)
    (should (eq askk--input-mode 'fullwidth-ascii))))

(ert-deftest askk-test-handle-normal-n-sticky ()
  (with-askk-test-output-buffer #'askk-kana--normal
    (askk-test-trigger-events "n;")
    (should (equal (buffer-string) "ん▽"))
    (should-not askk-trans--events)
    (should (eq askk--conversion-mode 'composing))))

(ert-deftest askk-test-handle-normal-nQ ()
  (with-askk-test-output-buffer #'askk-kana--normal
    (askk-test-trigger-events "nQ")
    (should (equal (buffer-string) "ん▽"))
    (should-not askk-trans--events)
    (should (eq askk--conversion-mode 'composing))))

(ert-deftest askk-test-handle-normal-nA ()
  (with-askk-test-output-buffer #'askk-kana--normal
    (askk-test-trigger-events "nA")
    (should (equal (buffer-string) "ん▽あ"))
    (should-not askk-trans--events)))

(ert-deftest askk-test-handle-normal-nK ()
  (with-askk-test-output-buffer #'askk-kana--normal
    (askk-test-trigger-events "nK")
    (should (equal (buffer-string) "ん▽"))
    (should (equal askk-trans--events '(?k)))))

(ert-deftest askk-test-handle-normal-n/ ()
  (with-askk-test-output-buffer #'askk-kana--normal
    (askk-test-trigger-events "n/")
    (should (equal (buffer-string) "ん▽"))
    (should-not askk-trans--events)
    (should (eq askk--abbrev-flag t))
    (should (eq askk--conversion-mode 'composing))))

(ert-deftest askk-test-handle-normal-nl ()
  (with-askk-test-output-buffer #'askk-kana--normal
    (askk-test-trigger-events "nl")
    (should (equal (buffer-string) "ん"))
    (should-not askk-trans--events)
    (should (eq askk--input-mode 'ascii))))

(ert-deftest askk-test-handle-normal-nq ()
  (with-askk-test-output-buffer #'askk-kana--normal
    (askk-hiragana-mode)

    (askk-test-trigger-events "nq")
    (should (equal (buffer-string) "ん"))
    (should-not askk-trans--events)
    (should (eq askk--input-mode 'katakana))

    (askk-test-trigger-events "q")
    (should (eq askk--input-mode 'hiragana))))

(ert-deftest askk-test-handle-normal-zl ()
  (with-askk-test-output-buffer #'askk-kana--normal
    (askk-test-trigger-events "z")
    (should (equal (buffer-string) ""))

    (askk-test-trigger-events "l")
    (should (equal (buffer-string) "→"))
    (should-not askk-trans--events)))

(ert-deftest askk-test-handle-normal-zL ()
  (with-askk-test-output-buffer #'askk-kana--normal
    (with-askk-test-transliteration '(("zL" "⇒"))
      (askk-test-trigger-events "zL"))
    (should (equal (buffer-string) "⇒"))))

(ert-deftest askk-test-handle-normal-xtl ()
  (with-askk-test-output-buffer #'askk-kana--normal
    (askk-test-trigger-events "xtl")
    (should (equal (buffer-string) ""))
    (should-not askk-trans--events)
    (should (eq askk--input-mode 'ascii))))

(ert-deftest askk-test-handle-normal-k-undefined ()
  (with-askk-test-output-buffer #'askk-kana--normal
    (askk-test-trigger-events "k'")
    (should (equal (buffer-string) "'"))
    (should-not askk-trans--events)))

(ert-deftest askk-test-handle-composing-space-empty ()
  (with-askk-test-output-buffer #'askk-kana--composing
    (should (equal (buffer-string) "▽"))
    (askk-test-trigger-events " ")
    (should (equal (buffer-string) ""))
    (should-not askk-trans--events)
    (should (eq askk--conversion-mode 'normal))))

(ert-deftest askk-test-handle-composing-space-n ()
  (with-askk-test-output-buffer #'askk-kana--composing
    (with-askk-test-lookup '("ん" . ("ン"))
      (askk-test-trigger-events "n "))
    (should (equal (buffer-string) "▼ン"))
    (should-not askk-trans--events)
    (should (eq askk--conversion-mode 'selecting))))

(ert-deftest askk-test-handle-composing-space-kan ()
  (with-askk-test-output-buffer #'askk-kana--composing
    (with-askk-test-lookup '("かん" . ("缶"))
      (askk-test-trigger-events "kan "))
    (should (equal (buffer-string) "▼缶"))
    (should-not askk-trans--events)
    (should (eq askk--conversion-mode 'selecting))))

(ert-deftest askk-test-handle-composing-abbrev ()
  (with-askk-test-output-buffer #'askk-kana--composing
    (setq askk--abbrev-flag t)
    (askk-test-trigger-events "asdfg/")
    (should (equal (buffer-string) "▽asdfg/"))
    (should-not askk-trans--events)))

(ert-deftest askk-test-handle-composing-sticky-empty ()
  (with-askk-test-output-buffer #'askk-kana--composing
    (askk-test-trigger-events ";")
    (should (equal (buffer-string) ";"))
    (should-not askk-trans--events)
    (should (eq askk--conversion-mode 'normal))))

(ert-deftest askk-test-handle-composing-sticky-empty-transliterated ()
  (with-askk-test-output-buffer #'askk-kana--composing
    (with-askk-test-transliteration '((";" "；"))
      (askk-test-trigger-events ";"))
    (should (equal (buffer-string) "；"))))

(ert-deftest askk-test-handle-composing-sticky-n ()
  (with-askk-test-output-buffer #'askk-kana--composing
    (askk-test-trigger-events "n;")
    (should (equal (buffer-string) "▽ん*"))
    (should-not askk-trans--events)))

(ert-deftest askk-test-handle-composing-upper-headword ()
  (with-askk-test-output-buffer #'askk-kana--composing
    (askk-test-trigger-events "Ka")
    (should (equal (buffer-string) "▽か"))))

(ert-deftest askk-test-handle-composing-upper-headword-transliterated ()
  (with-askk-test-output-buffer #'askk-kana--composing
    (with-askk-test-transliteration '(("zL" "⇒"))
      (askk-test-trigger-events "zL"))
    (should (equal (buffer-string) "▽⇒"))))

(ert-deftest askk-test-handle-composing-upper-okurigana ()
  (with-askk-test-output-buffer #'askk-kana--composing
    (with-askk-test-lookup '("あt" . ("会"))
      (askk-test-trigger-events "a;TTA"))
    (should (equal (buffer-string) "▼会った"))))

(ert-deftest askk-test-handle-composing-okurigana-event-delayed ()
  (with-askk-test-output-buffer #'askk-kana--composing
    (askk-test-trigger-events "a;t")
    (should (eq askk-okurigana--event ?t))))

(ert-deftest askk-test-handle-composing-nL ()
  (with-askk-test-output-buffer #'askk-kana--composing
    (askk-test-trigger-events "nL")
    (should (equal (buffer-string) "ん"))
    (should-not askk-trans--events)
    (should (eq askk--input-mode 'fullwidth-ascii))))

(ert-deftest askk-test-handle-composing-nQ ()
  (with-askk-test-output-buffer #'askk-kana--composing
    (askk-test-trigger-events "nQ")
    (should (equal (buffer-string) "ん▽"))
    (should-not askk-trans--events)
    (should (eq askk--conversion-mode 'composing))))

(ert-deftest askk-test-handle-composing-nA ()
  (with-askk-test-output-buffer #'askk-kana--composing
    (askk-test-trigger-events "nA")
    (should (equal (buffer-string) "▽な"))
    (should-not askk-trans--events)))

(ert-deftest askk-test-handle-composing-aKi ()
  (with-askk-test-output-buffer #'askk-kana--composing
    (askk-test-trigger-events "aK")
    (should (equal (buffer-string) "▽あ*"))
    (should (equal askk-trans--events '(?k)))

    (with-askk-test-lookup '("あk" . ("開"))
      (askk-test-trigger-events "i"))
    (should (equal (buffer-string) "▼開き"))
    (should-not askk-trans--events)
    (should (eq askk--conversion-mode 'selecting))))

(ert-deftest askk-test-handle-composing-aKI ()
  (with-askk-test-output-buffer #'askk-kana--composing
    (with-askk-test-lookup '("あk" . ("開"))
      (askk-test-trigger-events "aKI"))
    (should (equal (buffer-string) "▼開き"))))

(ert-deftest askk-test-handle-composing-aI ()
  (with-askk-test-output-buffer #'askk-kana--composing
    (with-askk-test-lookup '("あi" . ("合"))
      (askk-test-trigger-events "aI"))
    (should (equal (buffer-string) "▼合い"))
    (should (eq askk--conversion-mode 'selecting))))

(ert-deftest askk-test-handle-composing-auto ()
  (with-askk-test-output-buffer #'askk-kana--composing
    (askk-test-trigger-events "ai")
    (should (equal (buffer-string) "▽あい"))

    (with-askk-test-lookup '("あい" . ("愛"))
      (askk-test-trigger-events "wo"))
    (should (equal (buffer-string) "▼愛を"))
    (should-not askk-trans--events)
    (should (eq askk--conversion-mode 'selecting))))

(ert-deftest askk-test-handle-composing-auto-empty ()
  (with-askk-test-output-buffer #'askk-kana--composing
    (askk-test-trigger-events "wo")
    (should (equal (buffer-string) "▽を"))
    (should-not askk-trans--events)
    (should (eq askk--conversion-mode 'composing))))

(ert-deftest askk-test-handle-composing-suffix-empty ()
  (with-askk-test-output-buffer #'askk-kana--composing
    (askk-test-trigger-events ">")
    (should (equal (buffer-string) "▽>"))
    (should-not askk-trans--events)))

(ert-deftest askk-test-handle-composing-prefix ()
  (with-askk-test-output-buffer #'askk-kana--composing
    (with-askk-test-lookup '("し>" . ("私"))
      (askk-test-trigger-events "si>"))
    (should (equal (buffer-string) "▼私"))
    (should-not askk-trans--events)
    (should (eq askk--conversion-mode 'selecting))))

(ert-deftest askk-test-handle-composing-nl ()
  (with-askk-test-output-buffer #'askk-kana--composing
    (askk-test-trigger-events "nl")
    (should (equal (buffer-string) "ん"))
    (should-not askk-trans--events)
    (should (eq askk--input-mode 'ascii))))

(ert-deftest askk-test-handle-composing-undefined ()
  (with-askk-test-output-buffer #'askk-kana--composing
    (askk-test-trigger-events "'")
    (should (equal (buffer-string) "▽'"))
    (should-not askk-trans--events)))

(ert-deftest askk-test-handle-composing-okurigana-nn ()
  (with-askk-test-output-buffer #'askk-kana--composing
    (with-askk-test-lookup '("ふくn" . ("含"))
      (askk-test-trigger-events "fukuNn"))
    (should (equal (buffer-string) "▼含ん"))))

(ert-deftest askk-test-handle-composing-okurigana-nn-sticky ()
  (with-askk-test-output-buffer #'askk-kana--composing
    (with-askk-test-lookup '("ふくn" . ("含"))
      (askk-test-trigger-events "fuku;nn"))
    (should (equal (buffer-string) "▼含ん"))))

(ert-deftest askk-test-handle-composing-okurigana-sokuon ()
  (with-askk-test-output-buffer #'askk-kana--composing
    (with-askk-test-lookup '("いっs" . ("逸"))
      (askk-test-trigger-events "isSi"))
    (should (equal (buffer-string) "▼逸し"))))

(ert-deftest askk-test-handle-composing-okurigana-sokuon-sticky ()
  (with-askk-test-output-buffer #'askk-kana--composing
    (with-askk-test-lookup '("いっs" . ("逸"))
      (askk-test-trigger-events "is;si"))
    (should (equal (buffer-string) "▼逸し"))))

(ert-deftest askk-test-delete-backward-char-empty ()
  (with-askk-test-output-buffer #'askk-kana--composing
    (call-interactively #'askk-delete-backward-char)
    (should (equal (buffer-string) ""))
    (should (eq askk--conversion-mode 'normal))))

(ert-deftest askk-test-delete-backward-char-empty-event ()
  (with-askk-test-output-buffer #'askk-kana--composing
    (askk-test-trigger-events "k")
    (askk-delete-backward-char 1)
    (should (equal (buffer-string) "▽"))
    (should-not askk-trans--events)))

(ert-deftest askk-test-delete-backward-char-empty-events ()
  (with-askk-test-output-buffer #'askk-kana--composing
    (askk-test-trigger-events "ky")
    (askk-delete-backward-char 1)
    (should (equal (buffer-string) "▽"))
    (should (equal askk-trans--events '(?k)))))

(ert-deftest askk-test-delete-backward-char-str ()
  (with-askk-test-output-buffer #'askk-kana--composing
    (askk-test-trigger-events "a")
    (askk-delete-backward-char 1)
    (should (equal (buffer-string) "▽"))))

(ert-deftest askk-test-delete-backward-char-str-prompt ()
  (with-askk-test-output-buffer #'askk-kana--composing
    (askk-test-trigger-events "a")
    (askk-delete-backward-char 2)
    (should (equal (buffer-string) ""))
    (should (eq askk--conversion-mode 'normal))))

(ert-deftest askk-test-delete-backward-char-str-event-prompt ()
  (with-askk-test-output-buffer #'askk-kana--composing
    (askk-test-trigger-events "aiky")
    (askk-delete-backward-char 3)
    (should (equal (buffer-string) "▽あ"))))

(ert-deftest askk-test-delete-backward-char-okurigana-event ()
  (with-askk-test-output-buffer #'askk-kana--composing
    (askk-test-trigger-events "aK")
    (askk-delete-backward-char 1)
    (should (equal (buffer-string) "▽あ*"))
    (should-not askk-trans--events)
    (should-not askk-okurigana--event)
    (should (= (char-after askk-okurigana--start) ?*))))

(ert-deftest askk-test-delete-backward-char-okurigana-prompt ()
  (with-askk-test-output-buffer #'askk-kana--composing
    (askk-test-trigger-events "aK")
    (askk-delete-backward-char 2)
    (should (equal (buffer-string) "▽あ"))
    (should-not askk-trans--events)
    (should-not askk-okurigana--event)
    (should-not askk-okurigana--start)))

(ert-deftest askk-test-delete-backward-char-okurigana-tt ()
  (with-askk-test-output-buffer #'askk-kana--composing
    (askk-test-trigger-events "aTt")
    (askk-delete-backward-char 1)
    (should (equal (buffer-string) "▽あ*っ"))
    (should-not askk-trans--events)
    (should (= askk-okurigana--event ?t))
    (should (= (char-after askk-okurigana--start) ?*))))

(ert-deftest askk-test-delete-backward-char-okurigana-all ()
  (with-askk-test-output-buffer #'askk-kana--composing
    (askk-test-trigger-events "aiTt")
    (askk-delete-backward-char 6)
    (should (equal (buffer-string) ""))
    (should-not askk-trans--events)
    (should-not askk-okurigana--event)
    (should-not askk-okurigana--start)
    (should (eq askk--conversion-mode 'normal))))

(provide 'askk-tests)
;;; askk-tests.el ends here
