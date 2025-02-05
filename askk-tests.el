;;; askk-tests.el --- Tests for askk.el -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2024-2025 askk contributors
;; SPDX-License-Identifier: Apache-2.0 OR GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'askk)

(ert-deftest askk-tests-input-mode-color ()
  (let ((askk--input-mode 'hiragana))
    (should (equal (askk--input-mode-color) "#cb4b16")))
  (let ((askk--abbrev-flag t))
    (should (equal (askk--input-mode-color) "#268bd2"))))

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
  (let (askk--output)
    (should (equal (askk--output-commit "あい") '("あい")))
    (should (equal (askk--output-commit ?う) '("う" "あい")))
    (askk-katakana-mode)
    (should (equal (askk--output-commit "え") '("エ" "う" "あい")))
    (should (equal (askk--output-commit ?お) '("オ" "エ" "う" "あい")))))

(defmacro askk-tests-with-transliteration (alist &rest body)
  (declare (indent defun))
  `(let ((askk-transliteration-base-alist ,alist)
         askk--output
         askk-trans--root
         askk-trans--node
         askk-trans--events)
     ,@body))

(ert-deftest askk-tests-transliterate-a ()
  (askk-tests-with-transliteration '(("a" "あ"))
    (askk-trans--transliterate ?a)
    (should (equal askk--output '("あ")))
    (should-not askk-trans--events)))

(ert-deftest askk-tests-transliterate-ka ()
  (askk-tests-with-transliteration '(("ka" "か"))
    (askk-trans--transliterate ?k)
    (should-not askk--output)
    (should (equal askk-trans--events '(?k)))

    (askk-trans--transliterate ?a)
    (should (equal askk--output '("か")))
    (should-not askk-trans--events)))

(ert-deftest askk-tests-transliterate-kya ()
  (askk-tests-with-transliteration '(("kya" "きゃ"))
    (askk-trans--transliterate ?k)
    (askk-trans--transliterate ?y)
    (should-not askk--output)
    (should (equal askk-trans--events '(?y ?k)))

    (askk-trans--transliterate ?a)
    (should (equal askk--output '("きゃ")))
    (should-not askk-trans--events)))

(ert-deftest askk-tests-transliterate-undefined ()
  (askk-tests-with-transliteration nil
    (askk-trans--transliterate ?a)
    (should (equal askk--output '("a")))
    (should-not askk-trans--events)

    (should-not (askk-trans--transliterate ?b #'ignore))
    (should (equal askk--output '("a")))
    (should-not askk-trans--events)))

(ert-deftest askk-tests-transliterate-nn ()
  (askk-tests-with-transliteration '(("n" "ん") ("nn" "ん"))
    (askk-trans--transliterate ?n)
    (should-not askk--output)
    (should (equal askk-trans--events '(?n)))

    (askk-trans--transliterate ?n)
    (should (equal askk--output '("ん")))
    (should-not askk-trans--events)))

(ert-deftest askk-tests-transliterate-na ()
  (askk-tests-with-transliteration '(("n" "ん") ("nn" "ん") ("na" "な"))
    (askk-trans--transliterate ?n)
    (askk-trans--transliterate ?a)
    (should (equal askk--output '("な")))
    (should-not askk-trans--events)))

(ert-deftest askk-tests-transliterate-nka ()
  (askk-tests-with-transliteration '(("n" "ん") ("nn" "ん") ("ka" "か"))
    (askk-trans--transliterate ?n)
    (askk-trans--transliterate ?k)
    (should (equal askk--output '("ん")))
    (should (equal askk-trans--events '(?k)))

    (askk-trans--transliterate ?a)
    (should (equal askk--output '("か" "ん")))
    (should-not askk-trans--events)))

(ert-deftest askk-tests-transliterate-n? ()
  (askk-tests-with-transliteration '(("n" "ん") ("nn" "ん"))
    (askk-trans--transliterate ?n)
    (askk-trans--transliterate ??)
    (should (equal askk--output '("?" "ん")))
    (should-not askk-trans--events))

  (askk-tests-with-transliteration '(("n" "ん") ("nn" "ん") ("?" "？"))
    (askk-trans--transliterate ?n)
    (askk-trans--transliterate ??)
    (should (equal askk--output '("？" "ん")))
    (should-not askk-trans--events)))

(ert-deftest askk-tests-transliterate-atta ()
  (askk-tests-with-transliteration '(("a" "あ") ("ta" "た") ("tt" "っ" . ?t))
    (askk-trans--transliterate ?a)
    (askk-trans--transliterate ?t)
    (askk-trans--transliterate ?t)
    (should (equal askk--output '("っ" "あ")))
    (should (equal askk-trans--events '(?t)))

    (askk-trans--transliterate ?a)
    (should (equal askk--output '("た" "っ" "あ")))
    (should-not askk-trans--events)))

(ert-deftest askk-tests-transliterate-discard-1 ()
  (askk-tests-with-transliteration '(("ka" "か") ("ta" "た"))
    (askk-trans--transliterate ?k)
    (askk-trans--transliterate ?t)
    (should-not askk--output)
    (should (equal askk-trans--events '(?t)))))

(ert-deftest askk-tests-transliterate-discard-2 ()
  (askk-tests-with-transliteration '(("a" "あ") ("xtsu" "っ"))
    (askk-trans--transliterate ?x)
    (askk-trans--transliterate ?t)
    (askk-trans--transliterate ?a)
    (should (equal askk--output '("あ")))
    (should-not askk-trans--events)))

(ert-deftest askk-tests-transliterate-discard-3 ()
  (askk-tests-with-transliteration '(("ka" "か"))
    (askk-trans--transliterate ?k)
    (askk-trans--transliterate ??)
    (should (equal askk--output '("?")))
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
     ,@body
     (let ((candidate (nth askk-cand--index askk-cand--candidates)))
       (askk-headword--replace (car candidate)))))

(defmacro askk-tests-with-output-buffer (init-func &rest body)
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

(defun askk-tests-trigger-events (events)
  (seq-doseq (event events)
    (setq last-command-event event)
    (setq this-command (keymap-lookup (askk--current-keymap)
                                      (single-key-description event)))
    (command-execute this-command)))

(ert-deftest askk-tests-fullwidth-ascii ()
  (askk-tests-with-output-buffer #'askk-fullwidth-ascii-mode
    (askk-tests-trigger-events "a1;!.(")
    (should (equal (buffer-string) "ａ１；！．（"))))

(ert-deftest askk-tests-fullwidth-ascii-electric-pair ()
  (askk-tests-with-output-buffer #'askk-fullwidth-ascii-mode
    (askk-tests-trigger-events "([{")
    (should (equal (buffer-string) "（［｛"))
    (erase-buffer)
    (electric-pair-mode)
    (unwind-protect
        (askk-tests-trigger-events "([{")
      (electric-pair-mode -1))
    (should (equal (buffer-string) "（［｛｝］）"))))

(ert-deftest askk-tests-kana-electric-pair ()
  (askk-tests-with-output-buffer #'askk-kana--normal
    (askk-tests-with-transliteration '(("(" "（") ("z(" "（"))
      (askk-tests-trigger-events "(z([{")
      (should (equal (buffer-string) "（（[{"))
      (erase-buffer)
      (electric-pair-mode)
      (unwind-protect
          (askk-tests-trigger-events "(z([{")
        (electric-pair-mode -1))
      (should (equal (buffer-string) "（（[{}]））")))))

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

(ert-deftest askk-tests-handle-normal-nL ()
  (askk-tests-with-output-buffer #'askk-kana--normal
    (askk-tests-trigger-events "nL")
    (should (equal (buffer-string) "ん"))
    (should-not askk-trans--events)
    (should (eq askk--input-mode 'fullwidth-ascii))))

(ert-deftest askk-tests-handle-normal-n-sticky ()
  (askk-tests-with-output-buffer #'askk-kana--normal
    (askk-tests-trigger-events "n;")
    (should (equal (buffer-string) "ん▽"))
    (should-not askk-trans--events)
    (should (eq askk--conversion-mode 'composing))))

(ert-deftest askk-tests-handle-normal-nQ ()
  (askk-tests-with-output-buffer #'askk-kana--normal
    (askk-tests-trigger-events "nQ")
    (should (equal (buffer-string) "ん▽"))
    (should-not askk-trans--events)
    (should (eq askk--conversion-mode 'composing))))

(ert-deftest askk-tests-handle-normal-nA ()
  (askk-tests-with-output-buffer #'askk-kana--normal
    (askk-tests-trigger-events "nA")
    (should (equal (buffer-string) "ん▽あ"))
    (should-not askk-trans--events)))

(ert-deftest askk-tests-handle-normal-nK ()
  (askk-tests-with-output-buffer #'askk-kana--normal
    (askk-tests-trigger-events "nK")
    (should (equal (buffer-string) "ん▽"))
    (should (equal askk-trans--events '(?k)))))

(ert-deftest askk-tests-handle-normal-n/ ()
  (askk-tests-with-output-buffer #'askk-kana--normal
    (askk-tests-trigger-events "n/")
    (should (equal (buffer-string) "ん▽"))
    (should-not askk-trans--events)
    (should (eq askk--abbrev-flag t))
    (should (eq askk--conversion-mode 'composing))))

(ert-deftest askk-tests-handle-normal-nl ()
  (askk-tests-with-output-buffer #'askk-kana--normal
    (askk-tests-trigger-events "nl")
    (should (equal (buffer-string) "ん"))
    (should-not askk-trans--events)
    (should (eq askk--input-mode 'ascii))))

(ert-deftest askk-tests-handle-normal-nq ()
  (askk-tests-with-output-buffer #'askk-kana--normal
    (askk-hiragana-mode)

    (askk-tests-trigger-events "nq")
    (should (equal (buffer-string) "ん"))
    (should-not askk-trans--events)
    (should (eq askk--input-mode 'katakana))

    (askk-tests-trigger-events "q")
    (should (eq askk--input-mode 'hiragana))))

(ert-deftest askk-tests-handle-normal-zl ()
  (askk-tests-with-output-buffer #'askk-kana--normal
    (askk-tests-trigger-events "z")
    (should (equal (buffer-string) ""))

    (askk-tests-trigger-events "l")
    (should (equal (buffer-string) "→"))
    (should-not askk-trans--events)))

(ert-deftest askk-tests-handle-normal-zL ()
  (askk-tests-with-output-buffer #'askk-kana--normal
    (askk-tests-with-transliteration '(("zL" "⇒"))
      (askk-tests-trigger-events "zL"))
    (should (equal (buffer-string) "⇒"))))

(ert-deftest askk-tests-handle-normal-xtl ()
  (askk-tests-with-output-buffer #'askk-kana--normal
    (askk-tests-trigger-events "xtl")
    (should (equal (buffer-string) ""))
    (should-not askk-trans--events)
    (should (eq askk--input-mode 'ascii))))

(ert-deftest askk-tests-handle-normal-k-undefined ()
  (askk-tests-with-output-buffer #'askk-kana--normal
    (askk-tests-trigger-events "k'")
    (should (equal (buffer-string) "'"))
    (should-not askk-trans--events)))

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
    (should (eq askk--conversion-mode 'converting))))

(ert-deftest askk-tests-handle-composing-space-kan ()
  (askk-tests-with-output-buffer #'askk-kana--composing
    (askk-tests-with-lookup '("かん" . ("缶"))
      (askk-tests-trigger-events "kan "))
    (should (equal (buffer-string) "▼缶"))
    (should-not askk-trans--events)
    (should (eq askk--conversion-mode 'converting))))

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
    (should (eq askk--conversion-mode 'converting))))

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
    (should (eq askk--conversion-mode 'converting))))

(ert-deftest askk-tests-handle-composing-auto ()
  (askk-tests-with-output-buffer #'askk-kana--composing
    (askk-tests-trigger-events "ai")
    (should (equal (buffer-string) "▽あい"))

    (askk-tests-with-lookup '("あい" . ("愛"))
      (askk-tests-trigger-events "wo"))
    (should (equal (buffer-string) "▼愛を"))
    (should-not askk-trans--events)
    (should (eq askk--conversion-mode 'converting))))

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
    (should (eq askk--conversion-mode 'converting))))

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

(ert-deftest askk-tests-delete-backward-char-empty ()
  (askk-tests-with-output-buffer #'askk-kana--composing
    (call-interactively #'askk-delete-backward-char)
    (should (equal (buffer-string) ""))
    (should (eq askk--conversion-mode 'normal))))

(ert-deftest askk-tests-delete-backward-char-empty-event ()
  (askk-tests-with-output-buffer #'askk-kana--composing
    (askk-tests-trigger-events "k")
    (askk-delete-backward-char 1)
    (should (equal (buffer-string) "▽"))
    (should-not askk-trans--events)))

(ert-deftest askk-tests-delete-backward-char-empty-events ()
  (askk-tests-with-output-buffer #'askk-kana--composing
    (askk-tests-trigger-events "ky")
    (askk-delete-backward-char 1)
    (should (equal (buffer-string) "▽"))
    (should (equal askk-trans--events '(?k)))))

(ert-deftest askk-tests-delete-backward-char-str ()
  (askk-tests-with-output-buffer #'askk-kana--composing
    (askk-tests-trigger-events "a")
    (askk-delete-backward-char 1)
    (should (equal (buffer-string) "▽"))))

(ert-deftest askk-tests-delete-backward-char-str-prompt ()
  (askk-tests-with-output-buffer #'askk-kana--composing
    (askk-tests-trigger-events "a")
    (askk-delete-backward-char 2)
    (should (equal (buffer-string) ""))
    (should (eq askk--conversion-mode 'normal))))

(ert-deftest askk-tests-delete-backward-char-str-event-prompt ()
  (askk-tests-with-output-buffer #'askk-kana--composing
    (askk-tests-trigger-events "aiky")
    (askk-delete-backward-char 3)
    (should (equal (buffer-string) "▽あ"))))

(ert-deftest askk-tests-delete-backward-char-okurigana-event ()
  (askk-tests-with-output-buffer #'askk-kana--composing
    (askk-tests-trigger-events "aK")
    (askk-delete-backward-char 1)
    (should (equal (buffer-string) "▽あ*"))
    (should-not askk-trans--events)
    (should-not askk-okurigana--event)
    (should (= (char-after askk-okurigana--start) ?*))))

(ert-deftest askk-tests-delete-backward-char-okurigana-prompt ()
  (askk-tests-with-output-buffer #'askk-kana--composing
    (askk-tests-trigger-events "aK")
    (askk-delete-backward-char 2)
    (should (equal (buffer-string) "▽あ"))
    (should-not askk-trans--events)
    (should-not askk-okurigana--event)
    (should-not askk-okurigana--start)))

(ert-deftest askk-tests-delete-backward-char-okurigana-tt ()
  (askk-tests-with-output-buffer #'askk-kana--composing
    (askk-tests-trigger-events "aTt")
    (askk-delete-backward-char 1)
    (should (equal (buffer-string) "▽あ*っ"))
    (should-not askk-trans--events)
    (should (= askk-okurigana--event ?t))
    (should (= (char-after askk-okurigana--start) ?*))))

(ert-deftest askk-tests-delete-backward-char-okurigana-all ()
  (askk-tests-with-output-buffer #'askk-kana--composing
    (askk-tests-trigger-events "aiTt")
    (askk-delete-backward-char 6)
    (should (equal (buffer-string) ""))
    (should-not askk-trans--events)
    (should-not askk-okurigana--event)
    (should-not askk-okurigana--start)
    (should (eq askk--conversion-mode 'normal))))

(provide 'askk-tests)
;;; askk-tests.el ends here
