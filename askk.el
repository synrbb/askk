;;; askk.el --- ASKK -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2024-2025 askk contributors
;; SPDX-License-Identifier: Apache-2.0 OR GPL-3.0-or-later

;; Version: 0.1.0
;; Keywords: i18n, text
;; URL: https://github.com/synrbb/askk
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; This package provides SKK.

;;; Code:

(eval-when-compile (require 'inline))
(require 'askk-user-dict)

(defgroup askk nil
  "Options for ASKK."
  :group 'leim)

(defcustom askk-lookup-sources
  '((askk-user-dict-lookup))
  "検索ソースのリスト。先頭から順にすべてのソースを検索する。"
  :type '(alist :key-type symbol :value-type (repeat sexp)))

(defcustom askk-candidates-style-function #'askk-candidates-inplace-style
  "候補の一覧のスタイル。"
  :type 'function)

(defcustom askk-auto-conversion-triggers
  (mapcar #'char-to-string "を、。")
  "自動変換のきっかけとなる文字列のリスト。"
  :type '(repeat string))

(defcustom askk-sticky-shift ?\;
  "Non-nil の場合その文字を sticky shift key として使用する。"
  :type '(choice (const nil)
                 character))

(defcustom askk-composing-prompt ?▽
  "▽モードのプロンプト。"
  :type 'character)

(defcustom askk-selecting-prompt ?▼
  "▼モードのプロンプト。"
  :type 'character)

(defcustom askk-okurigana-prompt ?*
  "送り仮名のプロンプト。"
  :type 'character)

(defcustom askk-fullwidth-ascii-table
  (eval-when-compile
    (let ((vec (make-vector (1+ ?~) nil))
          (i ?!)
          (c ?\N{FULLWIDTH EXCLAMATION MARK}))
      (aset vec ?\s ?\N{IDEOGRAPHIC SPACE})
      (aset vec ?~ ?\N{WAVE DASH})
      (while (< i ?~)
        (aset vec i c)
        (setq i (1+ i))
        (setq c (1+ c)))
      vec))
  "全角 ASCII モードでの変換テーブル。
対応する ASCII コードポイントでインデックスされる全角 ASCII 文字のベクタ。"
  :type '(vector (character :inline t)))

(defcustom askk-transliteration-alist nil
  "ひらがな／カタカナモードでの transliteration 定義の連想リスト。
連想リストの値は、変換結果の文字列と自動入力文字の cons セル、または
そのような cons セルを返す関数。

同じキーに対する定義が `askk-transliteration-base-alist' にあっても
こちらの定義の方が優先される。"
  :type '(alist :key-type string
                :value-type (choice (cons string (choice (const nil)
                                                         character))
                                    symbol)))

(defcustom askk-transliteration-base-alist
  '(("xa" "ぁ") ("a" "あ") ("xi" "ぃ") ("i" "い") ("xu" "ぅ")
    ("u" "う") ("xe" "ぇ") ("e" "え") ("xo" "ぉ") ("o" "お")
    ("ka" "か") ("ga" "が") ("ki" "き") ("gi" "ぎ") ("ku" "く")
    ("gu" "ぐ") ("ke" "け") ("ge" "げ") ("ko" "こ") ("go" "ご")
    ("sa" "さ") ("za" "ざ") ("si" "し") ("zi" "じ") ("su" "す")
    ("zu" "ず") ("se" "せ") ("ze" "ぜ") ("so" "そ") ("zo" "ぞ")
    ("ta" "た") ("da" "だ") ("ti" "ち") ("di" "ぢ") ("xtu" "っ")
    ("tu" "つ") ("du" "づ") ("te" "て") ("de" "で") ("to" "と")
    ("do" "ど")
    ("na" "な") ("ni" "に") ("nu" "ぬ") ("ne" "ね") ("no" "の")
    ("ha" "は") ("ba" "ば") ("pa" "ぱ") ("hi" "ひ") ("bi" "び")
    ("pi" "ぴ") ("hu" "ふ") ("bu" "ぶ") ("pu" "ぷ") ("he" "へ")
    ("be" "べ") ("pe" "ぺ") ("ho" "ほ") ("bo" "ぼ") ("po" "ぽ")
    ("ma" "ま") ("mi" "み") ("mu" "む") ("me" "め") ("mo" "も")
    ("xya" "ゃ") ("ya" "や") ("xyu" "ゅ") ("yu" "ゆ") ("xyo" "ょ")
    ("yo" "よ")
    ("ra" "ら") ("ri" "り") ("ru" "る") ("re" "れ") ("ro" "ろ")
    ("xwa" "ゎ") ("wa" "わ") ("xyi" "ゐ") ("xye" "ゑ") ("wo" "を")
    ("nn" "ん")
    ("vu" "ゔ") ("xka" "ゕ") ("xke" "ゖ")
    ;; JIS X 4063:2000 MUST
    ("shi" "し") ("ji" "じ") ("chi" "ち") ("tsu" "つ") ("fu" "ふ")
    ("n" "ん") ("n'" "ん")
    ("kya" "きゃ") ("kyu" "きゅ") ("kyo" "きょ")
    ("gya" "ぎゃ") ("gyu" "ぎゅ") ("gyo" "ぎょ")
    ("sya" "しゃ") ("syu" "しゅ") ("syo" "しょ")
    ("sha" "しゃ") ("shu" "しゅ") ("sho" "しょ")
    ("zya" "じゃ") ("zyu" "じゅ") ("zyo" "じょ")
    ("ja" "じゃ") ("ju" "じゅ") ("jo" "じょ")
    ("tya" "ちゃ") ("tyu" "ちゅ") ("tyo" "ちょ")
    ("cha" "ちゃ") ("chu" "ちゅ") ("cho" "ちょ")
    ("dya" "ぢゃ") ("dyu" "ぢゅ") ("dyo" "ぢょ")
    ("nya" "にゃ") ("nyu" "にゅ") ("nyo" "にょ")
    ("hya" "ひゃ") ("hyu" "ひゅ") ("hyo" "ひょ")
    ("bya" "びゃ") ("byu" "びゅ") ("byo" "びょ")
    ("pya" "ぴゃ") ("pyu" "ぴゅ") ("pyo" "ぴょ")
    ("mya" "みゃ") ("myu" "みゅ") ("myo" "みょ")
    ("rya" "りゃ") ("ryu" "りゅ") ("ryo" "りょ")
    ("sye" "しぇ") ("zye" "じぇ")
    ("she" "しぇ") ("je" "じぇ")
    ("tye" "ちぇ")
    ("che" "ちぇ")
    ("tsa" "つぁ") ("tse" "つぇ") ("tso" "つぉ")
    ("thi" "てぃ") ("dhi" "でぃ")
    ("dhu" "でゅ")
    ("fa" "ふぁ") ("fi" "ふぃ") ("fe" "ふぇ")("fo" "ふぉ")
    ;; JIS X 4063:2000 WANT
    ("ye" "いぇ")
    ("whi" "うぃ") ("whe" "うぇ") ("who" "うぉ")
    ("wi" "うぃ") ("we" "うぇ")
    ("va" "ゔぁ") ("vi" "ゔぃ") ("ve" "ゔぇ") ("vo" "ゔぉ")
    ("vyu" "ゔゅ")
    ("kwa" "くゎ") ("kwi" "くぃ") ("kwe" "くぇ") ("kwo" "くぉ")
    ("gwa" "ぐぁ")
    ("jya" "じゃ") ("jyu" "じゅ") ("jyo" "じょ")
    ("cya" "ちゃ") ("cyu" "ちゅ") ("cyo" "ちょ")
    ("tsi" "つぃ")
    ("thi" "てぃ") ("dhi" "でぃ")
    ("t'i" "てぃ") ("d'i" "でぃ")
    ("thu" "てゅ") ("d'yu" "でゅ")
    ("t'yu" "てゅ")
    ("twu" "とぅ") ("dwu" "どぅ")
    ("t'u" "とぅ") ("d'u" "どぅ")
    ("hwa" "ふぁ") ("hwi" "ふぃ") ("hwe" "ふぇ") ("hwo" "ふぉ")
    ("fyu" "ふゅ")
    ("hwyu" "ふゅ")
    ("xtsu" "っ")
    ;; other
    ("bb" "っ" . ?b)
    ("cc" "っ" . ?c)
    ("dd" "っ" . ?d)
    ("ff" "っ" . ?f)
    ("gg" "っ" . ?g)
    ("hh" "っ" . ?h)
    ("jj" "っ" . ?j)
    ("kk" "っ" . ?k)
    ("mm" "っ" . ?m)
    ("pp" "っ" . ?p)
    ("rr" "っ" . ?r)
    ("ss" "っ" . ?s)
    ("tt" "っ" . ?t)
    ("vv" "っ" . ?v)
    ("ww" "っ" . ?w)
    ("xx" "っ" . ?x)
    ("yy" "っ" . ?y)
    ("zz" "っ" . ?z)
    ("," "、")
    ("." "。")
    ("-" "ー")
    ("z " "　")
    ("zh" "←")
    ("zj" "↓")
    ("zk" "↑")
    ("zl" "→")
    ("L" . askk-fullwidth-ascii-mode)
    ("Q" . ignore)
    ("/" . askk-abbrev-mode)
    ("l" . askk-ascii-mode)
    ("q" . askk-toggle-kana))
  "ひらがな／カタカナモードでのデフォルトの transliteration 定義の連想リスト。

同じキーに対する定義が `askk-transliteration-alist' にある場合は
こちらの定義は無視される。"
  :type '(alist :key-type string
                :value-type (choice (cons string (choice (const nil)
                                                         character))
                                    symbol)))

(defgroup askk-faces nil
  "Faces for ASKK"
  :group 'askk
  :group 'faces)

(defface askk-preedit
  '((t :inherit underline))
  "Face for preedit.")

(defface askk-candidate-preview
  '((t :inherit highlight))
  "Face for candidate preview.")

;;; Input modes

(defvar-keymap askk-ascii-mode-map
  :doc "Keymap for ASCII mode."
  "C-j" #'askk-hiragana-mode)

(defvar-keymap askk-fullwidth-ascii-mode-map
  :doc "Keymap for fullwidth ASCII mode."
  "C-j" #'askk-hiragana-mode
  "C-q" #'askk-katakana-mode)

(defvar-keymap askk-kana-mode-map
  :doc "Keymap for normal or composing conversion mode."
  "C-j" #'askk-kana--commit
  "DEL" #'askk-delete-backward-char)

(defvar-keymap askk-kana-selecting-mode-map
  :doc "Keymap for selecting conversion mode."
  :parent askk-kana-mode-map
  "C-n" #'askk-next-candidate
  "C-p" #'askk-previous-candidate
  "C-v" #'askk-candidates-next-page
  "M-v" #'askk-candidates-previous-page
  "SPC" #'askk-next-candidate
  "X" #'askk-delete-candidate
  "x" #'askk-previous-candidate)

(defvar askk--input-mode-alist
  '((hiragana
     :title "あ"
     :color "#cb4b16"
     :keymap askk-kana-mode-map)
    (katakana
     :title "ア"
     :color "#859900"
     :keymap askk-kana-mode-map
     :convert-function askk--hira2kata)
    (ascii
     :title "AS"
     :color "#93a1a1"
     :keymap askk-ascii-mode-map)
    (fullwidth-ascii
     :title "Ａ"
     :color "#b58900"
     :keymap askk-fullwidth-ascii-mode-map)
    (abbrev
     :title "AB"
     :color "#268bd2"
     :keymap askk-kana-mode-map)))

(defvar askk--input-mode-hook nil)

(defvar-local askk--input-mode nil)
(defvar-local askk--conversion-mode nil)
(defvar-local askk--abbrev-flag nil)

(defun askk--input-mode-color ()
  (plist-get (askk--input-mode-plist) :color))

(defun askk--input-mode-plist (&optional input-mode)
  (alist-get (or input-mode askk--input-mode) askk--input-mode-alist))

(defun askk--update-input-mode (&optional input-mode)
  (when (and input-mode (not (eq input-mode 'abbrev)))
    (setq askk--input-mode input-mode))
  (let* ((plist (askk--input-mode-plist input-mode))
         (keymap (symbol-value (plist-get plist :keymap)))
         (title (plist-get plist :title)))
    (askk--enable-keymap keymap)
    (setq current-input-method-title title))
  (run-hooks 'askk--input-mode-hook)
  (force-mode-line-update))

(defun askk--enable-keymap (keymap)
  (setf (alist-get 'askk-mode minor-mode-map-alist) keymap))

(defun askk--current-keymap ()
  (if (eq askk--conversion-mode 'selecting)
      askk-kana-selecting-mode-map
    (symbol-value (plist-get (askk--input-mode-plist) :keymap))))

(defun askk-hiragana-mode ()
  (interactive)
  (askk--update-input-mode 'hiragana))

(defun askk-katakana-mode ()
  (interactive)
  (askk--update-input-mode 'katakana))

;;; Output

(defvar-local askk-kana--default-string nil)
(defvar-local askk-kana--additional-string nil)
(defvar-local askk-kana--additional-flag nil)
(defvar-local askk-kana--last-substring nil)

(defun askk--output-commit (obj)
  (when (characterp obj)
    (setq obj (char-to-string obj)))
  (when-let* ((func (plist-get (askk--input-mode-plist) :convert-function)))
    (setq obj (concat (mapcar func obj))))
  (setq askk-kana--last-substring obj)
  (if askk-kana--additional-flag
      (setq askk-kana--additional-string
            (concat askk-kana--additional-string obj))
    (setq askk-kana--default-string
          (concat askk-kana--default-string obj))))

;;; Preedit

(defvar-local askk-preedit--overlay nil)

(defun askk-preedit--setup ()
  (let ((pos (point)))
    (if (overlayp askk-preedit--overlay)
        (move-overlay askk-preedit--overlay pos pos)
      (setq askk-preedit--overlay (make-overlay pos pos))
      (overlay-put askk-preedit--overlay 'face 'askk-preedit))))

(defun askk-preedit--teardown ()
  (when-let* ((start (and (overlayp askk-preedit--overlay)
                          (overlay-start askk-preedit--overlay))))
    (delete-region start (overlay-end askk-preedit--overlay))
    (delete-overlay askk-preedit--overlay))
  (setq askk-kana--default-string nil)
  (setq askk-kana--additional-string nil)
  (setq askk-kana--additional-flag nil)
  (setq askk-kana--last-substring nil))

(defun askk-preedit--cleanup ()
  (askk-preedit--teardown)
  (setq askk-preedit--overlay nil))

(defun askk-preedit--update ()
  (let ((modified (buffer-modified-p))
        (start (overlay-start askk-preedit--overlay))
        (end (overlay-end askk-preedit--overlay)))
    (when (< start end)
      (delete-region start end))
    (goto-char start)
    (insert (concat (cond
                     ((eq askk--conversion-mode 'composing)
                      (char-to-string askk-composing-prompt))
                     ((eq askk--conversion-mode 'selecting)
                      (char-to-string askk-selecting-prompt)))
                    askk-kana--default-string
                    (and askk-kana--additional-flag
                         (eq askk--conversion-mode 'composing)
                         (askk-okurigana--prompt-string))
                    askk-kana--additional-string))
    (when (and (eq askk--conversion-mode 'selecting) (askk-cand--current))
      (askk-cand--make-overlay))
    (askk-trans--show-or-cleanup-events)
    (setf (overlay-end askk-preedit--overlay) (point))
    (when (memq modified '(nil autosaved))
      (restore-buffer-modified-p modified))))

(defun askk-preedit--to-list ()
  (and askk-kana--default-string
       (string-to-list (concat askk-kana--default-string
                               askk-kana--additional-string))))

;;; Transliteration

(defvar askk-trans--root nil)
(defvar-local askk-trans--node nil)
(defvar-local askk-trans--events nil)
(defvar-local askk-trans--overlay nil)

(defun askk-trans--root ()
  (or askk-trans--root (setq askk-trans--root (askk-trans--make))))

(defun askk-trans--make ()
  (let ((root (askk-trans--node-make)))
    (dolist (alist (list askk-transliteration-alist
                         askk-transliteration-base-alist))
      (dolist (kv alist)
        (askk-trans--node-insert root (car kv) (cdr kv))))
    root))

(define-inline askk-trans--node-make (&optional event value children)
  (inline-quote (list ,event ,value ,children)))

(define-inline askk-trans--node-value (node)
  (inline-quote (nth 1 ,node)))

(define-inline askk-trans--node-children (node)
  (inline-quote (nth 2 ,node)))

(defun askk-trans--node-insert (node key value)
  (seq-doseq (c key)
    (setq node (or (assq c (askk-trans--node-children node))
                   (car (push (askk-trans--node-make c)
                              (askk-trans--node-children node))))))
  (unless (askk-trans--node-value node)
    (setf (askk-trans--node-value node) value)))

(defun askk-trans--node ()
  (or askk-trans--node (setq askk-trans--node (askk-trans--root))))

(defun askk-trans--next-node (event &optional node)
  (assq event (askk-trans--node-children (or node (askk-trans--node)))))

(defun askk-trans--string ()
  (propertize (concat (reverse askk-trans--events)
                      (and (not askk-kana--additional-flag)
                           (askk-okurigana--prompt-string)))
              'face `(:foreground ,(askk--input-mode-color))))

(defun askk-trans--show-or-cleanup-events ()
  (if (eq askk-trans--node askk-trans--root)
      (askk-trans--cleanup)
    (let ((pos (point)))
      (if (overlayp askk-trans--overlay)
          ;; 促音のような transliteration の値の文字列とキーイベントの両方が
          ;; 非 nil の場合のために overlay を文字列の後ろに動かす。
          (move-overlay askk-trans--overlay pos pos)
        (setq askk-trans--overlay (make-overlay pos pos))))
    (overlay-put askk-trans--overlay 'after-string (askk-trans--string))))

(defun askk-trans--cleanup ()
  (when (overlayp askk-trans--overlay)
    (delete-overlay askk-trans--overlay))
  (setq askk-trans--node nil)
  (setq askk-trans--events nil)
  (setq askk-trans--overlay nil))

(defun askk-trans--commit (&optional value)
  (or value (setq value (askk-trans--node-value askk-trans--node)))
  (setq askk-trans--node askk-trans--root)
  (setq askk-trans--events nil)
  (when value
    (when (functionp value)
      (setq value (funcall value)))
    (when-let* ((str (car value)))
      (askk--output-commit str))
    (when-let* ((event (cdr value)))
      (askk-trans--transliterate event)))
  t)

(defun askk-trans--transliterate (event &optional default-function)
  (or default-function (setq default-function #'askk--output-commit))
  (let ((node (askk-trans--next-node event)))
    (if (askk-trans--node-children node)
        (progn
          (setq askk-trans--node node)
          (push event askk-trans--events))
      (if-let* ((value (askk-trans--node-value node)))
          (askk-trans--commit value)
        (if (eq askk-trans--node askk-trans--root)
            (funcall default-function event)
          (askk-trans--commit)
          (askk-trans--transliterate event default-function))))))

;;; Transliteration Functions

(defun askk-ascii-mode ()
  (unless (eq askk--conversion-mode 'normal)
    (askk-kana--normal))
  (askk--update-input-mode 'ascii))

(defun askk-fullwidth-ascii-mode ()
  (unless (eq askk--conversion-mode 'normal)
    (askk-kana--normal))
  (askk--update-input-mode 'fullwidth-ascii))

(defun askk-start-composing ()
  (unless (eq askk--conversion-mode 'normal)
    (askk-kana--normal))
  (askk-kana--composing)
  nil)

(defun askk-abbrev-mode ()
  (when (eq askk--conversion-mode 'normal)
    (setq askk--abbrev-flag t)
    (askk-kana--composing)
    (askk--update-input-mode 'abbrev)))

(defun askk-toggle-kana ()
  (cond
   ((eq askk--input-mode 'hiragana)
    (askk-katakana-mode))
   ((eq askk--input-mode 'katakana)
    (askk-hiragana-mode))))

;;; Headword and Okurigana

(defvar-local askk-headword--start nil)
(defvar-local askk-headword--string nil)
(defvar-local askk-headword--input-string nil)

(defvar-local askk-okurigana--prompt-flag nil)
(defvar-local askk-okurigana--event nil)
(defvar-local askk-okurigana--string nil)

(defun askk-headword--cleanup ()
  (setq askk-headword--start nil)
  (setq askk-headword--string nil)
  (setq askk-headword--input-string nil))

(defun askk-headword--empty-p ()
  (and (null askk-kana--default-string)
       (= (point) (1+ askk-headword--start))))

(defun askk-headword--make ()
  (let (cs)
    (seq-doseq (c askk-headword--input-string)
      (unless (memq c '(?\s ?\n))
        (push (askk--kata2hira c) cs)))
    (when askk-okurigana--event
      (push askk-okurigana--event cs))
    (concat (nreverse cs))))

(defun askk-okurigana--cleanup ()
  (setq askk-kana--additional-flag nil)
  (setq askk-okurigana--prompt-flag nil)
  (setq askk-okurigana--event nil)
  (setq askk-okurigana--string nil))

(defun askk-okurigana--start (&optional event)
  (when event
    (setq askk-okurigana--event event))
  (unless (memq event '(?a ?i ?u ?e ?o))
    (setq askk-okurigana--prompt-flag t))
  (when (or event (eq askk-trans--node askk-trans--root))
    (setq askk-kana--additional-flag t)))

(defun askk-okurigana--prompt-string ()
  (and askk-okurigana--prompt-flag (char-to-string askk-okurigana-prompt)))

;;; Candidate

(defun askk-candidates-inplace-style (method)
  (and (eq method :page-size) 1))

(defun askk--candidates-page-size ()
  (funcall askk-candidates-style-function :page-size))

(defvar-local askk-cand--candidates nil)
(defvar-local askk-cand--index nil)
(defvar-local askk-cand--overlay nil)

(defun askk-cand--current ()
  (nth askk-cand--index askk-cand--candidates))

(defun askk-cand--make-overlay ()
  (let* ((start (1+ (overlay-start askk-preedit--overlay)))
         (end (+ start (length askk-kana--default-string))))
    (if (overlayp askk-cand--overlay)
        (move-overlay askk-cand--overlay start end)
      (setq askk-cand--overlay (make-overlay start end))
      (overlay-put askk-cand--overlay 'face 'askk-candidate-preview))))

(defun askk-cand--cleanup ()
  (when (overlayp askk-cand--overlay)
    (delete-overlay askk-cand--overlay))
  (setq askk-cand--candidates nil)
  (setq askk-cand--index nil)
  (setq askk-cand--overlay nil))

(defun askk-cand--lookup ()
  (setq askk-headword--input-string askk-kana--default-string)
  (setq askk-headword--string (askk-headword--make))
  (setq askk-okurigana--string
        (and askk-okurigana--event
             (apply #'string (mapcar #'askk--kata2hira
                                     askk-kana--additional-string))))
  (setq askk-cand--index 0)
  (setq askk-cand--candidates
        (delete-dups (mapcan (lambda (source)
                               (apply (car source)
                                      `(,askk-headword--string
                                        ,askk-okurigana--string
                                        ,@(cdr source))))
                             askk-lookup-sources))))

(defun askk-next-candidate ()
  (interactive "*")
  (setq askk-cand--index (1+ askk-cand--index))
  (askk--handle-candidates))

(defun askk-previous-candidate ()
  (interactive "*")
  (setq askk-cand--index (1- askk-cand--index))
  (askk--handle-candidates))

(defun askk-candidates-next-page ()
  (interactive "*")
  (setq askk-cand--index (min (+ askk-cand--index (askk--candidates-page-size))
                              (length askk-cand--candidates)))
  (askk--handle-candidates))

(defun askk-candidates-previous-page ()
  (interactive "*")
  (setq askk-cand--index (max (- askk-cand--index (askk--candidates-page-size))
                              -1))
  (askk--handle-candidates))

(defun askk-delete-candidate ()
  (interactive "*")
  (let ((candidate (askk-cand--current)))
    (when (yes-or-no-p (concat "Delete "
                               (askk--format-headword-and-okurigana)
                               "【" (car candidate) "】?"))
      (askk-user-dict--delete-entry askk-headword--string
                                    askk-okurigana--string
                                    candidate)
      (setq askk-kana--default-string nil)
      (setq askk-kana--additional-string nil)
      (setq askk-kana--additional-flag nil)
      (askk-kana--normal)
      (funcall askk-candidates-style-function :hide))))

(defun askk--register-new-candidate ()
  (if-let* ((str (read-string (concat "Register "
                                      (askk--format-headword-and-okurigana)
                                      ": ")
                              nil t nil t))
            ((not (string-empty-p str)))
            (candidate (cons str nil)))
      (progn
        (askk-user-dict--add-entry askk-headword--string
                                   askk-okurigana--string
                                   candidate)
        (setq askk-kana--default-string (car candidate))
        (askk-kana--normal))
    (askk-previous-candidate)))

(defun askk--format-headword-and-okurigana ()
  (if askk-okurigana--string
      (concat (substring askk-headword--string nil -1)
              (char-to-string askk-okurigana-prompt)
              askk-okurigana--string)
    askk-headword--string))

(defun askk--handle-candidates ()
  (cond
   ((= askk-cand--index -1)
    (askk-kana--composing)
    (funcall askk-candidates-style-function :hide))
   ((= askk-cand--index (length askk-cand--candidates))
    (funcall askk-candidates-style-function :hide)
    (askk--register-new-candidate))
   (t
    (setq askk-kana--default-string (car (askk-cand--current)))
    (funcall askk-candidates-style-function :show))))

;;; Kana

(define-inline askk--hira2kata (c)
  (inline-letevals (c)
    (inline-quote (if (<= ?ぁ ,c ?ゖ) (+ ,c (- ?ア ?あ)) ,c))))

(define-inline askk--kata2hira (c)
  (inline-letevals (c)
    (inline-quote (if (<= ?ァ ,c ?ヶ) (- ,c (- ?ア ?あ)) ,c))))

(defun askk-kana--normal ()
  (setq askk--conversion-mode 'normal)
  (setq askk--abbrev-flag nil)
  (askk--update-input-mode)
  (askk-trans--cleanup)
  (askk-headword--cleanup)
  (askk-okurigana--cleanup)
  (askk-cand--cleanup))

(defun askk-kana--composing ()
  (when (eq askk--conversion-mode 'selecting)
    (setq askk-kana--default-string (concat askk-headword--input-string
                                            askk-kana--additional-string))
    (setq askk-kana--additional-string nil)
    (setq askk-kana--additional-flag nil)
    (setq askk-kana--last-substring nil)
    (askk-okurigana--cleanup)
    (askk-cand--cleanup))
  (setq askk-headword--start (overlay-start askk-preedit--overlay))

  (setq askk--conversion-mode 'composing)
  (askk--enable-keymap askk-kana-mode-map))

(defun askk-kana--selecting ()
  (setq askk--conversion-mode 'selecting)
  (askk--enable-keymap askk-kana-selecting-mode-map))

(defun askk-kana--handle-normal (event)
  (if (eq event askk-sticky-shift)
      (progn
        (askk-trans--commit)
        (askk-kana--composing))
    (unless (askk-trans--transliterate event #'ignore)
      (if (<= ?A event ?Z)
          (progn
            (askk-kana--composing)
            (askk-trans--transliterate (downcase event)))
        (askk--output-commit event)))))

(defun askk-kana--handle-composing (event)
  (cond
   ((= event ?\s)
    (askk-trans--commit)
    (if (askk-headword--empty-p)
        (askk-kana--normal)
      (askk-cand--lookup)
      (askk-kana--selecting)))
   (askk--abbrev-flag
    (askk--output-commit event))
   ((eq event askk-sticky-shift)
    (when (askk-trans--node-value (askk-trans--node))
      (askk-trans--commit))
    (if (askk-headword--empty-p)
        (progn
          (askk-trans--transliterate event)
          (when (eq askk-trans--node askk-trans--root)
            (askk-kana--normal)))
      (unless askk-okurigana--prompt-flag
        (askk-okurigana--start))))
   ((and askk-okurigana--prompt-flag
         (null askk-okurigana--event)
         (or (<= ?A event ?Z) (<= ?a event ?z)))
    (unless (prog1 (eq (askk-trans--node) askk-trans--root)
              (setq event (downcase event))
              (setq askk-okurigana--event event)
              (askk-trans--transliterate event))
      (askk-okurigana--start event)))
   ((and (<= ?A event ?Z)
         (null (askk-trans--next-node event))
         (null (askk-trans--next-node event askk-trans--root)))
    (setq event (downcase event))
    (if (or askk-okurigana--prompt-flag (askk-headword--empty-p))
        (askk-trans--transliterate event)
      (if (memq event '(?a ?i ?u ?e ?o))
          (progn
            (askk-okurigana--start event)
            (askk-trans--transliterate event))
        (askk-trans--transliterate event)
        (askk-okurigana--start event))))
   (t
    (unless (askk-trans--transliterate event #'ignore)
      (cond
       ((= event ?>)
        (unless (prog1 (askk-headword--empty-p)
                  (askk--output-commit event))
          (askk-cand--lookup)
          (askk-kana--selecting)))
       (t
        (askk--output-commit event))))))

  (when (and (eq askk--conversion-mode 'composing)
             (eq (askk-trans--node) askk-trans--root)
             (or (and askk-okurigana--event
                      (or (memq event '(?a ?i ?u ?e ?o))
                          (= event askk-okurigana--event ?n)))
                 (and (member askk-kana--last-substring
                              askk-auto-conversion-triggers)
                      (not (string= askk-kana--last-substring
                                    askk-kana--default-string))
                      (setq askk-kana--default-string
                            (substring
                             askk-kana--default-string
                             0 (- (length askk-kana--last-substring))))
                      (setq askk-kana--additional-string
                            (concat askk-kana--additional-string
                                    askk-kana--last-substring))
                      (setq askk-kana--additional-flag t))))
    (askk-cand--lookup)
    (askk-kana--selecting)))

(defvar askk--conversion-mode-handlers
  '((normal . askk-kana--handle-normal)
    (composing . askk-kana--handle-composing)))

(defun askk-kana--commit ()
  (interactive "*")
  (if (eq askk--conversion-mode 'selecting)
      (progn
        (askk-user-dict--add-entry askk-headword--string
                                   askk-okurigana--string
                                   (askk-cand--current))
        (funcall askk-candidates-style-function :hide))
    (askk-trans--commit))
  (unless (eq askk--conversion-mode 'normal)
    (askk-kana--normal)))

(defun askk-delete-backward-char (n)
  (interactive "p")
  (when (eq askk--conversion-mode 'selecting)
    (askk-kana--commit))

  ;; events の最後が okurigana prompt
  (when (and (> n 0)
             askk-okurigana--prompt-flag
             (not askk-kana--additional-flag))
    (setq n (1- n))
    (askk-okurigana--cleanup))

  (let ((new_n (- n (length askk-trans--events))))
    (if (< new_n 0)
        (progn
          (setq askk-trans--events (nthcdr n askk-trans--events))
          (setq askk-trans--node askk-trans--root)
          (dolist (event (reverse askk-trans--events))
            (setq askk-trans--node (askk-trans--next-node event))))
      (setq askk-trans--events nil)
      (setq askk-trans--node askk-trans--root)
      (setq n new_n)

      (when (> n 0)
        (setq new_n (- n (length askk-kana--additional-string)))
        (if (< new_n 0)
            (setq askk-kana--additional-string
                  (substring askk-kana--additional-string 0 (- n)))
          (setq askk-kana--additional-string nil))
        (setq n new_n)

        ;; default string と additional string の間が okurigana prompt
        (when (and (> n 0) askk-okurigana--prompt-flag)
          (setq n (1- n))
          (askk-okurigana--cleanup))

        (when (and (> n 0) askk-kana--default-string)
          (setq new_n (- n (length askk-kana--default-string)))
          (if (< new_n 0)
              (setq askk-kana--default-string
                    (substring askk-kana--default-string 0 (- n)))
            (setq askk-kana--default-string nil))
          (setq n new_n))

        (when (and (> n 0) (eq askk--conversion-mode 'composing))
          (setq n (1- n))
          (askk-kana--normal))

        (when (> n 0)
          (funcall-interactively #'delete-backward-char n))))))

;;; Input method

(defun askk-input-method (key)
  (cond
   ((or buffer-read-only
        overriding-terminal-local-map
        overriding-local-map
        (eq askk--input-mode 'ascii))
    (list key))
   ((eq askk--input-mode 'fullwidth-ascii)
    (list (or (aref askk-fullwidth-ascii-table key) key)))
   (t
    (askk-preedit--setup)
    (with-silent-modifications
      (unwind-protect
          (let ((input-method-function nil)
                (echo-keystrokes 0))
            (askk--handle-event key)
            (while (not (and (eq askk--conversion-mode 'normal)
                             (eq (askk-trans--node) askk-trans--root)))
              (askk--handle-event (read-event)))
            (askk-preedit--to-list))
        (askk-preedit--teardown))))))

(defun askk--handle-event (event)
  (let ((command (keymap-lookup (askk--current-keymap)
                                (single-key-description event))))
    (cond
     (command
      (setq last-command-event event)
      (setq last-command this-command)
      (setq this-command command)
      (command-execute command))
     ((or (not (and (characterp event) (<= ?\s event 255) (/= event 127)))
          (eq askk--conversion-mode 'selecting)
          (and (eq askk--conversion-mode 'normal)
               (null (askk-trans--next-node event))
               (askk-trans--node-value askk-trans--node)))
      (askk-kana--commit)
      (askk--make-event-unread event))
     (t
      (funcall (alist-get askk--conversion-mode askk--conversion-mode-handlers)
               event)
      (askk-preedit--update)
      (when (eq askk--conversion-mode 'selecting)
        (askk--handle-candidates)))))
  (askk-preedit--update))

(defun askk--make-event-unread (event)
  (setq unread-command-events
        (append (listify-key-sequence (list event)) unread-command-events)))

;;; Completion

(defun askk-completion-at-point ()
  "前方一致する見出し語を登録順に表示するための capf。

Emacs 29 では compat パッケージを使っても、capf から :display-sort-function
を効かせることはできず見出し語が `completions-sort' の値によってソートされる。
ただし補完のために corfu パッケージを使っている場合は、corfu 1.6 では
corfu から :display-sort-function が使われるため見出し語は登録順に表示される。"
  (and askk-headword--start
       (eq askk--conversion-mode 'composing)
       (let ((start (1+ askk-headword--start))
             (end (point)))
         (and (< start end)
              (list start end
                    (askk-user-dict--prefix-keys
                     (buffer-substring-no-properties start end))
                    :display-sort-function #'identity)))))

;;; Minor modes

(define-minor-mode askk-mode
  "ASKK input method."
  :lighter nil
  (cond
   (askk-mode
    (askk-user-dict-load)
    (askk-hiragana-mode)
    (askk-kana--normal)

    (when (eq (selected-window) (minibuffer-window))
      (add-hook 'minibuffer-exit-hook #'askk-exit-from-minibuffer))
    (add-hook 'completion-at-point-functions #'askk-completion-at-point nil 't)
    (add-hook 'window-selection-change-functions #'askk--restore-keymap nil t))
   (t
    (remove-hook 'window-selection-change-functions #'askk--restore-keymap t)
    (remove-hook 'completion-at-point-functions #'askk-completion-at-point t)

    (funcall askk-candidates-style-function :cleanup)
    (askk-trans--cleanup)
    (askk-headword--cleanup)
    (askk-okurigana--cleanup)
    (askk-cand--cleanup)
    (askk-preedit--cleanup)
    (setq askk--input-mode nil))))

(defun askk-exit-from-minibuffer ()
  (deactivate-input-method)
  (when (<= (minibuffer-depth) 1)
    (remove-hook 'minibuffer-exit-hook #'askk-exit-from-minibuffer)))

(defun askk--restore-keymap (window)
  (when (and askk-mode (eq (selected-window) window))
    (askk--enable-keymap (askk--current-keymap))))

(defvar askk-cursor--default-color nil)

;;;###autoload
(define-minor-mode askk-cursor-color-mode
  "ASKK cursor color mode."
  :global t
  (cond
   (askk-cursor-color-mode
    (or askk-cursor--default-color
        (setq askk-cursor--default-color (face-attribute 'cursor :background)))
    (add-hook 'askk--input-mode-hook #'askk-cursor--update)
    (add-hook 'post-command-hook #'askk-cursor--update))
   (t
    (remove-hook 'askk--input-mode-hook #'askk-cursor--update)
    (remove-hook 'post-command-hook #'askk-cursor--update))))

(defun askk-cursor--update ()
  (set-face-attribute 'cursor (selected-frame)
                      :background (if askk-mode
                                      (askk--input-mode-color)
                                    askk-cursor--default-color)))

;;;###autoload
(defun askk-register-input-method ()
  (register-input-method
   "japanese-askk"
   "Japanese"
   #'askk-activate
   "__"
   "Japanese input method ASKK."))

(defun askk-activate (_input-method)
  (setq deactivate-current-input-method-function #'askk-deactivate)
  (askk-mode)
  (setq-local input-method-function #'askk-input-method))

(defun askk-deactivate ()
  (askk-mode -1)
  (kill-local-variable 'input-method-function))

(provide 'askk)
;;; askk.el ends here
