;;; askk.el --- ASKK -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2024 synrbb
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

(defcustom askk-candidates-style nil
  "候補の一覧のスタイル。"
  :type '(choice (const nil)
                 symbol))

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

(defcustom askk-transliteration-alist
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
    ("Q" . askk-start-composing)
    ("/" . askk-abbrev-mode)
    ("l" . askk-ascii-mode)
    ("q" . askk-toggle-kana))
  "ひらがな／カタカナモードでの transliteration 定義の連想リスト。
連想リストの値は、変換結果の文字列と自動入力文字の cons セル、または
そのような cons セルを返す関数。"
  :type '(alist :key-type string
                :value-type (choice (cons string (choice (const nil)
                                                         character))
                                    symbol)))

(defgroup askk-faces nil
  "Faces for ASKK"
  :group 'askk
  :group 'faces)

(defface askk-candidate-preview
  '((t :inherit highlight))
  "Face for candidate preview.")

;;; Input modes

(defvar-keymap askk-ascii-mode-map
  :doc "Keymap for ASCII mode."
  "C-j" #'askk-hiragana-mode)

(defvar askk-fullwidth-ascii-mode-map
  (eval-when-compile
    (let ((map (define-keymap
                 :full t
                 "C-j" #'askk-hiragana-mode
                 "C-q" #'askk-katakana-mode))
          (c ?\s))
      (while (<= c ?~)
        (keymap-set map (single-key-description c)
                    #'askk-fullwidth-ascii-insert)
        (setq c (1+ c)))
      map))
  "Keymap for fullwidth ASCII mode.")

(defvar askk-kana-mode-map
  (eval-when-compile
    (let ((map (define-keymap
                 :full t
                 "C-j" #'askk-kana--commit
                 "DEL" #'askk-delete-backward-char
                 "RET" #'askk-newline))
          (c ?\s))
      (while (<= c ?~)
        (keymap-set map (single-key-description c) #'askk-kana--handle-event)
        (setq c (1+ c)))
      map))
  "Keymap for normal or composing conversion mode.")

(defvar askk-kana-selecting-mode-map
  (eval-when-compile
    (let ((map (define-keymap
                 :full t
                 "C-j" #'askk-kana--commit
                 "C-n" #'askk-next-candidate
                 "C-p" #'askk-previous-candidate
                 "C-v" #'askk-candidates-next-page
                 "M-v" #'askk-candidates-previous-page
                 "DEL" #'askk-delete-backward-char
                 "RET" #'askk-newline
                 "SPC" #'askk-next-candidate
                 "X" #'askk-delete-candidate
                 "x" #'askk-previous-candidate))
          (c ?\s)
          key)
      (while (<= c ?~)
        (setq key (single-key-description c))
        (unless (keymap-lookup map key)
          (keymap-set map key #'askk-commit-and-handle-event))
        (setq c (1+ c)))
      map))
  "Keymap for selecting conversion mode.")

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

(defun askk-fullwidth-ascii-insert (n)
  (interactive "*p")
  (let ((c (aref askk-fullwidth-ascii-table last-command-event)))
    (when c
      ;; Set last-command-event for electric-pair-mode
      (setq last-command-event c))
    (self-insert-command n c)))

(put 'askk-fullwidth-ascii-insert
     'delete-selection 'delete-selection-uses-region-p)

;;; Output

(defvar askk--output nil)

(defun askk--output-commit (obj)
  (when (characterp obj)
    (setq obj (char-to-string obj)))
  (when-let* ((func (plist-get (askk--input-mode-plist) :convert-function)))
    (setq obj (concat (mapcar func obj))))
  (push obj askk--output))

(defun askk--output-length ()
  (apply #'+ (mapcar #'length askk--output)))

(defun askk--output-flush ()
  (when askk--output
    (dolist (str (nreverse askk--output))
      (seq-doseq (c str)
        ;; Set last-command-event for electric-pair-mode
        (setq last-command-event c)
        (self-insert-command 1 c)))
    (setq askk--output nil)))

;;; Transliteration

(defvar askk-trans--root nil)
(defvar-local askk-trans--node nil)
(defvar-local askk-trans--events nil)
(defvar-local askk-trans--overlay nil)

(defun askk-trans--root ()
  (or askk-trans--root
      (setq askk-trans--root
            (let ((root (askk-trans--node-make)))
              (dolist (kv askk-transliteration-alist)
                (askk-trans--node-insert root (car kv) (cdr kv)))
              root))))

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
  (setf (askk-trans--node-value node) value))

(defun askk-trans--node ()
  (or askk-trans--node (setq askk-trans--node (askk-trans--root))))

(defun askk-trans--next-node (event &optional node)
  (assq event (askk-trans--node-children (or node (askk-trans--node)))))

(defun askk-trans--string ()
  (propertize (apply #'string (reverse askk-trans--events))
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

(defun askk-trans--cleanup-if-moved ()
  (when-let* (((overlayp askk-trans--overlay))
              (pos (overlay-start askk-trans--overlay))
              ((/= pos (point))))
    (askk-trans--cleanup)))

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
(defvar-local askk-headword--end nil)
(defvar-local askk-headword--string nil)
(defvar-local askk-headword--input-string nil)

(defvar-local askk-okurigana--start nil)
(defvar-local askk-okurigana--event nil)
(defvar-local askk-okurigana--string nil)

(defun askk-headword--cleanup ()
  (when askk-headword--start
    (delete-region askk-headword--start (1+ askk-headword--start))
    (setq askk-headword--start nil))
  (when (markerp askk-headword--end)
    (set-marker askk-headword--end nil))
  (setq askk-headword--end nil)
  (setq askk-headword--string nil)
  (setq askk-headword--input-string nil))

(defun askk-headword--empty-p ()
  (and (null askk--output)
       (= (point) (1+ askk-headword--start))))

(defun askk-headword--make ()
  (let (cs)
    (seq-doseq (c askk-headword--input-string)
      (unless (memq c '(?\s ?\n))
        (push (askk--kata2hira c) cs)))
    (when askk-okurigana--event
      (push askk-okurigana--event cs))
    (concat (nreverse cs))))

(defun askk-headword--replace (str &optional prompt)
  (save-excursion
    (goto-char (+ askk-headword--start (if prompt 0 1)))
    (insert (or prompt "") str)
    (delete-char (- askk-headword--end (point)))))

(defun askk-headword--start ()
  (setq askk-headword--start (+ (point) (askk--output-length)))
  (askk--output-commit askk-composing-prompt))

(defun askk-okurigana--cleanup ()
  (setq askk-okurigana--start nil)
  (setq askk-okurigana--event nil)
  (setq askk-okurigana--string nil))

(defun askk-okurigana--delete-prompt ()
  (when (and askk-okurigana--start
             (eq (char-after askk-okurigana--start) askk-okurigana-prompt))
    (delete-region askk-okurigana--start (1+ askk-okurigana--start))
    (setq askk-okurigana--start nil)))

(defun askk-okurigana--start (&optional event)
  (when event
    (setq askk-okurigana--event event))
  (unless (memq event '(?a ?i ?u ?e ?o))
    (setq askk-okurigana--start (+ (point) (askk--output-length)))
    (askk--output-commit askk-okurigana-prompt))
  ;; sticky shift が促音にかかる場合
  (unless (or event (eq askk-trans--node askk-trans--root))
    (pop askk--output)
    (push askk-okurigana-prompt askk-trans--events)))

;;; Candidate

(defvar askk-candidates-style-alist
  '((posframe
     :show askk-posframe-show
     :hide askk-posframe-hide
     :cleanup askk-posframe-cleanup
     :page-size askk-posframe-page-size)))

(defun askk--candidates-style-handle (method)
  (when-let* ((func (plist-get (alist-get askk-candidates-style
                                          askk-candidates-style-alist)
                               method)))
    (funcall func)))

(defun askk--candidates-page-size ()
  (or (askk--candidates-style-handle :page-size) 1))

(defvar-local askk-cand--candidates nil)
(defvar-local askk-cand--index nil)
(defvar-local askk-cand--overlay nil)

(defun askk-cand--cleanup ()
  (when (overlayp askk-cand--overlay)
    (delete-overlay askk-cand--overlay))
  (setq askk-cand--candidates nil)
  (setq askk-cand--index nil)
  (setq askk-cand--overlay nil))

(defun askk-cand--lookup (&optional okurigana-suffix)
  (setq askk-headword--input-string
        (apply #'concat
               (buffer-substring-no-properties (1+ askk-headword--start)
                                               (or askk-okurigana--start
                                                   (point)))
               (nreverse askk--output)))
  (setq askk-headword--string (askk-headword--make))
  (setq askk-okurigana--string
        (and askk-okurigana--event
             (concat (and askk-okurigana--start
                          (mapcar #'askk--kata2hira
                                  (buffer-substring-no-properties
                                   (1+ askk-okurigana--start)
                                   (point))))
                     (mapcar #'askk--kata2hira okurigana-suffix))))
  (setq askk--output nil)
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
  (let ((candidate (nth askk-cand--index askk-cand--candidates)))
    (when (yes-or-no-p (concat "Delete "
                               (askk--format-headword-and-okurigana)
                               "【" (car candidate) "】?"))
      (askk-user-dict--delete-entry askk-headword--string
                                    askk-okurigana--string
                                    candidate)
      (when askk-okurigana--string
        (delete-region askk-headword--end (point)))
      (askk-headword--replace "")
      (askk-kana--normal)
      (askk--candidates-style-handle :hide))))

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
        (askk-headword--replace (car candidate))
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
    (askk--candidates-style-handle :hide))
   ((= askk-cand--index (length askk-cand--candidates))
    (askk--candidates-style-handle :hide)
    (askk--register-new-candidate))
   (t
    (askk--preview-candidate)
    (askk--candidates-style-handle :show))))

(defun askk--preview-candidate ()
  (let ((candidate (nth askk-cand--index askk-cand--candidates))
        (beg (1+ askk-headword--start))
        (end askk-headword--end))
    (askk-headword--replace (car candidate))
    (if (overlayp askk-cand--overlay)
        (move-overlay askk-cand--overlay beg end)
      (setq askk-cand--overlay (make-overlay beg end))))
  (overlay-put askk-cand--overlay 'face 'askk-candidate-preview))

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
  (setq askk--conversion-mode 'composing)
  (askk--enable-keymap askk-kana-mode-map)
  (if askk-headword--start
      (progn
        (askk-headword--replace askk-headword--input-string
                                askk-composing-prompt)
        (set-marker askk-headword--end nil)
        (setq askk-headword--end nil)
        (askk-okurigana--cleanup)
        (askk-cand--cleanup))
    (askk-headword--start)))

(defun askk-kana--selecting ()
  (setq askk--conversion-mode 'selecting)
  (askk--enable-keymap askk-kana-selecting-mode-map)
  (setq askk-headword--end
        (set-marker (make-marker) (or askk-okurigana--start (point))))
  (askk-headword--replace askk-headword--input-string
                          askk-selecting-prompt)
  (askk-okurigana--delete-prompt))

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
      (unless askk-okurigana--start
        (askk-okurigana--start))))
   ((and askk-okurigana--start
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
    (unless (prog1 (or askk-okurigana--start (askk-headword--empty-p))
              (askk-trans--transliterate event))
      (askk-okurigana--start event)))
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

  (when-let* (((eq askk--conversion-mode 'composing))
              ((eq askk-trans--node askk-trans--root))
              (str (pop askk--output)))
    (when (or (and askk-okurigana--event
                   (or (memq event '(?a ?i ?u ?e ?o))
                       (= event askk-okurigana--event ?n)))
              (and (member str askk-auto-conversion-triggers)
                   (not (askk-headword--empty-p))))
      (askk-cand--lookup (and askk-okurigana--event str))
      (askk-kana--selecting))
    (push str askk--output)))

(defvar askk--conversion-mode-handlers
  '((normal . askk-kana--handle-normal)
    (composing . askk-kana--handle-composing)))

(defun askk-kana--handle-event ()
  (interactive "*")
  (funcall (alist-get askk--conversion-mode askk--conversion-mode-handlers)
           last-command-event)
  (askk--output-flush)
  (askk-trans--show-or-cleanup-events)
  (when (eq askk--conversion-mode 'selecting)
    (askk--handle-candidates)))

(put 'askk-kana--handle-event
     'delete-selection 'delete-selection-uses-region-p)

(defun askk-kana--commit ()
  (interactive "*")
  (if (eq askk--conversion-mode 'selecting)
      (let ((candidate (nth askk-cand--index askk-cand--candidates)))
        (askk-user-dict--add-entry askk-headword--string
                                   askk-okurigana--string
                                   candidate)
        (askk--candidates-style-handle :hide))
    (askk-trans--commit))
  (unless (eq askk--conversion-mode 'normal)
    (askk-kana--normal))
  (askk--output-flush))

(defun askk-commit-and-handle-event ()
  (interactive "*")
  (askk-kana--commit)
  (askk-kana--handle-event))

(defun askk-newline (&optional _arg _interactive)
  (interactive "*P\np")
  (if (minibufferp)
      (exit-minibuffer)
    (askk-kana--commit)
    (call-interactively #'newline)))

(put 'askk-newline 'delete-selection t)

(defun askk-delete-backward-char (n)
  (interactive "p")
  (when (eq askk--conversion-mode 'selecting)
    (askk-kana--commit))
  (let ((remaining (- n (length askk-trans--events))))
    (when (and (> n 0)
               askk-okurigana--start
               (not (eq (char-after askk-okurigana--start)
                        askk-okurigana-prompt)))
      ;; sticky shift が促音にかかっている場合で
      ;; askk-okurigana-prompt が askk-trans--events に push されている状態
      (setq askk-okurigana--start nil)
      (setq askk-okurigana--event nil))
    (if (< remaining 0)
        (dotimes (_ n)
          (pop askk-trans--events))
      (setq askk-trans--node askk-trans--root)
      (setq askk-trans--events nil)
      (when-let* ((askk-headword--start)
                  (pos (point))
                  (new-pos (- pos remaining)))
        (when (and askk-okurigana--start
                   (<= new-pos (1+ askk-okurigana--start) pos))
          (setq askk-okurigana--event nil)
          (when (<= new-pos askk-okurigana--start)
            (setq askk-okurigana--start nil)))
        (when (<= new-pos askk-headword--start pos)
          (setq remaining (1- remaining))
          (askk-kana--normal)))
      (funcall-interactively #'delete-backward-char remaining))
    (askk-trans--show-or-cleanup-events)))

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
    (add-hook 'post-command-hook #'askk-trans--cleanup-if-moved nil t)
    (add-hook 'window-selection-change-functions #'askk--restore-keymap nil t))
   (t
    (remove-hook 'window-selection-change-functions #'askk--restore-keymap t)
    (remove-hook 'post-command-hook #'askk-trans--cleanup-if-moved t)
    (remove-hook 'completion-at-point-functions #'askk-completion-at-point t)

    (askk--candidates-style-handle :cleanup)
    (askk-trans--cleanup)
    (askk-headword--cleanup)
    (askk-okurigana--cleanup)
    (askk-cand--cleanup)
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
    (add-hook 'post-command-hook #'askk-cursor--update))
   (t
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
   (lambda (_)
     (setq deactivate-current-input-method-function
           (lambda () (askk-mode -1)))
     (askk-mode 1))
   "__"
   "Japanese input method ASKK."))

(provide 'askk)
;;; askk.el ends here
