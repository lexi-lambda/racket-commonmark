#lang racket/base

(require (for-syntax racket/base)
         data/gvector
         racket/list
         racket/match
         racket/port
         racket/string
         threading

         "../regexp.rkt"
         "../struct.rkt"
         "common.rkt"
         "inline.rkt")

(provide read-document
         string->document
         current-parse-footnotes?)

;; -----------------------------------------------------------------------------

(define-for-syntax :eol (:: "\n|\r\n?|$"))
(define-for-syntax :codeblock-indent (:: " {4}| {,3}\t"))
(define-for-syntax :code-fence (:: "(`{3,})|~{3,}"))
(define-for-syntax :thematic-break (:: "([-_*])[ \t]*"
                                       (:* "\\1[ \t]*" #:min 2)
                                       :eol))

;; <https://spec.commonmark.org/0.30/#list-marker>
(define-for-syntax (:list-marker #:interrupt? interrupt?)
  ; If the list is ordered, it can only interrupt a paragraph if the
  ; start number is 1.
  (define :start-num (if interrupt?
                         "(0{,8}1)"
                         "([0-9]{1,9})"))
  ; When both a thematic break and a list item are possible interpretations of a
  ; line, the thematic break takes precedence.
  (:: (:not-ahead :thematic-break)
      (:or (:: :start-num
               "([.)])")
           "([-+*])")
      (if interrupt?
          ; If the list interrupts a paragraph, it must not start with a blank line.
          (:ahead "[ \t]" (:not-ahead "[ \t]*" (:group :eol)))
          ; If it doesn’t interrupt a paragraph, it may start with a blank line, but
          ; if it doesn’t, it must be followed by at least one space of indentation.
          (:or (:ahead "[ \t]*" (:group :eol))
               (:ahead "[ \t]")))))

;; -----------------------------------------------------------------------------

(define current-parse-footnotes? (make-parameter #f (λ (x) (and x #t))))

(struct o:blockquote (blocks) #:transparent)
(struct o:list  ; see Note [Lists overview]
  (blocks       ; blocks of the current open list item, or #f if no list item
                ;   is open; see Note [Open lists without an open item]
   blockss      ; blocks of closed list items
   indent       ; number of spaces needed to continue the open item
   marker-char  ; (or/c #\- #\+ #\* #\. #\))
   start-blank? ; #t if the open item starts with a blank line, otherwise #f;
                ;   see Note [Open lists without an open item]
   end-blank?   ; #t if the open item ends with a blank line, otherwise #f;
                ;   see Note [Open list tightness]
   style        ; (or/c 'tight 'loose)
   start-num)   ; (or/c exact-nonnegative-integer? #f)
  #:transparent)
(struct o:footnote-definition (blocks label) #:transparent)

; see Note [Open lists without an open item]
(define (accumulate-list-blockss open-list)
  (if (o:list-blocks open-list)
      (cons (reverse (o:list-blocks open-list))
            (o:list-blockss open-list))
      (o:list-blockss open-list)))

(struct o:indented-code-block (lines) #:transparent)
(struct o:fenced-code-block (lines indent fence-char fence-length info-string) #:transparent)
(struct o:paragraph (lines) #:transparent)
(struct o:html-block (lines end-type) #:transparent)

(define (leaf-needs-explicit-end? v)
  (or (o:fenced-code-block? v)
      (o:html-block? v)))

(define (string->document str)
  (read-document (open-input-string str)))

(define (read-document in)
  (define footnotes? (current-parse-footnotes?))

  (define root-blocks (make-gvector))
  (define link-reference-defns (make-hash))
  (define footnote-defns (make-gvector))
  (define footnote-defn-labels (make-hash))

  ;; ---------------------------------------------------------------------------
  ;; open blocks

  (define open-containers (make-gvector))
  (define entered-containers 0)
  (define open-leaf #f)

  ;; Adds a new block, closing the currently-open leaf block, if one exists.
  ;;
  ;; By default, `add-block!` also closes any unentered containers, which is
  ;; generally the right choice, since the start of a new block always closes
  ;; any unentered containers. However, functions like `close-leaf!` and
  ;; `close-container!` call `add-block!` to close a previously-opened block,
  ;; which should always be added to the innermost open container, so those
  ;; functions set #:close-entered? to #f.
  (define (add-block! block #:close-unentered? [close-unentered? #t])
    (when close-unentered?
      (close-unentered-containers!))
    (close-leaf!)
    (define ocs (gvector-count open-containers))
    (cond
      [(zero? ocs)
       (gvector-add! root-blocks block)]
      [else
       (define oc (gvector-ref open-containers (sub1 ocs)))
       (define oc*
         (match oc
           [(o:blockquote blocks)
            (o:blockquote (cons block blocks))]
           [(? o:list?)
            (struct-copy o:list oc
                         [blocks (cons block (o:list-blocks oc))]
                         [end-blank? #f]
                         ; see Note [Open list tightness]
                         [style (if (o:list-end-blank? oc)
                                    'loose
                                    (o:list-style oc))])]
           [(o:footnote-definition blocks label)
            (o:footnote-definition (cons block blocks) label)]))
       (gvector-set! open-containers (sub1 ocs) oc*)]))

  (define (open-leaf! new-open-leaf)
    (close-unentered-containers!)
    (close-leaf!)
    (set! open-leaf new-open-leaf))

  (define (open-container! open-container #:enter? [enter? #t])
    (close-unentered-containers!)
    (close-leaf!)
    (gvector-add! open-containers open-container)
    (when enter?
      (set! entered-containers (add1 entered-containers))))

  (define (close-leaf!)
    (when open-leaf
      (define leaf-to-close open-leaf)
      (set! open-leaf #f)
      (define new-block
        (match leaf-to-close
          [(o:indented-code-block lines)
           ; The spec says “Blank lines [...] following an indented code block are
           ; not included in it”, so drop trailing blank lines.
           (~> (dropf lines (λ~> (string=? "")))
               reverse
               (string-join "\n" #:after-last "\n")
               (code-block #f))]
          [(o:fenced-code-block lines _ _ _ info-string)
           (code-block (if (empty? lines)
                           ""
                           (string-join (reverse lines) "\n" #:after-last "\n"))
                       info-string)]
          [(o:paragraph lines)
           (define paragraph-str (close-paragraph-lines lines))
           (and (not (string=? paragraph-str ""))
                (paragraph paragraph-str))]
          [(o:html-block lines _)
           (html-block (string-join (reverse lines) "\n"))]))
      (when new-block
        (add-block! new-block #:close-unentered? #f))))

  (define (close-container!)
    ; If there’s an open leaf, we need to take care to close it /before/ we
    ; remove the open container.
    (close-leaf!)
    (define new-block
      (match (gvector-remove-last! open-containers)
        [(o:blockquote blocks)
         (blockquote (reverse blocks))]
        [(? o:list? open-list)
         ; see Note [Transfer `end-blank?` to parent lists]
         (when (and (o:list-end-blank? open-list)
                    (not (zero? (gvector-count open-containers))))
           (define last-idx (sub1 (gvector-count open-containers)))
           (define parent-container (gvector-ref open-containers last-idx))
           (when (o:list? parent-container)
             (gvector-set! open-containers
                           last-idx
                           (struct-copy o:list parent-container
                                        [end-blank? #t]))))

         (itemization (reverse (accumulate-list-blockss open-list))
                      (o:list-style open-list)
                      (o:list-start-num open-list))]
        [(o:footnote-definition blocks label)
         (gvector-add! footnote-defns (footnote-definition (reverse blocks) label))
         #f]))
    (when new-block
      (add-block! new-block #:close-unentered? #f)))

  (define (unentered-containers?)
    (< entered-containers (gvector-count open-containers)))

  (define (close-unentered-containers!)
    (when (unentered-containers?)
      (close-container!)
      (close-unentered-containers!)))

  ;; ---------------------------------------------------------------------------
  ;; indentation and block structure

  ; see Note [Tabs and tab stops]
  (define TAB-STOP-WIDTH 4)
  (define tab-stop-offset 0)
  (define partially-consumed-tab? #f)

  (define (advance-tab-stop-offset! amount)
    (set! tab-stop-offset (remainder (+ tab-stop-offset amount) TAB-STOP-WIDTH)))
  (define (current-leftover-indent)
    (if partially-consumed-tab?
        (- TAB-STOP-WIDTH tab-stop-offset)
        0))
  (define (current-tab-width [extra-offset 0])
    (- TAB-STOP-WIDTH (remainder (+ tab-stop-offset extra-offset) TAB-STOP-WIDTH)))

  ;; Attempts to consume the given number of spaces worth of identation,
  ;; handling tabs as appropriate; see Note [Tabs and tab stops] for details.
  (define (try-enter-indent needed-indent)
    (cond
      [(< needed-indent (current-leftover-indent))
       (set! tab-stop-offset (+ tab-stop-offset needed-indent))
       #t]
      [else
       (define (commit-indent pos partial-tab?)
         (read-bytes pos in)
         (advance-tab-stop-offset! needed-indent)
         (set! partially-consumed-tab? partial-tab?)
         #t)

       (let loop ([pos 0]
                  [indent (current-leftover-indent)])
         (cond
           [(< indent needed-indent)
            (match (peek-char in pos)
              [(or (? eof-object?) #\return #\newline)
               (read-bytes pos in)
               0]
              [#\space
               (loop (add1 pos) (add1 indent))]
              [#\tab
               (define new-indent (+ indent (current-tab-width indent)))
               (if (> new-indent needed-indent)
                   (commit-indent (add1 pos) #t)
                   (loop (add1 pos) new-indent))]
              [_ #f])]
           [else
            (commit-indent pos #f)]))]))

  ;; Attempts to peek through optional identation and immediately fails if there
  ;; is too much; see Note [Optional indentation] for details.
  (define (try-with-optional-indent proc)
    (let loop ([pos 0]
               [indent (current-leftover-indent)])
      (cond
        [(>= indent 4) #f]
        [else
         (match (peek-char in pos)
           [#\space
            (loop (add1 pos) (add1 indent))]
           [#\tab
            (loop (add1 pos) (+ indent (current-tab-width indent)))]
           [_
            (proc pos indent)])])))

  ;; Like `read-line`, but includes any indentation left over from a partially
  ;; consumed tab; see Note [Tabs and tab stops] for details.
  (define (read-line/leftover-indent [leftover-indent (current-leftover-indent)])
    (define spaces-str (leftover-indent-string leftover-indent))
    (define line-str (read-line in 'any))
    (set! tab-stop-offset 0)
    (set! partially-consumed-tab? #f)
    (match* {spaces-str line-str}
      [{"" _}              line-str]
      [{_ (? eof-object?)} spaces-str]
      [{_ _}               (string-append spaces-str line-str)]))

  ;; Like `read-line`, but handles any optional identation present at the start
  ;; of the line; see Note [Optional indentation] for details.
  (define (read-line/opt-indent)
    (define (commit-line indent)
      (values (min indent 3)
              (read-line/leftover-indent (max 0 (- indent 3)))))
    (let loop ([indent (current-leftover-indent)])
      (cond
        [(>= indent 3)
         (commit-line indent)]
        [else
         (match (peek-char in)
           [#\space
            (read-char in)
            (loop (add1 indent))]
           [#\tab
            (read-char in)
            (loop (+ indent (current-tab-width indent)))]
           [_
            (commit-line indent)])])))

  ;; Like `regexp-match-peek`, but handles any optional indentation present at
  ;; the start of the line; see Note [Optional indentation] for details.
  (define (regexp-peek/opt-indent pattern)
    (try-with-optional-indent
     (λ (pos indent)
       (match (regexp-match-peek pattern in pos)
         [#f #f]
         [(cons full-match group-matches)
          (list* full-match indent group-matches)]))))

  ;; Like `regexp-try-match`, but handles any optional indentation present at
  ;; the start of the line; see Note [Optional indentation] for details.
  (define (regexp-try/opt-indent pattern)
    (try-with-optional-indent
     (λ (pos indent)
       (match (regexp-try-match pattern in pos)
         [#f #f]
         [(cons full-match group-matches)
          (set! partially-consumed-tab? #f)
          (list* full-match indent group-matches)]))))

  ;; <https://spec.commonmark.org/0.30/#block-quote-marker>
  (define (try-read-blockquote-marker)
    (try-with-optional-indent
     (λ (pos indent)
       (match (peek-char in pos)
         [#\>
          (read-bytes (add1 pos) in)
          ; If a blockquote marker is followed by a space or tab, it consumes a
          ; single space of identation, which may result in a partially consumed
          ; tab; see Note [Tabs and tab stops] for details.
          (match (peek-char in)
            [#\space
             (read-char in)
             (advance-tab-stop-offset! (+ indent 2))
             (set! partially-consumed-tab? #f)]
            [#\tab
             (read-char in)
             (advance-tab-stop-offset! (+ indent 2))
             (set! partially-consumed-tab? (not (zero? tab-stop-offset)))]
            [_
             (advance-tab-stop-offset! (+ indent 1))
             (set! partially-consumed-tab? #f)])
          #t]
         [_ #f]))))

  ;; <https://spec.commonmark.org/0.30/#list-marker>
  (define (try-read-list-marker #:interrupt? interrupt?)
    (try-with-optional-indent
     (λ (pos indent)
       ; This is an unusually complicated regex, and we use a capture group in
       ; a lookahead expression to determine whether the list starts with a
       ; blank line, so we have to use `regexp-match-peek-positions` rather than
       ; `regexp-try-match` to work around <https://github.com/racket/racket/issues/4067>.
       (match (regexp-match-peek-positions
               (if interrupt?
                   (px "^" (:list-marker #:interrupt? #t))
                   (px "^" (:list-marker #:interrupt? #f)))
               in pos)
         [#f #f]
         [(list (cons _ marker-width) _ num-posns num-marker-posns bullet-marker-posns starts-with-blank?)
          (define marker-bytes (read-bytes marker-width in))
          (define marker-char (~>> (or num-marker-posns bullet-marker-posns)
                                   car
                                   (bytes-ref marker-bytes)
                                   integer->char))
          (define start-num (and num-posns
                                 (~> (subbytes marker-bytes (car num-posns) (cdr num-posns))
                                     bytes->string/utf-8
                                     string->number)))
          (advance-tab-stop-offset! marker-width)
          (set! partially-consumed-tab? #f)
          (cond
            ; If the list starts with a blank line, we don’t care about whatever
            ; whitespace follows, so just return.
            [starts-with-blank?
             (list (add1 marker-width) marker-char start-num)]
            [else
             ; Otherwise, we have to peek ahead to see how much indentation
             ; follows the marker, since that affects its width.
             ; See also Note [Tabs and tab stops].
             (let loop ([pos 0]
                        [indent 0])
               (cond
                 ; If there are more than four spaces of indentation, the list
                 ; starts with a single space of indentation followed by an
                 ; indented code block.
                 [(> indent 4)
                  (try-enter-indent 1)
                  (list (add1 marker-width) marker-char start-num)]
                 [else
                  (match (peek-char in pos)
                    [#\space
                     (loop (add1 pos) (add1 indent))]
                    [#\tab
                     (loop (add1 pos) (+ indent (current-tab-width indent)))]
                    ; Otherwise, all the identation present contributes to the
                    ; width of the list marker.
                    [_
                     (read-bytes pos in)
                     (advance-tab-stop-offset! indent)
                     (list (+ marker-width indent) marker-char start-num)])]))])]))))

  ;; ---------------------------------------------------------------------------
  ;; parser states

  (define (mode:line-start)
    (set! entered-containers 0)
    (set! tab-stop-offset 0)
    (set! partially-consumed-tab? #f)
    (cond
      [(eof-object? (peek-char in))
       (close-leaf!)
       (close-unentered-containers!)
       (document (for/list ([block (in-gvector root-blocks)])
                   (finish-block-content block))
                 (for/list ([footnote-defn (in-gvector footnote-defns)])
                   (match-define (footnote-definition blocks label) footnote-defn)
                   (footnote-definition (map finish-block-content blocks) label)))]
      [else
       (mode:enter-containers)]))

  (define (try-enter-container open-container)
    (match open-container
      [(? o:blockquote?)
       (try-read-blockquote-marker)]
      [(? o:list?)
       ; see Note [Entering open lists]
       (cond
         [(and (o:list-blocks open-container)
               (try-enter-indent (o:list-indent open-container)))
          #t]
         [(try-read-list-marker #:interrupt? #f)
          => (match-lambda
               [(list indent marker-char start-num)
                (cond
                  [(char=? marker-char (o:list-marker-char open-container))
                   (set! entered-containers (add1 entered-containers))
                   (close-leaf!)
                   (close-unentered-containers!)
                   (set! entered-containers (sub1 entered-containers))

                   (define open-list (gvector-ref open-containers entered-containers))
                   (gvector-set! open-containers
                                 entered-containers
                                 (struct-copy o:list open-list
                                              [blocks '()]
                                              [blockss (accumulate-list-blockss open-list)]
                                              [indent indent]
                                              [start-blank? #f]
                                              [end-blank? #f]
                                              ; see Note [Open list tightness]
                                              [style (if (o:list-end-blank? open-list)
                                                         'loose
                                                         (o:list-style open-list))]))]
                  [else
                   (open-container! (o:list '() '() indent marker-char #f #f 'tight start-num)
                                    #:enter? #f)])
                0])]
         [else #f])]
      [(? o:footnote-definition?)
       (try-enter-indent 4)]))

  (define (mode:enter-containers)
    (cond
      [(unentered-containers?)
       (cond
         [(try-enter-container (gvector-ref open-containers entered-containers))
          (set! entered-containers (add1 entered-containers))
          (mode:enter-containers)]
         [else
          ; Paragraphs can have lazy continuation lines, so we can’t close
          ; unentered containers yet if we’re in the middle of a paragraph.
          (unless (o:paragraph? open-leaf)
            (close-unentered-containers!))
          (mode:content-start)])]
      [else
       (mode:content-start)]))

  (define (mode:content-start)
    (cond
      ;; § 4.5 Fenced code blocks
      [(and (not (leaf-needs-explicit-end? open-leaf))
            (regexp-try/opt-indent
             (px "^" "(" :code-fence ")" ; code fence
                 "[ \t]*"                ; optional whitespace
                 (:? "((?(2)[^`\n\r]"    ; optional info string (no backticks)
                     "|[^\n\r])+?)"      ; optional info string (backticks allowed)
                     "[ \t]*")           ; optional whitespace
                 :eol)))
       => (match-lambda
            [(list _ indent fence _ info-bytes)
             (open-leaf!
              (o:fenced-code-block
               '()
               indent
               (integer->char (bytes-ref fence 0))
               (bytes-length fence)
               (and~> info-bytes bytes->string/utf-8 decode-entities+escapes)))
             (mode:line-start)])]

      [(o:fenced-code-block? open-leaf)
       (match/values (read-line/opt-indent)
         [{_ (? eof-object?)} (void)]
         [{_ (regexp (px "^" (:group :code-fence) "[ \t]*" "$") (list _ fence _))}
          #:when (and (char=? (string-ref fence 0)
                              (o:fenced-code-block-fence-char open-leaf))
                      (>= (string-length fence)
                          (o:fenced-code-block-fence-length open-leaf)))
          (close-leaf!)]
         [{indent line}
          (define extra-indent (- indent (o:fenced-code-block-indent open-leaf)))
          (define indented-line (if (> extra-indent 0)
                                    (string-append (leftover-indent-string extra-indent) line)
                                    line))
          (set! open-leaf
                (struct-copy o:fenced-code-block open-leaf
                             [lines (cons indented-line (o:fenced-code-block-lines open-leaf))]))])
       (mode:line-start)]

      ;; § 4.6 HTML blocks (opening, paragraph-interrupting)
      [(and (not (leaf-needs-explicit-end? open-leaf))
            (regexp-peek/opt-indent
             (px "^<"
                 (:or (:: (:group (:or "pre" "script" "style" "textarea"))
                          (:or "[ \t>]" :eol))
                      (:group (:or "!--" "\\?" "!\\[CDATA\\["))
                      (:group "![a-zA-Z]")
                      (:: "/?" (:ci (:or "address" "article" "aside" "base" "basefont"
                                         "blockquote" "body" "caption" "center" "col"
                                         "colgroup" "dd" "details" "dialog" "dir" "div"
                                         "dl" "dt" "fieldset" "figcaption" "figure"
                                         "footer" "form" "frame" "frameset" "h1" "h2"
                                         "h3" "h4" "h5" "h6" "head" "header" "hr" "html"
                                         "iframe" "legend" "li" "link" "main" "menu"
                                         "menuitem" "nav" "noframes" "ol" "optgroup"
                                         "option" "p" "param" "section" "source"
                                         "summary" "table" "tbody" "td" "tfoot" "th"
                                         "thead" "title" "tr" "track" "ul"))
                          (:or "[ \t>]" :eol "/>"))))))
       => (match-lambda
            [(list _ _ block-open directive doctype)
             (define end-type
               (cond
                 [block-open 'block-close]
                 [directive (match directive
                              [#"!--" 'comment-close]
                              [#"?" 'processor-close]
                              [#"![CDATA[" 'cdata-close])]
                 [doctype 'tag-end]
                 [else 'blank-line]))
             (open-leaf! (o:html-block '() end-type))
             (mode:content-start)])]

      ;; § 4.6 HTML blocks (closing)
      [(o:html-block? open-leaf)
       (define line (read-line/leftover-indent))
       (define end-type (o:html-block-end-type open-leaf))
       (define close?
         (match end-type
           ['block-close (regexp-match? #px"</(?i:pre|script|style|textarea)>" line)]
           ['comment-close (string-contains? line "-->")]
           ['processor-close (string-contains? line "?>")]
           ['cdata-close (string-contains? line "]]>")]
           ['tag-end (string-contains? line ">")]
           ['blank-line (regexp-match? #px"^[ \t]*$" line)]))
       (unless (and close? (eq? end-type 'blank-line))
         (set! open-leaf (struct-copy o:html-block open-leaf
                                      [lines (cons line (o:html-block-lines open-leaf))])))
       (when close?
         (close-leaf!))
       (mode:line-start)]

      ;; § 4.4 Indented code blocks (interior/closing lines)
      [(o:indented-code-block? open-leaf)
       (cond
         [(try-enter-indent 4)
          (set! open-leaf
                (match open-leaf
                  [(o:indented-code-block lines)
                   (o:indented-code-block (cons (read-line/leftover-indent) lines))]))
          (mode:line-start)]
         [else
          (close-leaf!)
          (mode:content-start)])]

      ;; § 4.9 Blank lines
      [(regexp-try-match (px "^[ \t]*" :eol) in)

       (match open-leaf
         [(? o:paragraph?)
          (close-leaf!)
          (close-unentered-containers!)]
         [_ (void)])

       (match (and (not (zero? entered-containers))
                   (gvector-ref open-containers (sub1 entered-containers)))
         [(? o:list? open-list)
          (gvector-set!
           open-containers
           (sub1 entered-containers)
           (if (empty? (o:list-blocks open-list))
               (if (o:list-start-blank? open-list)
                   ; see Note [Open lists without an open item]
                   (struct-copy o:list open-list
                                [blocks #f]
                                [blockss (cons '() (o:list-blockss open-list))]
                                [start-blank? #f]
                                [end-blank? #t])
                   ; Note that we don’t set `end-blank?` to #t here because if
                   ; we hit this path, we’re on the very first line of the list,
                   ; which does not count as a blank line for looseness.
                   (struct-copy o:list open-list [start-blank? #t]))
               (struct-copy o:list open-list [end-blank? #t])))]
         [_ (void)])

       (mode:line-start)]

      ;; § 4.4 Indented code blocks (opening)
      [(and (not open-leaf)
            (try-enter-indent 4))
       (set! open-leaf (o:indented-code-block (list (read-line/leftover-indent))))
       (mode:line-start)]

      ;; § 4.6 HTML blocks (opening, not paragraph-interrupting)
      [(and (not open-leaf)
            (regexp-peek/opt-indent
             (px "^" (:html-open-close #:allow-newlines? #f) "[ \t]*" :eol)))
       (open-leaf! (o:html-block '() 'blank-line))
       (mode:content-start)]

      ;; § 4.2 ATX headings
      [(regexp-try/opt-indent
        (px "^" "(#{1,6})"            ; heading prefix
            (:? "[ \t]+"              ; one or more space/tab to separate prefix from content
                "([^\n\r]+?)??"       ; heading content
                (:? "(?(2)[ \t]+)#+") ; optional suffix
                "[ \t]*")             ; optional trailing space
            :eol))
       => (match-lambda
            [(list _ _ prefix content-bytes)
             (close-leaf!)
             (add-block! (heading (if content-bytes
                                      (bytes->string/utf-8 content-bytes)
                                      "")
                                  (bytes-length prefix)))
             (mode:line-start)])]

      ;; § 4.3 Setext headings
      [(and (o:paragraph? open-leaf)
            ; A setext heading cannot be a lazy continuation line.
            (not (unentered-containers?))
            (regexp-try/opt-indent (px "^" "(([=-])\\2*)" "[ \t]*" :eol)))
       => (match-lambda
            [(list _ _ underline underline-char)
             (define para-str (close-paragraph-lines (o:paragraph-lines open-leaf)))
             (cond
               [(string=? para-str "")
                (set! open-leaf (o:paragraph (list (bytes->string/utf-8 underline))))]
               [else
                (set! open-leaf #f)
                (add-block! (heading para-str
                                     (match underline-char
                                       [#"=" 1]
                                       [#"-" 2])))])
             (mode:line-start)])]

      ;; § 4.1 Thematic breaks
      [(regexp-try/opt-indent (px "^" :thematic-break))
       (close-leaf!)
       (add-block! thematic-break)
       (mode:line-start)]

      ;; § 5.1 Block quotes
      [(try-read-blockquote-marker)
       (open-container! (o:blockquote '()))
       (mode:content-start)]

      ;; § 5.2 List items
      [(try-read-list-marker #:interrupt? (and open-leaf #t))
       => (match-lambda
            [(list indent marker-char start-num)
             (open-container! (o:list '() '() indent marker-char #f #f 'tight start-num))
             (mode:content-start)])]

      ;; Extension: Footnote definitions
      [(and footnotes?
            (regexp-try/opt-indent
             ; This is the regexp used by _scan_footnote_definition in cmark-gfm:
             ;   <https://github.com/github/cmark-gfm/blob/766f161ef6d61019acf3a69f5099489e7d14cd49/src/scanners.re#L323>
             ; Note that it consumes all whitespace after the label, so it is
             ; impossible for a footnote to start with an indented code block on the
             ; same line as the label (but one can begin on the following line).
             (px "^"
                 "\\[\\^"
                 (:group "[^]" :space: "]+")
                 "\\]:[ \t]*")))
       => (match-lambda
            [(list _ _ label-bytes)
             (define label (bytes->string/utf-8 label-bytes))
             (define normalized-label (normalize-link-label label))
             (unless (hash-has-key? footnote-defn-labels normalized-label)
               (hash-set! footnote-defn-labels normalized-label label))
             (open-container! (o:footnote-definition '() label))
             (mode:content-start)])]

      ;; § 4.8 Paragraphs
      [else
       (define line (~> (read-line in 'any)
                        (string-trim #px"[ \t]+" #:right? #f)))
       (match open-leaf
         [#f
          (open-leaf! (o:paragraph (list line)))]
         [(o:paragraph lines)
          (set! open-leaf (o:paragraph (cons line lines)))])
       (mode:line-start)]))

  (define (close-paragraph-lines lines)
    (~> (string-join (reverse lines) "\n")
        peel-link-reference-definitions
        ; The spec says “Final spaces or tabs are stripped before inline
        ; parsing, so a paragraph that ends with two or more spaces will not end
        ; with a hard line break.”
        (string-trim #px"[ \t]+" #:left? #f)))

  (define (peel-link-reference-definitions para-str)
    (define (record! label ref)
      (unless (hash-has-key? link-reference-defns label)
        (hash-set! link-reference-defns label ref)))

    (define in (open-input-string para-str))
    (match (try-read-link-reference-definition in)
      ; If we didn’t read any reference definitions at all, just return the
      ; input string.
      [#f para-str]
      ; Otherwise, record the parsed definition and keep looking for more.
      [(cons label ref)
       (record! label ref)
       (let loop ()
         (match (try-read-link-reference-definition in)
           [#f (port->string in)]
           [(cons label ref)
            (record! label ref)
            (loop)]))]))

  (define (finish-block-content block)
    (match block
      [(heading content depth)
       (heading (parse-inline content) depth)]
      [(paragraph content)
       (paragraph (parse-inline content))]
      [(blockquote blocks)
       (blockquote (map finish-block-content blocks))]
      [(itemization blockss style start-num)
       (itemization (map (λ~>> (map finish-block-content)) blockss) style start-num)]
      [_ block]))

  (define (parse-inline content)
    (string->inline content
                    #:link-defns link-reference-defns
                    #:footnote-defns footnote-defn-labels))

  (mode:line-start))

; see Note [Tabs and tab stops]
(define (leftover-indent-string leftover-indent)
  (match leftover-indent
    [0 ""]
    [1 " "]
    [2 "  "]
    [3 "   "]))

#| Note [Tabs and tab stops]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The CommonMark spec mandates that tabs be left alone when they appear inside
block content, but they must be treated specially when they appear in
indentation used to define block structure. Such tabs are interpreted as if they
were replaced by spaces up to the nearest tab stop, where each tab stop has a
width of four spaces.

Note that this does NOT mean a tab character is always interpreted as four
spaces. A tab can have a width of 1, 2, 3, or 4 spaces depending on the width of
the line up to that point. For example, all five of the following lines have
identical amounts of logical indentation, namely four spaces worth (where ‘.’
represents a space and ‘→’ represents a tab):

    ....a
    ...→b
    ..→c
    .→d
    →e

The placement of tab stops is absolute and is not altered by any nested block
structure. For example, in the document

    >.→hello

the inner tab has a width of two spaces, not four, so the blockquote contains a
paragraph, not an indented code block. This is something of a departure from
other aspects of the spec, which generally treats contexts inside a container
block identically to how they would be treated if the container were removed,
but it makes sense given that the special treatment of tabs is designed to
accommodate the way tabs are rendered in text editors, which do not know
anything about markdown document structure.

Handling this properly requires keeping track of two additional pieces of state:

  1. The `tab-stop-offset` variable keeps track of the current column modulo 4,
     which is used to determine the width of tabs when they’re encountered.

  2. The `partially-consumed-tab?` variable is set to #t after encountering a
     tab in a context that consumes fewer spaces worth of indentation than the
     tab expands to. For example, consider the following document:

         -.foo

         →..bar

     When the parser encounters the third line, it must consume two spaces worth
     of indentation to enter the open list item. However, the line starts with a
     four-space tab, which leaves two spaces unconsumed. To record this fact,
     `tab-stop-offset` is set to 2 and `partially-consumed-tab?` is set to #t,
     which means 2 spaces in the current tab remain before reaching the next tab
     stop. These can then be combined with the two additional spaces to parse
     the four-space indent required to start an indented code block.

Most of the parser does not need to worry about this tricky column tracking, as
spaces are treated uniformly once all blocks have been entered. However, many
blocks may be prefixed with “optional indentation” of up to three spaces, which
must be tab-aware, see Note [Optional indentation] for more details.

Note [Optional indentation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
As described in Note [Tabs and tab stops], tabs must be treated with care when
parsing block structure. Generally, that structure is only relevant when parsing
container blocks, as tabs inside leaf blocks have no special meaning. However,
most leaf blocks may be optionally prefixed with up to three spaces of
indentation, and that indentation must be tab-aware. (Why up to three spaces?
Because four or more spaces of indentation would form an indented code block.)

To handle this optional indentation uniformly, we define a few helper functions:

  * The `regexp-try/opt-indent` and `regexp-peek/opt-indent` functions are like
    `regexp-try-match` and `regexp-match-peek`, respectively, but they attempt
    to consume an optional indent before matching the regular expression. If
    more than three spaces worth of indentation are present, they return #f.

  * The `read-line/opt-indent` function is used only when parsing the body of a
    fenced code block, which has somewhat unusual parsing rules. The entire line
    is unconditionally consumed, regardless of the number of spaces of
    indentation, but the amount of optional identation is returned as well, as
    some portion of it may be incorporated into the line (depending on the
    amount of indentation used by the opening fence).

Note [Lists overview]
~~~~~~~~~~~~~~~~~~~~~
Lists are by far the subtlest part of CommonMark to get right, as they have lots
of special cases. CommonMark specifies lists in terms of two separate types of
container block: lists and list items. However, this is awkward to implement, as
a list is simply defined as a sequence of list items, so the lifecycle of a list
container is necessarily tied to the lifecycle of its constituent items.

We therefore unify open lists and open list items into a single structure,
`o:list`, which almost always represents an open list item that happens to have
a set of earlier list items recorded alongside it. The one exception is a very
unusual special case, see Note [Open lists without an open item].

Note [Entering open lists]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Since an `o:list` structure represents both an open list and an open list item,
“entering” an open list can actually involve one of two different things:

  1. In the simple case, the currently-open list item remains open, so we enter
     the `o:list` container as usual.

  2. In the less simple case, we might need to close the currently-open list
     item and start a new one, in which case `try-enter-container` must handle
     closing the previous list item and updating the `o:list` structure before
     it can be entered.

This is a bit aesthetically unsatisfying, since it would be nice if
`try-enter-container` were guaranteed to do nothing except consume the
container’s start-of-line prefix, but this allows us to preserve the invariant
that `add-block!` can always add any type of block to the innermost open
container, rather than having to worry about closing an open list if a non-list
item block is added to it.

Note [Open lists without an open item]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
As discussed in Note [Lists overview], an `o:list` container almost always
represents an open list item, since it’s almost impossible to end up in a
parser state where a list container remains open even though the most recent
list item has been closed. The only exception stems from the rule that a list
item may not begin with two blank lines:

> A list item can begin with at most one blank line. In the following example,
> `foo` is not part of the list item:
>
>     -     | <ul>
>           | <li></li>
>     ..foo | </ul>
>           | <p>foo</p>

This necessitates two features of the `o:list` structure:

  1. The `blocks` field can be #f, which means “no open list item”.

  2. The `start-blank?` field tracks whether or not a starting blank line has
     already been seen, in which case a second blank line must close the open
     list item.

Importantly, `blocks` can never be #f at the beginning of `mode:content-start`,
since `try-enter-container` will not enter an `o:list` container unless it can
enter an already-open list item or it can start a new list item (see
Note [Entering open lists]).

Note [Open list tightness]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Determining whether a list is tight or loose is awkward: a list is generally
loose if it contains any blank lines, but blank lines that occur at the start of
a list item or at the end of the list don’t count. Ignoring blank lines at the
start of a list item is easy, but ignoring blank lines at the end of the list is
harder, because we don’t know whether or not the list will have more blocks
until we read more lines.

To avoid having to do arbitrary amounts of lookahead, we store whether or not an
open list ends in a blank line in the `end-blank?` field of an `o:list`
structure. Whenever a new block is added to an open list, we set `style` to
'loose if `end-blank?` is #t and reset `end-blank?` to #f. Even though the value
of `end-blank?` is no longer relevant for determining the style of the current
open list, it’s still important to track it accurately, as it may affect the
style of /enclosing/ lists; see Note [Transfer `end-blank?` to parent lists].

Note [Transfer `end-blank?` to parent lists]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider the following markdown input:

    * foo
      * bar

      baz

The inner list should be tight, and the outer list should be loose. But if we
aren’t careful, the strategy described in Note [Open list tightness] will miss
that the outer list is loose, since the blank line will set `end-blank?` on the
inner list, not the outer one. Therefore, to ensure the outer list is properly
marked loose, we must transfer the value of `end-blank?` to any immediately-
enclosing parent list whenever a list is closed.

One might wonder whether it’s safe to only do this transferring to immediately-
enclosing lists, as there could be a blockquote in the way:

    * foo
      > * bar
      >
      baz

Should the outer list still be loose even though the blank line is contained
within a blockquote? Fortunately, the answer is /no/. The spec says

> A list is loose if any of its constituent list items are separated by blank
> lines, or if any of its constituent list items directly contain two block-
> level elements with a blank line between them.

which makes it clear that the blank line must appear between two direct children
of the list item. |#

;; -----------------------------------------------------------------------------
;; Link reference definitions

;; § 4.7 Link reference definitions
;; <https://spec.commonmark.org/0.30/#link-reference-definition>
(define (try-read-link-reference-definition in)
  (define (try-peek-eol start-pos)
    (match (regexp-match-peek-positions (px "^" "[ \t\r\n]*" :eol) in start-pos)
      [(list (cons _ eol-pos)) eol-pos]
      [#f #f]))

  (match-and*
   ; A link reference definition consists of a link label, ...
   [(try-peek-link-label in) => (list label-pos label-str)]
   ; ...followed by a colon, optional spaces or tabs, ...
   [(regexp-match-peek-positions (px "^" ":" "[ \t\r\n]*") in label-pos)
    => (list (cons _ colon-pos))]
   ; ...a link destination, ...
   [(try-peek-link-destination in colon-pos) => (list dest-pos dest-str)]
   ; ...optional spaces or tabs, ...
   [(regexp-match-peek-positions (px "^" "[ \t\r\n]*") in dest-pos)
    => (list (cons _ spaces-pos))
    (or (match-and*
         ; ...and an optional link title, which if present must be separated
         ; from the link destination by spaces or tabs.
         [(and (> spaces-pos dest-pos)
               (try-peek-link-title in spaces-pos))
          => (list title-pos title-str)]
         ; The spec says “no further character may occur” after a link title,
         ; but the reference implementation allows trailing spaces or tabs,
         ; which seems reasonable, so we allow that, too.
         [(try-peek-eol title-pos) => end-pos
          (read-bytes end-pos in)
          (cons label-str (link-reference dest-str title-str))])
        (match-and*
         ; If we failed to parse a link title, we have to back up to right after
         ; the destination and check if we can get to the end of the line from
         ; there. If so, this is a legal link reference definition with no title.
         [(try-peek-eol dest-pos) => end-pos
          (read-bytes end-pos in)
          (cons label-str (link-reference dest-str #f))]))]))
