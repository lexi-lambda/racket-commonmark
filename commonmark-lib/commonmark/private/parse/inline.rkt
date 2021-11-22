#lang racket/base

(require (for-syntax racket/base)
         racket/match
         racket/string
         threading

         "../regexp.rkt"
         "../struct.rkt"
         "common.rkt")

(provide string->inline
         (struct-out link-reference))

;; -----------------------------------------------------------------------------

(begin-for-syntax
  ;; § 6.5 Autolinks
  (define :uri-scheme (:: "[a-zA-Z][a-zA-Z0-9+.-]{1,31}"))
  (define :absolute-uri (:: :uri-scheme ":[^" :ascii-control: " <>]*"))
  (define :email-domain-segment
    (:: "[a-zA-Z0-9]"
        (:? "[a-zA-Z0-9-]{0,61}[a-zA-Z0-9]")))
  (define :email-address
    (:: "[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+"
        "@" :email-domain-segment
        (:* "\\." :email-domain-segment)))

  ;; § 6.5 Raw HTML
  (define :html-tag
    (let ()
      (define :comment (:: "<!--" "(?!>|->)" (:* (:or "[^-]" "-[^-]")) "-->"))
      (define :instruction (:: "<\\?" (:* (:or "[^?]" "\\?[^>]")) "\\?>"))
      (define :declaration (:: "<!" "[a-zA-Z]" "[^>]*" ">"))
      (define :cdata-section (:: "<!\\[CDATA\\[" (:* (:or "[^]]" "\\][^]]" "\\]\\][^>]")) "\\]\\]>"))

      (:or (:html-open-close #:allow-newlines? #t) :comment :instruction :declaration :cdata-section))))

#| Note [Link targets containing formatting]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When collapsed or shortcut links are used, the associated link label
can easily contain formatting characters, such as in the following example:

    A reference to [*foo* bar][].

    [*foo* bar]: /url "title"

The CommonMark specification mandates that such formatting characters are
treated as formatting in the rendered text, but are left untouched in the label.
This is arguably unhelpful, as it would probably be more convenient for
formatting to be stripped (e.g. resulting in the link target "foo bar" in the
above example), but for better or for worse, it’s what the spec says.

This is somewhat awkward to implement, as it means the content of a collapsed or
shortcut link must be interpreted two ways: once as formatted inline content,
then a second time as plain text. However, we still have to parse the content in
order to determine if we have a valid link or not, because nested links or other
formatting can affect whether or not a following close bracket should be
interpreted as a link. This means we have to parse the content as normal, and
when we discover the ']' at the end, we must somehow recover the unparsed text.

The reference implementation handles this by simply retaining the entire input
string in memory, recording the positions of the start and end of the link
content, and taking the substring between those positions to recover the
unparsed link label. For simplicity, we choose the same strategy — which is not
really leaving any performance on the table, as inlines can only be parsed after
reading the entire document into memory anyway (since link reference definitions
must be discovered).

Note [Track positions in characters]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Before parsing inline content, we must take care to call `port-count-lines!` on
the string port we use to manage our place in the input. This seems odd, since
the parser can never fail, so it seems unnecessary to track location information
at all.

However, it turns out that we do sometimes need to record where we are in the
input in order to recover link label text for collapsed and shortcut reference
links, as described in Note [Link targets containing formatting]. Since the
input is a string, we want to extract the label text using `substring`, which
indexes in terms of characters, not bytes. However, by default,
`port-next-location` returns positions in terms of bytes, so the indexes will
be wrong for multibyte characters.

Fortunately, enabling line counting has the convenient side effect of tracking
positions in characters rather than bytes, which explains why we need to call
`port-count-lines!` even though we never actually use line information. |#

(struct link-reference (dest title) #:transparent)

(define (string->inline str
                        #:link-defns link-reference-defns
                        #:footnote-defns footnote-defns)
  (define in (open-input-string str))
  (port-count-lines! in) ; see Note [Track positions in characters]

  ;; Reads the entire input and parses it into a list of `inline?` and
  ;; `delimiter-run?` values.
  (define (read-full-sequence prev-char)
    (match-define-values [nodes closer last-char _] (read-sequence prev-char))
    (match closer
      [(? eof-object?)
       (values nodes last-char)]
      ['link-close
       (define-values [nodes* last-char*] (read-full-sequence last-char))
       (values (append nodes (cons "]" nodes*)) last-char*)]))

  ;; Reads the input up to the first ‘]’ character, or to the EOF if no ‘]’ is
  ;; encountered. Returns four values:
  ;;   1. A list of parsed `inline?` and `delimiter-run?` values.
  ;;   2. Either #<eof> or 'link-close, depending on whether a ‘]’ was encountered.
  ;;   3. The last character consumed from the input.
  ;;   4. Whether the returned list of nodes contains any links, which is used
  ;;      to avoid parsing links inside links.
  (define (read-sequence prev-char)
    (define-values [node last-char] (read-one prev-char))
    (match node
      [(or (? eof-object?) 'link-close)
       (values '() node last-char #f)]

      [(or 'link-open 'image-open)
       (define image? (eq? node 'image-open))
       (define open-text (if image? "![" "["))

       (match-define-values [_ _ open-pos] (port-next-location in))
       (define-values [inner-nodes closer last-char* has-link?] (read-sequence last-char))
       (match closer
         [(? eof-object?)
          (values (cons open-text inner-nodes) closer last-char* has-link?)]
         ['link-close
          (define (ignore-closer)
            (define-values [nodes closer last-char** has-link?*] (read-sequence last-char*))
            (values (cons open-text (append inner-nodes (cons "]" nodes)))
                    closer
                    last-char**
                    (or has-link? has-link?*)))
          (cond
            [(and (not image?) has-link?)
             (ignore-closer)]
            [else
             (match-define-values [_ _ close-pos] (port-next-location in))
             ; see Note [Link targets containing formatting]
             (define content-label (substring str (sub1 open-pos) (- close-pos 2)))
             (match (try-read-link-target content-label)
               [#f (ignore-closer)]
               [(link-reference dest title)
                (define content (process-emphasis inner-nodes))
                (cond
                  [image?
                   (define node (image content dest title))
                   (define-values [nodes closer last-char** has-link?*] (read-sequence last-char*))
                   (values (cons node nodes) closer last-char** (or has-link? has-link?*))]
                  [else
                   (define node (link content dest title))
                   (match-define-values [nodes closer last-char** _] (read-sequence last-char*))
                   (values (cons node nodes) closer last-char** #t)])]
               [(? footnote-reference? node)
                (match-define-values [nodes closer last-char** _] (read-sequence last-char*))
                (values (if image?
                            (list* "!" node nodes)
                            (cons node nodes))
                        closer last-char** #t)])])])]

      [_
       (define-values [nodes closer last-char* has-link?] (read-sequence last-char))
       (values (cons node nodes) closer last-char* has-link?)]))

  ;; Reads and parses a single `inline?` or `delimiter-run?` from the input
  ;; stream. Also returns the last character consumed from the input, which is
  ;; used to determine whether delimiter runs are left- or right-flanking.
  (define (read-one prev-char)
    (match (peek-char in)
      [(? eof-object?)
       (values eof prev-char)]

      ;; § 2.3 Insecure characters
      [#\null
       (read-char in)
       (values "\uFFFD" #\null)]

      ;; § 2.4 Backslash escapes
      [#\\
       (read-char in)
       (define next-c (read-char in))
       (cond
         [(eof-object? next-c)
          (values "\\" #\\)]
         [(char=? next-c #\newline)
          (values line-break #\newline)]
         [(ascii-punctuation? next-c)
          (values (string next-c) next-c)]
         [else
          (values (string #\\ next-c) next-c)])]

      ;; § 2.5 Entity and numeric character references
      [#\&
       (cond
         [(try-read-entity in)
          => (λ (replacement-str)
               (values replacement-str #\;))]
         [else
          (read-char in)
          (values "&" #\&)])]

      ;; § 6.1 Code spans
      [#\`
       (match (regexp-try-match
               (px "^(`+)"           ; a sequence of one or more backticks
                   "([^`].*?)"       ; non-backtick character followed by any characters
                   "(?<!`)\\1(?!`)") ; the initial backtick sequence, which must
               in)                   ;  stand apart from other backticks
         [#f (lex-rx #px"^`+")]
         [(list _ _ code-bytes)
          (define code-str (~> (bytes->string/utf-8 code-bytes)
                               ; Line endings are treated like spaces.
                               (string-replace (px :newline) " ")))
          (define code-len (string-length code-str))
          ; Leading and trailing spaces are stripped if present and the string
          ; is not only spaces.
          (define stripped-str
            (if (and (>= code-len 2)
                     (char=? (string-ref code-str 0) #\space)
                     (char=? (string-ref code-str (sub1 code-len)) #\space)
                     (not (for/and ([c (in-string code-str 1 (sub1 code-len))])
                            (char=? c #\space))))
                (substring code-str 1 (sub1 code-len))
                code-str))

          (values (code stripped-str) #\`)])]

      ;; § 6.2 Emphasis and strong emphasis
      [(or #\* #\_)
       (define c (read-char in))
       (define len (let loop ([len 1])
                     (if (eqv? c (peek-char in))
                         (begin
                           (read-char in)
                           (loop (add1 len)))
                         len)))

       (define next-char (peek-char in))
       (define left-flanking? (flanking? prev-char next-char))
       (define right-flanking? (flanking? next-char prev-char))

       (define-values [opener? closer?]
         (match c
           [#\* (values left-flanking? right-flanking?)]
           [#\_ (values (and left-flanking?
                             (or (not right-flanking?)
                                 (unicode-punctuation?* prev-char)))
                        (and right-flanking?
                             (or (not left-flanking?)
                                 (unicode-punctuation?* next-char))))]))

       (values (delimiter-run c len len opener? closer?) c)]

      ;; § 6.3 Links
      [#\[ (read-char in) (values 'link-open #\[)]
      [#\] (read-char in) (values 'link-close #\])]

      [#\!
       (read-char in)
       (cond
         [(eqv? (peek-char in) #\[)
          (read-char in)
          (values 'image-open #\[)]
         [else
          (values "!" #\!)])]

      ;; § 6.5 Autolinks
      ;; § 6.6 Raw HTML
      [#\<
       (match (regexp-try-match
               (px "^" (:or (:: "<" (:group :absolute-uri) ">")
                            (:: "<" (:group :email-address) ">")
                            (:group :html-tag)))
               in)
         [(list _ (? values (app bytes->string/utf-8 absolute-uri)) _ _)
          (values (link absolute-uri absolute-uri #f) #\>)]
         [(list _ _ (? values (app bytes->string/utf-8 email-address)) _)
          (values (link email-address (string-append "mailto:" email-address) #f) #\>)]
         [(list _ _ _ (? values (app bytes->string/utf-8 html-tag)))
          (values (html html-tag) #\>)]
         [#f
          (read-char in)
          (values "<" #\<)])]

      ;; § 6.7 Hard line breaks
      ;; § 6.8 Soft line breaks
      [#\space
       (match (regexp-match #px"^( +)(\n)?" in)
         [(list _ spaces #f)
          (values (bytes->string/utf-8 spaces) #\space)]
         [(list _ spaces _)
          (values (if (>= (bytes-length spaces) 2) line-break "\n") #\newline)])]
      [#\newline
       (read-char in)
       (values "\n" #\newline)]

      [_
       (lex-rx #px"^[^\0\\\\&[\\]!`*_< \n]+")]))

  (define (lex-rx rx)
    (match (regexp-match rx in)
      [(cons result _)
       (define str (bytes->string/utf-8 result))
       (values str (string-ref str (sub1 (string-length str))))]
      [else
       (raise-arguments-error 'lex-rx "lex failed"
                              "next char" (peek-char in)
                              "expected regexp" rx)]))

  (define (try-read-link-target content-label-str)
    (or
     ;; Full reference links <https://spec.commonmark.org/0.30/#reference-link>
     (match-and*
      [(try-peek-link-label in) => (list label-pos label-str)]
      [(hash-ref link-reference-defns label-str #f)
       => link-ref
       (read-bytes label-pos in)
       link-ref])

     ;; Inline links <https://spec.commonmark.org/0.30/#inline-link>
     (let ()
       (define (try-peek-end start-pos)
         (match-and*
          [(regexp-match-peek-positions #px"^[ \t\r\n]*\\)" in start-pos)
           => (list (cons _ end-pos))
           end-pos]))

       (match-and*
        [(regexp-match-peek-positions #px"^\\([ \t\r\n]*" in)
         => (list (cons _ start-pos))
         (or
          ; Case 1: No link destination or link title.
          (match-and*
           [(eqv? (peek-char in start-pos) #\))
            => #t
            (read-bytes (add1 start-pos) in)
            (link-reference "" #f)])
          ; Case 2: Link destination, optionally followed by a link title.
          (match-and*
           [(try-peek-link-destination in start-pos) => (list dest-pos dest-str)]
           [(regexp-match-peek-positions #px"^[ \t\r\n]*" in dest-pos)
            => (list (cons _ spaces-pos))
            (or (match-and*
                 ; Case 2a: Link destination followed by a link title, which must
                 ; be separated from the link destination by spaces or tabs.
                 [(and (> spaces-pos dest-pos)
                       (try-peek-link-title in spaces-pos))
                  => (list title-pos title-str)]
                 [(try-peek-end title-pos)
                  => end-pos
                  (read-bytes end-pos in)
                  (link-reference dest-str title-str)])
                (match-and*
                 ; Case 2b: Link destination without a following link title.
                 [(try-peek-end spaces-pos)
                  => end-pos
                  (read-bytes end-pos in)
                  (link-reference dest-str #f)]))])
          ; Case 3: Link title without a link destination.
          (match-and*
           [(try-peek-link-title in start-pos) => (list title-pos title-str)]
           [(try-peek-end title-pos)
            => end-pos
            (read-bytes end-pos in)
            (link-reference "" title-str)]))]))

     (let ()
       (define normalized-label (normalize-link-label content-label-str))
       (or
        ;; Collapsed reference links <https://spec.commonmark.org/0.30/#collapsed-reference-link>,
        ;;   and shortcut reference links <https://spec.commonmark.org/0.30/#shortcut-reference-link>
        (match-and*
         [(hash-ref link-reference-defns normalized-label #f) => link-ref]
         [(or (regexp-try-match #px"^\\[\\]" in)
              ; A shortcut reference link must not be followed by a link label,
              ; even if that link label is not defined.
              (not (try-peek-link-label in)))
          => _
          link-ref])

        ;; Extension: Footnote references
        (match-and*
         [(string-prefix? normalized-label "^") => _
          (define footnote-label (substring normalized-label 1))]
         [(hash-ref footnote-defns footnote-label #f) => unnormalized-label
          (footnote-reference unnormalized-label)])))))

  ;; Appendix A, Phase 2: inline structure
  (define (process-emphasis nodes-lst)
    (define nodes (list->vector nodes-lst))
    (define num-nodes (vector-length nodes))

    (let closer-loop ([closer-idx 0])
      (when (< closer-idx num-nodes)
        (define closer (vector-ref nodes closer-idx))
        (cond
          [(and (delimiter-run? closer)
                (delimiter-run-closer? closer))
           (let opener-loop ([opener-idx (sub1 closer-idx)])
             (cond
               [(>= opener-idx 0)
                (define opener (vector-ref nodes opener-idx))
                (cond
                  [(and (delimiter-run? opener)
                        (delimiter-run-opener? opener)
                        (char=? (delimiter-run-char opener)
                                (delimiter-run-char closer))
                        (not (odd-delimiter-match? opener closer)))
                   (define opener-len (delimiter-run-length opener))
                   (define closer-len (delimiter-run-length closer))

                   (define-values [match-len make-element]
                     (if (and (>= opener-len 2) (>= closer-len 2))
                         (values 2 bold)
                         (values 1 italic)))

                   (define drop-opener? (= opener-len match-len))
                   (define drop-closer? (= closer-len match-len))
                   (define element-idx (if drop-opener? opener-idx (add1 opener-idx)))
                   (define splice-idx (if drop-closer? (add1 closer-idx) closer-idx))
                   (define num-removed (- splice-idx (add1 element-idx)))

                   (define element (make-element (freeze-delimiter-runs nodes (add1 opener-idx) closer-idx)))

                   (unless drop-opener?
                     (vector-set! nodes opener-idx
                                  (struct-copy delimiter-run opener
                                               [length (- opener-len match-len)])))
                   (unless drop-closer?
                     (vector-set! nodes closer-idx
                                  (struct-copy delimiter-run closer
                                               [length (- closer-len match-len)])))

                   (vector-set! nodes element-idx element)
                   (vector-copy! nodes (add1 element-idx) nodes splice-idx num-nodes)
                   (set! num-nodes (- num-nodes num-removed))

                   (closer-loop (- closer-idx num-removed))]

                  [else
                   (opener-loop (sub1 opener-idx))])]
               [else
                (closer-loop (add1 closer-idx))]))]
          [else
           (closer-loop (add1 closer-idx))])))

    (freeze-delimiter-runs nodes 0 num-nodes))

  ;; Replaces all `delimiter-run` nodes with plain strings corresponding to
  ;; their textual content. At the same time, it also “cleans up” the result by
  ;; merging adjacent strings into a single string, which avoids lists of lots
  ;; of tiny strings.
  (define (freeze-delimiter-runs vec start stop)
    (let loop ([i (sub1 stop)]
               [nodes '()])
      (cond
        [(< i start)
         (match nodes
           [(list node) node]
           [_           nodes])]
        [else
         (define node (freeze-delimiter-run (vector-ref vec i)))
         (cond
           ; If this node is a string, and there are still more nodes to go,
           ; switch into string-scanning mode to concatenate runs of strings.
           [(and (> i start) (string? node))
            (let string-loop ([i (sub1 i)]
                              [str-nodes (list node)])
              (cond
                [(< i start)
                 (loop i (cons (string-append* str-nodes) nodes))]
                [else
                 (define node (freeze-delimiter-run (vector-ref vec i)))
                 (if (string? node)
                     (string-loop (sub1 i) (cons node str-nodes))
                     (loop i (cons (string-append* str-nodes) nodes)))]))]
           [else
            (loop (sub1 i) (cons node nodes))])])))

  (define (freeze-delimiter-run node)
    (if (delimiter-run? node)
        (match* {(delimiter-run-length node) (delimiter-run-char node)}
          ; These cases are overwhelmingly common, so special case them to
          ; avoid allocating tons of identical tiny strings.
          [{1 #\*} "*"]
          [{1 #\_} "_"]
          [{2 #\*} "**"]
          [{2 #\_} "__"]
          [{3 #\*} "***"]
          [{3 #\_} "___"]
          [{_ _} (make-string (delimiter-run-length node)
                              (delimiter-run-char node))])
        node))

  ;; § 6.2 Emphasis and strong emphasis, Rule 9: If one of the delimiters can
  ;; both open and close emphasis, then the sum of the lengths of the delimiter
  ;; runs containing the opening and closing delimiters must not be a multiple
  ;; of 3 unless both lengths are multiples of 3.
  (define (odd-delimiter-match? opener closer)
    (and (or (delimiter-run-opener? closer)
             (delimiter-run-closer? opener))
         (not (zero? (remainder (delimiter-run-orig-length closer) 3)))
         (zero? (remainder (+ (delimiter-run-orig-length closer)
                              (delimiter-run-orig-length opener))
                           3))))

  (match-define-values [nodes _] (read-full-sequence #f))
  (process-emphasis nodes))

;; § 6.2 Emphasis and strong emphasis
(struct delimiter-run (char length orig-length opener? closer?) #:transparent)
(define (flanking? outer-c inner-c)
  (and (not (unicode-whitespace?* inner-c))
       (or (not (unicode-punctuation?* inner-c))
           (unicode-whitespace?* outer-c)
           (unicode-punctuation?* outer-c))))
(define (unicode-whitespace?* c)
  (or (not c) (eof-object? c) (unicode-whitespace? c)))
(define (unicode-punctuation?* c)
  (and (char? c) (unicode-punctuation? c)))
