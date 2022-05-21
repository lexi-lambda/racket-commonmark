#lang scribble/manual

@(require (for-label commonmark
                     commonmark/struct
                     racket/base
                     racket/contract
                     racket/port
                     (except-in xml document document? struct:document))
          (only-in commonmark/parse current-parse-footnotes?)
          racket/format
          racket/string
          scribble/core
          scribble/decode
          scribble/example
          scribble/html-properties
          threading
          "commonmark/private/scribble-render.rkt")

@title{CommonMark: Standard Markdown}
@author{@author+email["Alexis King" "lexi.lambda@gmail.com"]}
@margin-note{The source of this manual is available on @hyperlink["https://github.com/lexi-lambda/racket-commonmark/blob/master/commonmark-doc/scribblings/commonmark.scrbl"]{GitHub.}}

@(define highlight-style (style 'tt (list (alt-tag "mark")
                                          (make-background-color-property "yellow"))))
@(define (highlight . content)
   (element highlight-style content))

@(define (reftech . pre-content)
   (apply tech pre-content #:doc '(lib "scribblings/reference/reference.scrbl")))
@(define (xml-tech . pre-content)
   (apply tech pre-content #:doc '(lib "xml/xml.scrbl")))
@(define X-expression @xml-tech{X-expression})
@(define X-expressions @xml-tech{X-expressions})

@(define mod:markdown @racketmodname[markdown #:indirect])

@(define CommonMark @hyperlink["https://commonmark.org/"]{CommonMark})
@(define cmark-gfm @hyperlink["https://github.com/github/cmark-gfm"]{cmark-gfm})

@(define (cm-link #:singularize? [singularize? #f] #:style [style #f] tag . pre-content)
   (define maybe-singularize (if singularize?
                                 (λ~> (string-replace #px"s$" ""))
                                 values))
   (apply hyperlink
          #:style style
          (~a "https://spec.commonmark.org/0.30/#"
              (~> (string-foldcase tag)
                  maybe-singularize
                  (string-replace #px"[^a-z]+" "-")))
          pre-content))

@(define (cm-tech . pre-content)
   (define content (decode-content pre-content))
   (cm-link (content->string content) content #:singularize? #t))
@(define (cm-section . pre-content)
   (define content (decode-content pre-content))
   (cm-link (content->string content) "§" ~ content))

@(define (see-cm what where)
   @margin-note{See @where in the @CommonMark specification for more information about @|what|.})
@(define (see-extension what where)
   @margin-note{@what are an @tech{extension} to the @CommonMark specification and are not enabled by default; see @where in the @secref{extensions} section of this manual for more details.})

@(define make-commonmark-eval (make-eval-factory '(commonmark
                                                   commonmark/struct
                                                   racket/list
                                                   racket/match)))
@(define-syntax-rule (cm-examples body ...)
   (examples #:eval (make-commonmark-eval) #:once body ...))

@defmodule[commonmark]{

The @racketmodname[commonmark] library implements a @|CommonMark|-compliant Markdown parser. Currently, it passes all test cases in @hyperlink["https://spec.commonmark.org/0.30/"]{v0.30 of the specification}. By default, only the Markdown features specified by @CommonMark are supported, but non-standard support for @tech{footnotes} can be optionally enabled; see the @secref{extensions} section of this manual for more details.

The @racketmodname[commonmark] module reprovides all of the bindings provided by @racketmodname[commonmark/parse] and @racketmodname[commonmark/render/html] (but @emph{not} the bindings provided by @racketmodname[commonmark/struct]).}

@local-table-of-contents[]

@section[#:tag "quick-start"]{Quick start}

@(define quick-eval (make-base-eval))

@margin-note{For information about the Markdown syntax supported by @racketmodname[commonmark], see the @CommonMark website.}

In @racketmodname[commonmark], processing Markdown is split into two steps: @seclink["parsing"]{parsing} and @seclink["rendering-html"]{rendering}. To get started, use @racket[string->document] or @racket[read-document] to parse Markdown input into a @tech{document} structure:

@(examples
  #:eval quick-eval
  #:label #f
  (eval:alts @#,racket[(require @#,racketmodname[commonmark])]
             (require commonmark))
  (define doc (string->document "*Hello*, **markdown**!"))
  doc)

A @tech{document} is an abstract syntax tree that represents Markdown content. If you’d like, you can choose to render it however you wish, but most uses of Markdown render it to HTML, so @racketmodname[commonmark] provides the @racket[document->html] and @racket[write-document-html] functions, which render a @tech{document} to HTML in the way recommended by the @CommonMark specification:

@(examples
  #:eval quick-eval
  #:label #f
  (write-document-html doc))

The @racket[document->xexprs] function can also be used to render a @tech{document} to a @reftech{list} of @X-expressions, which can make it more convenient to incorporate rendered Markdown into a larger HTML document (though do be aware of the caveats involving @tech{HTML blocks} and @tech{HTML spans} described in the documentation for @racket[document->xexprs]):

@(examples
  #:eval quick-eval
  #:label #f
  (document->xexprs doc))

@(close-eval quick-eval)

@section[#:tag "parsing"]{Parsing}
@declare-exporting[commonmark/parse commonmark]
@defmodule[commonmark/parse #:no-declare]{

The @racketmodname[commonmark/parse] module provides functions for parsing Markdown content into a @tech{document} structure. To render Markdown to HTML, use this module in combination with the functions provided by @racketmodname[commonmark/render/html].

All of the bindings provided by @racketmodname[commonmark/parse] are also provided by @racketmodname[commonmark].}

@defproc[(string->document [str string?]) document?]{
Parses @racket[str] as a Markdown @tech{document}.

@(cm-examples
  #:label "Example:"
  (define doc (string->document "*Hello*, **markdown**!"))
  doc
  (write-document-html doc))

This function cannot fail: every string of Unicode characters can---somehow---be interpreted as Markdown. Of course, the interpretation may be somewhat tortured if applied to input for which such interpretation was not intended.}

@defproc[(read-document [in input-port?]) document?]{
Like @racket[string->document], but the input is read from the given @reftech{input port} rather than from a @reftech{string}.

@(cm-examples
  #:label "Example:"
  (define doc (read-document (open-input-string "*Hello*, **markdown**!")))
  doc
  (write-document-html doc))

This function can sometimes be more efficient than @racket[(read-document (port->string in))], but probably not significantly so, as the entire @tech{document} structure must be realized in memory regardless.}

@defboolparam[current-parse-footnotes? parse-footnotes? #:value #f]{
Enables or disables @tech{footnote} parsing, which is an @tech{extension} to the @CommonMark specification; see @secref{extension:footnotes} for more details.

Note that the value of @racket[current-parse-footnotes?] only affects parsing, @emph{not} rendering. If a @tech{document} containing @tech{footnotes} is rendered to HTML, the @tech{footnotes} will still be rendered even if @racket[(current-parse-footnotes?)] is @racket[#f].

@history[#:added "1.1"]}

@section[#:tag "rendering-html"]{Rendering HTML}
@declare-exporting[commonmark/render/html commonmark]
@defmodule[commonmark/render/html #:no-declare]{

The @racketmodname[commonmark/render/html] module provides functions for rendering a parsed Markdown @tech{document} to HTML, as recommended by the @CommonMark specification. This module should generally be used in combination with @racketmodname[commonmark/parse], which provides functions for producing a @tech{document} structure from Markdown input.

All of the bindings provided by @racketmodname[commonmark/render/html] are also provided by @racketmodname[commonmark].}

@defproc[(document->html [doc document?]) string?]{
Renders @racket[doc] to HTML in the format recommended by the @CommonMark specification.

@(cm-examples
  (document->html (string->document "*Hello*, **markdown**!")))}

@defproc[(write-document-html [doc document?] [out output-port? (current-output-port)]) void?]{
Like @racket[document->html], but writes the rendered HTML directly to @racket[out] rather than returning it as a @reftech{string}.

@(cm-examples
  (write-document-html (string->document "*Hello*, **markdown**!")))}

@defproc[(document->xexprs [doc document?]) (listof xexpr/c)]{
Like @racket[document->html], but returns the rendered HTML as a @reftech{list} of @X-expressions rather than as a string.

@(cm-examples
  (document->xexprs (string->document "*Hello*, **markdown**!")))

Note that @tech{HTML blocks} and @tech{HTML spans} are not parsed and may not even contain valid HTML, which makes them difficult to represent as an @|X-expression|. As a workaround, raw HTML will be represented as @racket[cdata] elements:

@(cm-examples
  #:label #f
  (document->xexprs
   (string->document "A paragraph with <marquee>raw HTML</marquee>.")))

This generally works out okay, since @racket[cdata] elements render directly as their unescaped content, but it is, strictly speaking, an abuse of @racket[cdata].}

@deftogether[(@defparam[current-italic-tag tag symbol? #:value 'em]
              @defparam[current-bold-tag tag symbol? #:value 'strong])]{
These @reftech{parameters} determine which HTML tag is used to render @tech{italic spans} and @tech{bold spans}, respectively. The default values of @racket['em] and @racket['strong] correspond to those required by the @CommonMark specification, but this can be semantically incorrect if “emphasis” syntax is used for purposes other than emphasis, such as italicizing the title of a book.

Reasonable alternate values for @racket[current-italic-tag] and @racket[current-bold-tag] include @racket['i], @racket['b], @racket['mark], @racket['cite], or @racket['defn], all of which are elements with semantic (rather than presentational) meaning in HTML5. Of course, the “most correct” choice depends on how @tech{italic spans} and @tech{bold spans} will actually be used, so no one set of choices can be universally called the best.

@(cm-examples
  (eval:alts
   (parameterize ([current-italic-tag 'cite]
                  [current-bold-tag 'mark])
     (document->xexprs
      (string->document
       (string-append
        "> First, programming is about stating and solving problems,\n"
        "> and this activity normally takes place in a context with its\n"
        "> own language of discourse; **good programmers ought to\n"
        "> formulate this language as a programming language**.\n"
        "\n"
        "— *The Racket Manifesto* (emphasis mine)"))))
   (parameterize ([current-italic-tag 'cite]
                  [current-bold-tag 'mark])
     ; In this example, we’ll end up with really long strings in the output
     ; containing \n characters, which looks bad in the docs, so we want to quietly
     ; split them on \n characters just to make the example’s output more readable.
     (define (split-inline-strs v)
       (match v
         [(? list?) (flatten (map split-inline-strs v))]
         [(document v fns) (document (split-inline-strs v) fns)]
         [(paragraph v) (paragraph (split-inline-strs v))]
         [(blockquote v) (blockquote (split-inline-strs v))]
         [(bold v) (bold (split-inline-strs v))]
         [(italic v) (italic (split-inline-strs v))]
         [(? string?) (regexp-split #px"(?<=\n)" v)]))
     (document->xexprs
      (split-inline-strs
       (string->document
        (string-append
         "> First, programming is about stating and solving problems,\n"
         "> and this activity normally takes place in a context with its\n"
         "> own language of discourse; **good programmers ought to\n"
         "> formulate this language as a programming language**.\n"
         "\n"
         "— *The Racket Manifesto* (emphasis mine)")))))))}

@defparam[current-anchor-proc proc (-> string? string?) #:value values]{
This @reftech{parameter} determines the content of HTML element @tt{id}s referenced by anchor links, such as @tech{footnote references} and @tech{backreference links}. It is set to a function that takes as its only argument the value which the renderer would use for the element’s @tt{id} by default. The return value of the function is used as the element’s @tt{id}.

Use this parameter when it is likely that rendered @tech{documents} will appear together, such as on a blog that displays the full text of multiple posts on a single page. The use of a custom @racket[_proc] in these cases can prevent a scenario where the use of identical footnote labels (e.g. @racketvalfont{[^1]}, @racketvalfont{[^2]}, etc.) in multiple Markdown sources result in colliding anchor links.

@(cm-examples
  (define doc
    (parameterize ([current-parse-footnotes? #t])
      (string->document
        (string-append "Hello[^1]\n"
                       "\n"
                       "[^1]: Goodbye."))))
  ; Selecting only the body portion for brevity
  (car (document->xexprs doc))
  (parameterize ([current-anchor-proc (λ (id) (format "~a_post01" id))])
    (car (document->xexprs doc))))}

@section[#:tag "structure"]{Document structure}
@defmodule[commonmark/struct]{

The @racket[commonmark/struct] module provides @reftech{structure types} used to represent the abstract syntax of Markdown content. The root of the syntax tree hierarchy is a @tech{document}, which contains @tech{blocks}, which in turn contain @tech{inline content}. Most users will not need to interact with these structures directly, but doing so can be useful to perform additional processing on the document before rendering it, or to render Markdown to a format other than HTML.

Note that the bindings in this section are only provided by @racketmodname[commonmark/struct], @emph{not} @racketmodname[commonmark].}

@defstruct*[document ([blocks (listof block?)]
                      [footnotes (listof footnote-definition?)])
            #:transparent]{
A parsed Markdown @deftech{document}, which has a body @tech{flow} and a @reftech{list} of @tech{footnote definitions}. It can be parsed from Markdown input using @racket[read-document] or @racket[string->document] and can be rendered to HTML using @racket[document->html].

@history[#:changed "1.1" @elem{Added the @racket[footnotes] field.}]}

@defstruct*[footnote-definition ([blocks (listof block?)] [label string?]) #:transparent]{
@see-extension[@tech{Footnotes} @secref{extension:footnotes}]

A @tech{footnote definition} contains a @tech{flow} that can be referenced by a @tech{footnote reference} via its @tech{footnote label}.

Note: although @tech{footnote definitions} are syntactically blocks in Markdown input, they are @emph{not} a type of @tech{block} (as recognized by the @racket[block?] predicate) and cannot be included directly in the main @tech{document} @tech{flow}. @tech{Footnote definitions} are collected into the separate @racket[document-footnotes] field of the @tech{document} structure during parsing, since they represent auxiliary definitions, and their precise location in the Markdown input does not matter.

(This is quite similar to the way the parser processes @cm-tech{link reference definitions}, except that @tech{footnote definitions} must be retained separately for later rendering, whereas @cm-tech{link reference definitions} can be discarded after all link targets have been resolved.)

@history[#:added "1.1"]}

@subsection[#:tag "blocks"]{Blocks}

@defproc[(block? [v any/c]) boolean?]{
@see-cm[@tech{blocks} @cm-section{Blocks and inlines}]

Returns @racket[#t] if @racket[v] is a @deftech{block}: a @tech{paragraph}, @tech{itemization}, @tech{block quote}, @tech{code block}, @tech{HTML block}, @tech{heading}, or @tech{thematic break}. Otherwise, returns @racket[#f].

A @deftech{flow} is a list of @tech{blocks}. The body of a @tech{document}, the contents of a @tech{block quote}, and each item in an @tech{itemization} are flows.}

@defstruct*[paragraph ([content inline?]) #:transparent]{
@see-cm[@tech{paragraphs} @cm-section{Paragraphs}]
                                                         
A @deftech{paragraph} is a @tech{block} that contains @tech{inline content}. In HTML output, it corresponds to a @tt{<p>} element. Most blocks in a @tech{document} are usually paragraphs.}

@defstruct*[itemization ([blockss (listof (listof block?))]
                         [style (or/c 'loose 'tight)]
                         [start-num (or/c exact-nonnegative-integer? #f)])
            #:transparent]{
@see-cm[@tech{itemizations} @elem{@cm-section{Lists} and @cm-section{List items}}]
 
An @deftech{itemization} is a @tech{block} that contains a list of @tech{flows}. In HTML output, it corresponds to a @tt{<ul>} or @tt{<ol>} element.

The @racket[style] field records whether the itemization is @cm-tech{loose} or @cm-tech{tight}: if @racket[style] is @racket['tight], paragraphs in HTML output are not wrapped in @tt{<p>} tags.

If @racket[start-num] is @racket[#f], then the itemization represents a @cm-tech{bullet list}. Otherwise, the itemization represents an @cm-tech{ordered list}, and the value of @racket[start-num] is its @cm-tech{start number}.}

@defstruct*[blockquote ([blocks (listof block?)]) #:transparent]{
@see-cm[@tech{block quotes} @cm-section{Block quotes}]

A @deftech{block quote} is a @tech{block} that contains a nested @tech{flow}. In HTML output, it corresponds to a @tt{<blockquote>} element.}

@defstruct*[code-block ([content string?] [info-string (or/c string? #f)]) #:transparent]{
@see-cm[@tech{code blocks} @elem{@cm-section{Indented code blocks} and @cm-section{Fenced code blocks}}]

A @deftech{code block} is a @tech{block} that has unformatted content and an optional info string. In HTML output, it corresponds to a @tt{<pre>} element that contains a @tt{<code>} element.

The @CommonMark specification does not mandate any particular treatment of the info string, but it notes that “the first word is typically used to specify the language of the code block.” In HTML output, the language is indicated by adding a CSS class to the rendered @tt{<code>} element consisting of @litchar{language-} followed by the language name, per the spec’s recommendation.}

@defstruct*[html-block ([content string?]) #:transparent]{
@see-cm[@tech{HTML blocks} @cm-section{HTML Blocks}]

An @deftech{HTML block} is a @tech{block} that contains raw HTML content (and will be left unescaped in HTML output). Note that, in general, the content may not actually be well-formed HTML, as @CommonMark simply treats everything that “looks sufficiently like” HTML---according to some heuristics---as raw HTML.}

@defstruct*[heading ([content inline?] [depth (integer-in 1 6)]) #:transparent]{
@see-cm[@tech{headings} @elem{@cm-section{ATX headings} and @cm-section{Setext headings}}]

A @deftech{heading} has @tech{inline content} and a @tech{heading depth}. In HTML output, it corresponds to one of the @tt{<h1>} through @tt{<h6>} elements.

A @deftech{heading depth} is an integer between @racket[1] and @racket[6], inclusive, where higher numbers correspond to more-nested headings.}

@deftogether[(@defthing[thematic-break thematic-break?]
              @defproc[(thematic-break? [v any/c]) boolean?])]{
@see-cm[@tech{thematic breaks} @cm-section{Thematic breaks}]

A @deftech{thematic break} is a @tech{block}. It is usually rendered as a horizontal rule, and in HTML output, it corresponds to an @tt{<hr>} element.}

@subsection[#:tag "inlines"]{Inline content}

@defproc[(inline? [v any/c]) boolean?]{
@see-cm[@tech{inline content} @cm-section{Blocks and inlines}]

Returns @racket[#t] if @racket[v] is @deftech{inline content}: a @reftech{string}, @tech{italic span}, @tech{bold span}, @tech{code span}, @tech{link}, @tech{image}, @tech{footnote reference}, @tech{HTML span}, @tech{hard line break}, or @reftech{list} of @tech{inline content}. Otherwise, returns @racket[#f].}

@defstruct*[italic ([content inline?]) #:transparent]{
@see-cm[@tech{italic spans} @cm-section{Emphasis and strong emphasis}]

An @deftech{italic span} is @tech{inline content} that contains nested @tech{inline content}. By default, in HTML output, it corresponds to an @tt{<em>} element (but an alternate tag can be used by modifying @racket[current-italic-tag]).}

@defstruct*[bold ([content inline?]) #:transparent]{
@see-cm[@tech{bold spans} @cm-section{Emphasis and strong emphasis}]

A @deftech{bold span} is @tech{inline content} that contains nested @tech{inline content}. By default, in HTML output, it corresponds to a @tt{<strong>} element (but an alternate tag can be used by modifying @racket[current-bold-tag]).}

@defstruct*[code ([content string?]) #:transparent]{
@see-cm[@tech{code spans} @cm-section{Code spans}]

A @deftech{code span} is @tech{inline content} that contains unformatted content. In HTML output, it corresponds to a @tt{<code>} element.}

@defstruct*[link ([content inline?] [dest string?] [title (or/c string? #f)]) #:transparent]{
@see-cm[@tech{links} @cm-section{Links}]

A @deftech{link} is @tech{inline content} that contains nested @tech{inline content}, a link destination, and an optional link title. In HTML output, it corresponds to an @tt{<a>} element.}

@defstruct*[image ([description inline?] [source string?] [title (or/c string? #f)]) #:transparent]{
@see-cm[@tech{images} @cm-section{Images}]

An @deftech{image} is @tech{inline content} with a source path or URL that should point to an image. It has an @tech{inline content} description (which is used as the @tt{alt} attribute in HTML output) and an optional title. In HTML output, it corresponds to an @tt{<img>} element.}

@defstruct*[footnote-reference ([label string?]) #:transparent]{
@see-extension[@tech{Footnotes} @secref{extension:footnotes}]

A @tech{footnote reference} is @tech{inline content} that references a @tech{footnote definition} with a matching @tech{footnote label}. In HTML output, it corresponds to a superscript @tt{<a>} element.

@history[#:added "1.1"]}

@defstruct*[html ([content string?]) #:transparent]{
@see-cm[@tech{HTML spans} @cm-section{Raw HTML}]

An @deftech{HTML span} is @tech{inline content} that contains raw HTML content (and will be left unescaped in HTML output). Note that, in general, the content may not actually be well-formed HTML, as @CommonMark simply treats everything that “looks sufficiently like” HTML---according to some heuristics---as raw HTML.}

@deftogether[(@defthing[line-break line-break?]
              @defproc[(line-break? [v any/c]) boolean?])]{
@see-cm[@tech{hard line breaks} @cm-section{Hard line breaks}]

A @deftech{hard line break} is @tech{inline content} used for separating inline content within a block. In HTML output, it corresponds to a @tt{<br>} element.}

@section[#:tag "extensions"]{Extensions}

By default, @racketmodname[commonmark] adheres precisely to the @CommonMark specification, which is the subset of Markdown that behaves consistently across implementations. However, many Markdown libraries implement @deftech{extensions} beyond what is specified, several of which are useful enough to have become @italic{de facto} standards across major Markdown implementations.

Unfortunately, since such features are not precisely specified, implementations of Markdown extensions rarely agree on how exactly they ought to be parsed and rendered, especially when interactions with other Markdown features leave edge cases and ambiguities. @racketmodname[commonmark] therefore deviates from the standard only if explicitly instructed to do so, and hopefully programmers who choose to venture into such uncharted waters understand they bear some responsibility for what they are getting themselves into.

This section documents all of the extensions @racketmodname[commonmark] currently supports. Note that, due to their inherently ill-specified nature, it can sometimes be difficult to determine whether a divergence in behavior between two Markdown implementations constitutes a bug or two incompatible features. For that reason, backwards compatibility of extensions’ behavior may not be perfectly maintained wherever the interpretation is not sufficiently “obvious”. Consider yourself warned.

@subsection[#:tag "extension:footnotes"]{Footnotes}

@margin-note{Footnotes enjoy support from a wide variety of Markdown implementations, including @hyperlink["https://michelf.ca/projects/php-markdown/extra/#footnotes"]{PHP Markdown Extra}, @hyperlink["https://python-markdown.github.io/extensions/footnotes/"]{Python-Markdown}, @hyperlink["https://pandoc.org/MANUAL.html#footnotes"]{Pandoc}, @hyperlink["https://docs.github.com/en/github/writing-on-github/getting-started-with-writing-and-formatting-on-github/basic-writing-and-formatting-syntax#footnotes"]{GitHub Flavored Markdown}, and @|mod:markdown|. The @tt{[^label]} syntax for references and definitions is nearly universal, but minor differences exist in interpretation, and rendering varies significantly. @racketmodname[commonmark]’s implementation is not precisely identical to any of them, but it was originally based on the @cmark-gfm implementation of GitHub Flavored Markdown.}

@deftech{Footnotes} allow auxiliary information to be lifted out of the main document flow to avoid cluttering the body text. When footnote parsing is enabled via the @racket[current-parse-footnotes?] parameter, @cm-tech{shortcut reference links} with a @cm-tech{link label} that begins with a @litchar{^} character are instead parsed as @deftech{footnote references}. For example, the following @tech{paragraph} includes three footnote references:

@nested[#:style 'code-inset]{@verbatim{
  Racket is a programming language@highlight{[^1]} descended from Scheme.@highlight{[^scheme]}
  Although not all Racket programs retain Lisp syntax, most Racket
  programs still include a great many parentheses.@highlight{[^(()())]}}}

Text between the @litchar{[^} and @litchar{]} characters constitutes the @deftech{footnote label}, and the content of the footnote is provided via a @deftech{footnote definition} with a @cm-link["matches"]{matching} @tech{footnote label}. Footnote definitions have similar syntax to @cm-tech{link reference definitions}, but unlike link reference definitions the body of a footnote definition is an arbitrary @tech{flow}. For example, the following syntax defines two footnotes matched by the @tech{footnote references} above:

@nested[#:style 'code-inset]{@verbatim{
  [^1]: Technically, the name *Racket* refers to both the runtime
      environment and the primary language used to program it.

  [^scheme]: The original name for the Racket project was PLT Scheme,
      but it was renamed in 2010 [to avoid confusion and to reflect its
      departure from its roots](https://racket-lang.org/new-name.html).}}

Syntactically, footnote definitions are a type of @cm-link["container-blocks"]{container block} and may appear within any @tech{flow}, though they are not semantically children of any flow in which they appear. Their placement does not affect their interpretation---a footnote reference may reference any footnote defined in the same document---unless two definitions have matching @tech{footnote labels}, in which case the later definition is ignored.

As mentioned above, a footnote definition may contain an arbitrary @tech{flow} consisting of any number of @tech{blocks}. All lines after the first must be indented by 4 spaces to be included in the definition (unless they are @cm-tech{lazy continuation lines}). For example, the following footnote definition includes a @tech{block quote}, an @cm-tech{indented code block}, and a @tech{paragraph}:

@nested[#:style 'code-inset]{@verbatim{
  [^long note]:
      > This is a block quote that is nested inside
      > the body of a footnote.

          This is an indented code block
          inside of a footnote.

      This paragraph is also inside the footnote.}}

A footnote reference @emph{must} @cm-link["matches"]{match} a footnote definition somewhere in the document to be parsed as a footnote reference. If no such definition exists, the label will be parsed as literal text. Each footnote definition can be referenced an arbitrary number of times.

When footnotes are parsed, each @tech{footnote reference} is represented in-place by an instance of @racket[footnote-reference], but @tech{footnote definitions} are removed from the main @tech{document} flow and collected into a list of @racket[footnote-definition] instances in a separate @racket[document-footnotes] field. This allows renderers to more easily match references to their corresponding definitions and ensures that the placement of definitions within a document cannot affect the rendered output.

When given a @tech{document} containing @tech{footnotes}, the @seclink["rendering-html"]{default HTML renderer} mimicks the output produced by @|cmark-gfm|. Specifically, the renderer appends a @tt{<section class="footnotes">} element to the end of the output, which wraps an @tt{<ol>} element containing the footnotes’ content:

@(parameterize ([current-parse-footnotes? #t])
   @markdown-example{
     Here is a paragraph[^1] with
     two footnote references.[^2]

     [^1]: Here is the first footnote.
     [^2]: And here is the second.})

Each rendered @tech{footnote definition} includes a @deftech{backreference link}, denoted by a @litchar{↩} character, that links to the corresponding @tech{footnote reference} in the body text. If a definition is referenced multiple times, the rendered footnote will include multiple backreference links:

@(parameterize ([current-parse-footnotes? #t])
   @markdown-example{
     Here is a paragraph[^1] that
     references a footnote twice.[^1]

     [^1]: Here is the footnote.})

In both of the previous examples, the chosen @tech{footnote labels} happen to line up with the rendered footnote numbers, but in general, that does not need to be the case. Footnote references are @emph{always} rendered numerically, in the order they appear in the document, regardless of the footnote labels used in the document’s source:

@margin-note{Although footnotes are visually renumbered by the renderer, the generated links and link anchors are based on the original @tech{footnote labels}. This means that a link to particular @tech{footnote definition} will remain stable even if a document is modified as long as its label remains unchanged.}

@(parameterize ([current-parse-footnotes? #t])
   @markdown-example{
     Here are some footnotes[^a]
     with non-numeric[^b] names.

     And here are some footnotes[^2]
     numbered out of order.[^3]

     [^a]: Here is footnote a.
     [^b]: Here is footnote b.
     [^2]: Here is footnote 2.
     [^3]: Here is footnote 3.})

In a similar vein, the order in which footnote definitions appear does not matter, as they will be rendered in the order they are first referenced in the document. If a definition is never referenced, it will not be rendered at all:

@(parameterize ([current-parse-footnotes? #t])
   @markdown-example{
     Here is a paragraph[^1] with
     two footnote references.[^3]

     [^3]: Here is footnote 3.
     [^2]: Here is footnote 2.
     [^1]: Here is footnote 1.})

Footnote references may appear inside footnote definitions, and @racketmodname[commonmark] will not object (though your readers might). Footnotes that are first referenced in a footnote definition will be numbered so that they immediately follow the referencing footnote:

@(parameterize ([current-parse-footnotes? #t])
   @markdown-example{
     Here is a paragraph[^1] with
     two footnote references.[^2]

     [^1]: Here is footnote 1.[^3]
     [^2]: Here is footnote 2.
     [^3]: Here is footnote 3.})

Note that while matching footnote references to their corresponding definitions is handled by the parser, pruning and renumbering of footnote definitions is handled entirely by the renderer, which allows alternate renderers to use alternate schemes if they so desire.

@section[#:tag "versus-markdown"]{Comparison with @mod:markdown}

The @racketmodname[commonmark] library is not the first Markdown parser implemented in Racket: it is long predated by the venerable @mod:markdown library, which in fact also predates the @CommonMark specification itself. The libraries naturally provide similar functionality, but there are some key differences:

@itemlist[
  @item{Most obviously and most significantly, @racketmodname[commonmark] conforms to the @CommonMark specification, while @mod:markdown does not. This has both pros and cons:

        @itemlist[
          @item{@racketmodname[commonmark] enjoys consistency with other @CommonMark implementations and is therefore likely to behave better on existing Markdown content than @mod:markdown is. Additionally, @racketmodname[commonmark] handles some tricky edge cases more gracefully than @mod:markdown does, such as @hyperlink["https://github.com/greghendershott/markdown/issues/64"]{parsing of emphasis adjacent to Unicode punctuation}.}

          @item{On the other hand, @mod:markdown is more featureful than @racketmodname[commonmark], as it provides some @tech{extensions} that @racketmodname[commonmark] does not. Additionally, some users may find some of the ways that @|mod:markdown|’s parser diverges from the @CommonMark specification more intuitive (which is largely just a matter of personal taste).}]}

  @item{@racketmodname[commonmark] provides a full Markdown AST, while @mod:markdown always parses directly to HTML (in the form of @X-expressions). For many users, this difference is unlikely to be important, as almost all uses of Markdown render it to HTML, anyway. However, the option to process the intermediate representation affords additional flexibility if it is needed.}

  @item{@racketmodname[commonmark] is appreciably faster than @|mod:markdown|. On most documents, @mod:markdown is about 5× slower than @racketmodname[commonmark], but the performance gap increases dramatically given unusually large inputs: @mod:markdown is about 8× slower to parse a 4 MiB document and @bold{28× slower} to parse an 11 MiB document.}]

Takeaway: if you need the extra features provided by @mod:markdown, use @mod:markdown, otherwise use @racketmodname[commonmark].
