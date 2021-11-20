#lang scribble/manual

@(require (for-label commonmark
                     commonmark/struct
                     racket/base
                     racket/contract
                     racket/port
                     (except-in xml document document? struct:document))
          racket/format
          racket/string
          scribble/core
          scribble/decode
          scribble/example
          threading)

@title{CommonMark: Standard Markdown}
@author{@author+email["Alexis King" "lexi.lambda@gmail.com"]}
@margin-note{The source of this manual is available on @hyperlink["https://github.com/lexi-lambda/racket-commonmark/blob/master/commonmark-doc/scribblings/commonmark.scrbl"]{GitHub.}}

@(define (reftech . pre-content)
   (apply tech pre-content #:doc '(lib "scribblings/reference/reference.scrbl")))
@(define (xml-tech . pre-content)
   (apply tech pre-content #:doc '(lib "xml/xml.scrbl")))
@(define X-expression @xml-tech{X-expression})
@(define X-expressions @xml-tech{X-expressions})

@(define mod:markdown @racketmodname[markdown #:indirect])

@(define CommonMark @hyperlink["https://commonmark.org/"]{CommonMark})

@(define (cm-link #:style [style #f] tag . pre-content)
   (apply hyperlink
          #:style style
          (~a "https://spec.commonmark.org/0.30/#"
              (~> (string-downcase tag)
                  (string-replace #px"[^a-z]+" "-")))
          pre-content))

@(define (cm-tech . pre-content)
   (define content (decode-content pre-content))
   (cm-link (content->string content) content))
@(define (cm-section . pre-content)
   (define content (decode-content pre-content))
   (cm-link (content->string content) "§" ~ content))

@(define (see-cm what where)
   @margin-note{See @where in the @CommonMark specification for more information about @|what|.})

@(define make-commonmark-eval (make-eval-factory '(commonmark
                                                   commonmark/struct
                                                   racket/list
                                                   racket/match)))
@(define-syntax-rule (cm-examples body ...)
   (examples #:eval (make-commonmark-eval) #:once body ...))

@defmodule[commonmark]{

The @racketmodname[commonmark] library implements a @|CommonMark|-compliant Markdown parser. Currently, it passes all test cases in @hyperlink["https://spec.commonmark.org/0.30/"]{v0.30 of the specification}. No extensions beyond the syntax specified in the standard are currently supported.

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
         [(document v) (document (split-inline-strs v))]
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

@section[#:tag "structure"]{Document structure}
@defmodule[commonmark/struct]{

The @racket[commonmark/struct] module provides @reftech{structure types} used to represent the abstract syntax of Markdown content. The root of the syntax tree hierarchy is a @tech{document}, which contains @tech{blocks}, which in turn contain @tech{inline content}. Most users will not need to interact with these structures directly, but doing so can be useful to perform additional processing on the document before rendering it, or to render Markdown to a format other than HTML.

Note that the bindings in this section are only provided by @racketmodname[commonmark/struct], @emph{not} @racketmodname[commonmark].}

@defstruct*[document ([blocks (listof block?)]) #:transparent]{
A parsed Markdown @deftech{document}, which is simply a @tech{flow}. It can be parsed from Markdown input using @racket[read-document] or @racket[string->document] and can be rendered to HTML using @racket[document->html].}

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

Returns @racket[#t] if @racket[v] is @deftech{inline content}: a @reftech{string}, @tech{italic span}, @tech{bold span}, @tech{code span}, @tech{link}, @tech{image}, @tech{HTML span}, @tech{hard line break}, or @reftech{list} of @tech{inline content}. Otherwise, returns @racket[#f].}

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

@defstruct*[html ([content string?]) #:transparent]{
@see-cm[@tech{HTML spans} @cm-section{Raw HTML}]

An @deftech{HTML span} is @tech{inline content} that contains raw HTML content (and will be left unescaped in HTML output). Note that, in general, the content may not actually be well-formed HTML, as @CommonMark simply treats everything that “looks sufficiently like” HTML---according to some heuristics---as raw HTML.}

@deftogether[(@defthing[line-break line-break?]
              @defproc[(line-break? [v any/c]) boolean?])]{
@see-cm[@tech{hard line breaks} @cm-section{Hard line breaks}]

A @deftech{hard line break} is @tech{inline content} used for separating inline content within a block. In HTML output, it corresponds to a @tt{<br>} element.}

@section[#:tag "versus-markdown"]{Comparison with @mod:markdown}

The @racketmodname[commonmark] library is not the first Markdown parser implemented in Racket: it is long predated by the venerable @mod:markdown library, which in fact also predates the @CommonMark specification itself. The libraries naturally provide similar functionality, but there are some key differences:

@itemlist[
  @item{Most obviously and most significantly, @racketmodname[commonmark] conforms to the @CommonMark specification, while @mod:markdown does not. This has both pros and cons:

        @itemlist[
          @item{@racketmodname[commonmark] enjoys consistency with other @CommonMark implementations and is therefore likely to perform better on existing Markdown content than @mod:markdown is. Additionally, @racketmodname[commonmark] handles some tricky edge cases more gracefully than @mod:markdown does, such as @hyperlink["https://github.com/greghendershott/markdown/issues/64"]{parsing of emphasis adjacent to Unicode punctuation}.}

          @item{On the other hand, @mod:markdown is more featureful than @racketmodname[commonmark], as it supports syntax not included in the @CommonMark specification (such as footnotes). Additionally, some users may find some of the ways that @|mod:markdown|’s parser diverges from the @CommonMark specification more intuitive (which is largely just a matter of personal taste).}]}

  @item{@racketmodname[commonmark] provides a full Markdown AST, while @mod:markdown always parses directly to HTML (in the form of @X-expressions). For many users, this difference is unlikely to be important, as almost all uses of Markdown render it to HTML, anyway. However, the option to process the intermediate representation affords additional flexibility if it is needed.}

  @item{@racketmodname[commonmark] is appreciably faster than @|mod:markdown|. On most documents, @mod:markdown is about 5× slower than @racketmodname[commonmark], but the performance gap increases dramatically given unusually large inputs: @mod:markdown is about 8× slower to parse a 4 MiB document and @bold{28× slower} to parse an 11 MiB document.}]

Takeaway: if you need the extra features provided by @mod:markdown, use @mod:markdown, otherwise use @racketmodname[commonmark].
