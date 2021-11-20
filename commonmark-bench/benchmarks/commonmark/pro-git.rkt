#lang racket/base

;; This module benchmarks commonmark against markdown, using an input corpus
;; derived by concatenating the Markdown sources of all the localizations of the
;; first edition of Pro Git by Scott Chacon. (This is the benchmarking technique
;; used by cmark <https://github.com/commonmark/cmark/blob/master/benchmarks.md>.)

(require benchmark
         net/git-checkout
         racket/file
         racket/format
         racket/list
         racket/match
         racket/path
         racket/port

         (prefix-in cm: commonmark)
         (prefix-in md: markdown))

(define-logger cm-bench)

(define current-build-directory (make-parameter "build"))

(define (bench-path . sub)
  (simplify-path (apply build-path (current-build-directory) "bench" sub)))

(define (clone-progit)
  (define dest-dir (bench-path "progit"))
  (cond
    [(directory-exists? dest-dir)
     (log-cm-bench-debug "clone-progit: ‘~a’ already exists, skipping" dest-dir)]
    [else
     (log-cm-bench-info "clone-progit: cloning into ‘~a’" dest-dir)
     (make-parent-directory* dest-dir)
     (git-checkout #:transport 'https "github.com" "progit/progit.git"
                   #:dest-dir dest-dir)])
  dest-dir)

(define document-sizes #(tiny small medium large))

(define (build-bench-inputs)
  (define progit-dir (clone-progit))
  (define langs '("ar" "az" "be" "ca" "cs" "de" "en" "eo" "es" "es-ni"
                       "fa" "fi" "fr" "hi" "hu" "id" "it" "ja" "ko"
                       "mk" "nl" "no-nb" "pl" "pt-br" "ro" "ru" "sr"
                       "th" "tr" "uk" "vi" "zh" "zh-tw"))

  (for/vector #:length (vector-length document-sizes) ([size (in-vector document-sizes)])
    (define out-path (bench-path "input" (~a size ".md")))
    (cond
      [(file-exists? out-path)
       (log-cm-bench-debug "build-bench-input: ‘~a’ already exists, skipping" out-path)
       (file->string out-path)]
      [else
       (log-cm-bench-info "build-bench-input: writing ‘~a’" out-path)
       (make-parent-directory* out-path)
       (define str-out (open-output-string))
       (call-with-output-file* #:mode 'text out-path
         (λ (out)
           (for* ([lang (in-list (match size
                                   [(or 'tiny 'small) '("en")]
                                   ['medium (take langs 15)]
                                   ['large langs]))]
                  [in-path (in-directory (match size
                                           ['tiny (build-path progit-dir lang "01-introduction")]
                                           [_ (build-path progit-dir lang)]))]
                  #:when (file-exists? in-path)
                  #:when (equal? (path-get-extension in-path) #".markdown"))
             (call-with-input-file* #:mode 'text in-path
               (λ (in) (copy-port in str-out out))))))
       (get-output-string str-out)])))

(define (benchmark-results-path)
  (bench-path "results" "result"))

(define (size->string bytes)
  (define Ki 1024)
  (define Mi (* Ki Ki))
  (cond
    [(< bytes Ki) (~a (~r (/ bytes Ki) #:precision 1) " KiB")]
    [(< bytes Mi) (~a (~r (/ bytes Ki) #:precision 0) " KiB")]
    [else         (~a (~r (/ bytes Mi) #:precision 0) " MiB")]))

(define (do-run-benchmarks #:num-trials [num-trials 1])
  (define bench-inputs (build-bench-inputs))
  (define results-file (benchmark-results-path))
  (make-parent-directory* results-file)

  (log-cm-bench-info "running benchmarks...")
  (run-benchmarks
   #:extract-time 'delta-time
   #:num-trials num-trials
   #:make-name (λ (size)
                 (define bytes (string-utf-8-length (vector-ref bench-inputs size)))
                 (~a (vector-ref document-sizes size) " (" (size->string bytes) ")"))
   #:results-file results-file
   (range (vector-length document-sizes))
   '([commonmark markdown])
   (λ (size impl)
     (define input (vector-ref bench-inputs size))
     (match impl
       ['commonmark (cm:document->html (cm:string->document input))]
       ['markdown (map md:xexpr->string (md:parse-markdown input))]))))

(module+ main
  (require plot
           racket/class)

  (define (visualize-benchmark-results [results (get-past-results (benchmark-results-path))])
    (parameterize ([plot-x-ticks no-ticks]
                   [current-benchmark-color-scheme (cons '("white" "black") '(solid))])
      (define frame
        (plot-frame
         #:title "commonmark vs markdown"
         #:x-label "input size"
         #:y-label "normalized time"
         (render-benchmark-alts '(commonmark) results)))
      (send frame show #t)))

  (visualize-benchmark-results (do-run-benchmarks)))
