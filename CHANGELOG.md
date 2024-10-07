## 1.2 (2024-10-07)

* Updated from CommonMark v0.30 to v0.31.2. ([f28bafb](https://github.com/lexi-lambda/racket-commonmark/commit/f28bafb69a3cdf4ffc9f0f0a77aefadb507421f7))

  The behavioral changes are quite minimal. The relevant bullets from [the CommonMark changelog](https://spec.commonmark.org/changelog.txt) are:

  > * Add symbols to unicode punctuation (Titus Wormer).
  > * Add `search` element to list of known block elements (Titus Wormer).
  > * Remove `source` element as HTML block start condition (Lukas Spieß).
  > * Remove restrictive limitation on inline comments; now we match the HTML spec (Titus Wormer).

## 1.1.1 (2024-10-07)

* Fixed bug that caused inline links to sometimes fail to parse. ([#4](https://github.com/lexi-lambda/racket-commonmark/issues/4), [f96082a](https://github.com/lexi-lambda/racket-commonmark/commit/f96082a21d5577c57c5c00d916f666567cb41a1c))
* Fixed nested list tightness sometimes being incorrect. ([#5](https://github.com/lexi-lambda/racket-commonmark/issues/5), [e0b9dec](https://github.com/lexi-lambda/racket-commonmark/commit/e0b9dec454e9ebca23c4578f7e58a3774546d4a9))

## 1.1 (2021-11-22)

* Added support for footnotes as an optional extension. ([d40156b](https://github.com/lexi-lambda/racket-commonmark/commit/d40156bce42088aea1a742d6cce4c8697318db70))

## 1.0 (2021-11-20)

* Initial release.
