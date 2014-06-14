#lang scribble/manual

@(require (for-label racket binary-class/mp3 binary-class/base))

@title{Binary-class/mp3: parsing ID3v2.1 and ID3v2.2}
@author{@(author+email "Roman Klochkov" "kalimehtar@mail.ru")}

@(defmodule binary-class/mp3)
This package is based upon idea in 
@(hyperlink "http://www.gigamonkeys.com/book/practical-an-id3-parser.html" 
            "Practical Common Lisp").

@defproc[(read-id3 [path path-string?]) (is-a?/c id3-tag%)]{
Returns interface for reading ID3 from given file from @racket[_path]}

@defproc[(song [id3 (is-a?/c id3-tag%)]) string?]{
Returns song name of the mp3 track.}

@defproc[(album [id3 (is-a?/c id3-tag%)]) string?]{
Returns album name of the mp3 track.}

@defproc[(artist [id3 (is-a?/c id3-tag%)]) string?]{
Returns artist name of the mp3 track.}

@defproc[(track [id3 (is-a?/c id3-tag%)]) string?]{
Returns track name of the mp3 track.}

@defproc[(year [id3 (is-a?/c id3-tag%)]) string?]{
Returns year of the mp3 track.}

@defproc[(genre [id3 (is-a?/c id3-tag%)]) string?]{
Returns genre (as integer in parens) of the mp3 track.}

@defproc[(translated-genre [id3 (is-a?/c id3-tag%)]) string?]{
Returns genre (as text) of the mp3 track.}

@defclass[id3-tag% object% (binary<%>)]{Class, implementing interface to ID3 tags}
                   