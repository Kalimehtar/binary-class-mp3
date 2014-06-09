#lang racket/base
(require binary-class racket/class)
(provide read-id3 song album artist track year genre translated-genre id3-tag%)

(define id3-tag-size (unsigned-integer 4 7))

(define-binary-class id3-tag%
  ((identifier     (iso-8859-1-string 3))
   (major-version  u1)
   (revision       u1)
   (flags          u1)
   (size           id3-tag-size))
  #:dispatch 
  (case major-version
    ((2) id3v2.2-tag%)
    ((3) id3v2.3-tag%)))

(define (read-id3 file)
  (call-with-input-file file
    (λ (in) (read-object id3-tag% in))))

(define (show-tag-header file)
  (define data (read-id3 file))
  (displayln 
   (apply format "~a ~a.~a ~a ~a bytes -- ~a~%"
          (append (for/list ([field 
                      (in-list 
                       '(identifier major-version revision flags size))])
                    (dynamic-get-field field data))
                  (list file)))))

(define (optional type condition)
  (if condition
      (binary 
       (λ (in) (read-value type in))
       (λ (out value) (write-value type value out)))
      (binary 
       (λ (in) #f)
       (λ (out value) (void)))))

(define (extended? flags) (bitwise-bit-set? flags 6))
(define (crc? flags extra-flags) (and (extended? flags) (bitwise-bit-set? extra-flags 15)))

(define-binary-class id3v2.2-tag% id3-tag% 
  ((frames (id3-frames size id3v2.2-frame%))))

(define-binary-class id3v2.3-tag% id3-tag%
  ((extended-header-size (optional u4 (extended? flags)))
   (extra-flags          (optional u2 (extended? flags)))
   (padding-size         (optional u4 (extended? flags)))
   (crc                  (optional u4 (crc? flags extra-flags)))
   (frames               (id3-frames size id3v2.3-frame%))))

(define (frame-id length)
  (binary
   (λ (in)
     (let ((first-byte (read-byte in)))
       (when (= first-byte 0) (raise 'in-padding))
       (define rest (read-value (iso-8859-1-string (sub1 length)) in))
       (string-append (string (integer->char first-byte)) rest)))
   (λ (out id)
     (write-value (iso-8859-1-string length) out id))))

(define-binary-class id3v2.2-frame%
  ((id   (frame-id 3))
   (size u3))
  #:dispatch (find-frame-class id)
  (define/public (header-size) 6))

(define-binary-class id3v2.3-frame%
  ((id                (frame-id 4))
   (size              u4)
   (flags             u2)
   (decompressed-size (optional u4 (frame-compressed? flags)))
   (encryption-scheme (optional u1 (frame-encrypted? flags)))
   (grouping-identity (optional u1 (frame-grouped? flags))))
  #:dispatch (find-frame-class id)
  (define/public (header-size) 10))

(define-binary-class generic-frame-v2.2% id3v2.2-frame%
  ((data (bytestring size))))

(define-binary-class generic-frame-v2.3% id3v2.3-frame%
  ((data (bytestring (- size
                        (if (frame-compressed? flags) 4 0)
                        (if (frame-encrypted? flags) 1 0)
                        (if (frame-grouped? flags) 1 0))))))

(define (find-frame-class id)
  (cond
    [(and (char=? (string-ref id 0) #\T)
          (not (string=? id "TXXX"))
          (not (string=? id "TXX")))
     (if (= (string-length id) 3) text-info-frame-v2.2% text-info-frame-v2.3%)]
    [(string=? id "COM")  comment-frame-v2.2%]
    [(string=? id "COMM") comment-frame-v2.3%]
    [else (if (= (string-length id) 3) generic-frame-v2.2% generic-frame-v2.3%)]))

(define (frame-compressed? flags) (bitwise-bit-set? flags 7))
(define (frame-encrypted? flags) (bitwise-bit-set? flags 6))
(define (frame-grouped? flags) (bitwise-bit-set? flags 5))

(define (id3-frames tag-size frame-type)
  (define (frame-size frame)
    (+ (send frame header-size) (get-field size frame)))
  (binary 
   (λ (in)
     (let loop ([to-read tag-size] [frames null])
       (cond 
         [(positive? to-read)
          (define frame 
            (with-handlers ([(λ (e) (eq? e 'in-padding)) (λ (e) null)])
              (read-object frame-type in)))
          (if (null? frame)
              (begin0
                (reverse frames)
                (for ([i (in-range to-read)]) (read-byte in)))
              (loop (- to-read (frame-size frame)) (cons frame frames)))]
         [else (reverse frames)])))
   (λ (out frames)
     (let loop ([to-write tag-size] [frames frames])
       (cond
         [(null? frames) 
          (for ([i (in-range to-write)]) (write-byte out 0))]
         [(positive? to-write)
          (define frame (car frames))
          (send frame-type write out)
          (loop (- to-write (frame-size frame)) (cdr frames))])))))

(define (select-type encoding length terminator)
  (cond
    [length 
     (case encoding
       [(0) (iso-8859-1-string length)]
       [(1) (ucs-2-string length)]
       [else (raise-argument-error 'select-type "0 or 1" encoding)])]
    [terminator
     (case encoding
       [(0) (iso-8859-1-terminated-string terminator)]
       [(1) (ucs-2-terminated-string terminator)]
       [else (raise-argument-error 'select-type "0 or 1" encoding)])]
    [else (raise-argument-error 'select-type 
                                "either length or terminator should be set"
                                2 
                                encoding length terminator)]))

(define (id3-encoded-string encoding #:length [length #f] #:terminator [terminator #f])
  (define type (select-type encoding length terminator))
  (binary
   (λ (in) 
     (read-value type in))
   (λ (out string)
     (write-value type out string))))

(define-syntax-rule (text-info-frame new-class base-class)
  (define-binary-class new-class base-class
    ((encoding u1)
     (information (id3-encoded-string encoding #:length (- size 1))))))

(text-info-frame text-info-frame-v2.2% id3v2.2-frame%)
(text-info-frame text-info-frame-v2.3% id3v2.3-frame%)

(define (encoded-string-length string encoding terminated)
  (let ((characters (+ (string-length string) (if terminated 1 0))))
    (* characters (case encoding [(0) 1] [(1) 2]))))

(define-syntax-rule (comment-frame new-class base-class)
  (define-binary-class new-class base-class
    ((encoding u1)
     (language (iso-8859-1-string 3))
     (description (id3-encoded-string encoding #:terminator #\0))
     (text (id3-encoded-string
            encoding
            #:length (- size
                        (+ 1 ; encoding
                           3 ; language
                           (encoded-string-length description encoding #t))))))))

(comment-frame comment-frame-v2.2% id3v2.2-frame%)
(comment-frame comment-frame-v2.3% id3v2.3-frame%)

(define (find-frame id3 ids)
  (define (in-ids str)
    (for/or ([id (in-list ids)])
      (string=? id str)))
  (for/first ([frame (in-list (get-field frames id3))] 
              #:when (in-ids (get-field id frame)))
    frame))

(define (get-text-info id3 . ids)
  (let ((frame (find-frame id3 ids)))
    (when frame (upto-null (get-field information frame)))))

(define (upto-null str)
  (define pos (for/first ([ch (in-string str)] [pos (in-naturals)] #:when (char=? ch #\0)) pos))
  (if pos (substring str 0 pos) str))

(define (song id3) (get-text-info id3 "TT2" "TIT2"))
(define (album id3) (get-text-info id3 "TAL" "TALB"))
(define (artist id3) (get-text-info id3 "TP1" "TPE1"))
(define (track id3) (get-text-info id3 "TRK" "TRCK"))
(define (year id3) (get-text-info id3 "TYE" "TYER" "TDRC"))
(define (genre id3) (get-text-info id3 "TCO" "TCON"))
(define (translated-genre id3)
  (define genre-id (genre id3))
  (if (and genre-id (char=? #\( (string-ref genre-id 0)))
      (translate-v1-genre genre-id)
      genre-id))

(define (translate-v1-genre genre)
  (vector-ref *id3-v1-genres* (string->number 
                               (substring genre 1 
                                          (sub1 (string-length genre))))))

(define *id3-v1-genres*
  #(
    ;; These are the official ID3v1 genres.
    "Blues" "Classic Rock" "Country" "Dance" "Disco" "Funk" "Grunge"
    "Hip-Hop" "Jazz" "Metal" "New Age" "Oldies" "Other" "Pop" "R&B" "Rap"
    "Reggae" "Rock" "Techno" "Industrial" "Alternative" "Ska"
    "Death Metal" "Pranks" "Soundtrack" "Euro-Techno" "Ambient"
    "Trip-Hop" "Vocal" "Jazz+Funk" "Fusion" "Trance" "Classical"
    "Instrumental" "Acid" "House" "Game" "Sound Clip" "Gospel" "Noise"
    "AlternRock" "Bass" "Soul" "Punk" "Space" "Meditative"
    "Instrumental Pop" "Instrumental Rock" "Ethnic" "Gothic" "Darkwave"
    "Techno-Industrial" "Electronic" "Pop-Folk" "Eurodance" "Dream"
    "Southern Rock" "Comedy" "Cult" "Gangsta" "Top 40" "Christian Rap"
    "Pop/Funk" "Jungle" "Native American" "Cabaret" "New Wave"
    "Psychadelic" "Rave" "Showtunes" "Trailer" "Lo-Fi" "Tribal"
    "Acid Punk" "Acid Jazz" "Polka" "Retro" "Musical" "Rock & Roll"
    "Hard Rock"

    ;; These were made up by the authors of Winamp but backported into
    ;; the ID3 spec.
    "Folk" "Folk-Rock" "National Folk" "Swing" "Fast Fusion"
    "Bebob" "Latin" "Revival" "Celtic" "Bluegrass" "Avantgarde"
    "Gothic Rock" "Progressive Rock" "Psychedelic Rock" "Symphonic Rock"
    "Slow Rock" "Big Band" "Chorus" "Easy Listening" "Acoustic" "Humour"
    "Speech" "Chanson" "Opera" "Chamber Music" "Sonata" "Symphony"
    "Booty Bass" "Primus" "Porn Groove" "Satire" "Slow Jam" "Club"
    "Tango" "Samba" "Folklore" "Ballad" "Power Ballad" "Rhythmic Soul"
    "Freestyle" "Duet" "Punk Rock" "Drum Solo" "A capella" "Euro-House"
    "Dance Hall"

    ;; These were also invented by the Winamp folks but ignored by the
    ;; ID3 authors.
    "Goa" "Drum & Bass" "Club-House" "Hardcore" "Terror" "Indie"
    "BritPop" "Negerpunk" "Polsk Punk" "Beat" "Christian Gangsta Rap"
    "Heavy Metal" "Black Metal" "Crossover" "Contemporary Christian"
    "Christian Rock" "Merengue" "Salsa" "Thrash Metal" "Anime" "Jpop"
    "Synthpop"))
