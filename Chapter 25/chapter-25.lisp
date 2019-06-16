(in-package :chapter-25-package)

;; Unsigned integers:

;;    Generic type:

(define-binary-type unsigned-integer (bytes bits-per-byte)
  (:reader (in)
	   (loop with value = 0
	      for low-bit downfrom (* bits-per-byte (1- bytes)) to 0 by bits-per-byte do
		(setf (ldb (byte bits-per-byte low-bit) value) (read-byte in))
	      finally (return value)))
  (:writer (out value)
	   (loop for low-bit downfrom (* bits-per-byte (1- bytes)) to 0 by bits-per-byte
	      do (write-byte (ldb (byte bits-per-byte low-bit) value) out))))

;;    Specific types:

(define-binary-type u1 () (unsigned-integer :bytes 1 :bits-per-byte 8))
(define-binary-type u2 () (unsigned-integer :bytes 2 :bits-per-byte 8))
(define-binary-type u3 () (unsigned-integer :bytes 3 :bits-per-byte 8))
(define-binary-type u4 () (unsigned-integer :bytes 4 :bits-per-byte 8))
(define-binary-type id3-tag-size () (unsigned-integer :bytes 4 :bits-per-byte 7))

;; Strings:

;;    Generic types:

(define-binary-type generic-string (length character-type)
  (:reader (in)
	   (let ((string (make-string length)))
	     (dotimes (i length)
	       (setf (char string i) (read-value character-type in)))
	     string))
  (:writer (out string)
	   (dotimes (i length)
	     (write-value character-type out (char string i)))))

(define-binary-type generic-terminated-string (terminator character-type)
  (:reader (in)
	   (with-output-to-string (s)
	     (loop for char = (read-value character-type in)
		until (char= char terminator) do (write-char char s))))
  (:writer (out string)
	   (loop for char across string
	      do (write-value character-type out char)
	      finally (write-value character-type out terminator))))

(define-binary-type ucs-2-string (length)
  (:reader (in)
	   (let ((byte-order-mark (read-value 'u2 in))
		 (characters (1- (/ length 2))))
	     (read-value
	      'generic-string in
	      :length characters
	      :character-type (ucs-2-char-type byte-order-mark))))
  (:writer (out string)
	   (write-value 'u2 out #xfeff)
	   (write-value
	    'generic-string out string
	    :length (length string)
	    :character-type (ucs-2-char-type #xfeff))))

(define-binary-type ucs-2-terminated-string (terminator)
  (:reader (in)
	   (let ((byte-order-mark (read-value 'u2 in)))
	     (read-value
	      'generic-terminated-string in
	      :terminator terminator
	      :character-type (ucs-2-char-type byte-order-mark))))
  (:writer (out string)
	   (write-value 'u2 out #xfeff)
	   (write-value
	    'generic-terminated-string out string
	    :terminator terminator
	    :character-type (ucs-2-char-type #xfeff))))

;;    Specific types:

(define-binary-type iso-8859-1-string (length)
  (generic-string :length length :character-type 'iso-8859-1-char))

(define-binary-type iso-8859-1-terminated-string (terminator)
  (generic-terminated-string :terminator terminator
			     :character-type 'iso-8859-1-char))

;; Characters:

;;     Generic types:

(define-binary-type iso-8859-1-char ()
  (:reader (in)
	   (let ((code (read-byte in)))
	     (or (code-char code)
		 (error "Character code ~d not supported" code))))
  (:writer (out char)
	   (let ((code (char-code char)))
	     (if (<= 0 code #xff)
		 (write-byte code out)
		 (error
		  "Illegal character for iso-8859-1 encoding: character ~c with code: ~d"
		  char code)))))

(define-binary-type ucs-2-char (swap)
  (:reader (in)
	   (let ((code (read-value 'u2 in)))
	     (when swap (setf code (swap-bytes code)))
	     (or (code-char code) (error "Character code ~d not supported" code))))
  (:writer (out char)
	   (let ((code (char-code char)))
	     (unless (<= 0 code #xffff)
	       (error "Illegal character for ucs-2 encoding: ~c with char-code: ~d" char code))
	     (when swap (setf code (swap-bytes code)))
	     (write-value 'u2 out code))))

;;     Specific types:

(define-binary-type ucs-2-char-big-endian () (ucs-2-char :swap nil))

(define-binary-type ucs-2-char-little-endian () (ucs-2-char :swap t))

;;    Auxiliary:

(defun swap-bytes (code)
  "Inverts the two bytes of a char."
  (assert (<= code #xffff))
  (rotatef (ldb (byte 8 0) code) (ldb (byte 8 8) code))
  code)

(defun ucs-2-char-type (byte-order-mark)
  "Identifies if a char is little- or big-endian."
  (ecase byte-order-mark
    (#xfeff 'ucs-2-char-big-endian)
    (#xfffe 'ucs-2-char-little-endian)))

;; ID3 tags:

;;     Classes:

(define-tagged-binary-class id3-tag ()
  ((identifier (iso-8859-1-string :length 3))
   (major-version u1)
   (revision      u1)
   (flags         u1)
   (size          id3-tag-size))
  (:dispatch
   (ecase major-version
     (2 'id3v2.2-tag)
     (3 'id3v2.3-tag))))

(define-binary-class id3v2.2-tag (id3-tag)
  ((frames (id3-frames :tag-size size :frame-type 'id3v2.2-frame))))

(define-binary-class id3v2.3-tag (id3-tag)
  ((extended-header-size (optional :type 'u4 :if (extended-p flags)))
   (extra-flags          (optional :type 'u2 :if (extended-p flags)))
   (padding-size         (optional :type 'u4 :if (extended-p flags)))
   (crc                  (optional :type 'u4 :if (crc-p flags extra-flags)))
   (frames               (id3-frames :tag-size size :frame-type 'id3v2.3-frame))))

;;     Auxiliary functions:

(defun read-id3 (file)
  "Reads the ID3 tag from the beginning of a mp3 file."
  (with-open-file (in file :element-type '(unsigned-byte 8))
    (read-value 'id3-tag in)))

(defun show-tag-header (file)
  "Pretty prints an ID3 tag header from a file."
  (with-slots (identifier major-version revision flags size) (read-id3 file)
    (format t "~a ~d.~d ~8,'0b ~d bytes -- ~a~%"
	    identifier major-version revision flags size (enough-namestring file))))

;; File managing:

(defun show-tag-headers (dir)
  "Walks a directory showing all ID3 tag headers from mp3 files."
  (walk-directory dir #'show-tag-header :test #'mp3-p))

(defun count-versions (dir)
  "Counts how many mp3 files of each ID3 version are in a directory."
  (let ((versions (mapcar #'(lambda (x) (cons x 0)) '(2 3 4)))) ; creating an accumulator for each version.
    (flet ((count-version (file)
	     (incf (cdr (assoc (major-version (read-id3 file)) versions))))) ; increasing the count of the accumulator which version matches the file's ID3 tag version.
      (walk-directory dir #'count-version :test #'mp3-p))
    versions))

;;     file managing predicates:

(defun mp3-p (file)
  "Tests whether a file is an mp3 file."
  (and
   (not (directory-pathname-p file))
   (string-equal "mp3" (pathname-type file))))

(defun id3-p (file)
  "Tests whether a file starts with an ID3 tag."
  (with-open-file (in file)
    (string= "ID3" (read-value 'iso-8859-1-string in :length 3))))

;; Frames:

;; id3-frames: "Responsible for creating frame objects for every frame in a tag."
(define-binary-type id3-frames (tag-size frame-type)
  (:reader (in)
	   (loop with to-read = tag-size
	      while (plusp to-read)
	      for frame = (read-frame frame-type in)
	      while frame ; while the bytes aren't padding bytes.
	      do (decf to-read (+ (frame-header-size frame) (size frame))) ; skipping the bytes read plus the header size.
	      collect frame
	      finally (loop repeat (1- to-read) do (read-byte in)))) ; reading the padding.
  (:writer (out frames)
	   (loop with to-write = tag-size
	      for frame in frames
	      do (write-value frame-type out frame)
		(decf to-write (+ (frame-header-size frame) (size frame)))
	      finally (loop repeat to-write do (write-byte 0 out))))) ; writing the necessary padding.

;; generic-frame:   "Used as superclass by the concrete frame classes."
(define-binary-class generic-frame ()
  ((data (raw-bytes :size (data-bytes (current-binary-object))))))

;; id3-frame:   "Represents any ID3 frame."
(define-tagged-binary-class id3-frame ()
  ((id (frame-id :length 3))
   (size u3))
  (:dispatch (find-frame-class id)))

;; id3v2.2-frame:   "A specific case of id3-frame for version 2.2. Used along with generic-frame by concrete frame classes."
(define-tagged-binary-class id3v2.2-frame ()
  ((id (frame-id :length 3))
   (size u3))
  (:dispatch (find-frame-class id)))

;; id3v2.3-frame:   "A specific case of id3-frame for version 2.3. Used along with generic-frame by concrete frame classes."
(define-tagged-binary-class id3v2.3-frame ()
  ((id                (frame-id :length 4))
   (size              u4)
   (flags             u2)
   (decompressed-size (optional :type 'u4 :if (frame-compressed-p flags))) ;; these three are created only if the frame satisfies the tests.
   (encryption-scheme (optional :type 'u1 :if (frame-encrypted-p flags)))
   (grouping-identity (optional :type 'u1 :if (frame-grouped-p flags))))
  (:dispatch (find-frame-class id)))

;;    Concrete classes:

(define-binary-class generic-frame-v2.2 (id3v2.2-frame generic-frame) ())

(define-binary-class generic-frame-v2.3 (id3v2.3-frame generic-frame) ())

;;     Auxiliary:

(define-binary-type frame-id (length)
  (:reader (in)
	   (let ((first-byte (read-byte in)))
	     (when (zerop first-byte) (signal 'in-padding))
	     (let ((rest (read-value 'iso-8859-1-string in :length (1- length))))
	       (concatenate
		'string (string (code-char first-byte)) rest))))
  (:writer (out id)
	   (write-value 'iso-8859-1-string out id :length length)))

(define-binary-type raw-bytes (size)
  (:reader (in)
	   (let ((buf (make-array size :element-type '(unsigned-byte 8))))
	     (read-sequence buf in)
	     buf))
  (:writer (out buf)
	   (write-sequence buf out)))

(define-binary-type optional (type if)
  (:reader (in)
	   (when if (read-value type in)))
  (:writer (out value)
	   (when if (write-value type out value))))

(define-condition in-padding () ())

(defun read-frame (frame-type in)
  (handler-case (read-value frame-type in)
    (in-padding () nil)))

(defun find-frame-class (name)
  "Finds which class a frame belongs to."
  (cond
    ((and (char= (char name 0) #\T)
	  (not (member name '("TXX" "TXXX") :test #'string=)))
     (ecase (length name)
       (3 'text-info-frame-v2.2)
       (4 'text-info-frame-v2.3)))
    ((string= name "COM") 'comment-frame-v2.2)
    ((string= name "COMM") 'comment-frame-v2.3)
    (t
     (ecase (length name)
       (3 'generic-frame-v2.2)
       (4 'generic-frame-v2.3)))))

;;     data-bytes:

(defgeneric data-bytes (frame))

(defmethod data-bytes ((frame id3v2.2-frame))
  (size frame))

(defmethod data-bytes ((frame id3v2.3-frame))
  (let ((flags (flags frame)))
    (- (size frame)
       (if (frame-compressed-p flags) 4 0)
       (if (frame-encrypted-p flags) 1 0)
       (if (frame-grouped-p flags) 1 0))))

;;     frame-header-size:

(defgeneric frame-header-size (frame))

(defmethod frame-header-size ((frame id3v2.2-frame)) 6)

(defmethod frame-header-size ((frame id3v2.3-frame)) 10)

;;     frame flag predicates:

(defun frame-compressed-p (flags) (logbitp 7 flags))

(defun frame-encrypted-p (flags) (logbitp 6 flags))

(defun frame-grouped-p (flags) (logbitp 5 flags))

;;     tag predicates:

(defun extended-p (flags)
  "Returns whether an ID3v2.3 tag header is extended or not."
  (logbitp 6 flags))

(defun crc-p (flags extra-flags)
  "Returns whether an ID3v2.3 tag has CRC or not."
  (and (extended-p flags) (logbitp 15 extra-flags)))

;;    Text info frames:

;; id3-encoded-string:   "Represents strings from text frames in ID3 tags."
(define-binary-type id3-encoded-string (encoding length terminator)
  (:reader (in)
	   (multiple-value-bind (type keyword arg)
	       (string-args encoding length terminator)
	     (read-value type in keyword arg)))
  (:writer (out string)
	   (multiple-value-bind (type keyword arg)
	       (string-args encoding length terminator)
	     (write-value type out string keyword arg))))

;; text-info-frame:   "Represents text information frames from an ID3 tag."
(define-binary-class text-info-frame ()
  ((encoding u1)
   (information (id3-encoded-string :encoding encoding :length (bytes-left 1)))))

;;     Concrete classes:

(define-binary-class text-info-frame-v2.2 (id3v2.2-frame text-info-frame) ())

(define-binary-class text-info-frame-v2.3 (id3v2.3-frame text-info-frame) ())

;;     Auxiliary:

(defun non-terminated-type (encoding)
  "Identifies the encoding of a non-terminated string."
  (ecase encoding
    (0 'iso-8859-1-string)
    (1 'ucs-2-string)))

(defun terminated-type (encoding)
  "Identifies the encoding of a terminated string."
  (ecase encoding
    (0 'iso-8859-1-terminated-string)
    (1 'ucs-2-terminated-string)))

(defun string-args (encoding length terminator)
  "Defines the arguments of a string based on the presence of length or terminator."
  (cond
    (length
     (values (non-terminated-type encoding) :length length))
    (terminator
     (values (terminated-type encoding) :terminator terminator))))

(defun bytes-left (bytes-read)
  (- (size (current-binary-object)) bytes-read))

;; Comment frames:

;; comment-frame:   "Represents a comment frame of an ID3 tag."
(define-binary-class comment-frame ()
  ((encoding u1)
   (language (iso-8859-1-string :length 3))
   (description (id3-encoded-string :encoding encoding :terminator +null+))
   (text (id3-encoded-string
	  :encoding encoding
	  :length (bytes-left
		   (+ 1 ; encoding
		      3 ; language
		      (encoded-string-length description encoding t)))))))

(define-binary-class comment-frame-v2.2 (id3v2.2-frame comment-frame) ())

(define-binary-class comment-frame-v2.3 (id3v2.3-frame comment-frame) ())

;;     Auxiliary:

(defun encoded-string-length (string encoding terminated)
  (let ((characters (+ (length string) (if terminated 1 0))))
    (* characters (ecase encoding (0 1) (1 2)))))

;; Exploring:

(defun frame-types (file)
  "Makes a list of the frame types in an mp3 file."
  (delete-duplicates (mapcar #'id (frames (read-id3 file))) :test #'string=))

(defun frame-types-in-dir (dir)
  "Makes a list of the frame types in a directory."
  (let ((ids ()))
    (flet ((collect (file)
	     (setf ids (nunion ids (frame-types file) :test #'string=))))
      (walk-directory dir #'collect :test #'mp3-p))
    ids))

(defun upto-null (string)
  (subseq string 0 (position +null+ string)))

(defun find-frame (id3 ids)
  "Finds a frame in an ID3 tag, given its possible ids."
  (find-if #'(lambda (x) (find (id x) ids :test #'string=)) (frames id3)))

(defun get-text-info (id3 &rest ids)
  "Extracts text information from a text frame in the ID3 tag."
  (let ((frame (find-frame id3 ids)))
    (when frame (upto-null (information frame)))))

(defun album (id3) (get-text-info id3 "TAL" "TALB"))
(defun artist (id3) (get-text-info id3 "TP1" "TPE1"))
(defun track (id3) (get-text-info id3 "TRK" "TRCK"))
(defun year (id3) (get-text-info id3 "TYE" "TYER" "TDRC"))
(defun genre (id3) (get-text-info id3 "TCO" "TCON"))

(defun translated-genre (id3)
  "Translates a genre code from an ID3 tag's genre frame."
  (let ((genre (genre id3)))
    (flet ((translate-v1-genre (genre)
	     (aref *id3-v1-genres* ; I prefered not to include this variable in the code because it would pollute the text.
		   (parse-integer genre :start 1 :junk-allowed t))))
      (if (and genre (char= #\( (char genre 0)))
	  (translate-v1-genre genre)
	  genre))))
