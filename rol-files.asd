;;
;;  RoL-files  -  Pathname, files and streams utility library
;;
;;  Copyright 2012-2015 Thomas de Grivel <thomas@lowh.net>
;;
;;  Permission to use, copy, modify, and distribute this software for any
;;  purpose with or without fee is hereby granted, provided that the above
;;  copyright notice and this permission notice appear in all copies.
;;
;;  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;;  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;;  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;;  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;;  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;;  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;;  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
;;

(in-package #:cl-user)

(defpackage #:RoL-files.system
  (:use #:cl #:asdf))

(in-package #:RoL-files.system)

(asdf:defsystem :RoL-files
  :name "RoL-files"
  :author "Thomas de Grivel <thomas@lowh.net>"
  :version "0.1"
  :description "Pathname, files and streams utility library"
  :depends-on ("alexandria"
               "cl-fad"
               "cl-ppcre")
  :components
  ((:file "files")))
