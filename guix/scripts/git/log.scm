;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020, 2021 Magali Lemes <magalilemes00@gmail.com>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (guix scripts git log)
  #:use-module (git)
  #:use-module (guix channels)
  #:use-module (guix git)
  #:use-module (guix scripts)
  #:use-module (guix scripts pull)
  #:use-module (guix sets)
  #:use-module (guix ui)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 vlist)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-37)
  #:export (guix-git-log))

(define channels (channel-list '()))

(define %formats
  '("oneline" "medium" "full"))

(define %options
  ;; Specifications of the command-line options.
  (list (option '(#\h "help") #f #f
                (lambda args
                  (show-help)
                  (exit 0)))
        (option '(#\V "version") #f #f
                (lambda args
                  (show-version-and-exit "guix git log")))

        (option '("channel-cache-path") #f #t
                (lambda (opt name arg result)
                  (if arg
                      (alist-cons 'channel-cache-path
                                  (string->symbol arg) result)
                      (list-channels))))
        (option '("date") #t #f
                (lambda (opt name arg result)
                  (unless (string-match "^[0-9]{4}-[0-9]{2}-[0-9]{2}$" arg)
                    (leave (G_ "~a: invalid date format~%") arg))
                  (alist-cons 'date arg result)))
        (option '("format") #t #f
                (lambda (opt name arg result)
                  (unless (member arg %formats)
                    (leave (G_ "~a: invalid format~%") arg))
                  (alist-cons 'format (string->symbol arg) result)))
        (option '("grep") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'grep arg result)))
        (option '("oneline") #f #f
                (lambda (opt name arg result)
                  (alist-cons 'oneline? #t result)))
        (option '("parents") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'parents arg result)))
        (option '("pretty") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'pretty arg result)))))

(define %default-options
  '())

(define (list-channels)
  "List channels and their checkout path"
  (for-each (lambda (channel)
              (format #t "~a~%  ~a~%"
                      (channel-name channel)
                      (url-cache-directory (channel-url channel))))
            channels))

(define (show-help)
  (display (G_ "Usage: guix git log [OPTIONS...]
Show Guix commit logs.\n"))
  (display (G_ "
      --channel-cache-path[=CHANNEL]
                         show checkout path from CHANNEL"))
  (display (G_ "
      --date=YYYY-MM-DD  show commits from YYYY-MM-DD"))
  (display (G_ "
      --format=FORMAT    show log according to FORMAT"))
  (display (G_ "
      --grep=REGEXP      show commits whose message matches REGEXP"))
  (display (G_ "
      --oneline          show short hash and summary of commits"))
  (display (G_ "
      --parents=COMMIT   show commit parents"))
  (display (G_ "
      --pretty=STRING    show log according to STRING"))
  (newline)
  (display (G_ "
  -h, --help             display this help and exit"))
  (display (G_ "
  -V, --version          display version information and exit"))
  (newline)
  (show-bug-report-information))

(define commit-short-id
  (compose (cut string-take <> 7) oid->string commit-id))

(define placeholders-regex "%([Hhsb]|(an)|(cn))")

(define information-placeholders
  ;; Alist of placeholders and their corresponding procedure.
  `(("%b"  . ,commit-body)
    ("%H"  . ,(compose oid->string commit-id))
    ("%h"  . ,commit-short-id)
    ("%s"  . ,commit-summary)
    ("%an" . ,(compose signature-name commit-author))))

(define (replace-regex string)
  "Return a string replacing all information placeholders with ~a"
  (regexp-substitute/global #f placeholders-regex string 'pre "~a" 'post))

(define (procedure-list string)
  "Return a list of procedures according to the placeholders contained in
string, in the order they appear"
  (let* ((placeholders-in-the-string
          (map match:substring (list-matches placeholders-regex string))))
    (map (lambda (commit)
           (assoc-ref information-placeholders commit))
         placeholders-in-the-string)))

(define (pretty-show-commit string commit)
  "Display commit according to string"
  (format #t "~?~%" (replace-regex string) (map
                                            (lambda (f) (f commit))
                                            (procedure-list string))))

(define (show-channel-cache-path channel)
  "Display channel checkout path."
  (let ((found-channel (find (lambda (element)
                               (equal? channel (channel-name element)))
                             channels)))
    (if found-channel
        (format #t "~a~%" (url-cache-directory (channel-url found-channel)))
        (leave (G_ "~a: channel not found~%") (symbol->string channel)))))

(define (show-commit commit fmt abbrev-commit)
  "Display commit according to fmt. If abbrev-commit is #t, then show short hash
id instead of the 40-character one."
  (match fmt
    ('oneline
     (format #t "~a ~a~%"
             (if abbrev-commit
                 (commit-short-id commit)
                 (oid->string (commit-id commit)))
             (commit-summary commit)))
    ('medium
     (let ((author (commit-author commit))
           (merge-commit (if (> (commit-parentcount commit) 1) #t #f)))
       (format #t "commit ~a~[~%Merge:~]~{ ~a~}~%Author: ~a <~a>~%Date:   ~a~%~%~a~%"
               (if abbrev-commit
                   (commit-short-id commit)
                   (oid->string (commit-id commit)))
               (if merge-commit 0 1) ;; show "Merge:"
               (if merge-commit (map commit-short-id (commit-parents commit)) '())
               (signature-name  author)
               (signature-email author)
               (date->string
                (time-utc->date
                 (make-time time-utc 0
                            (time-time (signature-when author)))
                 (* 60 (time-offset (signature-when author))))
                "~a ~b ~e ~H:~M:~S ~Y ~z")
               (commit-message commit))))
    ('full
     (let ((merge-commit (if (> (commit-parentcount commit) 1) #t #f))
           (author    (commit-author commit))
           (committer (commit-committer commit)))
       (format #t "commit ~a~[~%Merge:~]~{ ~a~}~%Author: ~a <~a>~%Commit: ~a <~a>~%~%~a~%"
               (if abbrev-commit
                   (commit-short-id commit)
                   (oid->string (commit-id commit)))
               (if merge-commit 0 1) ;; show "Merge:"
               (if merge-commit (map commit-short-id (commit-parents commit)) '())
               (signature-name  author)
               (signature-email author)
               (signature-name  committer)
               (signature-email committer)
               (commit-message commit))))))

(define %channels-repositories
  (make-hash-table))

(define (get-commits)
  "Return a list with commits from all channels."
  (fold (lambda (channel commit-list)
          (let* ((channel-path (url-cache-directory (channel-url channel)))
                 (repository (repository-open channel-path))
                 (latest-commit
                  (commit-lookup repository
                                 (object-id
                                  (revparse-single
                                   repository "origin/master")))))
            (begin
              (hashq-set! %channels-repositories channel-path repository)
              (append (set->list (commit-closure latest-commit))
                      commit-list)))) '() channels))


(define (get-and-show-commits fmt abbrev-commit regexp)
  "Display commits from all channels following the format specified by fmt and
abbrev-commit. If regexp is set, then only commits whose message match that
regexp are displayed."
  (for-each
   (lambda (channel)
     (let* ((channel-path  (url-cache-directory (channel-url channel)))
            (repository    (repository-open channel-path))
            (latest-commit (commit-lookup repository
                                          (object-id
                                           (revparse-single
                                            repository "origin/master")))))
       (begin (hashq-set! %channels-repositories channel-path repository)
              (let loop ((commits (list latest-commit))
                         (visited vlist-null))
                (match commits
                  (()
                   (repository-close! repository))
                  ((head . tail)
                   (if (vhash-assq head visited)
                       (loop tail visited)
                       (begin
                         (when (or (not regexp)
                                   (string-match regexp (commit-message head)))
                           (show-commit head fmt abbrev-commit))
                         (loop (append tail (commit-parents head))
                               (vhash-consq head #t visited))))))))))
   channels))

;; commit-hash cannot be short commit hash
(define (show-parents commit-hash)
  "Show parents from a commit whose id is commit-hash"
  (for-each
   (lambda (channel)
     (let* ((channel-path  (url-cache-directory (channel-url channel)))
            (repository    (repository-open channel-path))
            (latest-commit (commit-lookup repository
                                          (object-id
                                           (revparse-single
                                            repository "origin/master")))))
       (begin (hashq-set! %channels-repositories channel-path repository)
              (let loop ((commits (list latest-commit))
                         (visited vlist-null))
                (match commits
                  (()
                   (repository-close! repository))
                  ((head . tail)
                   (if (vhash-assq head visited)
                       (loop tail visited)
                       (if (oid=? commit-hash (commit-id head))
                           (format #t "~{~a~%~}" (map (compose oid->string
                                                               commit-id)
                                                     (commit-parents head)))
                           (loop (append (commit-parents head) tail)
                                 (vhash-consq head #t visited))))))))))
   channels))

(define (get-commits-from time)
  "Return a list of commits, from all channels, committed in time, which is a
number of seconds since epoch"
  (fold (lambda (channel commit-list)
          (let* ((channel-path  (url-cache-directory (channel-url channel)))
                 (repository    (repository-open channel-path))
                 (latest-commit (commit-lookup repository
                                               (object-id
                                                (revparse-single
                                                 repository "origin/master")))))
            (begin (hashq-set! %channels-repositories channel-path repository)
                   (append (let loop ((commits (list latest-commit))
                                      (visited vlist-null)
                                      (result  '()))
                             (match commits
                               (()
                                result)
                               ((head . tail)
                                (if (vhash-assq head visited)
                                    (loop tail visited result)
                                    (if (and (>= (commit-time head) time)
                                             (< (commit-time head) (+ time 86400)))
                                        (loop (append tail (commit-parents head))
                                              (vhash-consq head #t visited)
                                              (cons head result))
                                        (loop (append tail (commit-parents head))
                                              (vhash-consq head #t visited)
                                              result)))))))))) '()
                                              channels))

(define (show-commits-from date)
  "Show all commits whose committer date are the same as date, which is in the
YYYY-MM-DD format."

  (define date-regex "^([0-9]{4})-([0-9]{2})-([0-9]{2})$")

  (define (date->seconds date)
    "Return the number of seconds since epoch from a given date in the
YYYY-MM-DD format"
    (let* ((y-m-d (map string->number
                       (string-split
                        (regexp-substitute #f
                                           (string-match date-regex date)
                                           'pre 3 " " 2 " " 1 'post)
                        #\space)))
           (date-parameters
            (append (list 0 0 0 0) y-m-d '(0))))
      (time-second (date->time-utc
                    (apply make-date date-parameters)))))

  (for-each (lambda (commit)
              (format #t "~a ~a~%"
                      (commit-short-id commit)
                      (commit-summary commit)))
            (get-commits-from (date->seconds date))))

(define (guix-git-log . args)
  (define options
    (parse-command-line args %options (list %default-options)))

  (let ((channel-cache      (assoc-ref options 'channel-cache-path))
        (date               (assoc-ref options 'date))
        (oneline?           (assoc-ref options 'oneline?))
        (format-type        (assoc-ref options 'format))
        (commit             (assoc-ref options 'parents))
        (pretty-string      (assoc-ref options 'pretty))
        (regexp             (assoc-ref options 'grep)))
    (with-error-handling
      (cond
       (channel-cache
        (show-channel-cache-path channel-cache))
       (date
        (show-commits-from date))
       (oneline?
        (leave-on-EPIPE
         (get-and-show-commits 'oneline #t regexp)))
       (format-type
        (leave-on-EPIPE
         (for-each (lambda (commit)
                     (when (or (not regexp)
                               (string-match regexp (commit-message commit)))
                       (show-commit commit format-type #f)))
                   (get-commits))))
       (commit
        (show-parents (string->oid commit)))
       (pretty-string
        (let ((pretty-show (cut pretty-show-commit pretty-string <>)))
          (leave-on-EPIPE
           (for-each (lambda (commit)
                       (when (or (not regexp)
                                 (string-match regexp
                                               (commit-message commit)))
                         (pretty-show commit)))
                     (get-commits)))))))))
