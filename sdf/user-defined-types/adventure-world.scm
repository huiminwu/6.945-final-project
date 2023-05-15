#| -*-Scheme-*-

Copyright (C) 2019, 2020, 2021 Chris Hanson and Gerald Jay Sussman

This file is part of SDF.  SDF is software supporting the book
"Software Design for Flexibility", by Chris Hanson and Gerald Jay
Sussman.

SDF is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

SDF is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with SDF.  If not, see <https://www.gnu.org/licenses/>.

|#

;;;; An adventure game at MIT

(define the-clock)
(define all-places)
(define heaven)
(define medical-center)
(define all-people)
(define all-trolls)
(define all-avatars '())

(define (start-web-world)
  (set! the-clock (make-clock))
  (set! medical-center (create-place 'medical-center))
  (set! all-places (create-mit))
  (set! heaven (create-place 'heaven))
  (set! all-trolls (create-trolls all-places))
  (set! all-people (create-people all-places))
  (set! all-avatars '()))

(define (start-web-adventure my-name client)
  (if (false? (find-object-by-name my-name all-avatars))
      (let ((avatar-obj (create-avatar my-name (random-choice all-places))))
	(set! all-avatars (append! all-avatars (list avatar-obj)))
	(display-message (list "<h4> Output: </h4>") client)
	(tell-web! (list "Welcome to MIT" my-name "\n") client avatar-obj)
	(whats-here-web client my-name))
      
      (tell-web! (list "This avatar already exists. Please go back to the main page and try again.") client avatar-obj)))
 
(define (avatar-exists name)
  (not (false? (find-object-by-name name all-avatars))))

(define (load-log name client)
  (let ((my-avatar (find-object-by-name name all-avatars)))
    (display-message (reverse (get-log my-avatar)) client)))
   

#|
(define (start-web-adventure my-name client)
  (set! the-clock (make-clock))
  (set! all-places (create-mit))
  (set! heaven (create-place 'heaven))
  (set! all-people (create-people all-places))
  (set! all-avatars '())
  (start-adventure my-name)
  #|(set! my-avatar (create-avatar my-name (random-choice all-places)))|#
  (whats-here-web client my-name))
|#
(define (get-all-places)
  all-places)

(define (get-heaven)
  heaven)

(define (get-medical-center)
  medical-center)

(define (get-clock)
  the-clock)

;;; User interface

(define (go direction name)
  (let ((my-avatar (find-object-by-name name all-avatars)))
    (let ((exit
	   (find-exit-in-direction direction
				   (get-location my-avatar))))
      (if exit
	  (take-exit! exit my-avatar)
	  (narrate! (list "No exit in" direction "direction")
		    my-avatar))))
    'done)

(define (go-web direction name client)
  (let ((my-avatar (find-object-by-name name all-avatars)))
    (load-log name client)
    (display-message (list "<h4> Output: </h4>") client)
    
    (let ((exit
	   (find-exit-in-direction direction
				   (get-location my-avatar))))
      (if exit
	  (take-exit-web! exit my-avatar client)
	  (tell-web! (list "No exit in" direction "direction")
		     client
		     my-avatar))))
    'done)

(define (take-thing name avatar-name)
  (let ((thing (find-thing name (here avatar-name) avatar-name))
	      (my-avatar (find-object-by-name avatar-name all-avatars)))
    (if thing
        (take-thing! thing my-avatar)))
  'done)

(define (take-thing-web name avatar-name client)
  (let ((thing (find-thing-web name (here avatar-name) avatar-name client))
	(my-avatar (find-object-by-name avatar-name all-avatars)))
    (load-log name client)
    (display-message (list "<h4> Output: </h4>") client)
    
    (if thing
        (take-thing-web! thing my-avatar client)))
  'done)

(define (drop-thing name avatar-name)
  (let* ((my-avatar (find-object-by-name avatar-name all-avatars))
	       (thing (find-thing name my-avatar avatar-name)))
    (if thing
        (drop-thing! thing my-avatar)))
  'done)

(define (drop-thing-web name avatar-name client)
  (let* ((my-avatar (find-object-by-name avatar-name all-avatars))
	 (thing (find-thing-web name my-avatar avatar-name client)))
    (load-log name client)
    (display-message (list "<h4> Output: </h4>") client)
    
    (if thing
        (drop-thing-web! thing my-avatar client)))
  'done)

(define (look-in-bag avatar-name client #!optional person-name)
  (let* ((my-avatar (find-object-by-name avatar-name all-avatars))
	       (person
         (if (default-object? person-name)
             my-avatar
             (find-person-web person-name avatar-name client))))
    (load-log name client)
    (display-message (list "<h4> Output: </h4>") client)
    
    (if person
        (tell-web! (let ((referent (local-possessive person avatar-name))
			 (things (get-things person)))
                     (if (n:pair? things)
			 (cons* referent "bag contains" things)
			 (list referent "bag is empty")))
		   client
		   my-avatar)))
  'done)

(define (whats-here avatar-name)
  (look-around (find-object-by-name avatar-name all-avatars))
  'done)

(define (whats-here-web client my-name)
  (look-around-web (find-object-by-name my-name all-avatars) client)
  'done)

(define (say avatar-name . message)
  (let ((my-avatar (find-object-by-name avatar-name all-avatars)))
    (say! my-avatar message))
  'done)

(define (say-web avatar-name client . message)
  (let ((my-avatar (find-object-by-name avatar-name all-avatars)))
    (load-log name client)
    (display-message (list "<h4> Output: </h4>") client)
    
    
    (say-web! my-avatar (car message) client))
  'done)


(define (tell avatar-name person-name . message)
  (tell! message (find-person person-name avatar-name))
  'done)

(define (hang-out ticks)
  (do ((i 0 (n:+ i 1)))
      ((not (n:< i ticks)))
    (tick! (get-clock)))
  'done)

(define (display-health avatar-name client)
  (let ((my-avatar (find-object-by-name avatar-name all-avatars)))
    (load-log name client)
    (display-message (list "<h4> Output: </h4>") client)
    (tell-web! (list "Your health is:" (get-health my-avatar)) client my-avatar))
  'done)

(define (fight avatar-name troll-name client)
  (let ((my-avatar (find-object-by-name avatar-name all-avatars))
	(troll-obj (find-object-by-name troll-name all-people)))
    (load-log name client)
    (display-message (list "<h4> Output: </h4>") client)
    (if (false? troll-obj)
	(display-message (list "Troll does not exist") client)
	(if (eqv? (get-location troll-obj) (get-location my-avatar))
	    (fight! my-avatar client troll-obj)
	    (display-message (list "Troll" troll-name "is not here") client))))
  'done)
    

;;; Support for UI

(define (here avatar-name)
  (get-location (find-object-by-name avatar-name all-avatars)))

(define (find-person name avatar-name)
  (let* ((my-avatar (find-object-by-name avatar-name all-avatars))
	       (person
         (find-object-by-name name (people-here my-avatar))))
    (if (not person)
        (tell! (list "There is no one called" name "here")
               my-avatar))
    person))

(define (find-person-web name avatar-name client)
  (let* ((my-avatar (find-object-by-name avatar-name all-avatars))
	       (person
         (find-object-by-name name (people-here my-avatar))))
    (if (not person)
        (tell-web! (list "There is no one called" name "here")
		   client
               my-avatar))
    person))

(define (find-thing name person-or-place avatar-name)
  (let ((thing
         (find-object-by-name
          name
          (person-or-place-things person-or-place))))
    (if (not thing)
        (tell (cons* "There is nothing called"
                      name
                      (person-or-place-name person-or-place avatar-name))
		   (find-object-by-name avatar-name all-avatars)))
    thing))

(define (find-thing-web name person-or-place avatar-name client)
  (let ((thing
         (find-object-by-name
          name
          (person-or-place-things person-or-place))))
    (if (not thing)
        (tell-web! (cons* "There is nothing called"
                      name
                      (person-or-place-name person-or-place avatar-name))
		   client
		   (find-object-by-name avatar-name all-avatars)))
    thing))

(define (person-or-place-things person-or-place)
  (if (place? person-or-place)
      (all-things-in-place person-or-place)
      (get-things person-or-place)))

(define (person-or-place-name person-or-place avatar-name)
  (if (place? person-or-place)
      '("here")
      (list "in" (local-possessive person-or-place avatar-name) "bag")))

(define (local-possessive person avatar-name)
  (if (eqv? person (find-object-by-name avatar-name all-avatars))
      "Your"
      (possessive person)))

(define (create-mit)
  (let ((great-dome (create-place 'great-dome))
        (little-dome (create-place 'little-dome))
        (lobby-10 (create-place 'lobby-10))
        (10-250 (create-place '10-250))
        (barker-library (create-place 'barker-library))
        (lobby-7 (create-place 'lobby-7))
        (infinite (create-place 'infinite-corridor))

        (bldg-26 (create-place 'bldg-26))
        (cp32 (create-place 'bldg-32-cp-hq))
        (tunnel (create-place 'lab-supplies))

        (32-123 (create-place '32-123))
        (32G (create-place 'gates-tower))
        (32D (create-place 'dreyfoos-tower))
        (student-street (create-place 'student-street))
        (great-court (create-place 'great-court))
        (bldg-54 (create-place 'green-building))
        (the-dot (create-place 'the-dot))
        (dorm-row (create-place 'dorm-row)))

    (can-go-both-ways lobby-10 'up 'down 10-250)
    (can-go-both-ways 10-250 'up 'down barker-library)
    (can-go-both-ways barker-library 'up 'down great-dome)
    (can-go-both-ways lobby-10 'west 'east lobby-7)
    (can-go-both-ways lobby-7 'west 'east dorm-row)
    (can-go-both-ways lobby-7 'up 'down little-dome)
    (can-go-both-ways lobby-10 'south 'north great-court)
    (can-go-both-ways lobby-10 'east 'west infinite)
    (can-go-both-ways infinite 'north 'south bldg-26)
    (can-go-both-ways infinite 'east 'west bldg-54)
    (can-go-both-ways bldg-26 'east 'west student-street)
    (can-go-both-ways student-street 'down 'up cp32)
    (can-go-both-ways cp32 'south 'north tunnel)
    (can-go-both-ways tunnel 'up 'down bldg-54)
    (can-go-both-ways bldg-54 'south 'north the-dot)
    (can-go-both-ways the-dot 'west 'east great-court)
    (can-go-both-ways student-street 'in 'out 32-123)
    (can-go-both-ways student-street 'up 'down 32G)
    (can-go-both-ways student-street 'skew 'down 32D)
    (can-go-both-ways (get-medical-center) 'west 'east bldg-54)
    
    ; Add line-of-sight into the mix
    (can-see bldg-54 32G)
    (can-see bldg-54 32D)
    (can-see bldg-54 great-dome)
    (can-see bldg-54 little-dome)
    (can-see bldg-54 great-court)
    (can-see bldg-54 the-dot)
    (can-see lobby-10 great-court)
    (can-see great-dome great-court)
    (can-see-both-ways 32D 32G)
    (can-see-both-ways great-dome little-dome)
    (can-see-both-ways lobby-10 infinite)
    (can-see-both-ways lobby-7 infinite)
    (can-see-both-ways infinite bldg-26)
    (can-see-both-ways lobby-10 lobby-7)

    ; Create some things
    (create-thing 'blackboard 10-250)
    (create-thing 'lovely-trees great-court)
    (create-thing 'flag-pole great-court)
    (create-thing 'calder-sculpture the-dot)
    (create-mobile-thing 'problem-set 32-123)
    (create-mobile-thing 'recitation-problem 32-123)
    (create-mobile-thing 'sicp student-street)
    (create-mobile-thing 'engineering-book barker-library)

    (list great-dome little-dome lobby-10
          10-250 barker-library lobby-7
          infinite bldg-26 cp32
          tunnel 32-123 32D 32G
          student-street bldg-54 the-dot
          dorm-row (get-medical-center))))

(define (create-people places)
  (append (create-students places)
          ;;(create-profs places)
          ;;(create-president places)
          (create-house-masters places)
          all-trolls))

(define (create-students places)
  (map (lambda (name)
         (create-student name
                         (random-choice places)
                         (random-bias 5)
                         (random-bias 5)))
       '(ben-bitdiddle alyssa-hacker course-6-frosh lambda-man)))

;; (define (create-profs places)
;;   (map (lambda (name)
;;          (create-professor name
;;                            (random-choice places)
;;                            1/3
;;                            1/3))
;;        '(rob-miller eric-grimson)))

;; (define (create-president places)
;;   (create-president 'rafael-reif
;;                     (random-choice places)
;;                     (random-bias 3)
;;                     (random-bias 3)))

(define (create-house-masters places)
  (map (lambda (name)
         (create-house-master name
                              (random-choice places)
                              (random-bias 3)
                              (random-bias 3)))
       '(dr-evil mr-bigglesworth)))

(define (create-trolls places)
  (map (lambda (name)
         (create-troll name
                       (random-choice places)
                       (random-bias 3)
                       (random-bias 3)))
       '(grendel registrar)))

(define (create-thing name location)
  (make-thing 'name name
              'location location))

(define (create-mobile-thing name location)
  (make-mobile-thing 'name name
                     'location location))

(define (create-place name)
  (make-place 'name name))

(define (create-exit from direction to)
  (make-exit 'name 'exit
             'from from
             'direction direction
             'to to))

(define (create-student name home restlessness acquisitiveness)
  (make-student 'name name
                'location home
                'restlessness restlessness
                'acquisitiveness acquisitiveness))

(define (create-house-master name home restlessness irritability)
  (make-house-master 'name name
                     'location home
                     'restlessness restlessness
                     'acquisitiveness 1/10
                     'irritability irritability))

(define (create-troll name place restlessness hunger)
  (make-troll 'name name
              'location place
              'restlessness restlessness
              'acquisitiveness 1/10
              'hunger hunger))

(define (create-avatar name place)
  (make-avatar 'name name
               'location place
               'screen (make-screen 'name 'the-screen)
	       'log '()))

(define (can-go-both-ways from direction reverse-direction to)
  (create-exit from direction to)
  (create-exit to reverse-direction from))

(define (can-see a b)
  (add-vista! a b))

(define (can-see-both-ways a b)
  (can-see a b)
  (can-see b a))
