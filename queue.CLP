(defglobal ?*queue* = (create$)) ; queue
(defglobal ?*queue-size* = 0) ; counter

(deffunction ask-question (?question $?allowed-values) ; prompt question: first parameter - question text; second parameter - possible inputs
   (printout t ?question)
   (bind ?answer (read)) 
   (if (lexemep ?answer) ; check if input is a type of string or symbol
       then (bind ?answer (lowcase ?answer)))
   (while (not (member ?answer ?allowed-values)) do ; repeat qustion until input is a valid value
      (printout t ?question)
      (bind ?answer (read))
      (if (lexemep ?answer) 
          then (bind ?answer (lowcase ?answer))))
   ?answer)

(deffunction add-item(?message) ; adds value to the end of the queue
	(printout t ?message)
	(bind ?answer (read))
	(if (lexemep ?answer)
		then (bind ?*queue* (insert$ ?*queue* ?*queue-size* ?answer))) ; add value to the queue
)

(deffunction yes-or-no-p (?question) ; return input (yes or no)
   (bind ?response (ask-question ?question yes no y n))
   (if (or (eq ?response yes) (eq ?response y))
       then yes
       else no))

(defrule enter-item ; 
	(not (add-more ?))
	=>
	(bind ?*queue-size* (+ ?*queue-size* 1)) ; counter++
	(add-item "Enter item: ") ; add value to the queue
	(bind ?need-more (yes-or-no-p "Add more item? "))
	(if (eq ?need-more yes)
		then
			(while (eq ?need-more yes) do ; prompt adding value while input is "yes"
				(bind ?*queue-size* (+ ?*queue-size* 1))
				(add-item "Enter item: ")
				(bind ?need-more (yes-or-no-p "Add more item? "))
			)
			(assert (add-more no))
		else
			(assert (add-more no))
	)	
)

(defrule confirm-items-removing ;  request confirmation for removing
	(add-more no)
	=>
	(bind ?removing-confirmed (yes-or-no-p "Remove items from queue? "))
	(if (eq ?removing-confirmed yes)
		then
			(assert (removing-confirmed yes))
		else
			(assert (removing-confirmed no))
	)
)

(defrule removing-items ; removing value from queue
    (add-more no)
    (removing-confirmed yes)
    =>
	(bind ?removing-confirmed yes)	

	(while (and (eq ?removing-confirmed yes) (neq ?*queue-size* 0)) do ; prompt adding value while input is "yes"
		(printout t "Removing item: " (first$ ?*queue*) crlf)
		(bind ?*queue* (delete$ ?*queue* 1 1)) ; 1 - start position, 1 - items to remove
		(bind ?*queue-size* (- ?*queue-size* 1))
		(if (eq ?*queue-size* 0)
			then (break))
		(bind ?removing-confirmed (yes-or-no-p "Remove more items? "))
	)
)

(defrule removing-dont-confirmed
	(add-more no)
	(removing-confirmed no)
	=>
	(printout t "No items were removed, see you later. Bye!" crlf)
)