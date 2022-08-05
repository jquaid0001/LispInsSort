; p3.lisp
; course: Organization of Programming Langauges, Spring 2022
; assignment: Programming Assignment 3
; author: Josh Quaid
; due: March 5, 11:59pm
; submitted: March 3, 10am
; directions: - To use the clisp REPL:
;             - Enter clisp by typing clisp at a prompt
;             - Type (load "p3.lisp")
;             - Type (lins '(list of nums here))
;             - ie (lins '(6 5 4 7 8))


; The insertion sort function definition "lins"
(defun lins (list)                              ; Define lins to accept one parameter "list" and accept only real number lists
  (declare (type real list))

  (if (null list)                               ; Base case, if list is empty do nothing it is either an empty list or is sorted
    (return-from lins list))                    ; so return the list.
  
  (insert (car list) (lins (cdr list))))        ; if list is not empty call insert with the head
                                                ; of the list as "item" and the tail of the list
                                                ; as "list".

; The insert helper function
(defun insert (item list)                                   ; Define insert to accept 2 parameters item to insert  and list to insert into
  
  (cond ((null list) (list item))                           ; Base case, if item is null, list is sorted so return to lins
        
        ((funcall #'< item (car list))                      ; else make a function call with < literal to insert item if item is less than the head of list 
         (cons item list))                                  ; and if true, cons the item to the front of the list and return to lins
        
        ((cons (car list) (insert item (cdr list))))))      ; else if false check the next item in the list by consing the head of the list with 
                                                            ; the result of inserting the item into the tail of the list continuing until 
                                                            ; the item is inserted into the proper position in the list
