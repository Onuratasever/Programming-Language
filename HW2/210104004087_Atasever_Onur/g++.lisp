(defvar user-input nil)
(defvar input "")
(defvar counter 0)
(defvar indexChar #\a)
(defvar lst)

(defvar KW_AND "and")
(defvar KW_OR "or")
(defvar KW_NOT "not")
(defvar KW_EQUAL "equal")
(defvar KW_LESS "less")
(defvar KW_NIL "nil")
(defvar KW_LIST "list")
(defvar KW_APPEND "append")
(defvar KW_CONCAT "concat")
(defvar KW_SET "set")
(defvar KW_DEF "def")
(defvar KW_FOR "for")
(defvar KW_IF "if")
(defvar KW_EXIT "exit")
(defvar KW_LOAD "load")
(defvar KW_DISPLAY "display")
(defvar KW_TRUE "true")
(defvar KW_FALSE "false")

(defvar OP_PLUS "+")
(defvar OP_MINUS "-")
(defvar OP_MULT "*")
(defvar OP_DIV "/")
(defvar OP_OP "(")
(defvar OP_CP ")")
(defvar OP_COMMA ",")
(defvar OP_COMMENT ";;")

(defvar isOperator 0)
(defvar startIndex 0)
(defvar lastIndex 0)
(defvar isContinue 1)
(defvar returnDegeri 0)
(defvar isValuef 0)
(defvar isThereB 0)


(defvar state "initial")
(defvar subString "")
(defvar user-input nil)
(defvar characters nil)
(defvar my-list ())

(defvar user-input "")
(defvar indexChar #\a)

;It reset all the variables
  (defun setEveryThing()
  	(setq isOperator 0)
  	(setq startIndex 0)
  	(setq lastIndex 0)
  	(setq isContinue 1)
  	(setq returnDegeri 0)
  	(setq isValuef 0)
  	(setq isThereB 0)
  	(setq indexChar #\a)
  	(setq counter 0)
  	(setq state "initial")
  )
  ;It iterates all characters of string and It goes other states according to it's state.
  ;For each state it checks it's properties and it calls necessary functions and it changes states
  (defun gppLexer(str)
  	
  	;(format t "My list: ~A~%" str)
  	(setEveryThing)
 	
  	(loop for i from 0 below (length str)
  	do
  		(setq indexChar (aref str i))
  		(setq lastIndex i)
  		(cond
  			((eq (isOp indexChar) 1)
  				(compare str startIndex lastIndex)
	  			(cond
	  				((eq returnDegeri 1)
	  					(cond
	  						((eq indexChar #\;)
	  							(setq i (length str))
	  						)
	  					)
	  					(setq isContinue 0)
	  					(setq returnDegeri 0)
	  					(setq startindex (+ i 1))
	  					;(setq i (- i 1))
	  					(setq state "next")
	  				)
	  			)
  			)
  				
  			((eq #\Space indexChar)
  				(compare str startIndex lastIndex)
	  			(cond
	  				((eq returnDegeri 1)
	  					(setq isContinue 0)
	  					(setq returnDegeri 0)
	  					(setq startindex (+ i 1))
	  					;(setq i (- i 1))
	  					(setq state "next")
	  				)
	  			)
  			)
  			((eq i (- (length str) 1))
  				(compare str startIndex (+ lastIndex 1))
	  			(cond
	  				((eq returnDegeri 1)
	  					(setq isContinue 0)
	  					(setq returnDegeri 0)
	  					(setq startindex (+ i 1))
	  					;(setq i (- i 1))
	  					(setq state "next")
	  				)
	  			)
  			)
  		)
  		
  		(cond
  			((string-equal state "initial")
  				(cond
  					((eq #\Space indexChar)

  					)
  					((digit-char-p indexChar)
  						(setq startIndex i)
  						(setq state "VALUEF")
  						
  					)
  					((alpha-char-p indexChar)
  						(setq startIndex i)
  						(setq state "IDENTIFIER")
  					)
  					((eq (isOp indexChar) 1)
  						(cond
	  						((eq indexChar #\;)
	  							(setq i (length str))
	  						)
	  					)
  						(print_op indexChar)
  						(setq isOperator 0)
  					)
  					(t
  						(write-line "SYNTAX ERROR!!")
  					)
  				)
  				;(setq i (- i 1))
  			)
  			((string-equal state "IDENTIFIER")
  				
  				(cond
  					((and (eq isContinue 1) (eq #\Space indexChar))
  						(write-line "IDENTIFIER")
  						(setq state "initial")
  					)
  					((and (eq isContinue 1) (digit-char-p indexChar))
  					
					)
					((alpha-char-p indexChar)
						
					)
					((eq (isOp indexChar) 1)
						(cond
	  						((eq indexChar #\;)
	  							(setq i (length str))
	  						)
	  						(t
								(write-line "IDENTIFIER")
	  						)
	  					)
						(print_op indexChar)
						(setq startIndex i)
						(setq state "initial")
					)
					(t
						(write-line "IDENTIFIER")
						(setq i (- i 1))
						(setq state "initial")
					)
  				)
  			)
  			((and (eq isContinue 1) (string-equal state "VALUEF"))
  				(cond
  					((and (eq isContinue 1) (eq #\Space indexChar))
  						
  						(cond
  							((eq isValuef 0)
  							)
  							(t
  								(write-line "VALUEF")
  								(setq isValuef 0)
  							)
  						)
  						(setq state "initial")
  					)
	  				((digit-char-p indexChar)
					)
					((alpha-char-p indexChar)
						(cond
							((eq indexChar #\b)
								(setq isValuef 1) ;;Bunu başka yerlerde düzeltmem gerekbilir
							)
							(t
								(cond
									((eq isValuef 1)
										(write-line "VALUEF")
							
									)
									((eq isValuef 0)
										(write-line "SYNTAX ERROR")
									)
								)
								(setq isValuef 0)
								(setq state "IDENTIFIER")
								(setq i (- i 1))
							)
						)
					)
					((eq (isOp indexChar) 1)
						(cond
	  						((eq indexChar #\;)
	  							(setq i (length str))
	  						)
	  						(t
		  						(cond
									((eq isValuef 1)
										(write-line "VALUEF")
										(setq isValuef 0)
									)
									((eq isValuef 0)
										(write-line "SYNTAX ERROR")
									)
								)	
	  						)
	  					)
						
						(print_op indexChar)
						(setq startIndex i)
						(setq state "initial")
					)
					(t
						(write-line "SYNTAX ERROR") ;bnu düzeltebilirim
					)
  				
  				)
  			)
  			
  			((eq (isOp indexChar) 1)
  				(cond
					((eq indexChar #\;)
						(setq i (length str))
					)
				)
  				(print_op indexChar)
  				(setq state "initial")
  			)
  			((string-equal state "next")
  				(setq state "initial")
  			)
  		)
  		(cond
  			((eq isContinue 0)
  				(setq isContinue 1)
  			)
  		)
  	)
  	(cond
  		((string-equal state "initial")
  		)
  		((string-equal state "VALUEF")
  			(when(eq isValuef 1)
  				(format t "~A~%" state)
  			)
  			(unless(eq isValuef 1)
  				(format t "SYNTAX ERROR~%")
  			)
  		)
  		(t
 		 	(format t "galiba geld~A~%" state)
  		)
  	)
  )
  ;It determines if it is operator
  (defun isOp(charIndex)
  	(cond
  		((equal charIndex #\+) 
  			(setq isOperator 1)
  			1
  		)
  		((equal charIndex #\-) 
  			1
  		)
  		((equal charIndex #\*) 
  			(setq isOperator 1)
  			1
  		)
  		((equal charIndex #\/) 
  			(setq isOperator 1)
  			1
  		)
  		((equal charIndex #\() 
  			(setq isOperator 1)
  			1
  		)
  		((equal charIndex #\)) 
  			(setq isOperator 1)
  			1
  		)
  		((equal charIndex #\,) 
  			(setq isOperator 1)
  			1
  		)
  		((equal charIndex #\;) 
  			(setq isOperator 1)
  			1
  		)
  		(t
  			0
  		)
  	)
  )
  ;It compares strings with tokens and changes return degeri
  (defun compare (str bas son)
  	(setq subString (subseq str bas son))
  	(dolist (item my-list)
  		(cond
  			((equal subString item)
  				(print_tokens subString)
  				(setq returnDegeri 1)
  			)
  		)
  	)
  )
  ;It prints tokens
  (defun print_tokens(subStr)
	(cond
	  	((equal subStr KW_AND)
	  		(format t "KW_AND~%")
	  	)
	  	((equal subStr KW_OR)
	  		(format t "KW_OR~%")
	  	)
	  	((equal subStr KW_EQUAL)
	  		(format t "KW_EQUAL~%")
	  	)
	  	((equal subStr KW_LESS)
	  		(format t "KW_LESS~%")
	  	)
	  	((equal subStr KW_NIL)
	  		(format t "KW_NIL~%")
	  	)
	  	((equal subStr KW_LIST)
	  		(format t "KW_LIST~%")
	  	)
	  	((equal subStr KW_APPEND)
	  		(format t "KW_APPEND~%")
	  	)
	  	((equal subStr KW_CONCAT)
	  		(format t "KW_CONCAT~%")
	  	)
	  	((equal subStr KW_SET)
	  		(format t "KW_SET~%")
	  	)
	  	((equal subStr KW_DEF)
	  		(format t "KW_DEF~%")
	  	)
	  	((equal subStr KW_FOR)
	  		(format t "KW_FOR~%")
	  	)
	  	((equal subStr KW_IF)
	  		(format t "KW_IF~%")
	  	)
	  	((equal subStr KW_EXIT)
	  		(format t "KW_EXIT~%")
	  	)
	  	((equal subStr KW_LOAD)
	  		(format t "KW_LOAD~%")
	  	)
	  	((equal subStr KW_DISPLAY)
	  		(format t "KW_DISPLAY~%")
	  	)
	  	((equal subStr KW_TRUE)
	  		(format t "KW_TRUE~%")
	  	)
	  	((equal subStr KW_FALSE)
	  		(format t "KW_FALSE~%")
	  	)
	  	((equal subStr OP_PLUS)
	  		(format t "OP_PLUS~%")
	  	)
	  	((equal subStr OP_MINUS)
	  		(format t "OP_MINUS~%")
	  	)
	  	((equal subStr OP_MULT)
	  		(format t "OP_MULT~%")
	  	)
	  	((equal subStr OP_DIV)
	  		(format t "OP_DIV~%")
	  	)
	  	((equal subStr OP_OP)
	  		(format t "OP_OP~%")
	  	)
	  	((equal subStr OP_CP)
	  		(format t "OP_CP~%")
	  	)
	  	((equal subStr OP_COMMA)
	  		(format t "OP_COMMA~%")
	  	)
	  	((equal subStr OP_COMMENT)
	  		(format t "OP_COMMENT~%")
	  	)
	  	(
	  		(format t "bulamadık bu operatoru~%")	
	  	)
  	)
  )
  ; It prints operators
 (defun print_op(getChar)
 	(cond
 		((eq getChar #\+)
 			(format t "OP_PLUS~%")
 		)
 		((eq getChar #\-)
 			(format t "OP_MINUS~%")
 		)
 		((eq getChar #\*)
 			(format t "OP_MULT~%")
 		)
 		((eq getChar #\/)
 			(format t "OP_DIV~%")
 		)
 		((eq getChar #\()
 			(format t "OP_OP~%")
 		)
 		((eq getChar #\))
 			(format t "OP_CP~%")
 		)
 		((eq getChar #\,)
 			(format t "OP_COMMA~%")
 		)
 		((eq getChar #\;)
 			(format t "OP_COMMENT~%")
 		)
 	)
 )
 ;It fill the list with tokens
  (defun fillList()
  	(setq my-list (cons KW_AND my-list))
  	(setq my-list (cons KW_OR my-list))
  	(setq my-list (cons KW_NOT my-list))
  	(setq my-list (cons KW_EQUAL my-list))
  	(setq my-list (cons KW_LESS my-list))
  	(setq my-list (cons KW_NIL my-list))
  	(setq my-list (cons KW_LIST my-list))
  	(setq my-list (cons KW_APPEND my-list))
  	(setq my-list (cons KW_CONCAT my-list))
  	(setq my-list (cons KW_SET my-list))
  	(setq my-list (cons KW_DEF my-list))
  	(setq my-list (cons KW_FOR my-list))
  	(setq my-list (cons KW_IF my-list))
  	(setq my-list (cons KW_EXIT my-list))
  	(setq my-list (cons KW_LOAD my-list))
  	(setq my-list (cons KW_DISPLAY my-list))
  	(setq my-list (cons KW_TRUE my-list))
  	(setq my-list (cons KW_FALSE my-list))
  )
;It determines if given instruction includes file or not
;If not it waits for new line
(defun gppinterpreter ()

	(format t "> " ) ;prints terminal symbol

	(setq user-input (read-line))
	(format t "~A~%" user-input)
	(cond
		((> (length user-input) 4)
			(setq input (subseq user-input 4 (length user-input)))
			(format t "~A~%" input)
		)
		(
			(setq input "randomnum")
		)
	)
	
	
	(fillList)
	(cond
		((eq (control input) 4)
			(read_file input)
		)
		(
			(setq user-input (read-line))
			(gppLexer user-input)
		)
	)
)
;It reades file line by line an It sends each line to gppLexer
(defun read_file(filename)

	(with-open-file (file filename :direction :input)
    	(	
    		loop for line = (read-line file nil)
			while line
			do (gppLexer line)
		)
	)
)
; It checks the last for character of file so it can understand the extension of file
(defun control (isFile)
	(loop for i from (- (length isFile) 4) below (length isFile)
	do
		(setq indexChar (aref isFile i))
		(cond
			((and (eq indexChar #\.) (eq counter 0))
				(setq counter (+ counter 1))
			)
			((and (eq indexChar #\g) (eq counter 1))
				(setq counter (+ counter 1))
			)
			((and (eq indexChar #\+) (eq counter 2))
				(setq counter (+ counter 1))
			)
			((and (eq indexChar #\+) (eq counter 3))
				(setq counter (+ counter 1))
			)
		)
	)
	counter
)

;Start function
(defun start()
	(format t "Enter 'g++ string' for the terminal~%")
	(terpri)
	(format t "Enter 'g++ filename.g++' to excute on file.")
	(terpri)
	(gppinterpreter)
)
(start)
