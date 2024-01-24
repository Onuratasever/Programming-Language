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

;yeni şeyler tanımlıyorum
(defvar tokens ())
(defvar sub-tokens())
(defvar token_list_contents ())
(defvar element "")
(defvar func-list ())



(defvar tokens_content())
(defvar isObey 0)

(defvar user-input "")
(defvar indexChar #\a)

(defstruct function_properties
	name
	numberOfArguments
	arguments
	implementationWithTokens
	implementationWithContents
)

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
	(setq tokens ())
	(setq sub-tokens())
	(setq token_list_contents ())
	(setq tokens_content ())
	
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
                        (setq tokens (cons "SYNTAX ERROR!!" tokens))
                        (setq tokens_content (cons (subseq str startindex lastindex) tokens_content))
  					)
  				)
  			)
  			((string-equal state "IDENTIFIER")
  				
  				(cond
  					((and (eq isContinue 1) (eq #\Space indexChar))
  						;(write-line "IDENTIFIER")
                        (setq tokens (cons "IDENTIFIER" tokens))
                        (setq tokens_content (cons (subseq str startindex lastindex) tokens_content))
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
								;(write-line "IDENTIFIER")
                                (setq tokens (cons "IDENTIFIER" tokens))
                                (setq tokens_content (cons (subseq str startindex lastindex) tokens_content))
	  						)
	  					)
						(print_op indexChar)
						(setq startIndex i)
						(setq state "initial")
					)
					(t
						;(write-line "IDENTIFIER")
                        (setq tokens (cons "IDENTIFIER" tokens))
                        (setq tokens_content (cons (subseq str startindex lastindex) tokens_content))
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
  								;(write-line "VALUEF")
                                (setq tokens (cons "VALUEF" tokens))
                                (setq tokens_content (cons (subseq str startindex lastindex) tokens_content))
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
										;(write-line "VALUEF")
                                        (setq tokens (cons "IDENTIFIER" tokens))
                                        (setq tokens_content (cons (subseq str startindex lastindex) tokens_content))
							
									)
									((eq isValuef 0)
										;(write-line "SYNTAX ERROR")
                                        (setq tokens (cons "SYNTAX ERROR" tokens))
                                        (setq tokens_content (cons (subseq str startindex lastindex) tokens_content))
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
										;(write-line "VALUEF")
                                        (setq tokens (cons "VALUEF" tokens))
                                        (setq tokens_content (cons (subseq str startindex lastindex) tokens_content))
										(setq isValuef 0)
									)
									((eq isValuef 0)
										;(write-line "SYNTAX ERROR")
                                        (setq tokens (cons "SYNTAX ERROR" tokens))
                                        (setq tokens_content (cons (subseq str startindex lastindex) tokens_content))
                                    )
								)	
	  						)
	  					)
						
						(print_op indexChar)
						(setq startIndex i)
						(setq state "initial")
					)
					(t
						(write-line "Tokenize SYNTAX ERROR") ;bnu düzeltebilirim
                        (setq tokens (cons "SYNTAX ERROR!!" tokens))
                        (setq tokens_content (cons (subseq str startindex lastindex) tokens_content))
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
  				;(format t "~A~%" state)
                (setq tokens (cons state tokens))
                (setq tokens_content (cons (subseq str startindex lastindex) tokens_content))
  			)
  			(unless(eq isValuef 1)
  				(format t "Tokenize SYNTAX ERROR~%")
                (setq tokens (cons "SYNTAX ERROR!" tokens))
                (setq tokens_content (cons (subseq str startindex lastindex) tokens_content))
  			)
  		)
  		(t
 		 	;(format t "galiba geld~A~%" state)
                                    (setq tokens (cons state tokens))
                                    (setq tokens_content (cons (subseq str startindex lastindex) tokens_content))
  		)
  	)
  )

(defun split-string-at-b (input-string)
	(let 
		((index-of-b (position #\b input-string)))
    	(if index-of-b
        	(list (subseq input-string 0 index-of-b) (subseq input-string (1+ index-of-b) ) )
        ;; If 'b' is not found, return the whole string as the first element
        	(list input-string)
		)
	)
)

(defun sum(valuef1 valuef2) ; To apply this operation I splitted valuef's two part and after operation I concatenated them
	; (format t "Gelen valuef 1: ~a~%" valuef1)
	; (format t "Gelen valuef 2: ~a~%" valuef2)

  	(multiple-value-bind (splittedValuef) (split-string-at-b valuef1)
		(multiple-value-bind (splittedValuef2) (split-string-at-b valuef2)
			
			(let(
					(numerator1 (+ (* (parse-integer (first splittedvaluef))  (parse-integer (second splittedvaluef2))) (* (parse-integer (second splittedvaluef)) (parse-integer (first splittedvaluef2)))	)	)
					(denominator1 (* (parse-integer (second splittedvaluef)) (parse-integer (second splittedvaluef2))) 	)
				)
				(let
					(
						(gcd_value (gcd numerator1 denominator1))
					)
					(let
						(
							(real_numerator1 (/ numerator1 gcd_value))
							(real_denominator1 (/ denominator1 gcd_value))
						)

						;(format t "The result is ~a~%" (concatenate 'string (write-to-string real_numerator1) "b" (write-to-string real_denominator1)))
						(return-from sum (concatenate 'string (write-to-string real_numerator1) "b" (write-to-string real_denominator1)))
					)
				)
			)
		)
	)
)

(defun sub(valuef1 valuef2); To apply this operation I splitted valuef's two part and after operation I concatenated them
	; (format t "Gelen valuef 1: ~a~%" valuef1)
	; (format t "Gelen valuef 2: ~a~%" valuef2)
	
  	(multiple-value-bind (splittedValuef) (split-string-at-b valuef1)
		(multiple-value-bind (splittedValuef2) (split-string-at-b valuef2)
			
			(let(
					(numerator1 (- (* (parse-integer (first splittedvaluef))  (parse-integer (second splittedvaluef2))) (* (parse-integer (second splittedvaluef)) (parse-integer (first splittedvaluef2)))	)	)
					(denominator1 (* (parse-integer (second splittedvaluef)) (parse-integer (second splittedvaluef2))) 	)
				)
				(let
					(
						(gcd_value (gcd numerator1 denominator1))
					)
					(let
						(
							(real_numerator1 (/ numerator1 gcd_value))
							(real_denominator1 (/ denominator1 gcd_value))
						)

						;(format t "The result is ~a~%" (concatenate 'string (write-to-string real_numerator1) "b" (write-to-string real_denominator1)))
						(return-from sub (concatenate 'string (write-to-string real_numerator1) "b" (write-to-string real_denominator1)))
					)
				)
			)
		)
	)
)

(defun mult(valuef1 valuef2); To apply this operation I splitted valuef's two part and after operation I concatenated them
	
  	(multiple-value-bind (splittedValuef) (split-string-at-b valuef1)
		(multiple-value-bind (splittedValuef2) (split-string-at-b valuef2)
			(let(
					(numerator1 (* (parse-integer (first splittedvaluef))  (parse-integer (first splittedvaluef2))  ) )	
					(denominator1 (* (parse-integer (second splittedvaluef)) (parse-integer (second splittedvaluef2))) 	)
				)
				(let
					(
						(gcd_value (gcd numerator1 denominator1))
					)
					(let
						(
							(real_numerator1 (/ numerator1 gcd_value))
							(real_denominator1 (/ denominator1 gcd_value))
						)

						;(format t "The result is ~d/~d~%" real_numerator1 real_denominator1)
						(return-from mult (concatenate 'string (write-to-string real_numerator1) "b" (write-to-string real_denominator1)))
					)
				)
			)
		)
	)
)

(defun operations(op_name valuef1 valuef2); To apply this operation I splitted valuef's two part and after operation I concatenated them and I checked their correctness
	; (format t "Gelen valuef 1: ~a~%" valuef1)
	; (format t "Gelen valuef 2: ~a~%" valuef2)
	
  	(multiple-value-bind (splittedValuef) (split-string-at-b valuef1)
		(multiple-value-bind (splittedValuef2) (split-string-at-b valuef2)
			(let(
					(numerator1 (- (* (parse-integer (first splittedvaluef))  (parse-integer (second splittedvaluef2))) (* (parse-integer (second splittedvaluef)) (parse-integer (first splittedvaluef2)))	)	)
					(denominator1 (* (parse-integer (second splittedvaluef)) (parse-integer (second splittedvaluef2))) 	)
				)
				(let
					(
						(gcd_value (gcd numerator1 denominator1))
					)
					(let
						(
							(real_numerator1 (/ numerator1 gcd_value))
							(real_denominator1 (/ denominator1 gcd_value))
						)

						; (format t "The result is ~a~%" (concatenate 'string (write-to-string real_numerator1) "b" (write-to-string real_denominator1)))
						; (return-from sub (concatenate 'string (write-to-string real_numerator1) "b" (write-to-string real_denominator1)))
						(cond
							((string-equal op_name "KW_LESS")
								(cond
									((< numerator1 0)
										(return-from operations "true")
									)
									(t
										(return-from operations "false")
									)
								)
							)
							((string-equal op_name "KW_EQUAL")
								(cond
									((= numerator1 0)
										(return-from operations "true")
									)
									(t
										(return-from operations "false")
									)
								)
							)
							((string-equal op_name "KW_AND")
								(cond
									((or (= (parse-integer (first splittedvaluef)) 0) (null (parse-integer (first splittedvaluef)))   (=  (parse-integer (first splittedvaluef2)) 0) (null (parse-integer (first splittedvaluef2)))) 
										(return-from operations "false")
									)
									(t
										(return-from operations "true")
									)
								)
							)
							((string-equal op_name "KW_OR")
								(cond
									((or (= (parse-integer (first splittedvaluef)) 1) (not (null (parse-integer (first splittedvaluef))))   (=  (parse-integer (first splittedvaluef2)) 1) (not (null (parse-integer (first splittedvaluef2))))) 
										(return-from operations "true")
									)
									(t
										(return-from operations "false")
									)
								)
							)
						)
					)
				)
			)
		)
	)
)

(defun divi(valuef1 valuef2); To apply this operation I splitted valuef's two part and after operation I concatenated them
	
  	(multiple-value-bind (splittedValuef) (split-string-at-b valuef1)
		(multiple-value-bind (splittedValuef2) (split-string-at-b valuef2)
			(let(
					(numerator1 (* (parse-integer (first splittedvaluef))   (parse-integer (second splittedvaluef2))   ) )	
					(denominator1 (* (parse-integer (second splittedvaluef)) (parse-integer (first splittedvaluef2)) ) 	)
				)
				(let
					(
						(gcd_value (gcd numerator1 denominator1))
					)
					(let
						(
							(real_numerator1 (/ numerator1 gcd_value))
							(real_denominator1 (/ denominator1 gcd_value))
						)

						;(format t "The result is ~d/~d~%" real_numerator1 real_denominator1)
						(return-from divi (concatenate 'string (write-to-string real_numerator1) "b" (write-to-string real_denominator1)))

					)
				)
			)
		)
	)
)

(defun findIndexesofExp(startIndex list_of_tokens) ; It finds first openening and matched closing paranthesis. Then according to their indexes it returns a sub list
	(setq counter 0)
	(setq element "")
            	;(format t "sTART INDEX OF FUNTCİON: ~d tokens: ~A~%" startIndex list_of_tokens)
	(loop for item-index from startIndex below (length list_of_tokens)
    	do	(progn
            	;(format t "~A~%" element)
             ;; Additional lines of code go here if needed
            	;(format t "Processing element: ~A~%" (nth item-index list_of_tokens))
				(setq element (nth item-index list_of_tokens))
				(cond
					((string-equal element "(")
						(setq counter (+ 1 counter))
            			;(format t "counter: ~d~%" counter)
					)
					((string-equal element ")")
						(setq counter (- counter 1))
            			;(format t "counter: ~d~%" counter)
					)
				)
				(cond
					((= 0 counter)
						
						;(format t "firstIndex: ~d secondIndex: ~d~%" startindex item-index)
						(return-from findIndexesofExp (list startindex (+ 1 item-index)))
					)
					(t
					)
				)
			)
	)
)

(defun expression(token_list token_content) ;This is expression rule
	(setq isValuef 0)
	
		; (format t "~%----------~%GELEN TOKEN: ~A ~% GELEN CONTENT ~A ~%-----------------~%" token_list token_content)

  		(cond 
  			((and ;It contains rules for plus minus division and multiplication
				(string-equal (first token_list) "OP_OP")
  				(or (string-equal (second token_list) "OP_PLUS") (string-equal (second token_list) "OP_MINUS") (string-equal (second token_list) "OP_MULT") (string-equal (second token_list) "OP_DIV"))
  				(string-equal (nth (- (length token_list) 1) token_list) "OP_CP")
  			)
  			
				(let
					(
						(index_list nil)
						(returnexpression nil)
						(returnexpression2 nil)
					)
					;(format t "Tokenlar içeri girdi ~A  GELEN CONTENT ~A ~%"token_list token_content)
					(cond ;It checks validity of input
						((string= (third token_content) ")")
							(format t "Hiç expression girdiniz! ~%")
							(return-from expression nil)
						)
					)
					(setq index_list (findIndexesofExp 2 token_content));It finds indexes to get a substring for sub expression
					;(format t "Indexlist 1: ~d Indexlist 2: ~d ~%"(first index_list) (second index_list))
					(setq sub-tokens (subseq token_list (first index_list) (second index_list))) ;index 2 yi içeren liste
					(setq token_list_contents (subseq token_content (first index_list) (second index_list)))
					;(format t "Subseq 1: ~A~%" sub-tokens)
					(setq returnExpression (expression sub-tokens token_list_contents) ); It finds next expression (between paranthesis or value or identifier) to process them recursively
					;(format t "return değeri ilk : ~a~%" returnExpression)
					(cond
						(returnExpression ; It finds next expression (between paranthesis or value or identifier) to process them recursively
							(cond	;It checks validity of input
								((string-equal (nth (second index_list) token_content) ")")
									(format t "Sadece bir expression girdiniz! ~%")
									(return-from expression nil)
								)
							)
							(setq index_list (findIndexesofExp (second index_list) token_content))
							(setq sub-tokens (subseq token_list (first index_list) (second index_list))) ;It finds indexes to get a substring for sub expression
							(setq token_list_contents (subseq token_content(first index_list) (second index_list)))
							;(format t "Subseq 2: ~A~%" sub-tokens)
							;(format t "Subseq 2 NİN TOKEN LİSTİ: ~A~%" token_list_contents)

							(setq returnExpression2 (expression sub-tokens token_list_contents) ); It finds next expression (between paranthesis or value or identifier) to process them recursively
		
							(cond
								( returnExpression2 ;;/bunun recursive ile valuef return etmesi lazım
									(setq isObey 1)
									(cond
										((= isValuef 1)
											; (format t "seçilecek token content : ~a~%" token_content)
											; (format t "seçilecek token content list mi : ~a~%" (third token_content))
											;(format t "En sondaki kapanan parantez mi : ~a~%"(nth (second index_list) token_content) )

											;There are for situations (non terminal, terminal) (terminal, non terminal)  (terminal, terminal) (non terminal, non terminal)
											;This conditions evaluate all valuefs 
											(cond
												((and (or (string-equal "IDENTIFIER" returnexpression) (string-equal "VALUEF" returnexpression)) (or (string-equal "IDENTIFIER" returnexpression2) (string-equal "VALUEF" returnexpression2)) (string-equal ")" (nth (second index_list) token_content)))
													;(format t "VALUEFE return 1 ve return 2 olarak GELDİİİİ :~%")
													; (format t "TOKEN 1: ~A TOKEN 2: ~a :~%"(third token_content) (fourth token_content))
													(cond
														( (string-equal (second token_list) "OP_PLUS")
															(return-from expression	(sum (third token_content) (fourth token_content))) ; recursive den gelen değer gönderilcek
														)
														( (string-equal (second token_list) "OP_MINUS")
															(return-from expression	(sub (third token_content) (fourth token_content))) ; recursive den gelen değer gönderilcek
														)
														( (string-equal (second token_list) "OP_MULT")
															(return-from expression	(mult (third token_content) (fourth token_content))) ; recursive den gelen değer gönderilcek
														)
														( (string-equal (second token_list) "OP_DIV")
															(return-from expression	(divi (third token_content) (fourth token_content))) ; recursive den gelen değer gönderilcek
														)
													)
												)
												((and (or (string-equal "IDENTIFIER" returnexpression) (string-equal "VALUEF" returnexpression)) (and (string/= "IDENTIFIER" returnexpression2) (string/= "VALUEF" returnexpression2)) (string-equal ")" (nth (second index_list) token_content)))
													(let(
															(returntemp2 returnexpression2)
														)

														(cond
															( (string-equal (second token_list) "OP_PLUS")
																(return-from expression (sum (third token_content) returntemp2)) ; recursive den gelen değer gönderilcek
															)
															( (string-equal (second token_list) "OP_MINUS")
																(return-from expression (sub (third token_content) returntemp2)) ; recursive den gelen değer gönderilcek
															)
															( (string-equal (second token_list) "OP_MULT")
																(return-from expression (mult (third token_content) returntemp2)) ; recursive den gelen değer gönderilcek
															)
															( (string-equal (second token_list) "OP_DIV")
																(return-from expression (divi (third token_content) returntemp2)) ; recursive den gelen değer gönderilcek
															)
														)
													)
												)
												((and (or (string-equal "IDENTIFIER" returnexpression2) (string-equal "VALUEF" returnexpression2)) (and (string/= "IDENTIFIER" returnexpression) (string/= "VALUEF" returnexpression)) (string-equal ")" (nth (second index_list) token_content)))
													(let(
															(returntemp1 returnexpression)
														)
													(cond
															( (string-equal (second token_list) "OP_PLUS")
																(return-from expression (sum returntemp1 (nth (- (length token_content) 2) token_content))) ; recursive den gelen değer gönderilcek
															)
															( (string-equal (second token_list) "OP_MINUS")
																(return-from expression (sub returntemp1 (nth (- (length token_content) 2) token_content))) ; recursive den gelen değer gönderilcek
															)
															( (string-equal (second token_list) "OP_MULT")
																(return-from expression (mult returntemp1 (nth (- (length token_content) 2) token_content))) ; recursive den gelen değer gönderilcek
															)
															( (string-equal (second token_list) "OP_DIV")
																(return-from expression (divi returntemp1 (nth (- (length token_content) 2) token_content))) ; recursive den gelen değer gönderilcek
															)
													)

													
													)
												)
												;
												((and (and (string/= "IDENTIFIER" returnexpression) (string/= "VALUEF" returnexpression)) 
														(and (string/= "IDENTIFIER" returnexpression2) (string/= "VALUEF" returnexpression2)) (string-equal ")" (nth (second index_list) token_content)))
														
													(cond
															( (string-equal (second token_list) "OP_PLUS")
																(return-from expression (sum returnexpression returnexpression2)) ; recursive den gelen değer gönderilcek
															)
															( (string-equal (second token_list) "OP_MINUS")
																(return-from expression (sub returnexpression returnexpression2)) ; recursive den gelen değer gönderilcek
															)
															( (string-equal (second token_list) "OP_MULT")
																(return-from expression (mult returnexpression returnexpression2)) ; recursive den gelen değer gönderilcek
															)
															( (string-equal (second token_list) "OP_DIV")
																(return-from expression (divi returnexpression returnexpression2)) ; recursive den gelen değer gönderilcek
															)
													)
												)
												(t
													(return-from expression nil)
												)
												
											)
										)
										((/= isValuef 1)
												;There are for situations (non terminal, terminal) (terminal, non terminal)  (terminal, terminal) (non terminal, non terminal)
												;This conditions evaluate all identifiers 
											(cond
												;(or (string-equal "IDENTIFIER" returnexpression) (string-equal "VALUEF" returnexpression)) (or (string-equal "IDENTIFIER" returnexpression2) (string-equal "VALUEF" returnexpression2))
												((and (or (string-equal "IDENTIFIER" returnexpression) (string-equal "VALUEF" returnexpression)) (or (string-equal "IDENTIFIER" returnexpression2) (string-equal "VALUEF" returnexpression2)) (string-equal ")" (nth (second index_list) token_content)))
													(cond
															( (string-equal (second token_list) "OP_PLUS")
																(return-from expression (concatenate 'string (third token_content) " + "  (fourth token_content))) ; recursive den gelen değer gönderilcek
															)
															( (string-equal (second token_list) "OP_MINUS")
																(return-from expression (concatenate 'string (third token_content) " - "  (fourth token_content))) ; recursive den gelen değer gönderilcek
															)
															( (string-equal (second token_list) "OP_MULT")
																(return-from expression (concatenate 'string (third token_content) " * "  (fourth token_content))) ; recursive den gelen değer gönderilcek
															)
															( (string-equal (second token_list) "OP_DIV")
																(return-from expression (concatenate 'string (third token_content) " / "  (fourth token_content))) ; recursive den gelen değer gönderilcek
															)
													)
													
												)
												;(or (string-equal "IDENTIFIER" returnexpression) (string-equal "VALUEF" returnexpression)) (and (string/= "IDENTIFIER" returnexpression2) (string/= "VALUEF" returnexpression2))
												((and (or (string-equal "IDENTIFIER" returnexpression) (string-equal "VALUEF" returnexpression)) (and (string/= "IDENTIFIER" returnexpression2) (string/= "VALUEF" returnexpression2)) (string-equal ")" (nth (second index_list) token_content)))
													(cond
															( (string-equal (second token_list) "OP_PLUS")
																(return-from expression (concatenate 'string (third token_content) " + "  returnexpression2)) ; recursive den gelen değer gönderilcek
															)
															( (string-equal (second token_list) "OP_MINUS")
																(return-from expression (concatenate 'string (third token_content) " - "  returnexpression2)) ; recursive den gelen değer gönderilcek
															)
															( (string-equal (second token_list) "OP_MULT")
																(return-from expression (concatenate 'string (third token_content) " * "  returnexpression2)) ; recursive den gelen değer gönderilcek
															)
															( (string-equal (second token_list) "OP_DIV")
																(return-from expression (concatenate 'string (third token_content) " / "  returnexpression2)) ; recursive den gelen değer gönderilcek
															)
													)
													
													
												)
												;(or (string-equal "IDENTIFIER" returnexpression2) (string-equal "VALUEF" returnexpression2)) (and (string/= "IDENTIFIER" returnexpression) (string/= "VALUEF" returnexpression))
												((and (or (string-equal "IDENTIFIER" returnexpression2) (string-equal "VALUEF" returnexpression2)) (and (string/= "IDENTIFIER" returnexpression) (string/= "VALUEF" returnexpression)) (string-equal ")" (nth (second index_list) token_content)))
													(cond
															( (string-equal (second token_list) "OP_PLUS")
																(return-from expression (concatenate 'string returnexpression " + "  (fourth token_content))) ; recursive den gelen değer gönderilcek
															)
															( (string-equal (second token_list) "OP_MINUS")
																(return-from expression (concatenate 'string returnexpression " - "  (fourth token_content))) ; recursive den gelen değer gönderilcek
															)
															( (string-equal (second token_list) "OP_MULT")
																(return-from expression (concatenate 'string returnexpression " * "  (fourth token_content))) ; recursive den gelen değer gönderilcek
															)
															( (string-equal (second token_list) "OP_DIV")
																(return-from expression (concatenate 'string returnexpression " / "  (fourth token_content))) ; recursive den gelen değer gönderilcek
															)
													)
													
													
												)
												;(string/= "1" returnexpression2) (string/= "1" returnexpression)
												((and (and (string/= "IDENTIFIER" returnexpression) (string/= "VALUEF" returnexpression)) 
														(and (string/= "IDENTIFIER" returnexpression2) (string/= "VALUEF" returnexpression2)) (string-equal ")" (nth (second index_list) token_content)))
														(cond
															( (string-equal (second token_list) "OP_PLUS")
																(return-from expression (concatenate 'string returnexpression " + "  returnexpression2)) ; recursive den gelen değer gönderilcek
															)
															( (string-equal (second token_list) "OP_MINUS")
																(return-from expression (concatenate 'string returnexpression " - "  returnexpression2)) ; recursive den gelen değer gönderilcek
															)
															( (string-equal (second token_list) "OP_MULT")
																(return-from expression (concatenate 'string returnexpression " * "  returnexpression2)) ; recursive den gelen değer gönderilcek
															)
															( (string-equal (second token_list) "OP_DIV")
																(return-from expression (concatenate 'string returnexpression " / "  returnexpression2)) ; recursive den gelen değer gönderilcek
															)
													)	
													
												)
												(t
													(return-from expression nil)
												)
												
											)
										)
									)
									
								)
							)
						)
					)

				)
  				
	  		);cond end
	  	
	  		((and (= (length token_list) 1) ;Identifier base case
	  				(string-equal (first token_list) "IDENTIFIER")
	  		)
	  				; (format t "IDENTIFIER BASE CASE GELDI~%")
            		(setq isValuef 0) ; not valuef
	   				(return-from expression "IDENTEFIER")
	  		);cond end
	  		
	  		
	  		((and (= (length token_list) 1);valuef base case
	  				(string-equal (first token_list) "VALUEF")
	  		)
	  				; (format t "VALUEF BASE CASE GELDI~%")
            		(setq isValuef 1)
		   			(return-from expression "VALUEF")
	  		);cond end
			

			((and	;Function call rules
					(string-equal (first token_list) "OP_OP")
	  				(string-equal (second token_list) "IDENTIFIER")
	  				(string-equal (nth (- (length token_list) 1) token_list) "OP_CP")
				)
				(let
					(
						(foundFunc (find (second token_content) func-list :key #'function_properties-name :test #'string=))
						(new_contents nil)
						(new_tokens nil)
						(index_list nil)
						(returnexpression nil)
						(returnexpression2 nil)
						(returnValue1 "")
						(returnValue2 "")
					)
					(cond
						((null foundfunc) ;If it can not find the name of function called
							(return-from expression nil)
						)
					)
					; (format t "Buradan First function's name: ~a~%" (function_properties-name foundFunc))
					; ;(format t "The type of my-variable is: ~a~%" (first (function_properties-implementationWithContents foundFunc)))
					; (format t "First function's arguments: ~a~%" (function_properties-arguments foundFunc))
					; (format t "First function's implementation: ~a~%" (function_properties-implementationWithContents foundFunc))
					; (format t "First function's implementation: ~a~%" (function_properties-implementationWithTokens foundFunc))

					

					(cond
						((string/= (third token_content) ")") ;It checks the number of arguments
							(setq index_list (findIndexesofExp 2 token_content))
							; (format t " ~A FONKSİYONDA  Indexlist 1: ~d Indexlist 2: ~d ~%"(function_properties-name foundFunc) (first index_list) (second index_list))
							(setq sub-tokens (subseq token_list (first index_list) (second index_list))) ;index 2 yi içeren liste
							(setq token_list_contents (subseq token_content (first index_list) (second index_list)))
							; (format t " ~A FONKSİYONDA Subseq 1: ~A~%" (function_properties-name foundFunc) sub-tokens)
							; (format t " ~A FONKSİYONDA Subseq 1 İN TOKEN LİSTİ: ~A~%" (function_properties-name foundFunc) token_list_contents)
							(setq returnExpression (expression sub-tokens token_list_contents) )
							; (format t " ~A FONKSİYONDA RETURN 1: ~A~%" (function_properties-name foundFunc) returnexpression)
							(setq returnvalue1 (first token_list_contents))
							
							(cond
								((string/= (nth (second index_list) token_content) ")" );It checks the number of arguments
										(setq index_list (findIndexesofExp (second index_list) token_content))
										; (format t " ~A ıKNCI PARTTT Indexlist 1: ~d Indexlist 2: ~d ~%"(function_properties-name foundFunc) (first index_list) (second index_list))
										(setq sub-tokens (subseq token_list (first index_list) (second index_list))) ;index 3 yi içeren liste 
										(setq token_list_contents (subseq token_content(first index_list) (second index_list)))
										; (format t " ~A FONKSİYONDA Subseq 2: ~A~%" (function_properties-name foundFunc) sub-tokens)
										; (format t " ~A FONKSİYONDA Subseq 2 İN TOKEN LİSTİ: ~A~%" (function_properties-name foundFunc) token_list_contents)
										(setq returnExpression2 (expression sub-tokens token_list_contents) )
										; (format t " ~A FONKSİYONDA RETURN 2: ~A~%" (function_properties-name foundFunc) returnexpression2)
										(setq returnvalue2 (first token_list_contents))
										; (format t " ~A ilk kısmın token listin ilk elemanı: ~A~%" (function_properties-name foundFunc) returnvalue2)


										; (format t " ~A FONKSİYONDA Son elemean: ~A~%" (function_properties-name foundFunc) (nth (second index_list) token_content))
										(cond ;Eğer eşit değilse ikiden çok expression var demek. 
											((string/= (nth (second index_list) token_content) ")" )
											(format t "Parametre sayısı bekleneenin üstünde ~%" )
											
												(return-from expression nil)
											)
										)
								)
								((= 2 (function_properties-numberOfArguments foundFunc))
									(format t "Beklenen parametre sayısı 2, sizinki 1 ~%" )
											
									(return-from expression nil)
								)
							)
						)
						((= 1 (function_properties-numberOfArguments foundFunc))
							(format t "Beklenen parametre sayısı 1, sizinki 0 ~%" )
											
									(return-from expression nil)
						)
						((= 2 (function_properties-numberOfArguments foundFunc))
							(format t "Beklenen parametre sayısı 2, sizinki 0 ~%" )
											
									(return-from expression nil)
						)
					)
					;When function called, according to implementation of defined function, new token lists, and contents according to inputs are created
					(loop for item-index from 0 below (length (function_properties-implementationWithContents foundFunc))
						do(progn
							(cond
								 ((string-equal (nth item-index (function_properties-implementationWithContents foundFunc)) (first (function_properties-arguments foundFunc)))
								 		(cond
											((string-equal returnexpression "VALUEF")
												 (setq new_contents (cons returnvalue1 new_contents))
												 (setq new_tokens (cons "VALUEF" new_tokens))
											)
											(t
												(setq new_contents (cons returnexpression new_contents))
										 		(setq new_tokens (cons "VALUEF" new_tokens))
											)
										)
								 )
								 ((string-equal (nth item-index (function_properties-implementationWithContents foundFunc)) (second (function_properties-arguments foundFunc)))
										(cond
											((string-equal returnexpression2 "VALUEF")
												(setq new_contents (cons returnvalue2 new_contents))
												(setq new_tokens (cons "VALUEF" new_tokens))
											)
											(t
												(setq new_contents (cons returnexpression2 new_contents))
										 		(setq new_tokens (cons "VALUEF" new_tokens))
											)
										)
								 )
								 (t
										 (setq new_contents (cons (nth item-index (function_properties-implementationWithContents foundFunc)) new_contents))
										 (setq new_tokens (cons (nth item-index (function_properties-implementationWithTokens foundFunc)) new_tokens))
								 )

							)
						)
					)
					(setq new_contents (reverse new_contents))
					(setq new_tokens (reverse new_tokens))
					; (format t "new content list is: ~a~%" new_contents)
					; (format t "new token list is: ~a~%" new_tokens)
					(return-from expression (expression new_tokens new_contents))

				)
			)
			((and	;If rules
					(string-equal (first token_list) "OP_OP")
	  				(string-equal (second token_list) "KW_IF")
	  				(string-equal (nth (- (length token_list) 1) token_list) "OP_CP")
				)
				(let
					(
						(index_list nil)
						(isTrue "")
						(returnValueExp "")
					)
					(setq index_list (findIndexesofExp 2 token_content))
					(setq sub-tokens (subseq token_list (first index_list) (second index_list))) ;index 3 yi içeren liste //bunun indexlerini düzgün bulup yollamak lazım
					(setq token_list_contents (subseq token_content(first index_list) (second index_list)))
					; (format t "Subseq 2: ~A~%" sub-tokens)
					; (format t "Subseq 2 NİN TOKEN LİSTİ: ~A~%" token_list_contents)
					(cond
						((or (string-equal (fourth token_list) "KW_LESS") (string-equal (fourth token_list) "KW_EQUAL") (string-equal (fourth token_list) "KW_OR") (string-equal (fourth token_list) "KW_AND"))
								
							(setq isTrue (expression sub-tokens token_list_contents)) ; It finds whether condition is true recursively
							(setq index_list (findIndexesofExp  (second index_list) token_content)); It finds the indexes of expression to find sub expression when condition is true
							(setq sub-tokens (subseq token_list (first index_list) (second index_list))) ;index 3 yi içeren liste //bunun indexlerini düzgün bulup yollamak lazım
							(setq token_list_contents (subseq token_content(first index_list) (second index_list)))
							; (format t "Subseq 2: ~A~%" sub-tokens)
							; (format t "Subseq 2 NİN TOKEN LİSTİ: ~A~%" token_list_contents)
							(cond
								((string-equal isTrue "true")
									(setq returnValueExp (expression sub-tokens token_list_contents))
									(return-from expression returnValueExp)
								)
								((string-equal isTrue "false")

									(setq index_list (findIndexesofExp  (second index_list) token_content)); It finds the indexes of expression to find sub expression when condition is true
									(setq sub-tokens (subseq token_list (first index_list) (second index_list))) ;index 3 yi içeren liste 
									(setq token_list_contents (subseq token_content(first index_list) (second index_list)))
									; (format t "Subseq 2: ~A~%" sub-tokens)
									; (format t "Subseq 2 NİN TOKEN LİSTİ: ~A~%" token_list_contents)
									(setq returnValueExp (expression sub-tokens token_list_contents))
									; (format t "Return Value from IF 2: ~A~%" returnvalueexp)
									(return-from expression returnValueExp)
								)
								(t
									;If conditions are created from identifiers, it assumes it is correct
									(setq returnValueExp (expression sub-tokens token_list_contents))
									(return-from expression returnValueExp)
								)

							)
						)
					)
				)

			)
			;These rules are added for if. There are four different rules to implement if correctly
			((and (string-equal (first token_list) "OP_OP")
  				(or (string-equal (second token_list) "KW_LESS") (string-equal (second token_list) "KW_EQUAL") (string-equal (second token_list) "KW_OR") (string-equal (second token_list) "KW_AND"))
  				(string-equal (nth (- (length token_list) 1) token_list) "OP_CP")
  			)
  			
				(let
					(
						(index_list nil)
						(returnexpression nil)
						(returnexpression2 nil)
					)
					(setq index_list (findIndexesofExp 2 token_content))
					; (format t "IF STATEMENT İÇİN Indexlist 1: ~d Indexlist 2: ~d ~%"(first index_list) (second index_list))
					(setq sub-tokens (subseq token_list (first index_list) (second index_list))) ;index 2 yi içeren liste
					(setq token_list_contents (subseq token_content (first index_list) (second index_list)))
					; (format t "IF STATEMENT İÇİN Subseq 1: ~A~%" sub-tokens)
					(setq returnExpression (expression sub-tokens token_list_contents) )
					; (format t "IF STATEMENT İÇİN return değeri ilk : ~a~%" returnExpression)
					(cond
						(returnExpression

							(setq index_list (findIndexesofExp (second index_list) token_content))
							; (format t "IF STATEMENT İÇİN ıKNCI PARTTT Indexlist 1: ~d Indexlist 2: ~d ~%"(first index_list) (second index_list))
							(setq sub-tokens (subseq token_list (first index_list) (second index_list))) ;index 3 yi içeren liste //bunun indexlerini düzgün bulup yollamak lazım
							(setq token_list_contents (subseq token_content(first index_list) (second index_list)))
							; (format t "IF STATEMENT İÇİN Subseq 2: ~A~%" sub-tokens)
							; (format t "IF STATEMENT İÇİN Subseq 2 NİN TOKEN LİSTİ: ~A~%" token_list_contents)

							(setq returnExpression2 (expression sub-tokens token_list_contents) )
							; (format t "IF STATEMENT İÇİN return değeri iki : ~a~%" returnExpression2)
											; (format t "IF STATEMENT İÇİN return1 : ~a return2 : ~a~%" returnexpression returnexpression2)
							(cond
								( returnExpression2 ;;/bunun recursive ile valuef return etmesi lazım
									; (format t "IF STATEMENT İÇİN HARBİ YAPTIM iki LAN~%")
									(setq isObey 1)
									(cond
										((= isValuef 1)
											; (format t "IF STATEMENT İÇİN seçilecek token content : ~a~%" token_content)
											; (format t "IF STATEMENT İÇİN seçilecek token content list mi : ~a~%" (third token_content))
											; (format t "IF STATEMENT İÇİN En sondaki kapanan parantez mi : ~a~%"(nth (second index_list) token_content) )
											(cond
												((and (or (string-equal "IDENTIFIER" returnexpression) (string-equal "VALUEF" returnexpression)) (or (string-equal "IDENTIFIER" returnexpression2) (string-equal "VALUEF" returnexpression2)) (string-equal ")" (nth (second index_list) token_content)))
													; (format t "IF STATEMENT İÇİN VALUEFE return 1 ve return 2 olarak GELDİİİİ :~%")
													; (format t "IF STATEMENT İÇİN TOKEN 1: ~A TOKEN 2: ~a :~%"(third token_content) (fourth token_content))
													(cond
														( (string-equal (second token_list) "KW_LESS")
															(return-from expression	(operations "KW_LESS" (third token_content) (fourth token_content))) ; recursive den gelen değer gönderilcek
														)
														( (string-equal (second token_list) "KW_EQUAL")
															(return-from expression	(operations "KW_EQUAL" (third token_content) (fourth token_content))) ; recursive den gelen değer gönderilcek
														)
														( (string-equal (second token_list) "KW_AND")
															(return-from expression	(operations "KW_AND" (third token_content) (fourth token_content))) ; recursive den gelen değer gönderilcek
														)
														( (string-equal (second token_list) "KW_OR")
															(return-from expression	(operations "KW_OR" (third token_content) (fourth token_content))) ; recursive den gelen değer gönderilcek
														)
													)
												)
												((and (or (string-equal "IDENTIFIER" returnexpression) (string-equal "VALUEF" returnexpression)) (and (string/= "IDENTIFIER" returnexpression2) (string/= "VALUEF" returnexpression2)) (string-equal ")" (nth (second index_list) token_content)))
													(let(
															(returntemp2 returnexpression2)
														)

														(cond
															( (string-equal (second token_list) "KW_LESS")
																(return-from expression (operations "KW_LESS" (third token_content) returntemp2)) ; recursive den gelen değer gönderilcek
															)
															( (string-equal (second token_list) "KW_EQUAL")
																(return-from expression (operations "KW_EQUAL" (third token_content) returntemp2)) ; recursive den gelen değer gönderilcek
															)
															( (string-equal (second token_list) "KW_AND")
																(return-from expression (operations "KW_AND" (third token_content) returntemp2)) ; recursive den gelen değer gönderilcek
															)
															( (string-equal (second token_list) "KW_OR")
																(return-from expression (operations "KW_OR" (third token_content) returntemp2)) ; recursive den gelen değer gönderilcek
															)
														)
													)
												)
												((and (or (string-equal "IDENTIFIER" returnexpression2) (string-equal "VALUEF" returnexpression2)) (and (string/= "IDENTIFIER" returnexpression) (string/= "VALUEF" returnexpression)) (string-equal ")" (nth (second index_list) token_content)))
													(let(
															(returntemp1 returnexpression)
														)
														; (format t "IF STATEMENT İÇİN 3. IFTEYİZZZ return1 : ~a return2 : ~a~%" returnexpression returnexpression2)
													
													(cond
															( (string-equal (second token_list) "KW_LESS")
																(return-from expression (operations "KW_LESS" returntemp1 (nth (- (length token_content) 2) token_content))) ; recursive den gelen değer gönderilcek
															)
															( (string-equal (second token_list) "KW_EQUAL")
																(return-from expression (operations "KW_EQUAL" returntemp1 (nth (- (length token_content) 2) token_content))) ; recursive den gelen değer gönderilcek
															)
															( (string-equal (second token_list) "KW_AND")
																(return-from expression (operations "KW_AND" returntemp1 (nth (- (length token_content) 2) token_content))) ; recursive den gelen değer gönderilcek
															)
															( (string-equal (second token_list) "KW_OR")
																(return-from expression (operations "KW_OR" returntemp1 (nth (- (length token_content) 2) token_content))) ; recursive den gelen değer gönderilcek
															)
													)

													
													)
												)
												((and (and (string/= "IDENTIFIER" returnexpression) (string/= "VALUEF" returnexpression)) 
														(and (string/= "IDENTIFIER" returnexpression2) (string/= "VALUEF" returnexpression2)) (string-equal ")" (nth (second index_list) token_content)))
														;(format t "IF STATEMENT İÇİN Son IFTEYIZZZ return1 : ~a return2 : ~a~%" returnexpression returnexpression2)
														
													(cond
															( (string-equal (second token_list) "KW_LESS")
																(return-from expression (operations "KW_LESS" returnexpression returnexpression2)) ; recursive den gelen değer gönderilcek
															)
															( (string-equal (second token_list) "KW_EQUAL")
																(return-from expression (operations "KW_EQUAL" returnexpression returnexpression2)) ; recursive den gelen değer gönderilcek
															)
															( (string-equal (second token_list) "KW_AND")
																(return-from expression (operations "KW_AND" returnexpression returnexpression2)) ; recursive den gelen değer gönderilcek
															)
															( (string-equal (second token_list) "KW_OR")
																(return-from expression (operations "KW_OR" returnexpression returnexpression2)) ; recursive den gelen değer gönderilcek
															)
													)
												)
												(t
													(return-from expression nil)
												)
												
											)
										)
										((/= isValuef 1)
											(cond
												((and (or (string-equal "IDENTIFIER" returnexpression) (string-equal "VALUEF" returnexpression)) (or (string-equal "IDENTIFIER" returnexpression2) (string-equal "VALUEF" returnexpression2)) (string-equal ")" (nth (second index_list) token_content)))
													; (format t "IF STATEMENT İÇİN VALUEF DEĞİLE return 1 ve return 2 olarak GELDİİİİ :~%")
													(cond
															( (string-equal (second token_list) "KW_LESS")
																(return-from expression (concatenate 'string (third token_content) " < "  (fourth token_content))) ; recursive den gelen değer gönderilcek
															)
															( (string-equal (second token_list) "KW_EQUAL")
																(return-from expression (concatenate 'string (third token_content) " = "  (fourth token_content))) ; recursive den gelen değer gönderilcek
															)
															( (string-equal (second token_list) "KW_AND")
																(return-from expression (concatenate 'string (third token_content) " && "  (fourth token_content))) ; recursive den gelen değer gönderilcek
															)
															( (string-equal (second token_list) "KW_OR")
																(return-from expression (concatenate 'string (third token_content) " || "  (fourth token_content))) ; recursive den gelen değer gönderilcek
															)
													)
													
												)
												((and (or (string-equal "IDENTIFIER" returnexpression) (string-equal "VALUEF" returnexpression)) (and (string/= "IDENTIFIER" returnexpression2) (string/= "VALUEF" returnexpression2)) (string-equal ")" (nth (second index_list) token_content)))
													(cond
															( (string-equal (second token_list) "KW_LESS")
																(return-from expression (concatenate 'string (third token_content) " < "  returnexpression2)) ; recursive den gelen değer gönderilcek
															)
															( (string-equal (second token_list) "KW_EQUAL")
																(return-from expression (concatenate 'string (third token_content) " = "  returnexpression2)) ; recursive den gelen değer gönderilcek
															)
															( (string-equal (second token_list) "KW_AND")
																(return-from expression (concatenate 'string (third token_content) " && "  returnexpression2)) ; recursive den gelen değer gönderilcek
															)
															( (string-equal (second token_list) "KW_OR")
																(return-from expression (concatenate 'string (third token_content) " || "  returnexpression2)) ; recursive den gelen değer gönderilcek
															)
													)
													
													
												)
												((and (or (string-equal "IDENTIFIER" returnexpression2) (string-equal "VALUEF" returnexpression2)) (and (string/= "IDENTIFIER" returnexpression) (string/= "VALUEF" returnexpression)) (string-equal ")" (nth (second index_list) token_content)))
													(cond
															( (string-equal (second token_list) "KW_LESS")
																(return-from expression (concatenate 'string returnexpression " < "  (fourth token_content))) ; recursive den gelen değer gönderilcek
															)
															( (string-equal (second token_list) "KW_EQUAL")
																(return-from expression (concatenate 'string returnexpression " = "  (fourth token_content))) ; recursive den gelen değer gönderilcek
															)
															( (string-equal (second token_list) "KW_AND")
																(return-from expression (concatenate 'string returnexpression " && "  (fourth token_content))) ; recursive den gelen değer gönderilcek
															)
															( (string-equal (second token_list) "KW_OR")
																(return-from expression (concatenate 'string returnexpression " || "  (fourth token_content))) ; recursive den gelen değer gönderilcek
															)
													)
													
													
												)
												((and (and (string/= "IDENTIFIER" returnexpression) (string/= "VALUEF" returnexpression)) 
														(and (string/= "IDENTIFIER" returnexpression2) (string/= "VALUEF" returnexpression2)) (string-equal ")" (nth (second index_list) token_content)))
														(cond
															( (string-equal (second token_list) "KW_LESS")
																(return-from expression (concatenate 'string returnexpression " < "  returnexpression2)) ; recursive den gelen değer gönderilcek
															)
															( (string-equal (second token_list) "KW_EQUAL")
																(return-from expression (concatenate 'string returnexpression " = "  returnexpression2)) ; recursive den gelen değer gönderilcek
															)
															( (string-equal (second token_list) "KW_AND")
																(return-from expression (concatenate 'string returnexpression " && "  returnexpression2)) ; recursive den gelen değer gönderilcek
															)
															( (string-equal (second token_list) "KW_OR")
																(return-from expression (concatenate 'string returnexpression " || "  returnexpression2)) ; recursive den gelen değer gönderilcek
															)
													)	
													
												)
												(t
													(return-from expression nil)
												)
												
											)
										)
									)
									
								)
							)
						)
					)

				)
  				
	  		);cond end

			(t
				(return-from expression nil)
			)
	  	)
)

;This is rules for function definitions
(defun function-nonterminal(token_list token_content)
	#|(def IDENTIFIER $EXP) | 
	(def IDENTIFIER IDENTIFIER$EXP)
	(def IDENTIFIER IDENTIFIERIDENTIFIER$EXP)|#
	(let
		(
			;Local variables
			(index_list nil)
			(sub_token_list nil)
			(sub_token_content "")
			(returnForExp nil)
			(listArguments nil)
			(foundFunc (find (third token_content) func-list :key #'function_properties-name :test #'string=))
		)
		(cond
			(foundFunc ;Checks if the name of function is already exist
				(format t "Function name is already exist ~%")
				(return-from function-nonterminal nil)
			)
		)
		
		(cond
			(	(and;With one arguments
					(string-equal (first token_list) "OP_OP")
					(string-equal (second token_list) "KW_DEF")
	  				(string-equal (third token_list) "IDENTIFIER")
	  				(string-equal (fourth token_list) "OP_OP")
  				)
	
					(setq index_list (findIndexesofExp 3 token_content))
					(setq sub_token_list (subseq token_list (first index_list) (second index_list)))
					(setq sub_token_content (subseq token_content (first index_list) (second index_list)))
		
					;It adds function to the function list
					(setq func-list (cons (make-function_properties :name (third token_content) :numberOfArguments 0 :arguments '() :implementationWithTokens sub_token_list :implementationWithContents sub_token_content) func-list))
					;(setq asd (make-function_properties :name (third token_list) :numberOfArguments 0 :arguments '() :implementationWithTokens sub_token_list :implementationWithContents sub_token_content))
					; (format t "First function's name: ~a~%" (function_properties-name (first func-list)))
					; (format t "First function's number of argumets: ~d~%" (function_properties-numberOfArguments (first func-list)))
					; (format t "First function's arguments: ~a~%" (function_properties-arguments (first func-list)))

					; (format t "First function's implementation: ~a~%" (function_properties-implementationWithContents (first func-list)))
					(return-from function-nonterminal "Function")


			);end of case

			(	(and ;One arguments function
					(string-equal (first token_list) "OP_OP")
					(string-equal (second token_list) "KW_DEF")
	  				(string-equal (third token_list) "IDENTIFIER")
	  				(string-equal (fourth token_list) "IDENTIFIER")
	  				(string-equal (fifth token_list) "OP_OP")
  				)
					(setq index_list (findIndexesofExp 4 token_content))
					(setq sub_token_list (subseq token_list (first index_list) (second index_list)))
					(setq sub_token_content (subseq token_content (first index_list) (second index_list)))
				
			
					; (format t "ikinci kurala uyar~%")
					; (format t "#Function ~%")
					(setq listArguments (cons (fourth token_content) listArguments))
					;It adds function to the function list
					(setq func-list (cons (make-function_properties :name (third token_content) :numberOfArguments 1 :arguments listArguments :implementationWithTokens sub_token_list :implementationWithContents sub_token_content) func-list))
					;(setq asd (make-function_properties :name (third token_list) :numberOfArguments 0 :arguments '() :implementationWithTokens sub_token_list :implementationWithContents sub_token_content))
					; (format t "First function's name: ~a~%" (function_properties-name (first func-list)))
					; (format t "First function's number of argumets: ~d~%" (function_properties-numberOfArguments (first func-list)))
					; (format t "First function's arguments: ~a~%" (function_properties-arguments (first func-list)))

					; (format t "First function's implementation: ~a~%" (function_properties-implementationWithContents (first func-list)))
					(return-from function-nonterminal "Function")
	  				
			);end of case

			(	(and ;Two arguments function
					(string-equal (first token_list) "OP_OP")
					(string-equal (second token_list) "KW_DEF")
	  				(string-equal (third token_list) "IDENTIFIER")
	  				(string-equal (fourth token_list) "IDENTIFIER")
	  				(string-equal (fifth token_list) "IDENTIFIER")
	  				(string-equal (sixth token_list) "OP_OP")
  				)
					(setq index_list (findIndexesofExp 5 token_content))
					(setq sub_token_list (subseq token_list (first index_list) (second index_list)))
					(setq sub_token_content (subseq token_content (first index_list) (second index_list)))
				
			
					; (format t "üçüncü kurala uyar~%")
					; (format t "#Function ~%")
					(setq listArguments (cons (fourth token_content) listArguments))
					(setq listArguments (cons (fifth token_content) listArguments))
					(setq listArguments (reverse listArguments))
					; (format t "The type of my-variable is: ~a~%" sub_token_list)
					(setq func-list (cons (make-function_properties :name (third token_content) :numberOfArguments 2 :arguments listArguments :implementationWithTokens sub_token_list :implementationWithContents sub_token_content) func-list))
					;(setq asd (make-function_properties :name (third token_list) :numberOfArguments 0 :arguments '() :implementationWithTokens sub_token_list :implementationWithContents sub_token_content))
					; (format t "First function's name: ~a~%" (function_properties-name (first func-list)))
					; (format t "First function's number of argumets: ~d~%" (function_properties-numberOfArguments (first func-list)))
					; (format t "First function's arguments: ~a~%" (function_properties-arguments (first func-list)))

					; (format t "First function's implementation: ~a~%" (function_properties-implementationWithContents (first func-list)))
					; (format t "First function's implementation: ~a~%" (function_properties-implementationWithTokens (first func-list)))
					(return-from function-nonterminal "Function")
	  				
			);end of case
			(t
					;(format t "NOT A FUNCTİON DEFINITION: ~%")
					(return-from function-nonterminal nil)
			)

		)	
	)
)
;Exit rule to terminate program
(defun ex(token_list)
	(cond
		((and 
					(string-equal (first token_list) "OP_OP")
					(string-equal (second token_list) "KW_EXIT")
	  				(string-equal (third token_list) "OP_CP")
  			)
			(return-from ex "exit")
		)
		(
			(return-from ex nil)
		)
	)
)
;This is cfg, It sends tokens every rule until to find proper one. If there is not it says syntax error
(defun cfg()
    (let
		(
			(returnForCFG nil)
		)
		(setq returnForCFG (expression tokens tokens_content))
		(cond
			((null returnForCFG)
				(setq returnForCFG (function-nonterminal tokens tokens_content))
				(cond
					((null returnForCFG)
						(setq returnForCFG (ex tokens))
						(cond
							((null returnForCFG)
								(format t "SYNTAX ERROR! ~%")
							)
							(t
								(quit)
							)
						)
					)
					(t
						(format t "~A ~%" returnforcfg)
					)
				)
			)
			(t
						(format t "Res of expression: ~A ~%" returnforcfg)
			)
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
	  		;(format t "KW_AND~%")
            (setq tokens (cons "KW_AND" tokens))
            (setq tokens_content (cons "and" tokens_content))
	  	)
	  	((equal subStr KW_OR)
	  		;(format t "KW_OR~%")
            (setq tokens (cons "KW_OR" tokens))
            (setq tokens_content (cons "or" tokens_content))
	  	)
	  	((equal subStr KW_EQUAL)
	  		;(format t "KW_EQUAL~%")
            (setq tokens (cons "KW_EQUAL" tokens))
            (setq tokens_content (cons "equal" tokens_content))
        )
	  	((equal subStr KW_LESS)
	  		;(format t "KW_LESS~%")
            (setq tokens (cons "KW_LESS" tokens))
            (setq tokens_content (cons "less" tokens_content))
	  	)
	  	((equal subStr KW_NIL)
	  		;(format t "KW_NIL~%")
            (setq tokens (cons "KW_NIL" tokens))
            (setq tokens_content (cons "nil" tokens_content))
	  	)
	  	((equal subStr KW_LIST)
	  		;(format t "KW_LIST~%")
            (setq tokens (cons "KW_LIST" tokens))
            (setq tokens_content (cons "list" tokens_content))
	  	)
	  	((equal subStr KW_APPEND)
	  		;(format t "KW_APPEND~%")
            (setq tokens (cons "KW_APPEND" tokens))
            (setq tokens_content (cons "append" tokens_content))
	  	)
	  	((equal subStr KW_CONCAT)
	  		;(format t "KW_CONCAT~%")
            (setq tokens (cons "KW_CONCAT" tokens))
            (setq tokens_content (cons "concat" tokens_content))
	  	)
	  	((equal subStr KW_SET)
	  		;(format t "KW_SET~%")
            (setq tokens (cons "KW_SET" tokens))
            (setq tokens_content (cons "set" tokens_content))
	  	)
	  	((equal subStr KW_DEF)
	  		;(format t "KW_DEF~%")
            (setq tokens (cons "KW_DEF" tokens))
            (setq tokens_content (cons "def" tokens_content))
	  	)
	  	((equal subStr KW_FOR)
	  		;(format t "KW_FOR~%")
            (setq tokens (cons "KW_FOR" tokens))
            (setq tokens_content (cons "for" tokens_content))
	  	)
	  	((equal subStr KW_IF)
	  		;(format t "KW_IF~%")
            (setq tokens (cons "KW_IF" tokens))
            (setq tokens_content (cons "if" tokens_content))
	  	)
	  	((equal subStr KW_EXIT)
	  		;(format t "KW_EXIT~%")
            (setq tokens (cons "KW_EXIT" tokens))
            (setq tokens_content (cons "exit" tokens_content))
	  	)
	  	((equal subStr KW_LOAD)
	  		;(format t "KW_LOAD~%")
            (setq tokens (cons "KW_LOAD" tokens))
            (setq tokens_content (cons "load" tokens_content))
	  	)
	  	((equal subStr KW_DISPLAY)
	  		;(format t "KW_DISPLAY~%")
            (setq tokens (cons "KW_DISPLAY" tokens))
            (setq tokens_content (cons "display" tokens_content))
	  	)
	  	((equal subStr KW_TRUE)
	  		;(format t "KW_TRUE~%")
            (setq tokens (cons "KW_TRUE" tokens))
            (setq tokens_content (cons "true" tokens_content))
	  	)
	  	((equal subStr KW_FALSE)
	  		;(format t "KW_FALSE~%")
            (setq tokens (cons "KW_FALSE" tokens))
            (setq tokens_content (cons "false" tokens_content))
	  	)
	  	((equal subStr OP_PLUS)
	  		;(format t "OP_PLUS~%")
            (setq tokens (cons "OP_PLUS" tokens))
            (setq tokens_content (cons "+" tokens_content))
	  	)
	  	((equal subStr OP_MINUS)
	  		;(format t "OP_MINUS~%")
            (setq tokens (cons "OP_MINUS" tokens))
            (setq tokens_content (cons "-" tokens_content))
	  	)
	  	((equal subStr OP_MULT)
	  		;(format t "OP_MULT~%")
            (setq tokens (cons "OP_MULT" tokens))
            (setq tokens_content (cons "*" tokens_content))
	  	)
	  	((equal subStr OP_DIV)
	  		;(format t "OP_DIV~%")
            (setq tokens (cons "OP_DIV" tokens))
            (setq tokens_content (cons "/" tokens_content))
	  	)
	  	((equal subStr OP_OP)
	  		;(format t "OP_OP~%")
            (setq tokens (cons "OP_OP" tokens))
            (setq tokens_content (cons "(" tokens_content))
	  	)
	  	((equal subStr OP_CP)
	  		;(format t "OP_CP~%")
            (setq tokens (cons "OP_CP" tokens))
            (setq tokens_content (cons ")" tokens_content))
	  	)
	  	((equal subStr OP_COMMA)
	  		;(format t "OP_COMMA~%")
            (setq tokens (cons "OP_COMMA" tokens))
            (setq tokens_content (cons "," tokens_content))
	  	)
	  	((equal subStr OP_COMMENT)
	  		;(format t "OP_COMMENT~%")
            (setq tokens (cons "OP_COMMENT" tokens))
            (setq tokens_content (cons ";;" tokens_content))
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
 			;(format t "OP_PLUS~%")
            (setq tokens (cons "OP_PLUS" tokens))
            (setq tokens_content (cons "+" tokens_content))
 		)
 		((eq getChar #\-)
 			;(format t "OP_MINUS~%")
            (setq tokens (cons "OP_MINUS" tokens))
            (setq tokens_content (cons "-" tokens_content))
 		)
 		((eq getChar #\*)
 			;(format t "OP_MULT~%")
            (setq tokens (cons "OP_MULT" tokens))
            (setq tokens_content (cons "*" tokens_content))
 		)
 		((eq getChar #\/)
 			;(format t "OP_DIV~%")
            (setq tokens (cons "OP_DIV" tokens))
            (setq tokens_content (cons "/" tokens_content))
 		)
 		((eq getChar #\()
 			;(format t "OP_OP~%")
            (setq tokens (cons "OP_OP" tokens))
            (setq tokens_content (cons "(" tokens_content))
 		)
 		((eq getChar #\))
 			;(format t "OP_CP~%")
            (setq tokens (cons "OP_CP" tokens))
            (setq tokens_content (cons ")" tokens_content))
 		)
 		((eq getChar #\,)
 			;(format t "OP_COMMA~%")
            (setq tokens (cons "OP_COMMA" tokens))
            (setq tokens_content (cons "," tokens_content))
 		)
 		((eq getChar #\;)
 			;(format t "OP_COMMENT~%")
            (setq tokens (cons "OP_COMMENT" tokens))
            (setq tokens_content (cons ";;" tokens_content))
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
			;(format t "~A~%" input)
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
			
			(loop
				(setq user-input (read-line))
				(gppLexer user-input)
				(setq tokens (reverse tokens))
				(setq tokens_content (reverse tokens_content))
				(cfg)
				#|(dolist (item tokens)
					(format t "Token: ~A~%" item)
				)
				(dolist (item tokens_content)
					(format t "Token content: ~A~%" item)
				)|#
			)
		)
	)
)
;It reades file line by line an It sends each line to gppLexer
(defun read_file(filename)

	(with-open-file (file filename :direction :input)
    	(	
    		loop for line = (read-line file nil)
			while line
			do(progn
				(gppLexer line)
				(setq tokens (reverse tokens))
				(setq tokens_content (reverse tokens_content))
				(cfg)
			) 
			
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
	(format t "Enter 'g++ filename.g++' to execute on file.")
	(terpri)
	(gppinterpreter)
    
)
(start)
