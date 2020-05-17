
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; DEFINITIONS
(setq keywords (list "and" "or" "not" "equal" "less"  "nil"  "list" "append" "concat" "set" "deffun" "for" "if" "exit" "load" "disp" "true" "false"))
(setq operators (list "+" "-" "/" "(" ")" "*" "**"  "," ))
(setq numbers	"0123456789")
(setq identifiers	"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXVZ0123456789")
(setq comments (list ";;"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; it takes a file or an input from console and do lexical analysis
(defun gppinterpreter (filename)
	(get_tokens (read_file filename))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; it reads all lines and parse them
(defun read_file (filename)
	(setq in (open filename :if-does-not-exist nil))
	(cond
		((null in) (write "No such file") nil)
		(T (setq all (read-lines in))
			(close in)
			(read_file_helper (sub_parser (clean all))))
	)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Reads all the lines from input filed
(defun read-lines (fd)
	(setq line (read-line in nil))
	(if (null line)
		line
		(if (equal (length line) 0)
			(read-lines fd)
			(cons line (read-lines fd))
		)
	)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Finds tab and space chars, then ignores it to take real needed chars
(defun delete-whites (line)
	(cond
		((null line) line)
		(T
			(setq startp 0)
			(setq pos_space (position #\SPACE line :test #'equal))
			(setq pos_tab (position #\TAB line :test #'equal))
			(cond
				((and (null pos_space) (null pos_tab)) (list line))
				((null pos_space) (cons (subseq line startp pos_tab) (delete-whites (subseq line (+ 1 pos_tab)))))
				((null pos_tab) (cons (subseq line startp pos_space) (delete-whites (subseq line (+ 1 pos_space)))))
				((> pos_space pos_tab) (cons (subseq line startp pos_tab) (delete-whites (subseq line (+ 1 pos_tab)))))
				(T (cons (subseq line startp pos_space) (delete-whites (subseq line (+ 1 pos_space)))))
			)
		)
	)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; it gets tokens with considering the given language rules.
; if there is a undefined token, then print a SYNTAX_ERROR
(defun get_tokens (input)
  ;(print input)
	(if (null input)
		input
		(cond
			((get_tokens_helper (car input) operators) (concatenate 'string (concatenate 'string "op_" (car input) '(#\Newline)) (get_tokens (cdr input))))
			((get_tokens_helper (car input) keywords) (concatenate 'string (concatenate 'string "kw_" (car input) '(#\Newline)) (get_tokens (cdr input))))
      ((get_tokens_helper (car input) comments) (concatenate 'string (concatenate 'string "comment = " (car input) '(#\Newline)) (get_tokens (cdr input))))
			((regex_int (car input)) (concatenate 'string (concatenate 'string "value = " (car input) '(#\Newline)) (get_tokens (cdr input))))
			((parse_identifier (car input)) (concatenate 'string (concatenate 'string "identifier  = " (car input) '(#\Newline)) (get_tokens (cdr input))))
			(T
        (write-line "SYNTAX_ERROR: The token is not defined in our language: ")
        (write-line (car input) )
      )
		)
	)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun clean (list)
	(cond
		((null list) nil)
		((or (null (car list)) (equal 0 (length (car list)))) (clean (cdr list)))
		(T (cons (car list) (clean (cdr list))))
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;	according to the language rule, splitting process
(defun split_process (input)
	(cond
		((null input) input)
		(T
			(setq 	pos_operator
					(regex_int_helper (list (position #\+ input :test #'equal)
							(if (null (setf temp (position #\- input :test #'equal)))
								nil
								(if (< temp (- (length input) 1))
									(if (null (position (char input (+ 1 temp)) numbers :test #'equal))
										temp
										nil)
									temp))
							(position #\/ input :test #'equal)
							(position #\( input :test #'equal)
							(position #\) input :test #'equal)
							(position #\* input :test #'equal))
						(length input)))
			(if (equal pos_operator (length input))
				(list input)
				(if (equal 0 pos_operator)
					(append
						(list (subseq input 0 (+ 1 pos_operator)))
						(split_process (subseq input (+ pos_operator 1))))
					(if (equal pos_operator (- (length input) 1))
						(list
							(subseq input 0 pos_operator)
							(subseq input pos_operator))
						(append
						(list
							(subseq input 0 pos_operator)
							(subseq input pos_operator (+ pos_operator 1)))
							(split_process (subseq input (+ pos_operator 1))))
					)
				)
			)
		)
	)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun regex_int_helper (listofIntegers cur_minimum)
	(if (null listofIntegers)
		cur_minimum
		(if (null (car listofIntegers))
			(regex_int_helper (cdr listofIntegers) cur_minimum)
		 	(if (> (car listofIntegers) cur_minimum)
				(regex_int_helper (cdr listofIntegers) cur_minimum)
				(regex_int_helper (cdr listofIntegers) (car listofIntegers))
			)
		)
	)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;	helper for  splitting ops
(defun sub_parser (input)
	(if (null input)
		input
		(append (helper_sub_parser (clean (delete-whites (car input)))) (sub_parser (cdr input)))
	)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;	helper for sub_parser
(defun helper_sub_parser (input)
	(if (null input)
		input
		(append (clean (split_process (car input))) (helper_sub_parser (cdr input)))
	)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun read_file_helper (parsed_file_content)
	(if (null parsed_file_content)
		parsed_file_content
		(if (equal #\* (char (car parsed_file_content) 0))
			(if (null (cdr parsed_file_content))
				parsed_file_content
				(if (equal #\* (char (car (cdr parsed_file_content)) 0))
					(append (list (concatenate 'string (car parsed_file_content) (car (cdr parsed_file_content))))
						(read_file_helper (cdr (cdr parsed_file_content))))
					(append (list (car parsed_file_content)) (read_file_helper (cdr parsed_file_content)))
				)
			)
			(append (list (car parsed_file_content)) (read_file_helper (cdr parsed_file_content)))
		)
	)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get_tokens_helper (input keyword_list)
	(if (null keyword_list)
		nil
		(if (string= input (car keyword_list))
			T
			(get_tokens_helper input (cdr keyword_list))
		)
	)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;	it checks integer is correct rule based
(defun regex_int (input)
	(if (> (length input) 1)
		(if (equal
				(position (char numbers 0) input)
				(regex_int_helper (list
					(position (char numbers 1) input)
					(position (char numbers 2) input)
					(position (char numbers 3) input)
					(position (char numbers 4) input)
					(position (char numbers 5) input)
					(position (char numbers 6) input)
					(position (char numbers 7) input)
					(position (char numbers 8) input)
					(position (char numbers 9) input))
					(length input)))
			nil
			(if (equal #\- (char input 0))
				(parse_int (subseq input 1))
				(parse_int input)
			)

		)
		(parse_int input)
	)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun parse_int (input)
	(if (or (null input) (equal (length input) 0))
		T
		(if (find (char input 0) numbers :test #'equal)
			(parse_int (subseq  input 1))
			nil
		)
	)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun parse_identifier (input)
	(if (or (null input) (equal (length input) 0))
		T
		(if (find (char input 0) identifiers :test #'equal)
			(parse_identifier (subseq  input 1))
			nil
		)
	)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
