;==============================================================================
;==============================================================================

 ;; read-file produces a list whose elements are the expressions in the file.
;;provided code
(define (read-file)
   (let ((expr (read)))
     (if (eof-object? expr)
         '()
         (cons expr (read-file)))))
  ;; Here we go:  read in the file that defines the graph
(define data (with-input-from-file "dist.dat" read-file))

;1-dimensional table to keep track of distance from individual node to end
;from chapter 3.2/lecture 8.
	(define (make-table)  
		(let ((oned-table (list ’*table*))) 
		(define (lookup key) 
			(let ((record (assoc key (cdr oned-table))))
				 (if record (cdr record) #f)))
		(define (assoc key records)
			(cond ((null? records) #f)
				((equal? key (caar records)) (car records))
				(else (assoc key (cdr records)))))
		(define (insert! key value) 
		 (let ((record (assoc key (cdr oned-table)))) 
			(if record
				(set-cdr! record value)
				 (set-cdr! table  (cons (cons key value) 
				 (cdr table))))) 'ok)		
		(define (dispatch m)      
			(cond ((equal? m ’lookup-proc) lookup)            
				((equal? m ’insert-proc!) insert!)            
				(else (error "Unknown operation -- TABLE" m))))   
	dispatch))
;2-dimensional look up table construction lecture 8/cahpter 3.2
; tabel to find cost between two nodes
	(define (make-two-dimensional-table)  
		(let ((local-table (list ’*table*)))    
			(define (lookup key-1 key-2)      
				(let ((subtable (assoc key-1 (cdr local-table))))        
					(if subtable            
						(let ((record (assoc key-2 (cdr subtable))))              
							(if record                  
								(cdr record)                  
								#f))            
						#f)))    
			(define (insert! key-1 key-2 value)      
				(let ((subtable (assoc key-1 (cdr local-table))))        
					(if subtable            
						(let ((record (assoc key-2 (cdr subtable))))              
							(if record                  
								(set-cdr! record value)                  
								(set-cdr! subtable                            
									(cons (cons key-2 value)
										(cdr subtable)))))            
							(set-cdr! local-table                      
								(cons (list key-1                                  
									(cons key-2 value))                            
								(cdr local-table)))))      
			'ok)        
			(define (dispatch m)      
				(cond ((equal? m 'lookup-proc) lookup)            
					((equal? m 'insert-proc!) insert!)            
					(else (error "Unknown operation -- TABLE" m))))    
	dispatch))
							

;helper function for table filler
(define fulfill-table)

; procedure naive-cost(node) // returns an integer
  ; if the node has no children, return "infinity".
  ; for each child of node
    ; if child is "end"
      ; just compute (lookup node child)
    ; else
      ; compute (lookup node child) + naive-cost(child)
  ; return the minimum of those computed values


 (define naive-cost(node)
	(lambda (children))
	(lambda (infinity))
	(if (= children 0)
	infinity
	)
	((for-each child node)
		(if (equal? child 'end)
			(lookup node child)
			(+ (lookup (node child)) (naive-cost (child))))
	
	)
	
	)
 (naive-cost 'start)