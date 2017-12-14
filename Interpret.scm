;Mike Menart
(load "classParser.scm")

;***********************************utility functions ********************************************
(define (atom? x) (not (or (pair? x) (null? x))))

;sees if expr is collapsable to numeric value
(define valued?
  (lambda (expr s)
    (cond
      ((atom? expr) #t)
      ((eq? (car expr) 'funcall) #t)
      ((eq? (car expr) '+) 
       (and (valued? (cadr expr) s)
            (valued? (caddr expr) s) s))
      ((eq? (car expr) '-) (if (null? (cdr (cdr expr)))
                                (valued? (cadr expr) s)
                                (and (valued? (cadr expr) s)
                                     (valued? (caddr expr) s))))
      ((eq? (car expr) '*)
       (and (valued? (cadr expr) s)
            (valued? (caddr expr) s)))
      ((eq? (car expr) '/)
       (and (valued? (cadr expr) s)
            (valued? (caddr expr) s)))
      ((eq? (car expr) '%) 
       (and (valued? (cadr expr) s)
            (valued? (caddr expr) s)))
      ((eq? (car expr) 'dot)
       (and (valued? (cadr expr) s)
            (valued? (caddr expr) s)))
      ((eq? (car expr) 'new) #t)
      ((eq? (car expr) 'super) #t)
      (else #f) )))

;sees if expr is boolean expression/comparison that will return true/false value
(define logic?
  (lambda (expr s)
    (cond
      ((eq? (car expr) '==) #t)
      ((eq? (car expr) '!=) #t)
      ((eq? (car expr) '<) #t)
      ((eq? (car expr) '>) #t)
      ((eq? (car expr) '<=) #t)
      ((eq? (car expr) '>=) #t)
      ((eq? (car expr) '&&) #t)
      ((eq? (car expr) '||) #t)
      ((eq? (car expr) '!) #t)
      (else #f) )))

;converts #t and #f to true and false
(define returnator
  (lambda (val)
    (cond
      ((eq? val #t) 'true)
      ((eq? val #f) 'false)
      (val))))
      
;*************************************state parsing functions (denoted by 's_'****************************
;state functions specific to main
(define s_getmain
  (lambda (mainclass s)
    (cond
      ((null? s) (error "homework err check: No such class instance"))
      ((s_layerhasvar? mainclass (car s)) (s_layergetmain mainclass (car s)))
      (else (s_getmain mainclass (cdr s))) )))

(define s_layergetmain
  (lambda (mainclass layer)
    (cond
      ((null? layer) (error "method incorrect"))
      ((eq? mainclass (caar layer)) (s_getclassmain (caddar layer)))
      (else (s_layergetmain mainclass (cdr layer)))
      )))

;see if variable is declared in state (not this)
(define s_layerhasvar?
  (lambda (a s)
    (if (eq? s '!)
        #f
        (s_layerhasvarhelper a s) )))


(define s_layerhasvarhelper
  (lambda (a s)
    (cond
          ((null? s) #f)
          ((eq? (car (car s)) a) #t)
          (else (s_layerhasvar? a (cdr s)))) ))

(define s_hasvar?
  (lambda (a s)
    (cond
      ((null? s) #f)
      ((eq? '! (car s)) (s_hasvar? a (cdr s)))
      (else (or (s_layerhasvar? a (car s)) (s_hasvar? a (cdr s)))) )))

(define s_redeclare?
  (lambda (a s)
    (cond
      ((or (null? s) (eq? (car s) '!)) #f)
      (else (or (s_layerhasvar? a (car s)) (s_redeclare? a (cdr s)))) )))

;get variable or attribute values, applysuper is a function which handles inheritance/polymorphic situations
(define s_lookup
  (lambda (a s superproc ths)
    (if (null? ths)
        (s_lookuphelper a s (lambda () (error "homework error check: variable is not declared")))
        (s_lookuphelper a s
               (lambda () ;if a is not found as local var, find it as class attribute
                 (s_getclassattr
                  (car ths)
                  a
                 superproc
                 (cadr ths)) )) )))

(define s_lookuphelper
  (lambda (a s k)
    (cond
      ((null? s) (k))
      ((eq? (car s) '!) (s_lookuphelper a (cdr s) k)) 
      ((s_layerhasvar? a (car s)) (s_layerlookup a (car s)))
      (else (s_lookuphelper a (cdr s) k)) )))

(define s_layerlookup
  (lambda (a s)
    (cond
      ((null? s) (error "homework err check: variable is not declared"))
      ((eq? (caar s) a) (if (null? (cdr (car s)))
                                 (error "homework err check: variable used before initialized")
                                 (unbox (cadr (car s)))))
      (else (s_layerlookup a (cdr s))) )))

;funtions to use for applysuper value in s_lookup
(define applysuper 
  (lambda (fields super)
    (cond
      ((zero? super) fields)
      (else (applysuper (cdr fields) (- super 1))) )))
    
(define ignoresuper
  (lambda (fields super)
    fields ))

(define errsuper
  (lambda (fields super)
    (error "err: shouldn't have gone to attribute search")))
    
;get attribute of class given instance value
(define (s_getclassattr fields attr superproc super) (s_getclassattr2 (superproc fields super) attr))

(define s_getclassattr2
  (lambda (fields attr)
    (cond
      ((null? fields) (error "homework err check: variable is not declared"))
      ((s_layerhasvar? attr (car fields)) (s_layerlookup attr (car fields)))
      (else (s_getclassattr2 (cdr fields) attr)) )))

;get number of levels beyonds super attr was found
(define (s_getattrlevel fields attr superproc super) (s_getattrlevel2 (superproc fields super) attr))

(define s_getattrlevel2
  (lambda (fields attr)
    (cond
      ((null? fields) "homework err check: variable is not declared")
      ((s_layerhasvar? attr (car fields)) 0)
      (else (+ 1 (s_getattrlevel2 (cdr fields) attr))) )))

(define s_getclassdef
  (lambda (name s)
    (cond
      ((null? s) (error "homework error: class undefined"))
      ((s_layerhasvar? name (car s)) (s_layergetclassdef name (car s)))
      (else (s_getclassdef name (cdr s))) )))

(define s_layergetclassdef
  (lambda (name layer)
    (cond
      ((null? layer) (error "method incorrect"))
      ((eq? (caar layer) name) (cdar layer))
      (else (s_layergetclassdef name (cdr layer))) )))

(define s_defaultconstruct
  (lambda (class s)
    (if (null? (car class))
        (list(s_classconstruct (cadr class)))
        (cons (s_classconstruct (cadr class)) (s_defaultconstruct (s_getclassdef (cadar class) s) s)) )))

     
(define s_classconstruct
  (lambda (class)
    (cond
      ((null? class) '())
      ((eq? 'var (caar class)) (if (null? (cddar class))
                                  (cons (list (cadar class) (box null)) (s_classconstruct (cdr class)))
                                  (cons (list (cadar class) (box (caddar class))) (s_classconstruct (cdr class))) ))
      ((eq? 'function (caar class)) (cons (list (cadar class) (box (cddar class))) (s_classconstruct (cdr class))))
      ((eq? 'static-function (caar class)) (cons (list (cadar class) (box (caddar class))) (s_classconstruct (cdr class))))
      (else "error: unsupported class field")
            )))

;set an already existing variable
(define s_statesetvar
 (lambda (var val s)
    (cond
      ((null? s) (error "homework err check: variable is not declared"))
      ((eq? (car s) '!) (cons '! (s_setvar var val (cdr s))))
      ((s_layerhasvar? var (car s)) (cons (s_layersetvar var val (car s)) (cdr s)))
      (else (cons (car s) (s_statesetvar var val (cdr s)))) )))

(define s_setmember
 (lambda (var context val s return break continue throw ths)
   (s_setmember2 var (resdot context s return break continue throw ths) val) ))

(define s_setmember2
  (lambda (var context val)
    (s_statesetvar var val (applysuper (car context) (cadr context))) ))
       
(define s_layersetvar
  (lambda (var val s)
    (cond
      ((eq? (car (car s)) var) (begin (set-box! (cadar s) val) s))
      (else (cons (car s) (s_layersetvar var val (cdr s))) ))))

(define s_addvar
  (lambda (var s)
    (cons (cons (list var (box null)) (car s)) (cdr s)) ))

(define addclass
  (lambda (class s)
    (cons (cons (cdr class) (car s)) (cdr s)) ))

(define (s_addlayer s) (cons '() s))

(define (s_poplayer s) (cdr s))
       
;*********************************main control functions*********************************************
;calls run on tree with empty state
(define interpret
  (lambda (file mainclass)
    ;(display (parser file)) (newline) ;for debugging purposes
    (returnator
     (call/cc
      (lambda (return)
        (add_classes
         (parser file)
         '(())
         return
         (lambda (s) (error "Break not in loop"))
         (lambda (s) (error "Continue not in loop"))
         (lambda (s) (error "Throw not in try"))
         (string->symbol mainclass) ) ))) )) 

(define s_getclassmain
  (lambda (class)
    (cond
      ((null? class) (error "homework err: Class does not have main"))
      ((or (eq? 'static-function (caar class)) (eq? 'function (caar class)))
       (if (eq? (cadar class) 'main)
           (cdar class)
           (s_getclassmain (cdr class)) ))
      (else (s_getclassmain (cdr class))) )))

(define (runmain main) (list (cons 'function main)))

;adds outer classes to state, then calls run_with_layer with tree that contains command to run main
(define add_classes
  (lambda (tree s return break continue throw mainclass)
    (cond
      ((null? tree) (run (runmain (s_getmain mainclass s)) s return break continue throw null))
      (else (add_classes (cdr tree) (addclass (car tree) s) return break continue throw mainclass))
      )))
      
;runs main process or function
(define run
  (lambda (tree s return break continue throw ths)
    (cond
      (else (call/cc
             (lambda (return)
               (run_block tree s return break continue throw ths) ))) 
      )))

(define run_with_layer
  (lambda (tree s return break continue throw ths)
    (cond
      (else (call/cc
             (lambda (return)
               (run_block tree (s_addlayer s) return break continue throw ths) )))
      )))

;runs any other kind of block that wouldn't have a new return continuation
(define run_block_with_layer
  (lambda (tree s return break continue throw ths)
    (run_block tree (s_addlayer s) return break continue throw ths) ))

;steps through code lines in block
(define run_block
  (lambda (tree s return break continue throw ths)
    ;(display "s:" ) (display s) (newline)
    ;(display "t:" ) (display tree) (newline)
    (cond
      ((null? tree) '())
      ((eq? (caar tree) 'funcall) (run_block (cdr tree) (begin (e_funcall (car tree) s return break continue throw ths) s) return break continue throw ths)) ;used if function's return value is not used 
      (else (run_block (cdr tree) (evaluate (car tree) s return break continue throw ths) return break continue throw ths)) ;call run on next expr, using evaluation of current expr as state
      )))

(define evaluate
  (lambda (expr s return break continue throw ths)
    (cond
      ((null? expr) s)
      ;these return values/lists
      ((eq? expr 'this) (getthis ths))
      ((valued? expr s) (e_value expr s return break continue throw ths)) 
      ((logic? expr s) (e_logic expr s return break continue throw ths)) 
      ;these return states
      ((eq? (car expr) 'function) (e_func expr s return break continue throw ths))
      ((eq? (car expr) 'begin) (begin (run_block_with_layer (cdr expr) s return break continue throw ths) s))
      ((eq? (car expr) 'break) (break (cdr s)))
      ((eq? (car expr) 'continue) (continue (cdr s)))
      ((eq? (car expr) 'try) (begin (e_try expr s return break continue throw ths) s))
      ((eq? (car expr) 'throw) (throw (evaluate (cadr expr) s return break continue throw ths)))
      ((eq? (car expr) 'var) (e_var expr s return break continue throw ths))
      ((eq? (car expr) '=) (e_= expr s return break continue throw ths))
      ((eq? (car expr) 'return) (return (e_return expr s return break continue throw ths)))
      ((eq? (car expr) 'if) (e_if expr s return break continue throw ths))
      ((eq? (car expr) 'while) (e_while expr s return break continue throw ths))
      (else (error "Undefined Expression")) )))
            
;***************************expression evaluation functions (denoted by 'e_')************************************
;**class related evaluators**
(define e_dot ;returns actual value of dot expression
  (lambda (expr s superproc return break continue throw ths)
     (e_dotfinal
      (respredot (cadr expr) s return break continue throw ths)
      (caddr expr)
      superproc ) ))

;for when you need last context resolution, aka rightmost operand
(define e_dotfinal 
  (lambda (dot attr superproc)
    (s_getclassattr (car dot) attr superproc (cadr dot))))

;returns number of layers beyond superlevel attr was found
(define e_dotfinalsuper
  (lambda (dot attr superproc)
    (s_getattrlevel (car dot) attr superproc (cadr dot)) ))
    
;returns (context, superlevel) pair. Resolves everything up to the rightmost operand 
(define resdot 
  (lambda (expr s return break continue throw ths)
    (cond
      ((and (pair? expr) (not (eq? (car expr) 'new)))
       (resdot2
        (respredot (cadr expr) s return break continue throw ths)
        (caddr expr)
        s return break continue throw ths) )
      (else (respredot expr s return break continue throw ths)) ))) ;for non chained dots

;resdot2 needs superproc, but can always use applysuper because its never a function
(define resdot2
  (lambda (predot right s return break continue throw ths)
    (cond
      ((eq? right 'super) (list (car predot) (+ 1 (cadr predot))))                    ;right super
      (else (list (s_getclassattr (car predot) right applysuper (cadr predot)) '0)) ))) ;right is class obj, since function that returns class would come from funcall

;returns (objcontext, superlevel), superlevel indicates where in obj context right of dot should be looked for
;never neads superproc because its not getting attributes, only contexts
;at the end of the day, only handels leftmost dot operand and recursion
(define respredot
  (lambda (expr s return break continue throw ths)
    (cond
      ((pair? expr) (cond
                             ((eq? (car expr) 'new) (list (evaluate expr s return break continue throw ths) '0)) ;new case
                             ((eq? (car expr) 'dot) (resdot expr s return break continue throw ths)) ;dot case
                             ((eq? (car expr) 'funcall) (list (e_funcall expr s return break continue throw (setsuper ths 0)) '0))
                             (else (error "hw error check: undefined dot operand")) ))
      ((eq? expr 'super) (incsuper ths 1)) ;super is leading dot
      ((eq? expr 'this) (setsuper ths 0)) ;why not just ths? 
      (else (list (s_lookup expr s applysuper ths) '0 expr)) ))) ;class obj case
    ;expr of form (dot Class AttributeName)

(define e_new
  (lambda (expr s return break continue throw ths)
    (s_defaultconstruct (s_getclassdef (cadr expr) s) s) ))

;**Function evaluation and helpers**
(define evalfuncparams
  (lambda (s params return break throw continue ths)
    (if (null? params)
        '()
        (cons (evaluate (car params) s return break throw continue ths) (evalfuncparams s (cdr params) return break throw continue ths)))))

;adds function parameters to subroutine call state
(define addvars
  (lambda (s varnames varvals)
    (cond
      ((null? varnames) s)
      (else (addvars (s_statesetvar (car varnames) (car varvals) (s_addvar (car varnames) s)) (cdr varnames) (cdr varvals))) )))

;returns a state with correct scope for function call
(define popstatetofunc
  (lambda (s classname)
    (cond
      ((null? s) (error "Class undeclared"))
      ((eq? (car s) '!) (popstatetofunc (cdr s) classname))
      ((s_layerhasvar? classname (car s)) s)
      (else (popstatetofunc (cdr s) classname)) )))

(define (sepfuncstate s) (cons '! s)) ;! acts as function delimeter so globals can be reinitialized locally

(define argsmatch?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      (else (argsmatch? (cdr l1) (cdr l2))) )))

(define prepstate
  (lambda (s func vars caller return break throw continue oldths)
    (if (argsmatch? (car func) vars)
         (addvars
          (cons '() (sepfuncstate (popstatetofunc s caller)))
          (car func)
          (evalfuncparams s vars return break throw continue oldths) )
         (error "homework check err: mismatched argumnets") )))

;functions for executing and declaring functions
(define e_func
  (lambda (expr s return break continue throw ths)
    (if (eq? (cadr expr) 'main)
        (run_block_with_layer (cadddr expr) (sepfuncstate s) return break continue throw ths) ;runs main function
        (s_statesetvar (cadr expr) (cddr expr) (s_addvar (cadr expr) s)) )))   ;adds any other function to state

(define getcaller
  (lambda (dotexpr ths)
    (cond
      ((atom? dotexpr) (if (or (eq? dotexpr 'this) (eq? dotexpr 'super))
                           (caddr ths)
                           (if (s_hasvar? dotexpr (getthis ths))
                               (caddr ths)
                               dotexpr)))
      (else (getcaller (cadr dotexpr) ths)) )))

(define funcsuper
  (lambda (dotexpr)
    (cond
      ((eq? (cadr dotexpr) 'super) applysuper)
      ((atom? (cadr dotexpr)) ignoresuper)
      ((eq? (caadr dotexpr) 'new) ignoresuper)
      ((eq? (caadr dotexpr) 'funcall) ignoresuper)
      ((eq? (caddr (cadr dotexpr)) 'super) applysuper)
      (else ignoresuper) )))

(define (funcname expr) (caddr (cadr expr)))
(define (getsuper ths) (if (null? ths) 0 (cadr ths)))
(define (funcmakethis expr) (cons (car expr) (cons (list 'dot 'this (cadr expr)) (cddr expr))))
(define (incsuper ths inc) (list (car ths) (+ inc (cadr ths)) (caddr ths)))
(define (setsuper ths super) (if (null? ths) '() (list (car ths) super (caddr ths))))
(define (getthis ths) (if (null? ths) '() (car ths)))
(define (getfuncbody s funcname) (s_lookup funcname s errsuper '()))

;resolves any dot expression
(define e_funcall
  (lambda (expr s return break continue throw ths)
    (if (atom? (cadr expr))
        (if (s_hasvar? (cadr expr) s)
            (e_funcallfunc expr s return break continue throw ths)
            (e_funcall (funcmakethis expr) s return break continue throw ths) ) ;always using 'this' ensures uniformity for evaluation
        (e_funcallclass (respredot (cadadr expr) s return break continue throw ths) expr s return break continue throw ths) ))) 

;resolves regular function call
(define e_funcallfunc
  (lambda (expr s return break continue throw ths)
    (e_funcallfunchelper (getfuncbody s (cadr expr)) expr s return break continue throw ths) ))

(define e_funcallfunchelper
  (lambda (funcbody expr s return break continue throw ths)
    (run (cadr funcbody)
         (prepstate s funcbody (cddr expr) (cadr expr) return break throw continue ths)
         return break continue throw ths) ))

;resolves funcbody and potential new object
(define e_funcallclass
  (lambda (dot expr s return break continue throw ths)
    (e_funcallclass2
         (e_dotfinal dot (funcname expr) (funcsuper (cadr expr)))
         expr s return break continue throw
         ths
         (list (car dot) (+ (cadr dot) (e_dotfinalsuper dot (funcname expr) (funcsuper (cadr expr)))) (getcaller (cadr expr) ths)) )
         ))

;executes function with closure
(define e_funcallclass2
  (lambda (funcbody expr s return break continue throw oldths ths)
    (run (cadr funcbody)
         (prepstate s funcbody (cddr expr) (caddr ths) return break throw continue oldths)
         return break continue throw
         ths) ))

;**try block helpers and evaluation**
;car/cdr abstraction for e_try
(define (getfinally expr) (if (hasfinally? expr)
                              (car (cdr (cadddr expr)))
                              '() ))
(define (hasfinally? expr) (not (null? (cadddr expr))))
(define (getcatch expr) (cdr (caddr expr)))
(define (getcatchblock expr) (car (cdr (getcatch expr))))
(define (gettry expr) (cadr expr))
(define (getthrowname expr) (car (car (getcatch expr))))

;executes try/catch/finally block
(define e_try
  (lambda (expr s return break continue throw ths)
    (e_tryhelper (gettry expr)
                 (getcatchblock expr)
                 (getthrowname expr)
                 (getfinally expr)
                 s return break continue throw ths) ))

(define e_tryhelper
  (lambda (try catch throwname finally s return break continue throw ths)
    (run_catch catch throwname finally s
               (call/cc ;will append value to beginning of state if anything is thrown
                (lambda (throw)
                  (run_block_with_layer try s
                                        (addfinally finally s return break continue throw ths return)
                                        (addfinally finally s return break continue throw ths break)
                                        (addfinally finally s return break continue throw ths continue)
                                        throw
                                        ths) ))
               return break continue throw
               ths) ))

(define addfinally
  (lambda (finally s return break continue throw ths continuation)
    (lambda (contval)
      (begin (run_finally finally s return break continue throw ths)
             (continuation contval))) ))
      
(define run_catch
  (lambda (catch throwname finally s throw_val return break continue throw ths)
    (if (null? throw_val)
        (run_finally finally s return break continue throw ths)
        (run_finally finally
                     (begin (run_block
                             catch
                             (cons (list (list throwname (box throw_val))) s)
                             (addfinally finally s return break continue throw ths return)
                             (addfinally finally s return break continue throw ths break)
                             (addfinally finally s return break continue throw ths continue)
                             (addfinally finally s return break continue throw ths throw)
                             ths) s)
                     return break continue throw ths)
        )))

(define run_finally
  (lambda (finally s return break continue throw ths)
    (if (not (null? finally))
        (run_block_with_layer finally s return break continue throw ths)
        s) ))

;**Other evaluators**
;returns a state
(define e_var
  (lambda (expr s return break continue throw ths)
    (cond
      ((s_redeclare? (cadr expr) s) (error "homework err check: variable reinitialization"))
      ((null? (cdr (cdr expr))) (s_addvar (cadr expr) s)) ;just create var
      (else (s_statesetvar (cadr expr) (evaluate (caddr expr) s return break continue throw ths) (s_addvar (cadr expr) s))) ;create and set var
      )))

;updates value of var in state, returns value of assignment with updated state
(define e_=
  (lambda (expr s return break continue throw ths)
    (cond 
      ((s_hasvar? (cadr expr) s) (s_statesetvar (cadr expr) (evaluate (caddr expr) s return break continue throw ths) s))
      ((atom? (cadr expr)) (begin (s_setmember
                                   (cadr expr)                                               
                                   'this                                                     
                                   (evaluate (caddr expr) s return break continue throw ths) 
                                 s return break continue throw ths) s))
      (else (begin (s_setmember
                    (caddr (cadr expr))                                       ;var name
                    (cadadr expr)                                             ;dot expr
                    (evaluate (caddr expr) s return break continue throw ths) ;set value
                    s return break continue throw ths) s)) )))

;appends return value to end of state s as non list, posssible ambiguity with multiple return expressions
(define e_return
  (lambda (expr s return break continue throw ths)
    (evaluate (cadr expr) s return break continue throw ths) ))

;chooses b/w two expression depending on condition
(define e_if
  (lambda (expr s return break continue throw ths)
    (if (evaluate (cadr expr) s return break continue throw ths)
        (evaluate (caddr expr) s return break continue throw ths)
        (if (null? (cdr (cdr (cdr expr)))) s (evaluate (cadddr expr) s return break continue throw ths)) ))) ;check for existence of then condition before eval

;evaluates while loop by interpreting body and then calling itself with new state
(define e_while
  (lambda (expr s return break continue throw ths)
    (call/cc ;return point for break statement
     (lambda (break)
       (if (evaluate (cadr expr) s return break continue throw ths) 
           (e_while expr (call/cc ;return point for continue statement
                          (lambda (continue)
                            (evaluate (caddr expr) s return break continue throw ths)))
                    return break continue throw ths) 
           s ) ))))

;evauluates basic expressions that contain operators
(define e_value
  (lambda (expr s return break continue throw ths)
    (cond
      ((number? expr) expr)
      ((atom? expr) (cond
                      ((eq? expr 'true) #t) ;boolen cases
                      ((eq? expr 'false) #f)
                      (else (s_lookup expr s applysuper ths)) )) ;var case
      ((eq? (car expr) '+) (+ (e_value (cadr expr) s return break continue throw ths) (e_value (caddr expr) s return break continue throw ths)))
      ((eq? (car expr) '-) (if (null? (cdr (cdr expr)))
                               (- 0 (e_value (cadr expr) s return break continue throw ths)) ;unary minus case
                               (- (e_value (cadr expr) s return break continue throw ths) (e_value (caddr expr) s return break continue throw ths))))
      ((eq? (car expr) '*) (* (e_value (cadr expr) s return break continue throw ths) (e_value (caddr expr) s return break continue throw ths)))
      ((eq? (car expr) '/) (quotient (e_value (cadr expr) s return break continue throw ths) (e_value (caddr expr) s return break continue throw ths)))
      ((eq? (car expr) '%) (modulo (e_value (cadr expr) s  return break continue throw ths) (e_value (caddr expr) s return break continue throw ths)))
      ((eq? (car expr) 'funcall) (e_funcall expr s return break continue throw ths)) ;this is whats run if function's return value is used
      ((eq? (car expr) 'dot) (e_dot expr s applysuper return break continue throw ths))
      ((eq? (car expr) 'new) (e_new expr s return break continue throw ths))
      (else (error "unkown operator" (cadr expr))))))

;evaluates boolean expressions
(define e_logic
  (lambda (expr s return break continue throw ths)
    (cond
      ((eq? (car expr) '==) (eq? (evaluate (cadr expr) s return break continue throw ths) (evaluate (caddr expr) s return break continue throw ths)))
      ((eq? (car expr) '!=) (not (eq? (evaluate (cadr expr) s return break continue throw ths) (evaluate (caddr expr) s return break continue throw ths))))
      ((eq? (car expr) '<) (< (evaluate (cadr expr) s return break continue throw ths) (evaluate (caddr expr) s return break continue throw ths)))
      ((eq? (car expr) '>) (> (evaluate (cadr expr) s return break continue throw ths) (evaluate (caddr expr) s return break continue throw ths)))
      ((eq? (car expr) '<=) (<= (evaluate (cadr expr) s return break continue throw ths) (evaluate (caddr expr) s return break continue throw ths)))
      ((eq? (car expr) '>=) (>= (evaluate (cadr expr) s return break continue throw ths) (evaluate (caddr expr) s return break continue throw ths)))
      ((eq? (car expr) '&&) (and (evaluate (cadr expr) s return break continue throw ths) (evaluate (caddr expr) s return break continue throw ths)))
      ((eq? (car expr) '||) (or (evaluate (cadr expr) s return break continue throw ths) (evaluate (caddr expr) s return break continue throw ths)))
      ((eq? (car expr) '!) (not (evaluate (cadr expr) s return break continue throw ths)))
      (else #f) )))

;*******run**********
;(interpret "test.txt" 'C)