;; Bill Molchan's CS 443 activity, Last updated 2/24/15
;; This program takes clojure code and converts it into equivalent C code

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Readme ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; All numbers are assumed to be integers
;; Supported clojure functions are: + - * < > <= >= def defn let do printf
;; The last line of a defn is assumed to be the return,
;; Using 'return' as a function will also work as expected

;;;;;;;;;;;;;;;;;; On Let Statements
;; There is support of arbitrarily many nested 'let' statements
;; let statements are limited in scope to their own variables and global variables
;; This means that nested 'let' statements can't refer to variables in parent 'let' statements
;; let statements may produce surperflous functions

;;;;;;;;;;;;;;;;;; Test Case
;; The following is a working test case to demonstrate how to use the program:
;; (myCompile '((def input 15) (def two 2) (defn square [x] (* x x)) (def twiceSquareOfThree (let [localTwo two] (* localTwo (let [arg 3] (square arg) ))))(defn compare_and_subtract [reference] (if (> reference twiceSquareOfThree) (do ((printf twiceSquareOfThree) (return (- twiceSquareOfThree reference)))) (printf reference)) (- reference twiceSquareOfThree))(def myResult (compare_and_subtract input ))))

;;;;;;;;;;;;;;;;;; Table of Contents
;; The code is divided into sections designated by the large comment blocks
;; The sections in order are:
;;
;;;; Readme
;;;; Declarations
;;;; Auxilary Functions
;;;; State Functions
;;;; Initiating Function
;;;; Sorting Functions
;;;; Compiler Components
;;;; Parser Functions
;;;; Printing Functions

;;;;;;;;;;;;;;;;;; Copying stuff
;; You have the author's permission to copy functions and ideas
;; If you do choose to use or copy these functions,
;; please cite Bill Molchan's clojure-c-compiler 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Declarations ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Not all functions are called in the order they are created
;; so I have grouped the declare  and namespace statements here
(ns clojure-c-compiler.core (require 
                 [clojure.edn :as edn]
                 [clojure.set :as set]
                 [clojure.string :as str])
)
(declare 
       compile-expression
       argMap
       defnBody
       argFormat
       symbolApplication
       printResult
       compile-def
       compile-defn
       compile-if
       compile-printf
       compile-let
       compile-call
       compile-do
       compile-return
       report
)

;; The following are strings that have been abstracted
;; Rather than rebind them with multiple lets they have been consolidated
(def strDeclareInt  "int ")
(def strEndLine     ";"   )
(def strOpenParen   "(")
(def strCloseParen  ")")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;; Auxilary Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;This section is for functions that aren't neccesary functional,
;;but are thought to make the code easier to read
(defn strCast      "Casts args as String " [x]    (if (seq? x) (apply str x) (str x)))
(defn mergeStrings "Casts and Concatonate" [& xx] (strCast (map strCast xx)))

(defn map-compile  "Maps compile-expression" [x]  (map compile-expression x))
(defn compile-head "Compile expression head" [x]  (compile-expression (first x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; State Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;These functions relate to state and volatile memory

(def lastDef 
  "This function is used to print the most recently 'def'-ed variable"
  (let [var (atom "no variables defined")]
    (fn 
      ([x] (reset! var x))
      ([] @var))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def aggLet
  "This function aggregates the 'let' clauses so that they can be printed at the end"
  (let [
        aggregate  (atom "")
        accumulate (fn [y] (partial mergeStrings y))
        ]
    (fn
      ( [x y] (reset! aggregate y))  ;This is to reset the memory between executions
      ( [x]   (swap!  aggregate (accumulate x)));
      ( []    @aggregate)))
)

(defn reset-aggLet! "This is to clean memory between executions" [] (aggLet "" ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; Initiating Function ;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;This is the function that is called by the user to convert into C code
;;The function evaluates each expression it is passed
;;Then the function creates a main class and prints the most recent variable
(defn myCompile [x]
  {:pre [(seq? x)]}
  (printResult (map-compile x))   
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; Sorting Functions ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;These functions collectively identify the head of the syntax tree
;;Then they call the corresponding compile statements


(defn what-kind 
 "This function Identifies the head of the tree and returns the name as a String"  
  [x] 
  (cond
    (integer? x) "Integer" 
    (symbol?  x) "Symbol" 
    (seq?     x) "Sequence" 
    true         "Other"
  ))



(defn compile-expression 
  "This function casts numbers as strings
   It also sends user-input-functions to the symbol application function"
   [x]
   (if (seq? x)
     (case (what-kind (first x))
       "Symbol"   (symbolApplication (first x) (rest x))
       "Integer"  (strCast x)
       "Sequence" (compile-head x)
       "Other"    (strCast x)
     )
     (strCast x)
   )
   
)


(defn symbolApplication
  "This function reads what kind of symbol the list starts with.
   It then sends the args to the corresponding compiling component"
  [x xs]
  (let [
        symbolIs? (fn [y] (= (name x) y))
        binop?    #{"+","*","/","-","<",">","==",">=","<="}
        ]
   (cond
     (binop?    (name x)) (strCast (interpose (mergeStrings " " x " ") (map-compile xs)))  
     (symbolIs? "def"   ) (compile-def    xs)
     (symbolIs? "defn"  ) (compile-defn   xs)
     (symbolIs? "if"    ) (compile-if     xs)
     (symbolIs? "printf") (compile-printf xs)
     (symbolIs? "let"   ) (compile-let    xs)
     (symbolIs? "do"    ) (compile-do     xs)
     (symbolIs? "return") (compile-return xs)
     true (compile-call x xs)
   )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;; Compiling Components ;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Each of these function groups compiles a specific type of symbol


(defn compile-def [x]
  "This funtion compiles a variable declaration"
  (let [
        strName        (lastDef (strCast (first x)))     
        strAssign      " = "
        strValue       (compile-expression (rest x))
       ]

  (mergeStrings 
     strDeclareInt
     strName
     strAssign
     strValue
     strEndLine)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;This collection of functions run the defn

(defn compile-defn [x]
  "This function compiles a function definition"
  (let [
        argMap         (fn [x] (mergeStrings " int " (strCast x)))
        argFormat      (fn [x] (mergeStrings (interpose "," (map argMap (into () (reverse x))))))
        strName        (strCast (first x))
        strArgList     (argFormat (second x))
        strOpenBody    "{"
        strDefnBody    (defnBody (rest (rest x)))
        strCloseBody   "}"    
        ]

    (mergeStrings 
      strDeclareInt
      strName 
      strOpenParen
      strArgList
      strCloseParen
      strOpenBody
      strDefnBody
      strCloseBody    
    ))
)


(defn defnBody [x] 
"This function scans the function definition for the last line to add a return"
  (let [
        remaining    (rest x)
        lastTerm?    (= remaining '())
        strNextLine  (compile-head x)
       ]
  (if lastTerm?
    (compile-return (first x)) 
    (mergeStrings
       strNextLine
       (defnBody remaining)
    )
  )
)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn compile-if [x] 
"This function compiles the 'If' statements"
  (let [
        ;This test determines if there is an 'else' clause
        hasElse?  (not= '() (rest (rest x)))

        ;These strings format the output,
        ;They are aggregated for abstraction purposes
        strBeginIf          "if ("
        strCloseConditional "){"
        strCloseFirstIf     "}"
        strBeginElse        "else{"
        strCloseElse        "}"
        strEmpty            ""

        ;These strings extract the 'if' components
        strConditional (compile-head x)
        strIfBody      (compile-expression (second x))
        strElse        (if  hasElse?
                         (let [strElseBody (compile-expression (rest (rest x)))]
                           (mergeStrings 
                             strBeginElse
                             strElseBody
                             strCloseElse
                           )
                          )
                         ;If there is no else statement, return empty string
                         strEmpty
                        )
                        
        ];This ends the let binding
  ;This is the main body of the compile-if
  (mergeStrings 
    strBeginIf
    strConditional
    strCloseConditional
    strIfBody
    strCloseFirstIf
    strElse
    )         
   )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Compile printf
(defn compile-printf [x] 
  (let [
        strBeginPrint  " printf( " 
        strPrintBody   (map-compile x)
        strClosePrint  ");" 
       ]

     (mergeStrings 
        strBeginPrint
        strPrintBody
        strClosePrint        
     )
  )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compile call
(defn compile-call [x xs] 
  (let [
        strFunctionName  (strCast x)
        strArgList       (mergeStrings (interpose "," (map-compile xs)))
        ]
    ;Begin compile-call body
    (mergeStrings 
      strFunctionName
      strOpenParen
      strArgList
      strCloseParen          
    )
  )
) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This is my attempt to create let-enabling functions
;; This first function takes a vector of the input arguments in the form [name1 value1 name2 value2] and converts it into a string of variable declarations
(defn generateVars [xx]
 (if (empty? xx)
   ""
;else
   (let [
         strVariableName  (strCast (first xx))
         strAssignment    " = " 
         strValue         (strCast (second xx))
         strRemainingVars (generateVars (rest (rest xx)) )
         ]
     (mergeStrings
       strDeclareInt
       strVariableName
       strAssignment
       strValue
       strEndLine
       strRemainingVars
     )
   )
 )
)

;; This function assumes something else has already stripped the word 'let' out of the list
;; The arguments are the [name1 value1 name2 value2] as the car, and the function definition as the cdr
;; The let statement prints the new let function at the top of the output buffer and returns a string calling the newly created function

(defn compile-let [xx] 
   (let [
         letName           (gensym "Let_")
         strLetName        (strCast letName)
         strEmptyArgs      "()"
         strOpenFunction   "{"
         strInternalVars   (generateVars (first xx))
         strLetBody        (defnBody (rest xx))   
         strCloseFunction  "}"
         strCallLet        (mergeStrings (strCast letName) strEmptyArgs)
         
         ]
      ; This will add this Let call to the aggregated let calls
      (aggLet
       (mergeStrings 
          strDeclareInt
          strLetName
          strEmptyArgs
          strOpenFunction
          strInternalVars
          strLetBody
          strCloseFunction               
       )
      )
      ; Then declare the 'let' function at the top of the page
      (report 
         (mergeStrings
            strDeclareInt
            strLetName
            strEmptyArgs
            strEndLine
         )
      )
      ; Since the let statement returns the last line,
      ; After printing a function to replace the let call
      ; This passes a string to call the new function
      strCallLet
   )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn compile-do [x] 
  (mergeStrings    (map-compile (first x)))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn compile-return [x]
  (let [
        strReturn   "return "
        strBody     (compile-expression  x)
        ]
    (mergeStrings
       strReturn
       strBody
       strEndLine
     )
   )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; Parser Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; These functions are used to run the parser



(defn parseThis [x] (let
                        [
                         incrementIndent (str/replace x
                                                      "{"
                                                      "NewLine  { IndentUp NewLine ")
                         decrementIndent (str/replace incrementIndent
                                                      "}"
                                                      "IndentDown NewLine  } NewLine")
                         addNewLines     (str/replace decrementIndent
                                                      ";"
                                                      ";NewLine")
                         removeDuplicate (str/replace addNewLines
                                                      "NewLineIndentDown NewLine"
                                                      "IndentDown NewLine")
                         cleanerResult   (str/replace removeDuplicate
                                                      "NewLine IndentDown NewLine"
                                                      "IndentDown NewLine")
                        ];end of let bindings

                      cleanerResult
                      )
)
; This begins the expression parser
(defn formatThis [expression]
  (let [
        splitOnIndentUp    (fn [x] (str/split x #"IndentUp"))
        splitOnIndentDown  (fn [x] (str/split x #"IndentDown"))
        fragmentedUps      (splitOnIndentUp expression)
        fragmentedDowns    (map splitOnIndentDown fragmentedUps)
        addIndent          (fn [x] (take x (repeat "\t")))
        appyIndentation    (fn [x y] (let [indents (addIndent y)
                                           newline (mergeStrings  "\n" indents )
                                          ]
                                       (str/replace x "NewLine" newline)))
        indentExpression   (fn [finished current remaining indentLevel]
                             (if (empty? current) 
                                   (if (empty? remaining) 
                                       finished
                                       (recur
                                          finished
                                          (first remaining)
                                          (rest remaining)
                                          ;+2 because the last call decremented
                                          (+ indentLevel 2)
                                       )
                                    )
                                   ;else:
                                   (recur
                                       ;new 'finished'
                                       (mergeStrings
                                          finished
                                          (appyIndentation 
                                               (first current)
                                               indentLevel
                                          )
                                       )
                                       ;new current
                                       (rest current)
                                       ;remaining is unchanged
                                       remaining
                                       ;decrement indentLevel
                                       (- indentLevel 1)
                                    )
                                )
                             )                                       
       ];end let bindings
    ;begin main body of formatThis
    (indentExpression "" '() fragmentedDowns -2)
    

  ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Printing Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;These functions handle printing and formatting operations
(defn finishingTouch [x] ((comp formatThis parseThis) x))
(defn report [& x] ((comp print formatThis parseThis mergeStrings) x))


(defn printResult [x] 
"This function circumvents some issues with laziness to print the final result
  pre-post ensure that all 'lets' have been evaluated and then clean memory"
 {:pre [(last x)]}
 (let [
       strProgramBody  (strCast x)
       strMainClass   "int main() {printf(\"%d \\n\","
       lastDefinedVar (lastDef)
       strCloseMain   ");}"
       ]

 (report
                        strProgramBody
                        strMainClass
                        lastDefinedVar
                        strCloseMain
                        (aggLet)))
                        ;This returns an empty string but clears memory
                        (reset-aggLet!))

