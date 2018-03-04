;Parkinson's Disease Symptom Recognizer

;Declaring fuzzy variables

(defglobal ?*age* = 
    (new nrc.fuzzy.FuzzyVariable "age" 0.0 100.0 "years"))
(defglobal ?*sleep* =
    (new nrc.fuzzy.FuzzyVariable "sleep" 0.0 11.0 "score"))
(defglobal ?*slowness* =
    (new nrc.fuzzy.FuzzyVariable "slowness" 0.0 11.0 "score"))
(defglobal ?*rating* =
    (new nrc.fuzzy.FuzzyVariable "rating" 0.0 11.0 "score"))


(defrule initial-parameters
    (declare (salience 10))
=>
(import nrc.fuzzy.*)
(load-package nrc.fuzzy.jess.FuzzyFunctions)

;Initial Parameters

(?*age* addTerm "high" (new ZFuzzySet 56.00 100.00))
(?*age* addTerm "medium" (new nrc.fuzzy.TriangleFuzzySet 34.00 44.00 55.00))
(?*age* addTerm "low" (new SFuzzySet 1.00 33.00))

(?*sleep* addTerm "low" (new ZFuzzySet 1.0 5.0))
(?*sleep* addTerm "medium" (new PIFuzzySet 6.0 4.0))
(?*sleep* addTerm "high" (new SFuzzySet 7.0 10.0))
    
    
(?*slowness* addTerm "low" (new ZFuzzySet 1.0 5.0))
(?*slowness* addTerm "medium" (new PIFuzzySet 6.0 4.0))
(?*slowness* addTerm "high" (new SFuzzySet 7.0 11.0))
    
    
(?*rating* addTerm "low" (new ZFuzzySet 1.0 5.0))
(?*rating* addTerm "medium" (new PIFuzzySet 6.0 4.0))
(?*rating* addTerm "high" (new SFuzzySet 7.0 11.0))
    
)


;Start of  patient-module


(defrule welcome-patient
(declare(salience 9))   
=>
(printout t "Hi Welcome to Parkinson's Disease Symptom Recognizer " crlf)
(printout t "Type the name of the patient and press Enter> ")
(bind ?name (read))
(printout t "Let us begin the Medical Evaluation for " ?name "." crlf)
(printout t "Please provide the required information and the PDSR will tell you whether the patient is exhibiting Symptoms of Parkinson's Disease." crlf))


;Initialization of questions asked to the patient

(defrule assert-answers "initialization"
=>
    (printout t "What is the patient's age ? ")
    (bind ?age-value (float (readline t)))
    (printout t "On a scale of 0-10, How much of a trouble is it for you to fall asleep : ")
    (bind ?sleep-value (float (readline t)))
    (printout t "On a scale of 0-10, Rate the Slowness of voluntary movements, especially in the initiation of such movements as walking or rolling over in bed if any : ")   
    (bind ?slowness-value (float (readline t)))
    (printout t "n a scale of 0-10, Rate the shuffling gait with poor arm swing and stooped posture problem if faced any: ")   
    (bind ?gait-value (float (readline t)))
    (printout t "On a scale of 0-10, Rate the difficulty you faced while rising from a sitting position or Unsteady balance if any: ")   
    (bind ?balance-value (float (readline t)))
    (printout t "On a scale of 0-10, Rate the constipation or urinary problems faced by you if any :")
    (bind ?urinary-value (float (readline t)))
    (printout t "On a scale of 0-10, Rate Abnormal tone or stiffness in the trunk and extremities issues if faced any: ")
    (bind ?stiffness-value (float (readline t)))
    (printout t "On a scale of 0-10, Rate if there has been decreased facial expression, monotonous speech, and decreased eye blinking: ")
    (bind ?facial-value (float (readline t)))


(assert(theAge
        (new nrc.fuzzy.FuzzyValue ?*age*
        (new nrc.fuzzy.TriangleFuzzySet
        (- ?age-value 2.0)
        ?age-value
        (+ ?age-value 2.0)))))
    (assert(theSleep
        (new nrc.fuzzy.FuzzyValue ?*sleep*
        (new nrc.fuzzy.TriangleFuzzySet
        (- ?sleep-value 0.5)
        ?sleep-value
        (+ ?sleep-value 0.5)))))
    (assert(theSlowness
        (new nrc.fuzzy.FuzzyValue ?*slowness*
        (new nrc.fuzzy.TriangleFuzzySet
        (- ?slowness-value 0.5)
        ?slowness-value
        (+ ?slowness-value 0.5)))))
    (assert(theGait ?gait-value))
    (assert(theBalance ?balance-value))
    (assert(theUrinary ?urinary-value))
    (assert(theStiffness  ?stiffness-value))
    (assert(theFacial ?facial-value)))


;Fuzzy Rules for calculation


(defrule patient-group1 ;"low age & low sleep & low Slowness=> rating very low or low"
    (theAge ?a &: (fuzzy-match ?a "low"))
    (theSleep ?e &: (fuzzy-match ?e "low"))
    (theSlowness ?r &: (fuzzy-match ?r "low"))
=>
    (assert(theRating (new nrc.fuzzy.FuzzyValue ?*rating* "very low or low"))))

(defrule patient-group2 ;"low age & high sleep & low Slowness => rating low or medium or high"
    (theAge ?a &: (fuzzy-match ?a "low"))
    (theSleep ?e &: (fuzzy-match ?e "high"))
    (theSlowness ?r &: (fuzzy-match ?r "low"))
=>
    (assert(theRating (new nrc.fuzzy.FuzzyValue ?*rating* "low or medium or high"))))

(defrule patient-group3 ;"low age & medium sleep & low Slowness => rating low or medium"
    (theAge ?a &: (fuzzy-match ?a "low"))
    (theSleep ?e &: (fuzzy-match ?e "medium"))
    (theSlowness ?r &: (fuzzy-match ?r "low"))
=>
    (assert(theRating (new nrc.fuzzy.FuzzyValue ?*rating* "low or medium"))))

(defrule patient-group4 ;"high age & high sleep & low Slowness=> rating high or medium or low"
    (theAge ?a &: (fuzzy-match ?a "high"))
    (theSleep ?e &: (fuzzy-match ?e "high"))
    (theSlowness ?r &: (fuzzy-match ?r "low"))
=>
    (assert(theRating (new nrc.fuzzy.FuzzyValue ?*rating* "high or medium or low"))))

(defrule patient-group5 ;"high age & low sleep & low Slowness => rating high or medium or low"
    (theAge ?a &: (fuzzy-match ?a "high"))
    (theSleep ?e &: (fuzzy-match ?e "low"))
    (theSlowness ?r &: (fuzzy-match ?r "low"))
=>
    (assert(theRating (new nrc.fuzzy.FuzzyValue ?*rating* "high or medium or low"))))

(defrule patient-group6 ;"high age & medium sleep & low Slowness=> rating low or medium or high"
    (theAge ?a &: (fuzzy-match ?a "high"))
    (theSleep ?e &: (fuzzy-match ?e "medium"))
    (theSlowness ?r &: (fuzzy-match ?r "low"))
=>
    (assert(theRating (new nrc.fuzzy.FuzzyValue ?*rating* "high or medium or low"))))

(defrule patient-group7 ;"medium age & high sleep & low Slowness=> rating low or medium or high"
    (theAge ?a &: (fuzzy-match ?a "medium"))
    (theSleep ?e &: (fuzzy-match ?e "high"))
    (theSlowness ?r &: (fuzzy-match ?r "low"))
=>
    (assert(theRating (new nrc.fuzzy.FuzzyValue ?*rating* "low or medium or high"))))

(defrule patient-group8 ;"medium age & medium sleep & low Slowness=> rating medium"
    (theAge ?a &: (fuzzy-match ?a "medium"))
    (theSleep ?e &: (fuzzy-match ?e "medium"))
    (theSlowness ?r &: (fuzzy-match ?r "low"))
=>
    (assert(theRating (new nrc.fuzzy.FuzzyValue ?*rating* "  medium "))))

(defrule patient-group9 ;"high age & high sleep & high Slowness=> rating very high"
    (theAge ?a &: (fuzzy-match ?a "high"))
    (theSleep ?e &: (fuzzy-match ?e "high"))
    (theSlowness ?r &: (fuzzy-match ?r "high"))
=>
    (assert(theRating (new nrc.fuzzy.FuzzyValue ?*rating* "very high"))))

(defrule patient-group10 ;"low age & low sleep & medium Slowness=> rating very low or low or medium"
    (theAge ?a &: (fuzzy-match ?a "low"))
    (theSleep ?e &: (fuzzy-match ?e "low"))
    (theSlowness ?r &: (fuzzy-match ?r "medium"))
=>
    (assert(theRating (new nrc.fuzzy.FuzzyValue ?*rating* "very low or low"))))

(defrule patient-group11 ;"low age & high sleep  & medium Slowness=> rating low or medium"
    (theAge ?a &: (fuzzy-match ?a "low"))
    (theSleep ?e &: (fuzzy-match ?e "high"))
    (theSlowness ?r &: (fuzzy-match ?r "medium"))
=>
    (assert(theRating (new nrc.fuzzy.FuzzyValue ?*rating* "low or medium"))))

(defrule patient-group12 ;"low age & medium sleep  & medium Slowness=> rating low or medium"
    (theAge ?a &: (fuzzy-match ?a "low"))
    (theSleep ?e &: (fuzzy-match ?e "medium"))
    (theSlowness ?r &: (fuzzy-match ?r "medium"))
=>
    (assert(theRating (new nrc.fuzzy.FuzzyValue ?*rating* "low or medium"))))

(defrule patient-group13 ;"high age & high sleep  & medium Slowness=> rating high or medium"
    (theAge ?a &: (fuzzy-match ?a "high"))
    (theSleep ?e &: (fuzzy-match ?e "high"))
    (theSlowness ?r &: (fuzzy-match ?r "medium"))
=>
    (assert(theRating (new nrc.fuzzy.FuzzyValue ?*rating* "high or medium"))))

(defrule patient-group14 ;"high age & low sleep  & medium Slowness=> rating high or medium"
    (theAge ?a &: (fuzzy-match ?a "high"))
    (theSleep ?e &: (fuzzy-match ?e "low"))
    (theSlowness ?r &: (fuzzy-match ?r "medium"))
=>
    (assert(theRating (new nrc.fuzzy.FuzzyValue ?*rating* "high or medium"))))

(defrule patient-group15 ;"medium age & high sleep  & medium Slowness=> rating medium or high"
    (theAge ?a &: (fuzzy-match ?a "medium"))
    (theSleep ?e &: (fuzzy-match ?e "high"))
    (theSlowness ?r &: (fuzzy-match ?r "medium"))
=>
    (assert(theRating (new nrc.fuzzy.FuzzyValue ?*rating* " medium"))))

(defrule patient-group16 ;"medium age & medium sleep  & medium Slowness=> rating medium"
    (theAge ?a &: (fuzzy-match ?a "medium"))
    (theSleep ?e &: (fuzzy-match ?e "medium"))
    (theSlowness ?r &: (fuzzy-match ?r "medium"))
=>
    (assert(theRating (new nrc.fuzzy.FuzzyValue ?*rating* " medium "))))

(defrule patient-group17 ;"medium age & low sleep  & medium Slowness=> rating medium or low"
    (theAge ?a &: (fuzzy-match ?a "medium"))
    (theSleep ?e &: (fuzzy-match ?e "low"))
    (theSlowness ?r &: (fuzzy-match ?r "medium"))
=>
    (assert(theRating (new nrc.fuzzy.FuzzyValue ?*rating* "medium or low"))))

(defrule patient-group18 ;"low age & high sleep & high Slowness=> rating low or medium or high"
    (theAge ?a &: (fuzzy-match ?a "low"))
    (theSleep ?e &: (fuzzy-match ?e "high"))
    (theSlowness ?r &: (fuzzy-match ?r "high"))
=>
    (assert(theRating (new nrc.fuzzy.FuzzyValue ?*rating* "low or medium"))))

(defrule patient-group19 ;"high age & high sleep & high Slowness=> rating very high or high"
    (theAge ?a &: (fuzzy-match ?a "high"))
    (theSleep ?e &: (fuzzy-match ?e "high"))
    (theSlowness ?r &: (fuzzy-match ?r "high"))
=>
    (assert(theRating (new nrc.fuzzy.FuzzyValue ?*rating* "high or very high"))))

(defrule patient-group20 ;"high age & medium sleep & high Slowness=> rating medium or high"
    (theAge ?a &: (fuzzy-match ?a "high"))
    (theSleep ?e &: (fuzzy-match ?e "medium"))
    (theSlowness ?r &: (fuzzy-match ?r "high"))
=>
    (assert(theRating (new nrc.fuzzy.FuzzyValue ?*rating* "high or medium"))))



;Defuzzification and display

(defrule defuzzification-and-display-result-rating
    (declare (salience -1))
    ?f <- (theRating ?z)
    (theFacial ?p)
    (theUrinary ?t)
    (theStiffness ?n)
    (theBalance ?v)
    (theGait ?i)
=>
	(bind ?result-rating (integer (+ (* 0.10 ?p) (* 0.05 ?t) (* 0.15 ?i)  (* .1 ?n) (* .1 ?v) (* .50 (?z momentDefuzzify)))))
    (if (eq ?result-rating 4) then
    (printout t "Results : Low chances that you might be exhitibiting just the early onset symptoms of Parkinson's Disease. " crlf)
    elif(eq ?result-rating 5) then
    (printout t "Results : Good chances that you are exhibiting symptoms of Parkinson's Disease. It is very essential that you get a thorough checkup done." crlf)
     elif(eq ?result-rating 6) then
    (printout t "Results : High chances that you are exhibiting symptoms of Parkinson's Disease. It is very essential that you get a thorough checkup done." crlf)
    elif(eq ?result-rating 7) then
    (printout t "Results : Very high chances that you are exhibiting symptoms of Parkinson's Disease. It is very essential that you get a thorough checkup done." crlf)
    elif(eq ?result-rating 8) then
    (printout t "Results : Very high chances that you are exhibiting symptoms of Parkinson's Disease. It is very essential that you get a thorough checkup done." crlf)
    elif(eq ?result-rating 9) then
    (printout t "Results : High chances that you are exhibiting symptoms of Parkinson's Disease. It is very essential that you get a thorough checkup done." crlf)
    elif(eq ?result-rating 10) then
    (printout t "Results : Very high chances that you are exhibiting symptoms of Parkinson's Disease. It is very essential that you get a thorough checkup done." crlf)
    elif(> ?result-rating 10) then
    (printout t "Results : Very high chances that you are exhibiting symptoms of Parkinson's Disease. It is very essential that you get a thorough checkup done." crlf)
    else
    (printout t "Results : You are not exhibiting any symptoms of Parkinson's Disease." crlf))  
    
    (halt))

(deffunction run-application ()
(reset)
(run))

;Run the above function in a loop to get back the prompt every time we have to enter the values for another candidate or re-run the program

(while TRUE
(run-application))

