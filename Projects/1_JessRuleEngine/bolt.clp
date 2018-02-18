;Parkinson's Disease Symptom Recognizer

(deftemplate patient
(slot age (default 0))
(slot sleep-score (default 0))
(slot slowness-score (default 0))
(slot facial-score (default 0))
(slot tremor-score (default 0))
(slot balance-score (default 0))
(slot urinary-score (default 0))
(slot stiffness-score (default 0))
(slot gait-score (default 0)))
(deftemplate rating (slot score))
(deftemplate result (slot rating) (slot verdict))
(deftemplate question (slot text) (slot type) (slot ident))
(deftemplate answer (slot ident) (slot text))

(deffunction ask-patient (?question)
"Ask a question, and return the answer"
(printout t ?question " ")
(return (read)))

;Module

(defmodule ask)
(defrule ask::ask-question-by-id
"Ask a question and assert the answer"
(declare (auto-focus TRUE))
(MAIN::question (ident ?id) (text ?text) (type ?type))
(not (MAIN::answer (ident ?id)))
?ask <- (MAIN::ask ?id)
=>
(bind ?answer (ask-patient ?text ?type))
(assert (MAIN::answer (ident ?id) (text ?answer)))
(retract ?ask)
(return))

;Start of PDSR module 

(defmodule start)
(defrule welcome-patient
=>
(printout t "Hi Welcome to Parkinson's Disease Symptom Recognizer " crlf)
(printout t "Type the name of the patient and press Enter> ")
(bind ?name (read))
(printout t "Let us begin the Medical Evaluation for " ?name "." crlf)
(printout t "Please provide the required information and the PDSR will tell you whether the patient is exhibiting Symptoms of Parkinson's Disease." crlf))

(deffacts questions
"The questions that are asked to the user by the system."

(question (ident age) (type number)
(text "What is the patient's age ?"))

(question (ident sleep-score) (type number)
(text "On a scale of 0-10, How much of a trouble is it for you to fall asleep : "))

(question (ident slowness-score) (type number)
(text "On a scale of 0-10, Rate the Slowness of voluntary movements, especially in the initiation of such movements as walking or rolling over in bed if any :"))

(question (ident facial-score) (type number)
(text "On a scale of 0-10, Rate if there has been decreased facial expression, monotonous speech, and decreased eye blinking: "))

(question (ident tremor-score) (type number)
(text "On a scale of 0-10, Rate the severity of shaking or tremor in your finger, thumb, hand or chin: "))

(question (ident balance-score) (type number)
(text "On a scale of 0-10, Rate the difficulty you faced while rising from a sitting position or Unsteady balance if any: "))

(question (ident urinary-score) (type number)
(text "On a scale of 0-10, Rate the constipation or urinary problems faced by you if any :"))

(question (ident stiffness-score) (type number)
(text "On a scale of 0-10, Rate Abnormal tone or stiffness in the trunk and extremities issues if faced any:"))

(question (ident gait-score) (type number)
(text "On a scale of 0-10, Rate the shuffling gait with poor arm swing and stooped posture problem if faced any:")))


(defmodule request-patient-details)
(defrule request-age
=>
(assert (ask age)))
(defrule request-sleep-score
=>
(assert (ask sleep-score)))
(defrule request-slowness-score
=>
(assert (ask slowness-score)))
(defrule request-facial-score
=>
(assert (ask facial-score)))
(defrule request-tremor-score
=>
(assert (ask tremor-score)))
(defrule request-balance-score
=>
(assert (ask balance-score)))
(defrule request-urinary-score
=>
(assert (ask urinary-score)))
(defrule request-stiffness-score
=>
(assert (ask stiffness-score)))
(defrule request-gait-score
=>
(assert (ask gait-score)))

(defrule assert-patient-fact
(answer (ident age) (text ?a))
(answer (ident sleep-score) (text ?s))
(answer (ident slowness-score) (text ?l))
(answer (ident facial-score) (text ?f))
(answer (ident tremor-score) (text ?t))
(answer (ident balance-score) (text ?b))
(answer (ident urinary-score) (text ?u))
(answer (ident stiffness-score)(text ?i))
(answer (ident gait-score)(text ?g))


=>
(assert (patient (age ?a) (sleep-score ?s) (slowness-score ?l) (facial-score ?f) (tremor-score ?t)(balance-score ?b)(urinary-score ?u)(stiffness-score ?i)(gait-score ?g))))


(defmodule symptom-recognition)
(defrule patient-group1

(patient (age ?a&:(>= ?a 150))
(sleep-score ?s) 
(slowness-score ?l) 
(facial-score ?f) 
(tremor-score ?t)
(balance-score ?b)
(urinary-score ?u)
(stiffness-score ?i)
(gait-score ?g))
=>
(bind ?symptom-score (integer (+ (* .15 ?s) (* .15 ?l) (* .15 ?f) (* .15 ?t)(*  .15 ?b)(*  .15 ?u)(* .1 ?g) )))
(if(eq ?symptom-score 10) then
(assert (result
(rating ?symptom-score)
(verdict "Very high chances that you are exhibiting symptoms of Parkinson's Disease. It is very essential that you get a thorough checkup done.")))
elif(eq ?symptom-score 9) then
(assert (result
(rating ?symptom-score)
(verdict "High chances that you are exhibiting symptoms of Parkinson's Disease. It is very essential that you get a thorough checkup done.")))
 elif(eq ?symptom-score 8) then
(assert (result
(rating ?symptom-score)
(verdict "High chances that you are exhibiting symptoms of Parkinson's Disease. It is very essential that you get a thorough checkup done.")))
elif(eq ?symptom-score 7) then
(assert (result
(rating ?symptom-score)
(verdict "Good chances that you might be exhitibiting just the early onset symptoms of Parkinson's Disease. A checkup is advised.")))       
 elif(eq ?symptom-score 6) then
(assert (result
(rating ?symptom-score)
(verdict "Good chances that you might be exhitibiting just the early onset symptoms of Parkinson's Disease. A checkup is advised.")))
elif(eq ?symptom-score 0) then
(assert (result
(rating ?symptom-score)
(verdict "You are not exhibiting any symptoms of Parkinson's Disease.")))
 else
(assert (result
(rating ?symptom-score)
(verdict "Very low chances that you are exhibiting symptoms of Parkinson's Disease.")))))

(defrule patient-group2

(patient (age  ?a&:(>= ?a 85)&:(< ?a 105))
(sleep-score ?s) 
(slowness-score ?l) 
(facial-score ?f) 
(tremor-score ?t)
(balance-score ?b)
(urinary-score ?u)
(stiffness-score ?i)
(gait-score ?g))
=>
(bind ?symptom-score (integer (+ (* .15 ?s) (* .15 ?l) (* .15 ?f) (* .15 ?t)(*  .15 ?b)(*  .15 ?u)(* .1 ?g) )))
(if(eq ?symptom-score 10) then
(assert (result
(rating ?symptom-score)
(verdict "Very high chances that you are exhibiting symptoms of Parkinson's Disease. It is very essential that you get a thorough checkup done.")))
elif(eq ?symptom-score 9) then
(assert (result
(rating ?symptom-score)
(verdict "High chances that you are exhibiting symptoms of Parkinson's Disease. It is very essential that you get a thorough checkup done.")))
 elif(eq ?symptom-score 8) then
(assert (result
(rating ?symptom-score)
(verdict "High chances that you are exhibiting symptoms of Parkinson's Disease. It is very essential that you get a thorough checkup done.")))
elif(eq ?symptom-score 7) then
(assert (result
(rating ?symptom-score)
(verdict "Good chances that you might be exhitibiting just the early onset symptoms of Parkinson's Disease. A checkup is advised.")))       
 elif(eq ?symptom-score 6) then
(assert (result
(rating ?symptom-score)
(verdict "Good chances that you might be exhitibiting just the early onset symptoms of Parkinson's Disease. A checkup is advised.")))
elif(eq ?symptom-score 0) then
(assert (result
(rating ?symptom-score)
(verdict "You are not exhibiting any symptoms of Parkinson's Disease.")))
 else
(assert (result
(rating ?symptom-score)
(verdict "Very low chances that you are exhibiting symptoms of Parkinson's Disease.")))))

(defrule patient-group3

(patient (age ?a&:(>= ?a 75)&:(< ?a 85))
(sleep-score ?s) 
(slowness-score ?l) 
(facial-score ?f) 
(tremor-score ?t)
(balance-score ?b)
(urinary-score ?u)
(stiffness-score ?i)
(gait-score ?g))
=>
(bind ?symptom-score (integer (+ (* .15 ?s) (* .15 ?l) (* .15 ?f) (* .15 ?t)(*  .15 ?b)(*  .15 ?u)(* .1 ?g) )))
(if(eq ?symptom-score 10) then
(assert (result
(rating ?symptom-score)
(verdict "Very high chances that you are exhibiting symptoms of Parkinson's Disease. It is very essential that you get a thorough checkup done.")))
elif(eq ?symptom-score 9) then
(assert (result
(rating ?symptom-score)
(verdict "High chances that you are exhibiting symptoms of Parkinson's Disease. It is very essential that you get a thorough checkup done.")))
 elif(eq ?symptom-score 8) then
(assert (result
(rating ?symptom-score)
(verdict "High chances that you are exhibiting symptoms of Parkinson's Disease. It is very essential that you get a thorough checkup done.")))
elif(eq ?symptom-score 7) then
(assert (result
(rating ?symptom-score)
(verdict "Good chances that you might be exhitibiting just the early onset symptoms of Parkinson's Disease. A checkup is advised.")))       
 elif(eq ?symptom-score 6) then
(assert (result
(rating ?symptom-score)
(verdict "Good chances that you might be exhitibiting just the early onset symptoms of Parkinson's Disease. A checkup is advised.")))
elif(eq ?symptom-score 0) then
(assert (result
(rating ?symptom-score)
(verdict "You are not exhibiting any symptoms of Parkinson's Disease.")))
 else
(assert (result
(rating ?symptom-score)
(verdict "Very low chances that you are exhibiting symptoms of Parkinson's Disease.")))))

(defrule patient-group4

(patient (age ?a&:(>= ?a 65)&:(< ?a 75))
(sleep-score ?s) 
(slowness-score ?l) 
(facial-score ?f) 
(tremor-score ?t)
(balance-score ?b)
(urinary-score ?u)
(stiffness-score ?i)
(gait-score ?g))
=>
(bind ?symptom-score (integer (+ (* .15 ?s) (* .15 ?l) (* .15 ?f) (* .15 ?t)(*  .15 ?b)(*  .15 ?u)(* .1 ?g) )))
(if(eq ?symptom-score 10) then
(assert (result
(rating ?symptom-score)
(verdict "Very high chances that you are exhibiting symptoms of Parkinson's Disease. It is very essential that you get a thorough checkup done.")))
elif(eq ?symptom-score 9) then
(assert (result
(rating ?symptom-score)
(verdict "High chances that you are exhibiting symptoms of Parkinson's Disease. It is very essential that you get a thorough checkup done.")))
 elif(eq ?symptom-score 8) then
(assert (result
(rating ?symptom-score)
(verdict "High chances that you are exhibiting symptoms of Parkinson's Disease. It is very essential that you get a thorough checkup done.")))
elif(eq ?symptom-score 7) then
(assert (result
(rating ?symptom-score)
(verdict "Good chances that you might be exhitibiting just the early onset symptoms of Parkinson's Disease. A checkup is advised.")))       
 elif(eq ?symptom-score 6) then
(assert (result
(rating ?symptom-score)
(verdict "Good chances that you might be exhitibiting just the early onset symptoms of Parkinson's Disease. A checkup is advised.")))
elif(eq ?symptom-score 0) then
(assert (result
(rating ?symptom-score)
(verdict "You are not exhibiting any symptoms of Parkinson's Disease.")))
 else
(assert (result
(rating ?symptom-score)
(verdict "Very low chances that you are exhibiting symptoms of Parkinson's Disease.")))))

(defrule patient-group5

(patient (age ?a&:(>= ?a 55)&:(< ?a 65))
(sleep-score ?s) 
(slowness-score ?l) 
(facial-score ?f) 
(tremor-score ?t)
(balance-score ?b)
(urinary-score ?u)
(stiffness-score ?i)
(gait-score ?g))
=>
(bind ?symptom-score (integer (+ (* .15 ?s) (* .15 ?l) (* .15 ?f) (* .15 ?t)(*  .15 ?b)(*  .15 ?u)(* .1 ?g) )))
(if(eq ?symptom-score 10) then
(assert (result
(rating ?symptom-score)
(verdict "Very high chances that you are exhibiting symptoms of Parkinson's Disease. It is very essential that you get a thorough checkup done.")))
elif(eq ?symptom-score 9) then
(assert (result
(rating ?symptom-score)
(verdict "High chances that you are exhibiting symptoms of Parkinson's Disease. It is very essential that you get a thorough checkup done.")))
 elif(eq ?symptom-score 8) then
(assert (result
(rating ?symptom-score)
(verdict "High chances that you are exhibiting symptoms of Parkinson's Disease. It is very essential that you get a thorough checkup done.")))
elif(eq ?symptom-score 7) then
(assert (result
(rating ?symptom-score)
(verdict "Good chances that you might be exhitibiting just the early onset symptoms of Parkinson's Disease. A checkup is advised.")))       
 elif(eq ?symptom-score 6) then
(assert (result
(rating ?symptom-score)
(verdict "Good chances that you might be exhitibiting just the early onset symptoms of Parkinson's Disease. A checkup is advised.")))
elif(eq ?symptom-score 0) then
(assert (result
(rating ?symptom-score)
(verdict "You are not exhibiting any symptoms of Parkinson's Disease.")))
 else
(assert (result
(rating ?symptom-score)
(verdict "Very low chances that you are exhibiting symptoms of Parkinson's Disease.")))))

(defrule patient-group6

(patient (age ?a&:(>= ?a 50)&:(< ?a 55))
(sleep-score ?s) 
(slowness-score ?l) 
(facial-score ?f) 
(tremor-score ?t)
(balance-score ?b)
(urinary-score ?u)
(stiffness-score ?i)
(gait-score ?g))
=>
(bind ?symptom-score (integer (+ (* .15 ?s) (* .15 ?l) (* .15 ?f) (* .15 ?t)(*  .15 ?b)(*  .15 ?u)(* .1 ?g) )))
(if(eq ?symptom-score 10) then
(assert (result
(rating ?symptom-score)
(verdict "Very high chances that you are exhibiting symptoms of Parkinson's Disease. It is very essential that you get a thorough checkup done.")))
elif(eq ?symptom-score 9) then
(assert (result
(rating ?symptom-score)
(verdict "High chances that you are exhibiting symptoms of Parkinson's Disease. It is very essential that you get a thorough checkup done.")))
 elif(eq ?symptom-score 8) then
(assert (result
(rating ?symptom-score)
(verdict "High chances that you are exhibiting symptoms of Parkinson's Disease. It is very essential that you get a thorough checkup done.")))
elif(eq ?symptom-score 7) then
(assert (result
(rating ?symptom-score)
(verdict "Good chances that you might be exhitibiting just the early onset symptoms of Parkinson's Disease. A checkup is advised.")))       
 elif(eq ?symptom-score 6) then
(assert (result
(rating ?symptom-score)
(verdict "Good chances that you might be exhitibiting just the early onset symptoms of Parkinson's Disease. A checkup is advised.")))
elif(eq ?symptom-score 0) then
(assert (result
(rating ?symptom-score)
(verdict "You are not exhibiting any symptoms of Parkinson's Disease.")))
 else
(assert (result
(rating ?symptom-score)
(verdict "Very low chances that you are exhibiting symptoms of Parkinson's Disease.")))))

(defrule patient-group7

(patient (age ?a&:(>= ?a 45)&:(< ?a 50))
(sleep-score ?s) 
(slowness-score ?l) 
(facial-score ?f) 
(tremor-score ?t)
(balance-score ?b)
(urinary-score ?u)
(stiffness-score ?i)
(gait-score ?g))
=>
(bind ?symptom-score (integer (+ (* .15 ?s) (* .15 ?l) (* .15 ?f) (* .15 ?t)(*  .15 ?b)(*  .15 ?u)(* .1 ?g) )))
(if(eq ?symptom-score 10) then
(assert (result
(rating ?symptom-score)
(verdict "Very high chances that you are exhibiting symptoms of Parkinson's Disease. It is very essential that you get a thorough checkup done.")))
elif(eq ?symptom-score 9) then
(assert (result
(rating ?symptom-score)
(verdict "High chances that you are exhibiting symptoms of Parkinson's Disease. It is very essential that you get a thorough checkup done.")))
 elif(eq ?symptom-score 8) then
(assert (result
(rating ?symptom-score)
(verdict "Good chances that you might be exhitibiting just the early onset symptoms of Parkinson's Disease. A checkup is advised.")))
 elif(eq ?symptom-score 7) then
(assert (result
(rating ?symptom-score)
(verdict "Considering the age factor, very low chances that you are exhibiting symptoms of Parkinson's Disease.")))
 elif(eq ?symptom-score 6) then
(assert (result
(rating ?symptom-score)
(verdict "Considering the age factor, very low chances that you are exhibiting symptoms of Parkinson's Disease.")))
         
 elif(eq ?symptom-score 0) then
(assert (result
(rating ?symptom-score)
(verdict "You are not exhibiting any symptoms of Parkinson's Disease.")))
 else
(assert (result
(rating ?symptom-score)
(verdict "Considering the age factor, very low chances that you are exhibiting symptoms of Parkinson's Disease.")))))

(defrule patient-group8

(patient (age ?a&:(>= ?a 35)&:(< ?a 45))
(sleep-score ?s) 
(slowness-score ?l) 
(facial-score ?f) 
(tremor-score ?t)
(balance-score ?b)
(urinary-score ?u)
(stiffness-score ?i)
(gait-score ?g))
=>
(bind ?symptom-score (integer (+ (* .15 ?s) (* .15 ?l) (* .15 ?f) (* .15 ?t)(*  .15 ?b)(*  .15 ?u)(* .1 ?g) )))
(if(eq ?symptom-score 10) then
(assert (result
(rating ?symptom-score)
(verdict "Very high chances that you are exhibiting symptoms of Parkinson's Disease. It is very essential that you get a thorough checkup done.")))
elif(eq ?symptom-score 9) then
(assert (result
(rating ?symptom-score)
(verdict "High chances that you are exhibiting symptoms of Parkinson's Disease. It is very essential that you get a thorough checkup done.")))
 elif(eq ?symptom-score 8) then
(assert (result
(rating ?symptom-score)
(verdict "Good chances that you might be exhitibiting just the early onset symptoms of Parkinson's Disease. A checkup is advised.")))
elif(eq ?symptom-score 7) then
(assert (result
(rating ?symptom-score)
(verdict "Considering the age factor, very low chances that you are exhibiting symptoms of Parkinson's Disease.")))
 elif(eq ?symptom-score 6) then
(assert (result
(rating ?symptom-score)
(verdict "Considering the age factor, very low chances that you are exhibiting symptoms of Parkinson's Disease.")))
      
elif(eq ?symptom-score 0) then
(assert (result
(rating ?symptom-score)
(verdict "You are not exhibiting any symptoms of Parkinson's Disease.")))
 else
(assert (result
(rating ?symptom-score)
(verdict "Considering the age factor, very low chances that you are exhibiting symptoms of Parkinson's Disease.")))))

(defrule patient-group9

(patient (age ?a&:(>= ?a 25)&:(< ?a 35))
(sleep-score ?s) 
(slowness-score ?l) 
(facial-score ?f) 
(tremor-score ?t)
(balance-score ?b)
(urinary-score ?u)
(stiffness-score ?i)
(gait-score ?g))
=>
(bind ?symptom-score (integer (+ (* .15 ?s) (* .15 ?l) (* .15 ?f) (* .15 ?t)(*  .15 ?b)(*  .15 ?u)(* .1 ?g) )))
(if(eq ?symptom-score 10) then
(assert (result
(rating ?symptom-score)
(verdict "Very high chances that you are exhibiting symptoms of Parkinson's Disease. It is very essential that you get a thorough checkup done.")))
elif(eq ?symptom-score 9) then
(assert (result
(rating ?symptom-score)
(verdict "High chances that you are exhibiting symptoms of Parkinson's Disease. It is very essential that you get a thorough checkup done.")))
 elif(eq ?symptom-score 8) then
(assert (result
(rating ?symptom-score)
(verdict "Good chances that you might be exhitibiting just the early onset symptoms of Parkinson's Disease. A checkup is advised.")))
 elif(eq ?symptom-score 7) then
(assert (result
(rating ?symptom-score)
(verdict "Considering the age factor, very low chances that you are exhibiting symptoms of Parkinson's Disease.")))
 elif(eq ?symptom-score 6) then
(assert (result
(rating ?symptom-score)
(verdict "Considering the age factor, very low chances that you are exhibiting symptoms of Parkinson's Disease.")))
 elif(eq ?symptom-score 0) then
(assert (result
(rating ?symptom-score)
(verdict "You are not exhibiting any symptoms of Parkinson's Disease.")))
 else
(assert (result
(rating ?symptom-score)
(verdict "Considering the age factor, very low chances that you are exhibiting symptoms of Parkinson's Disease.")))))

(defrule patient-group10

(patient (age ?a&:(>= ?a 15)&:(< ?a 25))
(sleep-score ?s) 
(slowness-score ?l) 
(facial-score ?f) 
(tremor-score ?t)
(balance-score ?b)
(urinary-score ?u)
(stiffness-score ?i)
(gait-score ?g))
=>
(bind ?symptom-score (integer (+ (* .15 ?s) (* .15 ?l) (* .15 ?f) (* .15 ?t)(*  .15 ?b)(*  .15 ?u)(* .1 ?g) )))
(if(eq ?symptom-score 10) then
(assert (result
(rating ?symptom-score)
(verdict "Very high chances that you are exhibiting symptoms of Parkinson's Disease. It is very essential that you get a thorough checkup done.")))
elif(eq ?symptom-score 9) then
(assert (result
(rating ?symptom-score)
(verdict "High chances that you are exhibiting symptoms of Parkinson's Disease. It is very essential that you get a thorough checkup done.")))
 elif(eq ?symptom-score 8) then
(assert (result
(rating ?symptom-score)
(verdict "Good chances that you might be exhitibiting just the early onset symptoms of Parkinson's Disease. A checkup is advised.")))
 elif(eq ?symptom-score 7) then
(assert (result
(rating ?symptom-score)
(verdict "Considering the age factor, very low chances that you are exhibiting symptoms of Parkinson's Disease.")))
 elif(eq ?symptom-score 6) then
(assert (result
(rating ?symptom-score)
(verdict "Considering the age factor, very low chances that you are exhibiting symptoms of Parkinson's Disease.")))
 elif(eq ?symptom-score 0) then
(assert (result
(rating ?symptom-score)
(verdict "You are not exhibiting any symptoms of Parkinson's Disease.")))
 else
(assert (result
(rating ?symptom-score)
(verdict "Considering the age factor, very low chances that you are exhibiting symptoms of Parkinson's Disease.")))))

(defrule patient-group11

(patient (age ?a&:(>= ?a 5)&:(< ?a 15))
(sleep-score ?s) 
(slowness-score ?l) 
(facial-score ?f) 
(tremor-score ?t)
(balance-score ?b)
(urinary-score ?u)
(stiffness-score ?i)
(gait-score ?g))
=>
(bind ?symptom-score (integer (+ (* .15 ?s) (* .15 ?l) (* .15 ?f) (* .15 ?t)(*  .15 ?b)(*  .15 ?u)(* .1 ?g) )))
(if(eq ?symptom-score 10) then
(assert (result
(rating ?symptom-score)
(verdict "Very high chances that you are exhibiting symptoms of Parkinson's Disease. It is very essential that you get a thorough checkup done.")))
elif(eq ?symptom-score 9) then
(assert (result
(rating ?symptom-score)
(verdict "High chances that you are exhibiting symptoms of Parkinson's Disease. It is very essential that you get a thorough checkup done.")))
 elif(eq ?symptom-score 8) then
(assert (result
(rating ?symptom-score)
(verdict "Good chances that you might be exhitibiting just the early onset symptoms of Parkinson's Disease. A checkup is advised.")))
 elif(eq ?symptom-score 7) then
(assert (result
(rating ?symptom-score)
(verdict "Considering the age factor, very low chances that you are exhibiting symptoms of Parkinson's Disease.")))
 elif(eq ?symptom-score 6) then
(assert (result
(rating ?symptom-score)
(verdict "Considering the age factor, very low chances that you are exhibiting symptoms of Parkinson's Disease.")))
 elif(eq ?symptom-score 0) then
(assert (result
(rating ?symptom-score)
(verdict "You are not exhibiting any symptoms of Parkinson's Disease.")))
 else
(assert (result
(rating ?symptom-score)
(verdict "Considering the age factor, very low chances that you are exhibiting symptoms of Parkinson's Disease.")))))



(defrule patient-group12

(patient (age ?a&:(>= ?a 0)&:(< ?a 5))
(sleep-score ?s) 
(slowness-score ?l) 
(facial-score ?f) 
(tremor-score ?t)
(balance-score ?b)
(urinary-score ?u)
(stiffness-score ?i)
(gait-score ?g))
=>
(bind ?symptom-score (integer (+ (* .15 ?s) (* .15 ?l) (* .15 ?f) (* .15 ?t)(*  .15 ?b)(*  .15 ?u)(* .1 ?g) )))
(if(eq ?symptom-score 10) then
(assert (result
(rating ?symptom-score)
(verdict "Very high chances that you are exhibiting symptoms of Parkinson's Disease. It is very essential that you get a thorough checkup done.")))
elif(eq ?symptom-score 9) then
(assert (result
(rating ?symptom-score)
(verdict "High chances that you are exhibiting symptoms of Parkinson's Disease. It is very essential that you get a thorough checkup done.")))
 elif(eq ?symptom-score 8) then
(assert (result
(rating ?symptom-score)
(verdict "Good chances that you might be exhitibiting just the early onset symptoms of Parkinson's Disease. A checkup is advised.")))
 elif(eq ?symptom-score 7) then
(assert (result
(rating ?symptom-score)
(verdict "Considering the age factor, very low chances that you are exhibiting symptoms of Parkinson's Disease.")))
 elif(eq ?symptom-score 6) then
(assert (result
(rating ?symptom-score)
(verdict "Considering the age factor, very low chances that you are exhibiting symptoms of Parkinson's Disease.")))
 elif(eq ?symptom-score 0) then
(assert (result
(rating ?symptom-score)
(verdict "You are not exhibiting any symptoms of Parkinson's Disease.")))
 else
(assert (result
(rating ?symptom-score)
(verdict "Considering the age factor, very low chances that you are exhibiting symptoms of Parkinson's Disease.")))))

(defrule check-score
(patient (age ?a)
(sleep-score ?s) 
(slowness-score ?l) 
(facial-score ?f) 
(tremor-score ?t)
(balance-score ?b)
(urinary-score ?u)
(stiffness-score ?i)
(gait-score ?g))
=>
(if (> ?s 10) then
(printout t "The value needs to be in the range of 0-10. Hence, For evaluation purpose value considered is 10. " crlf))
(if (> ?l 10) then
(printout t "The value needs to be in the range of 0-10. Hence, For evaluation purpose value considered is 10. " crlf))
(if (> ?f 10) then
(printout t "The value needs to be in the range of 0-10. Hence, For evaluation purpose value considered is 10. " crlf))
(if (> ?t 10) then
(printout t "The value needs to be in the range of 0-10. Hence, For evaluation purpose value considered is 10. " crlf))
(if (> ?b 10) then
(printout t "The value needs to be in the range of 0-10. Hence, For evaluation purpose value considered is 10. " crlf))
(if (> ?u 10) then
(printout t "The value needs to be in the range of 0-10. Hence, For evaluation purpose value considered is 10. " crlf))
(if (> ?i 10) then
(printout t "The value needs to be in the range of 0-10. Hence, For evaluation purpose value considered is 10. " crlf))
(if (> ?g 10) then
(printout t "The value needs to be in the range of 0-10. Hence, For evaluation purpose value considered is 10. " crlf))
(if (< ?s 0) then
(printout t "The value needs to be in the range of 0-10. Hence, For evaluation purpose value considered is 0. " crlf))
(if (< ?l 0) then
(printout t "The value needs to be in the range of 0-10. Hence, For evaluation purpose value considered is 0. " crlf))
(if (< ?f 0) then
(printout t "The value needs to be in the range of 0-10. Hence, For evaluation purpose value considered is 0. " crlf))
(if (< ?t 0) then
(printout t "The value needs to be in the range of 0-10. Hence, For evaluation purpose value considered is 0. " crlf))
(if (< ?b 0) then
(printout t "The value needs to be in the range of 0-10. Hence, For evaluation purpose value considered is 0. " crlf))
(if (< ?u 0) then
(printout t "The value needs to be in the range of 0-10. Hence, For evaluation purpose value considered is 0. " crlf))
(if (< ?i 0) then
(printout t "The value needs to be in the range of 0-10. Hence, For evaluation purpose value considered is 0. " crlf))
(if (< ?g 0) then
(printout t "The value needs to be in the range of 0-10. Hence, For evaluation purpose value considered is 0. " crlf))
(if (> ?a 150) then
(printout t "The age value entered needs to be less than 150. Hence, For evaluation purpose value considered is 150. " crlf))    
(if (< ?a 0) then
(printout t "The age should be a integer value and in the range of closest to the actual age and greater than 0. " crlf)))

(defmodule result)
(defrule print-result
?p1 <- (result (verdict ?e))
=>
(printout t "Medical Evaluation Results : " ?e crlf crlf))

(deffunction run-application ()
(reset)
(focus start request-patient-details symptom-recognition result)
(run))

(while TRUE
(run-application))
