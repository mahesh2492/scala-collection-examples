# scala-collection-examples

Every Student has some marks associated with it. Student details contains its id and name.
And for Marks, there are subjectId, studentId and number of marks a student scored.


Q1. Now, I require a case class named ScoreCard having fields (studentId: Long, marks: Map[Long, Float], percentage: Float).

case class ScoreCard(studentId: Long, marks: Map[Long, Float], percentage: Float)
Write a method which takes no parameter and generates a Map with key student name and value as ScoreCard. As there can be more than one student with same name, the logic we have to follow is that, if two or more student has same name the key shold be the name of the student and the values (ScoreCard s) should be in a List, otherwise the key should be the student name and value should be the case class ScoreCard. e.g. Map should be Map[String, AnyRef]. 
Write a method which takes input as student name and print the score cards. If it finds one or more than one score card  print all of them other wise print "No data found". The print should be in increasing order of the student id.

Q2. The Student class should contain one more field this time, gender. The values of gender must be set in a Enumeration. 

Write a method getScoreCardByGender to return a tuple of ScoreCards (e.g. (List[ScoreCard], List[ScoreCard])), where first field in the tuple has male student's score card and the second field has female student's score cards.

Write a method which calls the getScoreCardByGender method and gives the result which has more than 50%.

Write a method to find out similar percentage between the two groups (male, female). for example Geetika -75, Kunal -75

Write a method fo find out the percentage that girls group has scored but no same percentage has got in the boys group. e.g. ( Geetika -75, Neha - 73, charmy - 72) - (Kunal -75, Anmol - 73, Nitin - 71) = Charmy-72

