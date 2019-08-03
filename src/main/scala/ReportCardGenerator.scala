import CollectionExamples.{scoreCards, students}
import GenderEnum.Gender
import GenderEnum.{Female, Male}


object GenderEnum extends Enumeration {
  type Gender = Value
  val Male, Female = Value
}

case class Subject(id: Long, subject: String)

case class Student(id: Long, name: String, subjects: List[Subject], gender: Gender)

case class ScoreCard(studentId: Long, marks: Map[Int, Int], percentage: Float)

class ReportCardGenerator {

  /*
  Write a method which takes no parameter and generates a Map with key student name and value as ScoreCard.
  As there can be more than one student with same name, the logic we have to follow is that, if two or more
  student has same name the key should be the name of the student and the values (ScoreCard s) should be in
  a List, otherwise the key should be the student name and value should be the case class ScoreCard. e.g. Map should be Map[String, AnyRef].
   */
  def generateReport: Map[String, AnyRef] = {
    val studentMarks =
      for {
        student <- students
        scoreCard <- scoreCards
        if student.id == scoreCard.studentId
      } yield student.name -> scoreCard

    studentMarks.groupBy {
      case (k, _) => k
    }.map {
      case (k, v) if v.length > 1 => k -> v.map { case (_, scoreCard) => scoreCard }
      case (k, v)                 => k ->  v.head._2

    }
  }
  /*
  Write a method which takes input as student name and print the score cards. If it finds one or more than one score card
  print all of them other wise print "No data found". The print should be in increasing
   */

  def getScoreCardByName(name: String) = {
    generateReport.get(name).fold[Any]("No data found")(identity)
  }

  /*
  The Student class should contain one more field this time, gender. The values of gender must be set in a Enumeration.
  Write a method getScoreCardByGender to return a tuple of ScoreCards (e.g. (List[ScoreCard], List[ScoreCard])),
  where first field in the tuple has male student's score card and the second field has female student's score cards.

   */

  def getScoreCardByGender: ((GenderEnum.Value, List[ScoreCard]), (GenderEnum.Value, List[ScoreCard])) =
    students.zip(scoreCards).partition{
      case (student, _) => student.gender == Male
    } match {
      case (maleScoreCard, femaleScoreCard) =>
        ((Male, maleScoreCard.map(_._2)), (Female, femaleScoreCard.map(_._2)))
    }

  /*
  Write a method which calls the getScoreCardByGender method and gives the result which has more than 50%.
   */
  def getStudentHasAbove50: IterableOnce[(String, ScoreCard)] =
    (getScoreCardByGender match {
      case ((_, maleScorecard), (_, femaleScorecard)) =>
        maleScorecard.filter(_.percentage > 50) ++ femaleScorecard.filter(_.percentage > 50)
    }).zip(students).map{
      case (scorecard, student) => student.name -> scorecard
    }

  /*
  Write a method to find out similar percentage between the two groups (male, female). for example Charmy -82, Bobin -82

   */
  def getSimilarPercentageAmongGender: List[(String, Float)] = {
    val similarPercentage: List[ScoreCard] = scoreCards.groupBy(_.percentage).filter(_._2.length > 1).valuesIterator.toList.flatten
    for {
      a <- similarPercentage
      b <- students
      if a.studentId == b.id
    } yield b.name -> a.percentage
  }

  /*
  Write a method fo find out the percentage that girls group has scored but no same percentage has got in the boys group.
  Charmy - 82
   */
  def getPercentageOfFemale: List[(String, Long)] = {
    getScoreCardByGender match {
      case ((_, malescorecard), (_, femalescorecard)) =>


        val percentage = femalescorecard.map(_.percentage).diff(malescorecard.map(_.percentage))
        val femaleWithPercent =
          for {
            fscore <- femalescorecard
            percent <- percentage
            if fscore.percentage == percent
          } yield (fscore.studentId, percent)

        val femaleStudent = students.filter(_.gender == Female)

        for {
          fscore <- femaleWithPercent
          fstudent <- femaleStudent
          if fstudent.id == fscore._1
        } yield fstudent.name -> fscore._1
    }
  }
}
object CollectionExamples extends App {

  val subjects = List(
    Subject(101, "Maths"),
    Subject(102, "Science"),
    Subject(103, "Hindi"),
    Subject(104, "English"),
    Subject(105, "Social studies"))

  val students = List(
    Student(1, "Mahesh", subjects, Male),
    Student(2, "Shiva", subjects, Female),
    Student(3, "Ankit", subjects, Male),
    Student(4, "Charmy", subjects, Female),
    Student(5, "Bobin", subjects, Male),
    Student(6, "Mahesh", subjects, Male),
    Student(7, "Kamlesh", subjects, Male)
  )

  val scoreCards = List(
    ScoreCard(1, marks = Map(101 -> 79, 102 -> 90, 103 -> 86, 104 -> 74, 105 -> 99), 86),
    ScoreCard(2, marks = Map(101 -> 89, 102 -> 95, 103 -> 83, 104 -> 71, 105 -> 95), 86),
    ScoreCard(3, marks = Map(101 -> 91, 102 -> 50, 103 -> 82, 104 -> 74, 105 -> 92), 77),
    ScoreCard(4, marks = Map(101 -> 90, 102 -> 60, 103 -> 88, 104 -> 77, 105 -> 95), 81),
    ScoreCard(5, marks = Map(101 -> 70, 102 -> 80, 103 -> 87, 104 -> 78, 105 -> 96), 82),
    ScoreCard(6, marks = Map(101 -> 74, 102 -> 79, 103 -> 87, 104 -> 72, 105 -> 97), 86),
    ScoreCard(7, marks = Map(101 -> 34, 102 -> 59, 103 -> 51, 104 -> 42, 105 -> 44), 44)
  )

  val reportCardGenerator = new ReportCardGenerator

  println("==============================Report Card=========================================")
  println(reportCardGenerator.generateReport)
  println("==============================Report Card By Name=========================================")
  println(reportCardGenerator.getScoreCardByName("Shiva"))
  println("==============================Report Card Grouped By Gender=========================================")
  println(reportCardGenerator.getScoreCardByGender)
  println("==============================Students have percentage > 50=========================================")
  println(reportCardGenerator.getStudentHasAbove50)
  println("==============================Similar Percentage Among Gender=========================================")
  println(reportCardGenerator.getSimilarPercentageAmongGender)
  println("==============================A female who got percentage no boys got=========================================")
  println(reportCardGenerator.getPercentageOfFemale)
}