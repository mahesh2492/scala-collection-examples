package json

import net.liftweb.json.DefaultFormats
import net.liftweb.json.Serialization._

object LiftExample extends App {

  implicit val format: DefaultFormats.type = DefaultFormats

  val person = Person("mahesh", "chand", 26, "male")

  val json = write(person)

  println("Json of Person class using Lift: " + json)

  val personFromJson = read[Person](json)

  println("Person from json using Lift" + personFromJson)

}
