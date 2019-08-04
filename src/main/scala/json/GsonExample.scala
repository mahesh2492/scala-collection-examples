package json

import com.google.gson.Gson

object GsonExample extends App {

  val person = Person("mahesh", "chand", 26, "male")
  val gson = new Gson
  val json = gson.toJson(person)

  println("Json of Person class using Gson: " + json)

 val personFromJson = gson.fromJson[Person](json, classOf[Person])

 println("Person from json using Gsoon" + personFromJson)

}
