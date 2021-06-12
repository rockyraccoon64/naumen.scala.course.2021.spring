package example

object Main extends App {
  def greet(greeting: String, name: String): Unit = println(
    greeting + " Scala! This is " + name
  )

  var name = "Andrey Salo"
  val greetings = Array("Hello", "Hola", "Guten Tag")

  greetings.foreach(g => greet(g, name))

  name = name.reverse
  greetings.foreach(g => greet(g, name))
}