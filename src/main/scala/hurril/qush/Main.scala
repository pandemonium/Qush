package hurril.qush

// Can I simply work on the Json tree
// of the result set to make the selectionSet
// happen?

case class Person(name: String, age: Int)
object PersonRepository {
  def person(id: String): Option[Person] = Option.empty
}

object Main extends App {
  println("Hi, mom")
}