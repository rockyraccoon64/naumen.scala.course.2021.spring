object Exercises {
  trait Animal {
    def name: String
  }
  case class Cat(override val name: String) extends Animal
  case class Dog(override val name: String) extends Animal

  case class Shelter[+T <: Animal](collection: Iterable[T] = Iterable[T]()) {
    val m_animals : List[T] = collection.toList
    def +[A >: T <: Animal](animal: A) : Shelter[A] = Shelter(m_animals :+ animal)
    def ++[A >: T <: Animal](other: Shelter[A]) : Shelter[A] = Shelter(m_animals ++ other.m_animals)
    def getNames : List[String] = m_animals.map(x => x.name)
    def feed[A >: T <: Animal](food: Food[A]) : List[String] = m_animals.map(food.feed)
  }

  trait Food[T <: Animal] {
    def name : String
    def feed[A >: T <: Animal](animal: A): String = {
      "%s eats %s".format(animal.name, name)
    }
  }
  case object Meat extends Food[Animal] {
    override def name: String = "meat"
  }
  case object Milk extends Food[Cat] {
    override def name: String = "milk"
  }
  case object Bread extends Food[Dog] {
    override def name: String = "bread"
  }
}
