object Exercises {


  def reverse[T](seq: Seq[T]): Seq[T] = {
    def loop(reversed: Seq[T], remainder: Seq[T]) : Seq[T] = remainder match {
      case head :: tail => loop(reversed, tail) :+ head
      case _ => remainder
    }
    loop(Seq.empty, seq)
  }

  /**
   * https://ru.wikipedia.org/wiki/Числа_Фибоначчи
   *
   * @param idx
   * @return
   */
  def fibonacci4Index(idx: Int): Int = idx match {
    case 0 => 0
    case 1 => 1
    case n if n >= 2 => fibonacci4Index(n-1) + fibonacci4Index(n-2)
  }

  def fibonacci(idx: Int): Seq[Int] = idx match {
    case 0 => Seq(0)
    case 1 => Seq(0, 1)
    case n if n >= 2 => fibonacci(n-1) :+ fibonacci(n-1).last + fibonacci(n-2).last
  }

  lazy val MORSE = Map("A" -> ".-", "B" -> "-...", "C" -> "-.-.", "D" -> "-..", "E" -> ".", "F" -> "..-.",
                       "G" -> "--.", "H" -> "....", "I" -> "..", "J" -> ".---", "K" -> "-.-", "L" -> ".-..",
                       "M" -> "--", "N" -> "-.", "O" -> "---", "P" -> ".--.", "Q" -> "--.-", "R" -> ".-.",
                       "S" -> "...", "T" -> "-", "U" -> "..-", "V" -> "...-", "W" -> ".--", "X" -> "-..-",
                       "Y" -> "-.--", "Z" -> "--..")

  def morse(text: String): String = {
    text.toUpperCase.map(c => MORSE.get(c.toString) match {
      case Some(x) => x
      case None => c.toString
    }).mkString(" ")
  }


  def wordReverse(text: String): String = {
    // Разделить строку на границах слов
    text.split("\\b")
      .map(word => word.headOption match {
        // Если текущая часть не пустая и является словом
        case Some(c) => if (c.isLetter) {
          // Развернуть слово
          word.reverse
            // Сделать регистр символов развёрнутого слова
            // идентичным регистру символов с тем же индексом в исходном слове
            .toLowerCase
            .zipWithIndex
            .map(rChar =>
              if (word.zipWithIndex.filter(p => p._1 isUpper).unzip._2.contains(rChar._2))
                rChar._1.toUpper
              else rChar._1
            )
            .mkString
        }
        else word
      })
      .mkString
  }

}
