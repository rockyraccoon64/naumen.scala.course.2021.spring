trait Cell {
  def toString() : String
}

case class EmptyCell() extends Cell {
  override def toString: String = "empty"
}

case class NumberCell(number: Int) extends Cell {
  override def toString: String = number.toString
}

case class StringCell(string: String) extends Cell {
  override def toString: String = string
}

case class ReferenceCell(ix: Int, iy: Int, table: Table) extends Cell {
  // Возвращает ячейку, на которую непосредственно ссылается текущая ячейка, если такая имеется
  private def getRef() : Option[Cell] = {
    table.getCell(ix, iy)
  }

  // Возвращает последнюю ячейку в цепочке ссылок, если такая имеется (или текущую, если цепочка циклическая)
  private def getFinalRef() : Option[Cell] = {
    var found = false
    var currentCell = getRef()
    while (!found) {
      currentCell match {
        case Some(r : ReferenceCell) => {
          if (r != this) currentCell = r.getRef()
          else found = true
        }
        case _ => found = true
      }
    }
    currentCell
  }

  override def toString: String = {
    val finalRef = getFinalRef()
    finalRef match {
      case Some(r : ReferenceCell) => "cyclic"
      case Some(c) => c.toString
      case None => "outOfRange"
    }
  }
}