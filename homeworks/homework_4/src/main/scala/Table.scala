import scala.collection.mutable.ArrayBuffer

class Table(width: Int, height: Int) {
  private val size : Int = width*height
  private val cells : ArrayBuffer[Cell] = ArrayBuffer.fill[Cell](size)(EmptyCell())

  // Возвращает индекс ячейки (ix, iy) в массиве cells, если ячейка в пределах таблицы
  private def getArrayIndex(ix: Int, iy: Int) : Option[Int] = {
    val index = ix + iy * width
    if (index >= 0 && index < size) Option.apply(index)
    else Option.empty[Int]
  }

  def getCell(ix: Int, iy: Int) : Option[Cell] = getArrayIndex(ix, iy) match {
      case Some(index) => Option.apply(cells(index))
      case None => Option.empty[Cell]
  }

  def setCell(ix: Int, iy: Int, cell: Cell) : Unit = getArrayIndex(ix, iy) match {
    case Some(index) => cells.update(index, cell)
  }
}