package pawns

trait Position{
  def position : Point
}

case class Entity(position : Point) extends Position

// square field
case class Field(size : Int, heights: Map[Point, Int])

case class World(field : Field, entities : List[Entity]){
  def isClean(p : Point) = {
    val (x, y) = p

    (x > -1
     && x < field.size
     && y > -1
     && y <  field.size
     && ! entities.exists( e => e.position._1 == x && e.position._2 == y))
  }
}

