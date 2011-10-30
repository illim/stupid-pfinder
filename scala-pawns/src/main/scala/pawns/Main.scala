package pawns

object Main{
  import PathFinder._

  def main(args: Array[String]) {
    val worldSize = 10
    val orig = ( 3, 3)
    val dest = ( 10, 4)
    val w = World(
      Field(worldSize, Map.empty),
      (1 to 9).toList.map{i => new Entity(5, i)} ::: (0 to 8).toList.map{i => new Entity(7, i)})

    val bench = new Bench[List[(List[Point], Double)]](res => res.size + " paths found (" + res.head +")")

    val finders = List(new FastStupidFind(dest, w), new LazyStupidFind2(dest, w), new LazyStupidFind(dest, w), new StupidFind(dest, w), new StupidFind2(dest, w))

    (finders ++ finders.reverse).foreach( finder => bench.run(finder.name, finder.compute(orig)))
    bench.printReport
  }

  class Bench[A](resultMessage : A => String){
    var collecteds : List[BenchResult] = Nil

    case class BenchResult(name : String, score : Long, result : A)

    def run(name : String, f : => A) = {
      val st = System.currentTimeMillis
      val res = f
      collecteds = BenchResult(name, System.currentTimeMillis - st, res) :: collecteds
      // println(res.take(10).mkString("\n"))
//      println(message + " : " +  + " ms")
//      println(res.size + " paths found")
      res
    }

    def printReport = {
      collecteds.sortBy(x => x.score).foreach { x =>
        println(x.name + " : " + x.score + " ms")
        println(resultMessage(x.result))
      }
    }
  }


}
