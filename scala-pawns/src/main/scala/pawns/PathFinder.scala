package pawns

import math._

object PathFinder {

  val stepCosts : List[(Point, Double)]=
    (List((1, 0), (0, 1), (0, -1), (-1, 0)).map( step => (step, 1d))
      ::: List((1, 1) , (1, -1) , (-1, 1) , (-1, -1)).map( step => (step, 1.414213d)))

  class FastStupidFind(dest : Point, world : World) extends StupidFind2(dest, world) {

    override def bestDist(p : Point, cst : Double, path : List[Point]) : List[(List[Point], Double)]= {
      val (lefts, rights) = checkSteps(p, cst, path)

      lefts ::: (rights.sortBy(x => dumbdist(x._1, dest)).collectFirst(partialize{ case (pn, newcost) =>
        marks(pn._1)(pn._2)= newcost
        bestDist(pn, newcost, pn :: path).headOption
      }).toList)
    }
  }

  class LazyStupidFind2(dest : Point, world : World) extends StupidFind2(dest, world) {

    override def bestDist(p : Point, cst : Double, path : List[Point]) : List[(List[Point], Double)]= {
      val (lefts, rights) = checkSteps(p, cst, path)

      lefts ::: (rights.sortBy(x => dumbdist(x._1, dest) + x._2).view.flatMap{ case (pn, newcost) =>
        marks(pn._1)(pn._2)= newcost
        bestDist(pn, newcost, pn :: path).headOption
      }.take(1).toList)
    }
  }

  class LazyStupidFind(dest : Point, world : World) extends StupidFind2(dest, world) {

    override def bestDist(p : Point, cst : Double, path : List[Point]) : List[(List[Point], Double)]= {
      val (lefts, rights) = checkSteps(p, cst, path)

      lefts ::: (rights.sortBy(x => dumbdist(x._1, dest)).view.flatMap{ case (pn, newcost) =>
        marks(pn._1)(pn._2)= newcost
        bestDist(pn, newcost, pn :: path).headOption
      }.take(1).toList)
    }
  }

  class StupidFind2(dest : Point, world : World) extends StupidFind(dest, world) {
    override def bestDist(p : Point, cst : Double, path : List[Point]) : List[(List[Point], Double)]= {
      val (lefts, rights) = checkSteps(p, cst, path)

      rights.sortBy(x => dumbdist(x._1, dest)).flatMap{ case (pn, newcost) =>
        marks(pn._1)(pn._2)= newcost
        bestDist(pn, newcost, pn :: path)
      } ::: lefts
    }

    def checkSteps(p : Point, cst : Double, path : List[Point]) = {
      ((List.empty[(List[Point], Double)], List.empty[(Point, Double)]) /: stepCosts) { case ((l, r), (step, cost)) =>
        val pn = ptAdd(p, step)
        val newcost = cst + cost
        if (pn == dest){
          (((pn :: path).reverse, newcost) :: l, r)
        } else if (!world.isClean(pn) || marks(pn._1)(pn._2) < newcost){
          (l, r)
        } else {
          (l, (pn, newcost) :: r)
        }
      }
    }
  }

  class StupidFind(dest : Point, world : World) {
    protected var marks : Array[Array[Double]] = _
    def name = getClass.getSimpleName

    def bestDist(p : Point, cst : Double, path : List[Point]) : List[(List[Point], Double)]= {
      stepCosts.flatMap{ case (step, cost) =>
        val pn = ptAdd(p, step)
        val newcost = cst + cost
        if (pn == dest){
          List(((pn :: path).reverse, newcost))
        } else if (!world.isClean(pn) || marks(pn._1)(pn._2) < newcost){
          Nil
        } else {
          marks(pn._1)(pn._2)= newcost
          // TODO trier par distance
          bestDist(pn, newcost, pn :: path)
        }
      }
    }

    def compute(orig : Point) = {
      marks = Array.fill[Double](world.field.size, world.field.size)(java.lang.Double.MAX_VALUE)
      bestDist(orig, 0, Nil).sortBy(_._2)
    }
  }


  def partialize[A, B](f : A => Option[B])= new PartialFunction[A, B]{
    private var value : Option[B] = _
    private def compute(a : A) = {
      if (value == null) {
        value = f(a)
      }
      value
    }
    def apply(a : A) = compute(a).get
    def isDefinedAt(a : A) = compute(a).isDefined
  }


  def ptAdd(orig : Point, step : Point) = (orig._1 + step._1, orig._2 + step._2)

  def dumbdist(orig : Point, dest : Point) : Double = {
    pow(dest._2 - orig._2, 2) + pow(dest._1 - orig._1, 2)
  }

  def distance(orig : Point, dest : Point) : Double = {
    sqrt(pow(dest._2 - orig._2, 2) + pow(dest._1 - orig._1, 2))
  }

  def separate[A, B](es : List[Either[A, B]]) : (List[A], List[B]) = {
    (for (Left(x) <- es) yield x, for (Right(x) <- es) yield x)
  }
}
