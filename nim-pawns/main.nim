import sequtils, tables, algorithm, future, options

const WORLD_SIZE = 10

type
  Point         = tuple[x: int, y: int]
  StepCost      = tuple[step : Point, cost: float]
  LinkedList[T] = ref object
    value: T
    next: LinkedList[T]
  Path          = LinkedList[Point]
  SearchResult  = tuple[path: Path, cost: float]
  World         = object
    entities: seq[Point]
  Finder        = ref object
    stepCosts : seq[StepCost]
    marks     : TableRef[Point, float]
    world     : World
   
proc isClean(world : World, p: Point): bool =
  p.x > -1 and p.y > -1 and
    p.x < WORLD_SIZE and p.y < WORLD_SIZE and
    not world.entities.contains(p)

proc add(p, p2: Point) : Point =
  (p.x + p2.x, p.y + p2.y)

proc initWorld : World =
  World( entities: map(lc[x | (x <- 3..8), int], i => (x: 7, y: i)) )

proc getStepCosts : seq[StepCost] =
  concat(map(@[(1, 0), (0, 1), (0, -1), (-1, 0)],   p => (p, 1.0)),
         map(@[(1, 1), (1, -1), (-1, 1), (-1, -1)], p => (p, 1.414213)))
  
proc newFinder(world : World) : Finder =
  Finder(stepCosts : getStepCosts(), marks : newTable[Point, float](), world: world)

template foldLeft(sequence, init, operation : expr) : expr =
  var result {.gensym.}: type(init)
  result = init
  for i in 0..<sequence.len:
    let
      acc {.inject.} = result
      elt {.inject.} = sequence[i]
    result = operation
  result

proc newLinkedList[T](value : T, next : LinkedList[T]) : LinkedList[T] =
  LinkedList[T](value : value, next : next)

proc bestDist(finder : Finder, p : Point, dest : Point, path: Path, accCost: float) : Option[SearchResult]= 

  let accumStep = proc(acc : seq[Point], stepCost : StepCost) : seq[Point] =
    let next    = p.add(stepCost.step)
    let oldCost = finder.marks.getOrDefault(next)
    let cost    = accCost + stepCost.cost
    if finder.world.isClean(next) and (oldCost == 0 or oldCost > cost) :
      finder.marks[next]= cost
      result = acc & next
    else:
      result = acc

  let accumSearchResult = proc(acc : seq[SearchResult], next : Point) : seq[SearchResult] =
    let newPath = newLinkedList[Point](next, path)
    let dist    = bestDist(finder, next, dest, newPath, finder.marks[next])
    result = if dist.isSome: acc & dist.get
             else: acc

  if p == dest :
    result = some((path: path, cost: accCost))
  else :
    let nextSteps = finder.stepCosts.foldLeft(newSeq[Point](), accumStep(acc, elt))
    var searchResults = nextSteps.foldLeft(newSeq[SearchResult](), accumSearchResult(acc, elt))
    sort(searchResults, (x,y) => cmp(x.cost, y.cost))
    if searchResults.len > 0:
      result = some(searchResults[0])
    
proc findPath(finder : Finder, orig : Point, dest : Point) : Option[SearchResult] =
  finder.bestDist(orig, dest, newLinkedList[Point](orig, nil), 0)
  
proc main() =
  let world  = initWorld()
  let finder = newFinder(world)
  let res    = finder.findPath((x: 3, y: 3), (x: 8, y: 6))
  echo(repr(res))

main()

