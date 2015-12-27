package main

import "fmt"
import "sort"
import "errors"

type Point struct {
	x int
	y int
}

type World struct {
	entities map[Point]bool
	size     int
}

var stepCosts = getStepCosts()
var world = initWorld()
var notFound = errors.New("Not found")

func initWorld() World {
	world := World{}
	world.entities = make(map[Point]bool)
	for i := 3; i < 8; i++ {
		world.entities[pt(7, i)] = true
	}
	world.size = 10
	return world
}

func main() {
	path, error := stupidFind(pt(3, 3), pt(8, 6))
	if error != nil {
		fmt.Printf("%+v", error)
	} else {
		fmt.Printf("%+v", path.toString())
	}
}

func stupidFind(start Point, dest Point) (*Cons, error) {
	marks := make(map[Point]float64)
	res, err := bestDist(start, dest, &Cons{}, 0, marks)
	return res.path, err
}

type Cons struct {
	value interface{}
	next  *Cons
}

func (c Cons) toString() string {
	nextStr := ""
	if c.next != nil {
		nextStr = c.next.toString()
	}
	return fmt.Sprintf("%v %v", c.value, nextStr)
}

type Res struct {
	path *Cons
	cost float64
}
type ByCost []Res

func (a ByCost) Len() int           { return len(a) }
func (a ByCost) Swap(i, j int)      { a[i], a[j] = a[j], a[i] }
func (a ByCost) Less(i, j int) bool { return a[i].cost < a[j].cost }

func bestDist(p Point, dest Point, path *Cons, accCost float64, marks map[Point]float64) (Res, error) {
	if p == dest {
		return Res{path: path, cost: accCost}, nil
	} else {
		nextSteps := []Point{}
		for step, stepCost := range stepCosts {
			next := add(p, step)
			if !world.entities[next] && next.x > 0 && next.x < world.size && next.y > 0 && next.y < world.size {
				oldCost := marks[next]
				cost := accCost + stepCost
				if oldCost == 0 || oldCost > cost {
					marks[next] = cost
					nextSteps = append(nextSteps, next)
				}
			}
		}
		dists := []Res{}
		for _, next := range nextSteps {
			res, error := bestDist(next, dest, &Cons{value: next, next: path}, marks[next], marks)
			if error == nil {
				dists = append(dists, res)
			}
		}
		sort.Sort(ByCost(dists))
		if len(dists) > 0 {
			return dists[0], nil
		} else {
			return Res{}, notFound
		}
	}
}

func getStepCosts() map[Point]float64 {
	steps := make(map[Point]float64)
	for _, p := range []Point{pt(1, 0), pt(0, 1), pt(0, -1), pt(-1, 0)} {
		steps[p] = 1
	}
	for _, p := range []Point{pt(1, 1), pt(1, -1), pt(-1, 1), pt(-1, -1)} {
		steps[p] = 1.414213
	}
	return steps
}

func pt(x int, y int) Point {
	return Point{x: x, y: y}
}

func add(pt Point, step Point) Point {
	return Point{x: pt.x + step.x, y: pt.y + step.y}
}
