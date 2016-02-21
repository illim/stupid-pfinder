use std::cmp::Ordering;
use std::rc::Rc;
use std::collections::HashMap;

const WORLD_SIZE : usize = 10;

#[derive(PartialEq, Debug, Eq, Hash, Clone, Copy)]
struct Point {
  x: i32,
  y: i32
}

fn pt(x : i32, y : i32) -> Point {
    Point {x : x, y :y}
}

impl Point {
    fn add(&self, p : Point) -> Point {
        Point { x : self.x + p.x, y : self.y + p.y }
    }
}

struct World {
  entities : Vec<Point>
}

impl World {
    
    fn is_clean(&self, p : &Point) -> bool {
        p.x > -1
            && (p.x as usize) < WORLD_SIZE
            && p.y >-1
            && (p.y as usize) < WORLD_SIZE
            && self.entities.iter().all(|&pt| {
                pt != *p
            })
    }
}

#[derive(Debug)]
enum List<A> {
    Cons(A, Rc<List<A>>),
    Nil
}

use List::*;

struct StepCost {
    step : Point,
    cost : f64        
}

struct Finder<'a> {
    world : &'a World,
    marks : HashMap<Point, f64>,
    step_costs : Vec<StepCost>
}

type Path = List<Point>;

#[derive(Debug)]
struct SearchResult {
    path : Rc<Path>,
    cost : f64
}

impl<'a> Finder<'a> {

    fn new(world : &World) -> Finder {
        Finder {
            world : &world,
            marks : HashMap::new(),
            step_costs : get_step_costs()
        }
    }

    fn find(&mut self, orig : Point, dest : Point) -> Option<SearchResult> {
        self.marks = HashMap::new();
        self.best_dist(orig, dest, Rc::new(Cons(orig, Rc::new(Nil))), 0.0)
    }

    fn best_dist(&mut self, p : Point, dest : Point, path : Rc<Path>, acc_cost : f64) -> Option<SearchResult> {
        if p == dest {
            Some(SearchResult{ path: path.clone(), cost: acc_cost})
        } else {
            // create a block to release ownership of the closure to use self later
            let next_steps = {
                // Boilerplate to avoid borrowing self
                let world      = &self.world;
                let marks      = &mut self.marks;
                let step_costs = &self.step_costs;
               
                step_costs
                    .iter()
                    .filter_map(|step_cost : &StepCost| {
                        let next = p.add(step_cost.step);
                        let oldcost = *marks.get(&next).unwrap_or(&0.0);
                        let cost = acc_cost + step_cost.cost;

                        if world.is_clean(&next)
                            && (oldcost == 0.0 || oldcost > cost) {
                                marks.insert(next, cost);
                                Some(next)
                            } else {
                                None
                            }
                    })
                    .collect::<Vec<Point>>() // HACK?? can't return iterator because references are no more valid outside of the block
            };
                
            let search_results = next_steps
                .iter()
                .filter_map(|&next| {
                    let newpath = Rc::new(Cons(next, path.clone()));
                    let newcost = *self.marks.get(&next).unwrap();
                    self.best_dist(next, dest, newpath, newcost)
                });
            
            search_results.fold(None, |acc, search_result| {
                match acc {
                    Some(best) =>
                        if best.cost.partial_cmp(&search_result.cost) == Some(Ordering::Less) {
                            Some(best)
                        } else {
                            Some(search_result)
                        },
                    None => Some(search_result)
                }
            })
        }
    }
}

fn get_step_costs() -> Vec<StepCost> {
    [pt(1, 0), pt(0, 1), pt(0, -1), pt(-1, 0)]
        .iter()
        .map(|&p| StepCost{ step : p, cost : 1.0})
        .chain([pt(1, 1), pt(1, -1), pt(-1, 1), pt(-1, -1)]
            .iter()
            .map(|&p| StepCost{ step: p, cost: 1.414213} ))
        .collect()
}

fn main() {
    let entities = (3..)
        .take(6)
        .map(|i| pt(7, i))
        .collect::<Vec<_>>();
    let world = World { entities : entities };
    let orig = Point { x : 3, y : 3};
    let dest = Point { x : 8, y : 6};
    let mut finder = Finder::new(&world);
    
    let sol = finder.find(orig, dest);
    println!("sol = {:?}", sol);    
}
