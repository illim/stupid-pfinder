#![feature(convert, core, collections)] 

extern crate core;
use core::cmp::Ordering;

#[derive(PartialEq, Debug, Eq, Hash, Clone, Copy)]
struct Point {
  x: i32,
  y: i32
}

const WORLD_SIZE : usize = 10;
type Marks=[[f64;WORLD_SIZE];WORLD_SIZE];

struct Entity(Point);

struct World {
  entities : Vec<Entity>
}

#[derive(Debug)]
struct Computation {
  p : Point,
  cost : f64,
  path : Vec<Point>
}

fn pt(x : i32, y : i32) -> Point {
    Point {x : x, y :y}
}

fn add(p : Point, p2 : Point) -> Point {
    Point { x : p.x + p2.x, y : p.y + p2.y }
}

fn get_step_costs() -> Vec<(Point, f64)> {
    [pt(1, 0), pt(0, 1), pt(0, -1), pt(-1, 0)]
        .iter()
        .map(|&p| (p, 1.0))
        .chain([pt(1, 1), pt(1, -1), pt(-1, 1), pt(-1, -1)]
            .iter()
            .map(|&p| (p, 1.414213)))
        .collect()
}

fn main() {
    let entities = [3, 4, 5, 6, 7, 8]
        .iter()
        .map(|&i| Entity(pt(7, i)))
        .collect::<Vec<_>>();
    let world = World { entities : entities };
    let orig = Point { x : 3, y : 3};
    let dest = Point { x : 8, y : 6};
 
    let sol = stupid_find(orig, dest, world, get_step_costs());
    println!("sol = {:?}", sol);    
}


fn is_clean(p : &Point, world : &World) -> bool {
    p.x > -1
     && (p.x as usize) < WORLD_SIZE
     && p.y >-1
     && (p.y as usize) < WORLD_SIZE
     && world.entities.iter().all(|e| {
         let &Entity(pt) = e;
         pt != *p
     })
}

fn stupid_find(orig : Point, dest : Point, world : World,
               step_costs : Vec<(Point, f64)>) -> Option<Computation> {

    let init = Computation { p : orig, cost : 0.0, path : Vec::new() };
    let f = | x : Computation, m : &mut Marks | {
        step_costs
            .iter()
            .map( |&sc| {
                let (step, step_cost) = sc;
                let pn = add(x.p, step);
                let mut newpath = Vec::new();
                newpath.push(pn);
                newpath.push_all(x.path.as_slice());

                Computation {
                    p : pn,
                    cost : x.cost + step_cost,
                    path : newpath
                }
            }).fold(Vec::new(), |mut acc, c| {
                if is_clean(&c.p, &world)
                  && m[c.p.x as usize][c.p.y as usize] > c.cost {
                    m[c.p.x as usize][c.p.y as usize] = c.cost;
                    acc.push(c);
                }
                acc
            })
    };

    let mut marks = [[99999.0 ; WORLD_SIZE]; WORLD_SIZE]; // FIXME passing in closure env instead of params

    let dists = rec_dists(init, dest, &mut marks, &f );

    dists.into_iter().fold(None, |acc, c| {
        match acc {
            Some(best) =>
                if best.cost.partial_cmp(&c.cost) == Some(Ordering::Less) {
                    Some(best)
                } else {
                    Some(c)
                },
            None => Some(c)
        }
    })
}

fn rec_dists<F>(c : Computation, dest : Point, marks : &mut Marks, f : & F) -> Vec<Computation>
  where F : Fn(Computation, &mut Marks) -> Vec<Computation> {
  f(c, marks).into_iter().flat_map( |next : Computation| {
      if next.p == dest {
          vec![next]
      } else {
          rec_dists(next, dest, marks, f)
      }
  }).collect()
}
