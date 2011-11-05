open Models

let stepCosts =
  let ortho = List.map (fun step -> (step, 1.)) [1, 0 ; 0, 1; 0, -1; -1, 0]
  and diag  = List.map (fun step -> (step, 1.414213)) [1, 1; 1, -1; -1, 1; -1, -1]
  in ortho @ diag

let ptAdd (x, y) (i, j) = (x + i, y + j)

let initMarks size = Array.make_matrix size size 999999.

let stupidFind orig (dx, dy) ({field} as world) =
  let rec marks = initMarks field.size
  and bestDist p cost path =
    List.flatten
      (List.map
         (fun (step, stepCost)->
            let ((x, y) as pn) = ptAdd p step
            and newcost = cost +. stepCost
            in if (x == dx && y == dy)
              then [(List.rev (pn :: path), newcost)]
              else if (not (isClean world pn) || marks.(x).(y) < newcost)
              then []
              else begin
                marks.(x).(y) <- newcost;
                bestDist pn newcost (pn :: path)
              end ) stepCosts)
  in bestDist orig 0. []

let dumbDist (a, b) (c, d) =
  let pow2 x = x * x
  in pow2 (d - b) + pow2 (c - a)

let fastStupidFind orig ((dx, dy) as dest) ({field} as world) =
  let rec marks = initMarks field.size
  and checkSteps p cost path =
    let checkStep (l, r) (step, stepCost) =
      let ((x, y) as pn) = ptAdd p step
      and newcost = cost +. stepCost
      in if (x == dx && y == dy)
        then ([(List.rev (pn :: path), newcost)], r)
        else if (not (isClean world pn) || marks.(x).(y) < newcost)
        then (l, r)
        else (l, (pn, newcost) :: r)
    in List.fold_left checkStep ([], []) stepCosts
  and bestDist p cost path =
    let (l, r) = checkSteps p cost path
    in let rsorted = List.sort (fun (pn, _) (pn2, _) -> compare (dumbDist pn dest) (dumbDist pn2 dest)) r
       and getPath acc ((x, y) as pn, newcost) = match acc with
         | [] ->
             marks.(x).(y) <- newcost;
             bestDist pn newcost (pn :: path)
         | l -> l
      in l @ List.fold_left getPath [] rsorted
  in bestDist orig 0. []
