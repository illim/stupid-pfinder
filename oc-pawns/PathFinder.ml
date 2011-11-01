open Models

let stepCosts =
  let ortho = List.map (fun step -> (step, 1.)) [1, 0 ; 0, 1; 0, -1; -1, 0]
  and diag  = List.map (fun step -> (step, 1.414213)) [1, 1; 1, -1; -1, 1; -1, -1]
  in ortho @ diag
;;

let ptAdd (x, y) (i, j) = (x + i, y + j);;

let stupidFind orig (dx, dy) ({field = field} as world) =
  let rec marks = Array.make_matrix field.size field.size 999999.
  and bestDist p cost path =
    List.flatten
      (List.map
         (fun (step, stepCost)->
            let ((x, y) as pn) = ptAdd p step
            and newcost = cost +. stepCost
            in if (x == dx && y == dy)
              then [(path @ [pn], newcost)]
              else if (not (isClean world pn) || marks.(x).(y) < newcost)
              then []
              else begin
                marks.(x).(y) <- newcost;
                bestDist pn newcost (path @ [pn])
              end ) stepCosts)
  in bestDist orig 0. []
;;
