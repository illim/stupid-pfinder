open Models
open Point
open Batteries_uni

module type FINDER = sig
  type result
  val stupidFind : point -> point -> world -> result
  val printResult : result -> unit
end

module BaseStupidFinder = struct
  type result = (point list * float) list

  let sortBy f xs = List.sort ~cmp:(fun x y -> compare (f x) (f y)) xs

  let printResult result =
    let (bestpath, bestcost) = List.hd (sortBy snd result)
    in Printf.printf "> %a results\n%a: %f\n" Int.print (List.length result) (List.print (Pair.print Int.print Int.print)) bestpath bestcost

  let stepCosts =
    let ortho = List.map (fun step -> step, 1.) [1, 0 ; 0, 1; 0, -1; -1, 0]
    and diag  = List.map (fun step -> step, 1.414213) [1, 1; 1, -1; -1, 1; -1, -1]
    in ortho @ diag

  let initMarks size = Array.make_matrix size size 999999.

end

module Stupidfinder : FINDER = struct
  include BaseStupidFinder

  let stupidFind orig dest ({field} as world) =
    let rec marks = initMarks field.size
    and bestDist p cost path =
      List.flatten
        (List.map
           (fun (step, stepCost)->
              let x, y as pn = add p step
              and newcost = cost +. stepCost
              in if equals pn dest
                then [List.rev (pn :: path), newcost]
                else
                  if isClean world pn && marks.(x).(y) > newcost
                  then begin
                    marks.(x).(y) <- newcost;
                    bestDist pn newcost (pn :: path)
                  end
                  else [])
           stepCosts)
    in bestDist orig 0. []

end

module FastStupidfinder : FINDER = struct
  include BaseStupidFinder

  let dumbDist (a, b) (c, d) = let pow2 x = x * x in pow2 (d - b) + pow2 (c - a)

  let stupidFind orig dest ({field} as world) =
    let rec marks = initMarks field.size
    and checkSteps p cost path =
      let checkStep (l, r) (step, stepCost) =
        let x, y as pn = add p step
        and newcost = cost +. stepCost
        in if equals pn dest
          then [List.rev (pn :: path), newcost], r
          else
            if isClean world pn && marks.(x).(y) > newcost
            then l, (pn, newcost) :: r
            else l, r
      in List.fold_left checkStep ([], []) stepCosts
    and bestDist p cost path =
      let l, r = checkSteps p cost path
      in let rsorted = sortBy (fst |- dumbDist dest) r
         and getPath acc (x, y as pn, newcost) = match acc with
           | [] ->
               marks.(x).(y) <- newcost;
               bestDist pn newcost (pn :: path)
           | l -> l
      in l @ List.fold_left getPath [] rsorted
    in bestDist orig 0. []

end
