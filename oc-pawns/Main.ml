open Models
open PathFinder

let basicWorld = {
  field = {
    size = 10;
    heights = Hashtbl.create 0
  };
  entities = List.map (fun i -> {position = (7, i)}) [3; 4; 5; 6; 7; 8]
}


(* some bs :)) *)
let main =
  let rec print_pathresult (paths, cost) =
    List.iter (fun (x, y) -> Printf.printf "(%i," x; Printf.printf " %i)" y) paths;
    Printf.printf ": %f\n" cost;
  and print_result r = Printf.printf "%i results " (List.length r);
    print_pathresult (List.hd (List.sort (fun (_, x) (_, y) -> compare x y) r))
  and results = (stupidFind (3, 3) (8, 6) basicWorld)
  and results2 = (fastStupidFind (3, 3) (8, 6) basicWorld)
  in print_result results;
    print_result results2
