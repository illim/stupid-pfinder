open Models
open PathFinder

let basicWorld = {
  field = {
    size = 10;
    heights = Hashtbl.create 0
  };
  entities = List.map (fun i -> {position = (7, i)}) [3; 4; 5; 6; 7; 8]
}
;;

(* some bs :)) *)
let main =
  let print_result (paths, cost) =
    List.iter (fun (x, y) -> Printf.printf "(%i," x; Printf.printf " %i)" y) paths;
    Printf.printf ": %f\n" cost;
  and results = (stupidFind (3, 3) (8, 6) basicWorld)
  in print_int (List.length results);
    print_string " results";
    print_result (List.hd (List.sort (fun (_, x) (_, y) -> compare x y) results))
;;

