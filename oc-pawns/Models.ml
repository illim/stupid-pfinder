type point = int * int

type entity = { position : point }

type field = { size : int;  heights : (point, int) Hashtbl.t }

type world = { field : field; entities : entity list}

let isClean {field = field ; entities = entities } (x, y) =
  (x > -1
   && x < field.size
   && y > -1
   && y < field.size
   && List.for_all (fun e -> let (i,j) = e.position in i != x || j != y) entities)
;;

