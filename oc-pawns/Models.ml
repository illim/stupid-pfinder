module Point = struct
  type point = int * int

  let add (x, y) (i, j) = x + i, y + j
  let equals (x, y) (i, j) = x == i && y == j
end

open Point

type entity = { position : point }
and field = { size : int;  heights : (point, int) Hashtbl.t }
and world = { field : field; entities : entity list}

let isClean { field ; entities } (x, y) =
  (x > -1
   && x < field.size
   && y > -1
   && y < field.size
   && List.for_all (fun e -> let (i,j) = e.position in i != x || j != y) entities)

