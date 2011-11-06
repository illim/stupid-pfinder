open Models

module SF = PathFinder.Stupidfinder
module FSF = PathFinder.FastStupidfinder

let basicWorld = {
  field = {
    size = 10;
    heights = Hashtbl.create 0
  };
  entities = List.map (fun i -> {position = (7, i)}) [3; 4; 5; 6; 7; 8]
}

let main =
  SF.printResult(SF.stupidFind (3, 3) (8, 6) basicWorld);
  FSF.printResult(FSF.stupidFind (3, 3) (8, 6) basicWorld)
