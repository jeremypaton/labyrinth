type master_board= bool list list

type levels_board= float list list

type position= (int*int)

type game_progress= In_progress | Won | Lost | Unstarted

type game_state= {level_number: int;
                  game_progress: game_progress;
                  player_position: position;
                  monster_position: position list;
                  time: int}

type level = {levels_board: levels_board;
              master_board: master_board;
              player_start: position list;
              monster_start: position list;
              time: int}

type element = Wall | Path | Monster | Player
type design = element list list

let gen_lvl_board design wall_weight =
  List.map (List.map (fun x -> if x = Wall then wall_weight else 1.0)) design

let gen_master_board design =
  List.map (List.map (fun x -> if x = Wall then false else true)) design

let get_element_positions design element =
  let rec search_row element row r c acc =
    match row with
    | [] -> acc
    | x::xs -> search_row element xs r (c+1) (acc @ (if x = element then [(r,c)]
                                                                    else [])) in
  let rec search element rows r acc =
    match rows with
    | [] -> acc
    | x::xs -> (search_row element x r 0 []) @ (search element xs (r+1) acc) in
  search element design 0 []

let find_errors tentative =
  let rows_l = if List.length tentative.levels_board > 0 then
               List.length (List.hd tentative.levels_board)
               else 0 in
  let rows_m = if List.length tentative.master_board > 0 then
               List.length (List.hd tentative.master_board)
               else 0 in
  let l_size = rows_l > 0 in
  let l_rect = List.fold_left (fun a b -> a && List.length b = rows_l) true
                                tentative.levels_board in
  let m_size = rows_m > 0 in
  let m_rect = List.fold_left (fun a b -> a && List.length b = rows_m) true
                                tentative.master_board in
  let player = List.length tentative.player_start = 1 in
  let monsters = List.length tentative.monster_start > 0 in
  let t = tentative.time > 0 in

  match l_size,l_rect,m_size,m_rect,player,monsters,t with
  | false,_,_,_,_,_,_ -> failwith "level must be non-empty"
  | _,false,_,_,_,_,_ -> failwith "level must be rectangular"
  | _,_,false,_,_,_,_ -> failwith "masterboard must be non-empty"
  | _,_,_,false,_,_,_ -> failwith "masterboard must be rectangular"
  | _,_,_,_,false,_,_ -> failwith "there must be one (and only one) player"
  | _,_,_,_,_,false,_ -> failwith "there must be a monster"
  | _,_,_,_,_,_,false -> failwith "time must be greater than 0"
  | _,_,_,_,_,_,_     -> None

let gen_lvl design wall_weight time=
  let tentative_level = {levels_board= gen_lvl_board design wall_weight;
                         master_board= gen_master_board design;
                         player_start= get_element_positions design Player;
                         monster_start= get_element_positions design Monster;
                         time = time} in
  if find_errors tentative_level = None then tentative_level
                                        else failwith "invalid level"

(******************************************************************************
 ******************************************************************************
 ****                       do not edit above                              ****
 ****           design levels below here, following the examples           ****
 ****     remember to edit the "retrieve" match statement at the bottom    ****
 ******************************************************************************
 ******************************************************************************)

(*alliases for designing boards*)
let x = Wall
let o = Path
let m = Monster
let p = Player

let lvl0 =
  let design = [[x;x;x;x;o;x;x;x;x];
                [x;x;x;x;o;x;x;x;x];
                [x;x;x;x;o;x;x;x;x];
                [x;x;x;x;o;x;x;x;x];
                [m;o;o;o;p;o;o;o;o];
                [x;x;x;x;o;x;x;x;x];
                [x;x;x;x;o;x;x;x;x];
                [x;x;x;x;o;x;x;x;x];
                [x;x;x;x;o;x;x;x;x]]
  in gen_lvl design 1000.0 20

let lvl1 =
  let design = [[x;x;x;x;o;x;x;x;x];
                [x;x;x;x;o;x;o;o;x];
                [x;o;o;o;o;x;x;o;x];
                [x;o;x;x;o;x;x;o;x];
                [m;o;o;o;o;o;o;o;o];
                [x;o;x;x;o;x;x;o;x];
                [x;o;x;x;o;o;o;p;x];
                [x;o;o;o;o;x;x;o;x];
                [x;x;x;x;o;x;x;o;x]]
  in gen_lvl design 2.0 15

let lvl2 =
  let design = [[x;o;o;o;o;o;x;x;o;o;o;o;o;x;o;o;o;o;o;x];
                [x;o;x;x;x;o;x;x;x;o;x;x;o;x;o;x;o;x;o;x];
                [x;o;o;o;o;o;x;x;x;o;x;x;o;x;o;x;o;x;o;o];
                [x;x;x;x;x;o;o;o;o;o;o;x;o;x;o;o;o;o;o;x];
                [x;x;x;x;x;o;x;x;x;x;o;x;o;o;o;o;x;x;x;x];
                [x;x;x;x;x;o;x;o;o;o;o;o;o;x;x;o;o;x;x;x];
                [x;o;o;x;x;o;x;x;o;x;x;x;o;x;x;x;o;o;x;x];
                [x;o;x;x;x;o;o;o;o;o;o;o;o;x;o;x;x;o;o;o];
                [o;o;o;o;o;o;x;x;o;x;x;x;o;x;o;x;x;x;o;o];
                [x;o;x;x;o;o;o;x;o;o;o;o;o;o;o;o;x;x;o;o];
                [x;o;o;x;x;o;o;o;x;x;o;o;o;x;o;x;x;x;x;o];
                [x;o;x;x;x;x;x;o;o;o;o;m;o;x;o;x;o;x;x;o];
                [o;o;x;x;x;x;x;o;x;x;o;o;o;x;o;o;o;x;x;o];
                [o;x;x;x;x;x;x;x;x;o;o;o;x;x;x;x;o;x;x;o];
                [o;o;o;o;o;o;o;o;x;x;o;x;o;o;o;o;o;o;o;o];
                [x;o;o;x;x;x;o;x;o;o;o;x;x;o;x;x;o;x;x;x];
                [x;o;o;x;x;x;o;o;o;x;o;x;x;o;x;o;o;o;x;x];
                [x;o;o;o;o;o;p;x;x;x;o;x;x;o;x;o;x;o;x;x];
                [x;o;o;x;x;o;x;o;o;o;o;x;x;o;o;o;o;o;o;o];
                [x;o;o;x;x;x;x;o;x;x;x;o;o;o;x;x;x;o;x;o];
                [x;o;x;x;x;o;o;o;o;x;x;x;x;o;x;x;x;o;x;o];
                [o;o;o;x;x;x;x;x;o;x;x;x;x;o;x;x;x;o;o;o];
                [x;x;o;o;o;o;o;x;o;o;o;o;o;o;x;x;x;x;x;x]]
  in gen_lvl design 4.0 60

let retrieve lvl =
  match lvl with
  |0 -> Some lvl0
  |1 -> Some lvl1
  |2 -> Some lvl2
  |_ -> None

(******************************************************************************
 ******************************************************************************
 ****                                                                      ****
 ****                      do not edit below here                          ****
 ****                                                                      ****
 ******************************************************************************
 ******************************************************************************)

let get_master lvl =
  match retrieve lvl with
  | Some l -> Some l.master_board
  | None -> None

let get_weights lvl =
  match retrieve lvl with
  | Some l -> Some l.levels_board
  | None -> None

let init_level lvl =
  match retrieve lvl with
  | Some l -> Some
                 {level_number= lvl;
                  game_progress= Unstarted;
                  player_position= List.hd l.player_start;
                  monster_position= l.monster_start;
                  time= l.time}
  | None -> None
