type master_board= bool list list

type levels_board= float list list

type position= (int*int)

type game_progress= In_progress | Won | Lost | Unstarted

type move_type= Chasing | Random | Up | Down | Left | Right

type monster = (move_type * position)

type game_state= {previous : game_state option;
                  level_number: int;
                  game_progress: game_progress;
                  player_position: position;
                  monster_position: monster list;
                  time: int}


type level = {levels_board: levels_board;
              master_board: master_board;
              player_start: position list;
              monster_start: monster list;
              time: int}

type element =
|Wall | Path | Player
|MChasing
|MRandom
|MUp
|MDown
|MLeft
|MRight


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

let extract_monsters design m_type element =
  List.map (fun x-> (m_type,x)) (get_element_positions design element)

let gen_test design wall_weight time=
  let mCs = extract_monsters design Chasing MChasing in
  let mMms = extract_monsters design Random  MRandom  in
  let mUs = extract_monsters design Up      MUp      in
  let mDs = extract_monsters design Down    MDown    in
  let mLs = extract_monsters design Left    MLeft    in
  let mRs = extract_monsters design Right   MRight   in
  let monsters = mCs @ mMms @ mUs @ mDs @ mLs @ mRs in
  {levels_board= gen_lvl_board design wall_weight;
                         master_board= gen_master_board design;
                         player_start= get_element_positions design Player;
                         monster_start= monsters;
                         time = time}

let gen_lvl design wall_weight time =
  let tentative_level = gen_test design wall_weight time in
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
let mC = MChasing
let mMm = MRandom
let mU = MUp
let mD = MDown
let mL = MLeft
let mR = MRight
let p = Player

let lvlminus3 =
  let design = [[p]]
  in gen_test design 1000.0 20

let lvlminus2 =
  let design = [[o;o;o];
                [o;p;o];
                [o;o;o]]
  in gen_test design 1000.0 20

let lvlminus1 =
  let design = [[x;x;x];
               [x;p;x];
               [x;x;x]]
  in gen_test design 1000.0 20


let lvl0 =
  let design = [[x;x;x;x;o;x;x;x;x];
                [x;x;x;x;o;x;x;x;x];
                [x;x;x;x;o;x;x;x;x];
                [x;x;x;x;o;x;x;x;x];
                [mR;o;o;o;p;o;o;o;o];
                [x;x;x;x;o;x;x;x;x];
                [x;x;x;x;mU;x;x;x;x];
                [x;x;x;x;o;x;x;x;x];
                [x;x;x;x;o;x;x;x;x]]
  in gen_lvl design 1000.0 20

let lvl1 =
  let design = [[x;x;x;x;o;x;x;x;x];
                [x;x;x;x;o;x;o;o;x];
                [x;o;o;o;o;x;x;o;x];
                [x;o;x;x;o;x;x;o;x];
                [mC;o;o;o;o;o;o;o;o];
                [x;o;x;x;o;x;x;o;x];
                [x;o;x;x;o;o;o;p;x];
                [x;o;o;o;o;x;x;o;x];
                [x;x;x;x;o;x;x;o;x]]
  in gen_lvl design 1000.0 15

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
                [x;o;x;x;x;x;x;o;o;o;o;mR;o;x;o;x;o;x;x;o];
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
  in gen_lvl design 1000.0 60


let lvl3 =
  let design = [[mC;o; x; x; x; x; x; x; x; x; x; x; x; x; x; x; x; x; x; x; x; x; x; x; x; x; x; x; x; x];
                [o; p; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o;mL; o; o; o; o; o; o; o; o];
                [o; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o;mL; o; o; o; o; o; o; o; o];
                [o; x; x; x; x; x; x; x; x; x; o; x; x; x; x; x; x; x; x; x; x; x; x; x; x; x; x; x; x; o];
                [o;mD; o; o; o; o; o; o; o; o; o; o; o; o; o;mL; o; o; o; o; o; o; o; o; o; o; o; o; o;mR];
                [o; o; x; x; o; x; x; x; x; x; o; x; x; o; x; x; x; x; x; x; x; x; x; x; x; x; x; x; x; o];
                [o; o; x; x; o; x; x; x; x; x; o; x; x; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o];
                [o; o; x; x; o; x; x; x; x; x; o; x; x; o; x; x; x; x; x; x; x; x; x; x; x; x; x; x; x; x];
                [o;mD; o; o; o; x; x; x; x; x; o; x; x; o; o; o; o; o; o; o; o; o; o; o; o; o;mR; o; x;mD];
                [o; x; x; x; x; x; x; x; x; x; o; x; x; o; x; o; x; x; o; x; x; o; x; x; o; x; x; o; x; o];
                [o; x; x; x;mD; x; o; x; x; x;mU; x; x; o; x; o; o; o; o;mR; o; o; o; o; o; o; o; o; x; o];
                [o; o; o; o; o; o;mU; o; o; x; o; x; x; o; x; o; x; x; o; x; x; o; x; x; o; x; x;mD; x; o];
                [x; x; x; o; x; o; x; o; x; x; o; x; x; o; x; o; o; o; o; o; o; o; o; o; o; o; o; o; x; o];
                [x; x;mR; o; x; o; x;mL; o; x; o; x; x; o; x; o; x; x; o; x; x; o; x; x; o; x; x; o; x; o];
                [x; x; x; o; x;mD; x; o; x; x; o; x; x; o; x; o; o; o; o; o; o; o; o; o; o; o; o; o; x; o];
                [x; x;mR; o; x; o; x; o;mR; x; o; x; x;mD; x;mU; x; x; o; x; x; o; x; x; o; x; x; o; x; o];
                [x; x; x; o; x; o; x; o; x; x; o; x; x; o; x; o; o; o;mU; o; o; o; o;mR; o; o;mR; o; x; o];
                [x; x;mR; o; x; o; x;mL; o; x;mU; x; x; o; x; o; x; x; o; x; x;mU; x; x; o; x; x; o; x; o];
                [x; x; x; o; x; o; x; o; x; x; o; x; x; o; x; o; o; o; o; o; o;mL; o;mL;mU; o;mL; o; x; o];
                [x; x; o; o; o;mU; o; o; o;mR; o; o; o; o; x; o; o; o; o; o; o; o; o; o; o;mL; o;mU; o; o];
                [x; x; x; x; o; x; o; x; x; x; o; x; x; o; x; x; x; x; x; x; x; x; x; x; x; x; x; x; x; o];
                [x; x; x; x; x; x; x; x; x; x;mR; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o]]
  in gen_lvl design 1000.0 140

  let lvl4 =
  let design = [[p; o; o; o; o; o; o; o; o; o; o;mR; o];
                [o; x; x; o; x; x; o; x; x; o; x; x; o];
                [o; o; o; o;mR; o; o; o; o; o; o; o; o];
                [o; x; x; o; x; x; o; x; x; o; x; x;mD];
                [o; o; o; o; o; o; o; o; o; o; o; o; o];
                [o; x; x; o; x; x; o; x; x; o; x; x; o];
                [o; o; o; o; o; o; o; o; o; o; o; o; o];
                [mU; x; x; o; x; x; o; x; x; o; x; x; o];
                [o; o; o;mU; o; o; o; o;mR; o; o;mR; o];
                [o; x; x; o; x; x;mU; x; x; o; x; x; o];
                [o; o; o; o; o; o;mL; o;mL;mU; o;mL; mC];
                [o; o; o; o; o; o; o; o; o; o;mL; o;mU]]

  in gen_lvl design 1000.0 140




let retrieve lvl =
  match lvl with
  |(-3 )-> Some lvlminus3
  |(-2 )-> Some lvlminus2
  |(-1 )-> Some lvlminus1
  |0 -> Some lvl0
  |1 -> Some lvl1
  |2 -> Some lvl2
  |3 -> Some lvl3
  |4 -> Some lvl4
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
                 {previous = None;
                  level_number= lvl;
                  game_progress= Unstarted;
                  player_position= List.hd l.player_start;
                  monster_position= l.monster_start;
                  time= l.time}
  | None -> None

let is_level lvl =
  match retrieve lvl with
  | None -> false
  |_ -> true
