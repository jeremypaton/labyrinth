(* define variant move type - Chasing or Random
type move_type= Chasing | Random | Up | Down | Left | Right
type monster= (move_type*(int*int)) *)
(* let wait_left= ref 0

let curr_move_type= ref Chasing *)

(* hard code a monster intelligence list *)


(* let decision()=
  if (!wait_left > 0) then wait_left := !wait_left-1
  else let r= Random.int 100 in
  wait_left := 10;
  if r<75 then curr_move_type := Chasing
     else curr_move_type := Random *)


let update_random y x l=
  if (y==0 && x==0) then
    let r= Random.int 2 in
    if (r==0) then (Constants.Random,(y, x+1))
    else (Random,(y+1,x))
  else if (y==0 && x==l) then
    let r= Random.int 2 in
    if (r==0) then (Random,(y,x-1))
    else (Random, (y+1,x))
  else if (y==l && x==0) then
    let r= Random.int 2 in
    if (r==0) then (Random,(y,x+1))
    else (Random, (y-1,x))
  else if (y==l && x==l) then
    let r= Random.int 2 in
    if (r==0) then (Random,(y,x-1))
    else (Random, (y-1,x))
  else if (y==l) then
    let r= Random.int 3 in
    if (r==0) then (Random,(y,x-1))
    else if (r==1) then (Random,(y,x+1))
    else (Random, (y-1, x))
  else if (y==0) then
    let r= Random.int 3 in
    if (r==0) then (Random,(y,x-1))
    else if (r==1) then (Random,(y,x+1))
    else (Random, (y+1, x))
  else if (x==l) then
    let r= Random.int 3 in
    if (r==0) then (Random,(y,x-1))
    else if (r==1) then (Random,(y+1,x))
    else (Random, (y-1,x))
  else if (x==0) then
    let r= Random.int 3 in
    if (r==0) then (Random,(y,x+1))
    else if (r==1) then (Random,(y+1,x))
    else (Random, (y-1,x))
  else
    let r= Random.int 4 in
    if (r == 0) then (Random,(x+1, y))
    else if (r==1) then (Random,(x-1,y))
    else if (r==2) then (Random,(x,y+1))
    else (Random,(x,y-1))

let update_up y x l (master_board: Constants.master_board)=
  if ((y==l) || (List.nth (List.nth master_board (y+1)) x == false)) then
    (Constants.Down, (y-1,x))
  else (Up, (y+1,x))

let update_down y x l master_board=
  if ((y==0) || (List.nth (List.nth master_board (y-1)) x == false)) then
    (Constants.Up, (y+1,x))
  else (Down, (y-1,x))

let update_left y x l master_board=
  if ((x==0) || (List.nth (List.nth master_board y) (x-1) == false)) then
    (Constants.Right, (y,x+1))
  else (Left, (y,x-1))

let update_right y x l master_board=
  if ((x==l) || (List.nth (List.nth master_board y) (x+1) == false)) then
    (Constants.Left, (y,x-1))
  else (Right, (y,x+1))

let update_monster_position (monster:Constants.monster) player master_board
                            levels_board : Constants.monster=
  let move_type= fst monster in
  let pos= snd monster in
  let monster_y= fst pos in
  let monster_x= snd pos in
  let l= List.length (levels_board) in
  if (move_type == Random) then
    update_random monster_y monster_x l
  else if (move_type == Chasing) then
    let monster_pos= Dijkstra.dijkstra pos player levels_board in
    (Chasing, monster_pos)
  else if (move_type == Up) then
    update_up monster_y monster_x l master_board
  else if (move_type == Down) then
    update_down monster_y monster_x l master_board
  else if (move_type == Left) then
    update_left monster_y monster_x l master_board
  else update_right monster_y monster_x l master_board
