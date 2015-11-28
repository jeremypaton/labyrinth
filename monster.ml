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


let update_random y x l w=
  if (y==0 && x==0) then
    let r= Random.int 2 in
    if (r==0) then (Constants.Random,(y, x+1))
    else (Constants.Random,(y+1,x))
  else if (y==0 && x==w) then
    let r= Random.int 2 in
    if (r==0) then (Constants.Random,(y,x-1))
    else (Constants.Random, (y+1,x))
  else if (y==l && x==0) then
    let r= Random.int 2 in
    if (r==0) then (Constants.Random,(y,x+1))
    else (Constants.Random, (y-1,x))
  else if (y==l && x==w) then
    let r= Random.int 2 in
    if (r==0) then (Constants.Random,(y,x-1))
    else (Constants.Random, (y-1,x))
  else if (y==l) then
    let r= Random.int 3 in
    if (r==0) then (Constants.Random,(y,x-1))
    else if (r==1) then (Constants.Random,(y,x+1))
    else (Constants.Random, (y-1, x))
  else if (y==0) then
    let r= Random.int 3 in
    if (r==0) then (Constants.Random,(y,x-1))
    else if (r==1) then (Constants.Random,(y,x+1))
    else (Constants.Random, (y+1, x))
  else if (x==w) then
    let r= Random.int 3 in
    if (r==0) then (Constants.Random,(y,x-1))
    else if (r==1) then (Constants.Random,(y+1,x))
    else (Constants.Random, (y-1,x))
  else if (x==0) then
    let r= Random.int 3 in
    if (r==0) then (Constants.Random,(y,x+1))
    else if (r==1) then (Constants.Random,(y+1,x))
    else (Constants.Random, (y-1,x))
  else
    let r= Random.int 4 in
    if (r == 0) then (Constants.Random,(x+1, y))
    else if (r==1) then (Constants.Random,(x-1,y))
    else if (r==2) then (Constants.Random,(x,y+1))
    else (Constants.Random,(x,y-1))

let update_up y x l (master_board: Constants.master_board)=
  if ((y==0) || (y > 0 && (List.nth (List.nth master_board (y-1)) x == false))) then
    (Constants.Down, (y,x))
  else (Constants.Up, (y-1,x))

let update_down y x l master_board=
  if ((y==l) || (y < l && (List.nth (List.nth master_board (y+1)) x == false))) then
    (Constants.Up, (y,x))
  else (Constants.Down, (y+1,x))

let update_left y x l master_board=
  if ((x==0) || (x > 0 && (List.nth (List.nth master_board y) (x-1) == false))) then
    (Constants.Right, (y,x))
  else (Constants.Left, (y,x-1))

let update_right y x l master_board=
  if ((x==l) || (x < l && (List.nth (List.nth master_board y) (x+1) == false))) then
    (Constants.Left, (y,x))
  else (Constants.Right, (y,x+1))

let update_monster_position (monster:Constants.monster) player master_board
                            levels_board : Constants.monster=
  let move_type= fst monster in
  let pos= snd monster in
  let monster_y= fst pos in
  let monster_x= snd pos in
  let l= (List.length (levels_board))-1 in
  let w= (List.length (List.nth levels_board 0))-1 in
  if (move_type == Constants.Random) then
    update_random monster_y monster_x l w
  else if (move_type == Constants.Chasing) then
    let monster_pos= Dijkstra.dijkstra pos player levels_board in
    (Constants.Chasing, monster_pos)
  else if (move_type == Constants.Up) then
    update_up monster_y monster_x l master_board
  else if (move_type == Constants.Down) then
    update_down monster_y monster_x l master_board
  else if (move_type == Constants.Left) then
    update_left monster_y monster_x w master_board
  else update_right monster_y monster_x w master_board
