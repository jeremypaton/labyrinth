(* define variant move type - Chasing or Random *)
type move_type= Chasing | Random

let wait_left= ref 0

let curr_move_type= ref Chasing

(* hard code a monster intelligence list *)


let decision()=
  if (!wait_left > 0) then wait_left := !wait_left-1
  else let r= Random.int 100 in
  wait_left := 10;
  if r<75 then curr_move_type := Chasing
     else curr_move_type := Random

let update_monster_position monster player level levels_board=
  let monster_x= fst monster in
  let monster_y= snd monster in
  let player_x= fst player in
  let player_y= snd player in
  if (!curr_move_type == Random) then
    let r= Random.int 4 in
    if (r == 0) then (monster_x+1, monster_y)
      else if (r==1) then (monster_x-1,monster_y)
        else if (r==2) then (monster_x,monster_y+1)
          else (monster_x,monster_y-1)
  else
    Dijkstra.dijkstra monster player levels_board
