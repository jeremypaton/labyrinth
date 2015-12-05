(* gets distance between two points *)
let distance_between (a,b) (x,y) =
  Pervasives.sqrt(float_of_int (((a-x)*(a-x))+((b-y)*(b-y))))

(* updates a random monster position (y,x) with board length l and width w *)
let update_random y x l w=
  let r2= Random.int 2 in
  let r3= Random.int 3 in
  let r4= Random.int 4 in
  let pos =
  (match y,x with
  |0,0 -> if (r2=0) then (y, x+1)
          else (y+1,x)
  |0,x when x=w -> if (r2=0) then (y,x-1)
                    else (y+1,x)
  |y,0 when y=l -> if (r2=0) then (y,x+1)
                    else  (y-1,x)
  |y,x when (y=l && x=w) -> if (r2=0) then (y,x-1)
                    else (y-1,x)
  |y,_ when y=l -> if (r3=0) then (y,x-1)
                    else if (r3=1) then (y,x+1)
                    else (y-1, x)
  |0,_-> if (r3=0) then (y,x-1)
         else if (r3=1) then (y,x+1)
         else  (y+1, x)
  |_,x when x=w-> if (r3=0) then (y,x-1)
                    else if (r3=1) then (y+1,x)
                    else (y-1,x)
  |_,0 -> if (r3=0) then (y,x+1)
          else if (r3=1) then (y+1,x)
          else (y-1,x)
  |_,_ -> if (r4 =0) then (x+1, y)
          else if (r4=1) then (x-1,y)
          else if (r4=2) then (x,y+1)
          else (x,y-1)
  ) in (Constants.Random,pos)

(* updates up moving monster *)
let update_up y x l (master_board: Constants.master_board)=
  if ((y==0) || (y > 0 && (List.nth (List.nth master_board (y-1)) x == false))) then
    (Constants.Down, (y,x))
  else (Constants.Up, (y-1,x))

(* updates down moving monster *)
let update_down y x l master_board=
  if ((y==l) || (y < l && (List.nth (List.nth master_board (y+1)) x == false))) then
    (Constants.Up, (y,x))
  else (Constants.Down, (y+1,x))

(* updates left moving monster *)
let update_left y x l master_board=
  if ((x==0) || (x > 0 && (List.nth (List.nth master_board y) (x-1) == false))) then
    (Constants.Right, (y,x))
  else (Constants.Left, (y,x-1))

(* updates right moving monster *)
let update_right y x l master_board=
  if ((x==l) || (x < l && (List.nth (List.nth master_board y) (x+1) == false))) then
    (Constants.Left, (y,x))
  else (Constants.Right, (y,x+1))

(* updates radius monster *)
let update_radius m_pos m_center player l w master_board levels_board =
  if distance_between m_pos player <= Constants.monster_radius
  then
    (* if within 5 distance of player use Dijkstra else greedy search *)
    if distance_between m_pos player <= 5. then
    let monster_pos= Dijkstra.dijkstra m_pos player
                     levels_board in (Constants.Radius m_center, monster_pos)
    else let monster_pos= Greedysearch.greedy m_pos player levels_board in
                          (Constants.Radius m_center, monster_pos)
  else
    let monster_pos= Dijkstra.dijkstra m_pos m_center
                     levels_board in (Constants.Radius m_center, monster_pos)

(* updates circle monster *)
let update_circle m_pos m_center player l w master_board levels_board=
  if distance_between m_pos (m_center) <= Constants.monster_radius
  then
    (* if within 5 distance units of player use Dijkstra else use greedy search
     * algorithm *)
    if distance_between m_pos player <= 5. then
    let monster_pos= Dijkstra.dijkstra m_pos player
                     levels_board in (Constants.Circle m_center, monster_pos)
    else let monster_pos= Greedysearch.greedy m_pos player levels_board in
                          (Constants.Circle m_center, monster_pos)
  else
    let monster_pos= Dijkstra.dijkstra m_pos m_center
                     levels_board in (Constants.Circle m_center, monster_pos)

(* updates chasing monster *)
let update_chasing m_pos player levels_board=
    (* if within 5 distance of player use Dijkstra else use greedy search algorithm *)
    if distance_between m_pos player <= 5. then
    let monster_pos= Dijkstra.dijkstra m_pos player
                     levels_board in (Constants.Chasing, monster_pos)
    else let monster_pos= Greedysearch.greedy m_pos player levels_board in
                          (Constants.Chasing, monster_pos)



let update_monster_position (monster:Constants.monster) player master_board
                            levels_board : Constants.monster=
  let pos= snd monster in
  if pos = player
  then monster
  else
  let move_type= fst monster in
  let monster_y= fst pos in
  let monster_x= snd pos in
  let l= (List.length (levels_board))-1 in
  let w= (List.length (List.nth levels_board 0))-1 in
  match move_type with
  | Constants.Random -> update_random monster_y monster_x l w
  | Constants.Chasing -> let monster_pos= Dijkstra.dijkstra pos player
                                          levels_board in
                         (Constants.Chasing, monster_pos)
  | Constants.Up -> update_up monster_y monster_x l master_board
  | Constants.Down -> update_down monster_y monster_x l master_board
  | Constants.Left -> update_left monster_y monster_x w master_board
  | Constants.Right -> update_right monster_y monster_x w master_board
  | Constants.Circle m_center -> update_circle pos
                                       m_center player l w master_board
                                       levels_board
  | Constants.Radius m_center -> update_radius pos
                                       m_center player l w master_board
                                       levels_board
