(* gets distance between two points *)
let distance_between (a,b) (x,y) =
  Pervasives.sqrt(float_of_int (((a-x)*(a-x))+((b-y)*(b-y))))

(* gets neighbors of (x,y) given board length l and width w *)
(* gets left-right neighboring positions *)
let get_neighbors_of_x x l y=
  if (x=0) then
  [(y,x+1)]
  else if (x=l) then
  [(y,x-1)]
  else
  [(y,x+1);(y,x-1)]

(* gets vertically neighboring positions *)
let get_neighbors_of_y x l y=
  if (y=0) then
  [(y+1,x)]
  else if (y=l) then
  [(y-1,x)]
  else
  [(y+1,x);(y-1,x)]

let get_neighbors y x l w=
  List.append (get_neighbors_of_x x w y) (get_neighbors_of_y x l y)

(* updates a random monster position (y,x) with board length l and width w *)
let update_random y x l w m_board=
  let neighbors= get_neighbors y x l w in
  let r2= Random.int 2 in
  let r3= Random.int 3 in
  let r4= Random.int 4 in
  let (new_i,new_j)=
  (match List.length neighbors with
  | 0 -> (y,x)
  | 1 -> List.nth neighbors 0
  | 2 -> List.nth neighbors r2
  | 3 -> List.nth neighbors r3
  | _ -> List.nth neighbors r4) in
  let height =List.length m_board in
      let width = List.length (List.hd m_board) in
        if new_i< height && new_i>=0 &&  new_j< width && new_j>= 0
                          && (List.nth (List.nth m_board (new_i)) (new_j))
  then (Constants.Random, (new_i,new_j))
  else (Constants.Random, (y,x))

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
  | Constants.Random -> update_random monster_y monster_x l w master_board
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
