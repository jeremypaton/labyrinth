let rec helper_for_row list row_n col_n=
  match list with
  | [] -> []
  | h::t -> ((row_n,col_n),h)::(helper_for_row t row_n (col_n+1))

let rec board_to_positions (board: levels_board) row_n=
  match board with
  | [] -> []
  | h::t -> (helper_for_row h row_n 0)::(board_to_positions t (row_n + 1))

let rec weight_helper_for_row list position=
  let x= fst position in
  let y= snd position in
  match list with
  | [] -> []
  | ((a,b),c)::t -> ((a,b),c*.(sqrt(float_of_int((a+x)*(a+x)+(b+y)*(b+y)))))::
                    weight_helper_for_row t position

let rec board_to_weights board position=
  match board with
  | [] -> []
  | h::t -> (weight_helper_for_row h position)::(board_to_weights t position)

let put_everything_together board position=
  board_to_weights(board_to_positions board 0) position

let get_neighbors_of_x x l y=
  if (x=0) then
  [(x+1,y)]
  else if (x=l) then
  [(x-1,y)]
  else
  [(x+1,y);(x-1,y)]

let get_neighbors_of_y x l y=
  if (y=0) then
  [(x,y+1)]
  else if (y=l) then
  [(x,y-1)]
  else
  [(x,y+1);(x,y-1)]

let rec match_neighbor_to_weights neighbor board=
  match board with
  | [] -> []
  | ((a,b),c)::t -> if (a,b)==neighbor then ((a,b),c)
                    else match_neighbor_to_weights neighbor t

let rec match_all_neighbors neighbors board=
  match neighbors with
  | [] -> []
  | h::t -> (match_neighbor_to_weights h board)::match_all_neighbors t board

let get_neighbors position board=
  let x= fst position in
  let y= snd position in
  let l= List.length board in
  let list_of= List.append (get_neighbors_of_x x l y)
                           (get_neighbors_of_y x l y) in
  match_all_neighbors list_of board

let rec update_frontier frontier neighbors=
  match neighbors with
  | [] -> []
  | h::t -> if List.mem h frontier then update_frontier frontier t else
            let new_frontier= h::frontier in
            update_frontier new_frontier t

let rec find_elt_to_rem board pos=
  match board with
  | [] -> []
  | ((a,b),c)::t -> if (a,b)==pos then ((a,b),c) else find_elt_to_rem t pos

let rec rem_from_frontier frontier pos board=
  let elt= find_elt_to_rem board pos in
  match frontier with
  | [] -> []
  | h::t -> if (h==elt) then t else
            h::(rem_from_frontier t pos)

let rec find_closest frontier curr_smallest=
  let x= fst pos in
  let y= snd pos in
  match frontier with
  | [] -> curr_smallest
  | ((a,b),c)::t -> if c < curr_smallest then find_closest t pos ((a,b),c)

let add_to_explored explored pos=
  explored::pos

let rec helper_dijkstra board frontier explored=
  if (List.length frontier == 0) then explored else
  (* get closest position from frontier *)
  let pos= find_closest frontier start
  (* add pos to explored *)
  let new_explored= add_to_explored explored pos in
  let neighbors= get_neighbors pos in
  let add_frontier= update_frontier frontier neighbors in
  let new_frontier= rem_from_frontier add_Frontier pos in
  helper_dijkstra board new_frontier new_explored


let dijkstra (monster: position) (player: position) (board: levels_board)
    : position=
  let working_board= put_everything_together board monster in
  let explored= helper_dijkstra working_board [(monster,0)] [] in
  List.nth explored 0
