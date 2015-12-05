(* define type element, having position, backpointer, distance from start,
 * and weight *)
type element= {position: (int*int);
               mutable back_pointer: (int*int) option;
               mutable dist: float;
               mutable weight: float}

(* helper_for_row turns a row of a levels board from float list list to (pos, float)
 * list *)
let rec helper_for_row list row_n col_n=
  match list with
  | [] -> []
  | h::t -> ((row_n,col_n),h)::(helper_for_row t row_n (col_n+1))

(* turns whole levels board into (pos, float) list list *)
let rec board_to_positions board row_n=
  match board with
  | [] -> []
  | h::t -> (helper_for_row h row_n 0)::(board_to_positions t (row_n + 1))

(* turns row of (pos, float) list list into element list *)
let rec elt_helper_for_row list=
  match list with
  | [] -> []
  | ((a,b),c)::t -> let p= (a,b) in
                    let e= {position= p; back_pointer= None; dist= infinity;
                            weight=c} in
                    e::elt_helper_for_row t

(* turns a whole (pos, float) list list into an element list list *)
let rec board_to_elts board=
  match board with
  | [] -> []
  | h::t -> (elt_helper_for_row h)::(board_to_elts t)

(* changes a levels board into an element list list *)
let put_everything_together board=
  board_to_elts(board_to_positions board 0)

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

(* matches neighboring positions to respective elements *)
let rec match_all_neighbors neighbors board=
  match neighbors with
  | [] -> []
  | h::t -> let y= fst h in
            let x= snd h in
            (List.nth(List.nth board y) x)::match_all_neighbors t board

(* gets all neighbors from a position *)
let get_neighbors position board=
  let y= fst position in
  let x= snd position in
  let l= (List.length board)-1 in
  let w= (List.length(List.nth board 0))-1 in
  let list_of= List.append (get_neighbors_of_x x w y)
                           (get_neighbors_of_y x l y) in
  match_all_neighbors list_of board

(* gets distance from backpointer of an element *)
let get_dist_from_bck pos1 pos2 c=
  let a= fst pos1 in
  let y= fst pos2 in
  let b= snd pos1 in
  let x= snd pos2 in
  abs_float(c*.(sqrt(float_of_int((a-y)*(a-y)+(b-x)*(b-x)))))

(* updates neighbor element if distance to previous element + prev elt distance to
 * start is shorter than element's own distance to start *)
let update_single_neighbor neighbor bck=
  let dist_from_bck= get_dist_from_bck bck.position neighbor.position
                                       neighbor.weight in
  match neighbor.back_pointer with
  | Some x -> ()
  | None ->
    neighbor.dist <- dist_from_bck +. bck.dist;
    neighbor.back_pointer <- Some bck.position

(* updates all neighbors *)
let rec update_all_neighbors neighbors bck=
  match neighbors with
  | [] -> ()
  | h::t -> update_single_neighbor h bck; update_all_neighbors t bck

(* checks if position is in frontier *)
let rec find_in_list element frontier=
  match frontier with
  | [] -> false
  | h::t -> if (h.position = element.position) then true else find_in_list element t

(* adds neighbors to frontier *)
let rec update_frontier frontier neighbors explored=
  match neighbors with
  | [] -> frontier
  | h::t -> if (find_in_list h frontier||find_in_list h explored) then
              update_frontier frontier t explored else
            let new_frontier= h::frontier in
            update_frontier new_frontier t explored

(* removes current element from frontier *)
let rec rem_from_frontier frontier elt board=
  match frontier with
  | [] -> []
  | h::t -> if (h = elt) then t else
            h::(rem_from_frontier t elt board)

(* find closest element to start from frontier *)
let rec find_closest frontier curr_smallest=
  let small= curr_smallest.dist in
  match frontier with
  | [] -> curr_smallest
  | h::t -> if h.dist < small then find_closest t h else
                      find_closest t curr_smallest

(* adds an element to explored *)
let add_to_explored explored elt=
  if List.mem elt explored then explored else List.append explored [elt]

(* finds an element in element board based on position *)
let find_elt_in_board pos board=
  let y= fst pos in
  let x= snd pos in
  List.nth (List.nth board y) x

(* checks if target element is in explored *)
let rec is_target_there player explored=
  match explored with
  | [] -> false
  | h::t -> if (h.position= player) then true else is_target_there player t

(* finds target element in explored based on position*)
let rec find_elt_in_explored pos explored=
  match explored with
  | [] -> failwith "not found"
  | h::t -> if (h.position = pos) then h else find_elt_in_explored pos t

let rec helper_dijkstra board player frontier explored=
  (* if frontier empty or if finish explored *)
  if (List.length frontier == 0) then explored
  else
  let infinite_elt= {position= (0,0); back_pointer= None; dist= infinity;
                     weight= infinity} in
  (* get closest position from frontier *)
  let pos= find_closest frontier infinite_elt in
  (* get position of closest element *)
  let pos_coord= pos.position in
  (* add pos to explored *)
  let new_explored= add_to_explored explored pos in
  if (is_target_there player explored) then explored else
  (* get neighbors *)
  let neighbors= get_neighbors pos_coord board in
  update_all_neighbors neighbors pos;
  (* update frontier, run again *)
  let add_frontier= update_frontier frontier neighbors new_explored in
  let new_frontier= rem_from_frontier add_frontier pos board in
  helper_dijkstra board player new_frontier new_explored

(* create a path based on elements and backpointers *)
let rec add_to_path path elt board=
  match elt.back_pointer with
  | None -> path
  | Some x -> let new_elt= find_elt_in_board x board in
              let new_path= x::path in
              add_to_path new_path new_elt board

let build_path explored finish board=
  let finish_elt= find_elt_in_explored finish explored in
  let path= add_to_path [] finish_elt board in
  path

let dijkstra_path monster player board=
  let monster_elt= {position= monster; back_pointer= None; dist= 0.;
                    weight= 1.} in
  (* get explored list *)
  let explored= helper_dijkstra board player [monster_elt] [] in
  explored

(* create a path based on elements and backpointers *)
let rec add_to_path path elt explored=
  match elt.back_pointer with
  | None -> path
  | Some x -> let new_elt= find_elt_in_explored x explored in
              let new_path= x::path in
              add_to_path new_path new_elt explored

let build_path explored finish=
  let finish_elt= find_elt_in_explored finish explored in
  let path= add_to_path [finish_elt.position] finish_elt explored in
  path

(* gets first position move of path calculated by efficient path algorithm *)
let dijkstra monster player board=
  if (monster = player) then player else
    let real_board= put_everything_together board in
    let explored= dijkstra_path monster player real_board in
    let path= build_path explored player in
    (List.nth path 1)
