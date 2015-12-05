(* get horizontal neighbors of a position (y,x) *)
let get_neighbors_of_x x l y=
  if (x=0) then
  [(y,x+1)]
  else if (x=l) then
  [(y,x-1)]
  else
  [(y,x+1);(y,x-1)]

(* get vertical neighbors of a position (y,x) *)
let get_neighbors_of_y x l y=
  if (y=0) then
  [(y+1,x)]
  else if (y=l) then
  [(y-1,x)]
  else
  [(y+1,x);(y-1,x)]

(* match neighbors from the board and put in (pos, weight*dist to player) tuple format *)
let rec match_all_neighbors neighbors board player=
  let a= fst player in
  let b= snd player in
  match neighbors with
  | [] -> []
  | h::t -> (let y= fst h in
            let x= snd h in
            let c= (List.nth(List.nth board y) x) in
            ((y,x), (abs_float(c*.(sqrt(float_of_int((a-y)*(a-y)+(b-x)*(b-x)))))))::
              match_all_neighbors t board player)

(* get all neighbors *)
let get_neighbors position board player=
  let y= fst position in
  let x= snd position in
  let l= (List.length board)-1 in
  let w= (List.length(List.nth board 0))-1 in
  let list_of= List.append (get_neighbors_of_x x w y)
                           (get_neighbors_of_y x l y) in
  match_all_neighbors list_of board player

(* get closest neighbors to player *)
let rec find_closest neighbors curr_smallest=
  let small= snd curr_smallest in
  match neighbors with
  | [] -> curr_smallest
  | ((a,b),c)::t -> if c < small then find_closest t ((a,b),c) else
                      find_closest t curr_smallest

let greedy monster player board=
  let neighbors= get_neighbors monster board player in
  fst(find_closest neighbors ((0,0),infinity))
