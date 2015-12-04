open Constants

(*column-order matrix co-ordinates *)
let update_player_position ((i,j):position)  (keys:char list)
                           (m_board: master_board): position =
          let (new_i,new_j) = match keys with
                    |[] -> (i,j)
                    | h::t -> (match h with
                              |'w' |'W' -> (i-1,j) (*up*)
                              |'s' |'S' -> (i+1,j) (*down*)
                              |'a' |'A' -> (i,j-1) (*left*)
                              |'d' |'D' -> (i,j+1) (*right*)
                              | _ -> (i,j))
                  in
      (*check on board and not a wall*)
      let height =List.length m_board in
      let width = List.length (List.hd m_board) in
        if new_i< height && new_i>=0 &&  new_j< width && new_j>= 0
                          && (List.nth (List.nth m_board (new_i)) (new_j))
        then (new_i,new_j)
        else (i,j)




