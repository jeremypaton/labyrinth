open Constants
open Input

type key = char

let get_keys () =
      get_keypresses ()
type direction = Up | Down| Left | Right | Stay


(*Assumption- Top left corner of board is (0,0) *)

let update_player_position (p:position)   (m_board: master_board): position =
          (*master_board = bool list list, key = char, position = (int*int) *)

          let keys = get_keys () in
          let dir = match keys with
                    |[] -> Stay
                    | h::t -> (match h with
                              |'w' |'W' -> Up
                              |'a' |'A' -> Left
                              |'s' | 'S' -> Down
                              |'d'|'D' -> Right
                              | _ -> Stay
                            )

                  in
                  (*Calculate mathematically new_pos, later check if there
                  is oath there*)
          let (new_x,new_y) = match p with
          |(x,y) -> (match dir with
                    |Up -> (x,y-1)
                    |Down-> (x,y+1)
                    |Left -> (x-1,y)
                    |Right -> (x+1,y)
                    |Stay-> (x,y)
          | _ -> failwith "position must be a pair of ints"
        )
        in
        (*Check if the movement to that pos is actually possible, i.e, if it is
        on board, and not a wall*)
      let len_y =List.length master_board in
      let len_x = List.length (List.hd master_board) in
      let new_pos_real =
          if new_x< len_x && new_x>=0 &&  new_y < len_y && new_y>= 0 then
            if (List.nth ( List.nth new_y master_board) new_x )  (*Not a wall*)
              then (new_x,new_y)
            else p
        else p
      in
      new_pos_real
      (*Check wall*)



