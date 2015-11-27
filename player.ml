open Constants
type key = char

(* let get_keys () = *)
      (* get_keypresses () *)
type direction = Up | Down| Left | Right | Stay


(*Assumption- Top left corner of board is (0,0) *)

let update_player_position (p:position)  (k_list:key list) (m_board: master_board): position =
          (*master_board = bool list list, key = char, position = (int*int) *)
Printf.printf "%s\n%!" ("updating player position");
          let keys = k_list in
          let dir = match keys with
                    |[] -> Printf.printf "%s\n%!" ("STAY"); Stay
                    | h::t -> (match h with
                              |'w' |'W' -> Printf.printf "%s\n%!" ("UP"); Up
                              |'a' |'A' -> Printf.printf "%s\n%!" ("LEFT"); Left
                              |'s' |'S' -> Printf.printf "%s\n%!" ("DOWN"); Down
                              |'d' |'D' -> Printf.printf "%s\n%!" ("RIGHT"); Right
                              | _ -> Stay
                            )

                  in
                  (*Calculate mathematically new_pos, later check if there
                  is oath there*)
          let (new_y,new_x) = match p with
          |(y,x) -> (match dir with
                    |Up -> (y-1,x)
                    |Down-> (y+1,x)
                    |Left -> (y,x-1)
                    |Right -> (y,x+1)
                    |Stay-> (y,x)

        )
        in
        (*Check if the movement to that pos is actually possible, i.e, if it is
        on board, and not a wall*)
      let len_y =List.length m_board in
      let len_x = List.length (List.hd m_board) in
      let new_pos_real =
          if new_x< len_x && new_x>=0 &&  new_y < len_y && new_y>= 0 then
            if (List.nth ( List.nth m_board (new_y)) (new_x) )  (*Not a wall*)
              then (new_y,new_x)
            else let _ = Printf.printf "%s\n%!" ("THERES A WALL AT "^(string_of_int new_x)^", "^(string_of_int new_y)) in p
        else let _ = Printf.printf "%s\n%!" ("CAN'T RUN OFF THE BOARD") in p
      in
      new_pos_real
      (*Check wall*)



