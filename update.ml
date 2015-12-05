open Constants
open Player
open Monster

let update_play (game:game_state) keys =
  if List.mem ' ' keys
  then let _ = Printf.printf "%s\n%!" ("Game Paused") in
  {game with game_progress = Unstarted}
  else
    let master_b = Constants.get_master game.level_number in
    let level_b = Constants.get_weights game.level_number in
    let p_pos = Player.update_player_position game.player_position
                                           (keys)
                                            master_b in
    let f m = Monster.update_monster_position m p_pos master_b level_b in
    let updated_monsters = List.map f game.monster_position in
    let t = game.time - 1 in
    let state =
      let func a b = a || ((snd b) = p_pos)in
      (match t<=0, (List.fold_left func false (updated_monsters)) with
      |true,_ ->let _ = Printf.printf "%s\n%!" ("Game Won") in Constants.Won
      |_,true-> let _ = Printf.printf "%s\n%!" ("Game Lost")in Constants.Lost
      |_,_-> Constants.In_progress
      ) in
    {game with previous = Some game;
             game_progress = state;
             player_position = p_pos;
             monster_position = updated_monsters;
             time = t}

let update_won (game:game_state) keys =
  match List.mem 'r' keys with
  | false -> game
  | true -> Constants.init_level game.level_number

let update_lost (game:game_state) keys =
    match List.mem 'r' keys with
  | false -> game
  | true -> Constants.init_level game.level_number

let update_paused (game:game_state) keys =
  let _ = Printf.printf "%s\n%!"
  ("Pause Level " ^ (string_of_int game.level_number)) in game

let main_update (game:game_state) keys =
  match (List.mem ' ' keys),  (List.mem '[' keys), (List.mem ']' keys),
  (List.mem 'r' keys) , (List.mem 'z' keys)  with
  | true,_,_ ,_,_-> Printf.printf "%s\n%!" ("Game Started");
                {game with game_progress = if game.game_progress = Unstarted
                                           then In_progress
                                           else Unstarted}
  | _,true,_,_,_ -> let i = game.level_number - 1 in
                    let lvl = if is_level i then i else game.level_number in
                    Printf.printf "%s\n%!" ("Level " ^ (string_of_int lvl));
                    Constants.init_level lvl
  | _,_,true,_,_ -> let i = game.level_number + 1 in
                let lvl = if is_level i then i else game.level_number in
                Printf.printf "%s\n%!" ("Level " ^ (string_of_int lvl));
                Constants.init_level lvl
  |_,_,_ ,true,_ -> Constants.init_level game.level_number
  |_,_,_,_,true -> (match game.previous with
                |Some g -> g
               | None -> game)
  |_,_,_,_ ,_->  (match game.game_progress with
                | In_progress -> update_play game keys
                | Won -> update_won game keys
                | Lost -> update_lost game keys
                | Unstarted -> update_paused game keys)
