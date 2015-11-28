open Constants
open Player
open Monster

(** position is a tuple of ints- the coordinates **)
type position= (int*int)
type key= char

(** game progress is a variant of strings: in_progress, win, lose, unstarted **)
type game_progress= Constants.game_progress

(** game_state is a record that holds the level number, game progress, monster
 **  position, player position, and time **)
type game_state= Constants.game_state

let update_play (game:game_state) keys =
  if List.mem ' ' keys
  then let _ = Printf.printf "%s\n%!" ("Game Paused") in {game with game_progress = Unstarted}
  else
  let master_b = match (Constants.get_master game.level_number) with
                 | Some x -> x
                 | None -> failwith "update.ml : no master board!" in
  let level_b = match (Constants.get_weights game.level_number) with
                 | Some x -> x
                 | None -> failwith "update.ml : no level board!" in
  let p_pos = Player.update_player_position game.player_position
                                           (keys)
                                            master_b in
  let f m = Monster.update_monster_position m p_pos master_b level_b in
  let updated_monsters = List.map f game.monster_position in
  let t = game.time - 1 in
  let state = Constants.In_progress(*if t <= 0 then Constants.Won else Constants.In_progress*) in
  {game with game_progress = state;
             player_position = p_pos;
             monster_position = updated_monsters;
             time = t}

let update_won (game:game_state) keys =
  match List.mem 'r' keys with
  | false -> game
  | true -> match (Constants.init_level 0) with
            | Some x -> x
            | None -> failwith "no level 0"

let update_lost (game:game_state) keys =
    match List.mem 'r' keys with
  | false -> game
  | true -> match (Constants.init_level game.level_number) with
            | Some x -> x
            | None -> failwith "no level to reset"

let update_paused (game:game_state) keys =
  match (List.mem ' ' keys), (List.mem '[' keys), (List.mem ']' keys)  with
  | true,_,_ -> Printf.printf "%s\n%!" ("Game Started");
                {game with game_progress = In_progress}
  | _,true,_ -> let i = game.level_number - 1 in
                let lvl = if is_level i then i else game.level_number in
                Printf.printf "%s\n%!" ("Level " ^ (string_of_int lvl));
                (match (Constants.init_level lvl) with
                | Some x -> x
                | None -> failwith "no level to reset")

  | _,_,true -> let i = game.level_number + 1 in
                let lvl = if is_level i then i else game.level_number in
                Printf.printf "%s\n%!" ("Level " ^ (string_of_int lvl));
                (match (Constants.init_level lvl) with
                | Some x -> x
                | None -> failwith "no level to reset")
  |_,_,_ -> game

let main_update (game:game_state) keys =
  match game.game_progress with
  | In_progress -> update_play game keys
  | Won -> update_won game keys
  | Lost -> update_lost game keys
  | Unstarted -> update_paused game keys
