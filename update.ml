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
  then let _ = Printf.printf "%s\n%!" ("pausing game") in {game with game_progress = Unstarted}
  else
  let master_b = match (Constants.get_master game.level_number) with
                 | Some x -> x
                 | None -> failwith "update.ml : no master board!" in
  let level_b = match (Constants.get_weights game.level_number) with
                 | Some x -> x
                 | None -> failwith "update.ml : no level board!" in
  Printf.printf "%s\n%!" ("player position is: "^(string_of_int (snd game.player_position))^", "^(string_of_int (fst game.player_position)) );
  let p_pos = Player.update_player_position game.player_position
                                           (keys)
                                            master_b in
  Printf.printf "%s\n%!" ("player position is: "^(string_of_int (snd p_pos))^", "^(string_of_int (fst p_pos)) );
  let m_pos = Monster.update_monster_position (List.hd game.monster_position)
                                              p_pos (*maybe game.player_position*)
                                              game.level_number
                                              level_b in
  let t = game.time - 1 in
  let state = Constants.In_progress(*if t <= 0 then Constants.Won else Constants.In_progress*) in
  {game with game_progress = state;
             player_position = p_pos;
             monster_position = [m_pos];
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
  match List.mem ' ' keys with
  | false -> game
  | true -> Printf.printf "%s\n%!" ("starting game"); {game with game_progress = In_progress}


let main_update (game:game_state) keys =
  Printf.printf "%s\n%!" ("main_update called");
  match game.game_progress with
  | In_progress -> Printf.printf "%s\n%!" ("in progress"); update_play game keys
  | Won -> Printf.printf "%s\n%!" ("won"); update_won game keys
  | Lost -> Printf.printf "%s\n%!" ("lost"); update_lost game keys
  | Unstarted -> Printf.printf "%s\n%!" ("unstarted"); update_paused game keys
