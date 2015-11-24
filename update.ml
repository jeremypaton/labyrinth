open player
open monster
open input

(** position is a tuple of ints- the coordinates **)
type position= (int*int)

(** game progress is a variant of strings: in_progress, win, lose, unstarted **)
type game_progress=  In_progress | Won | Lost | Unstarted

(** game_state is a record that holds the level number, game progress, monster
 **  position, player position, and time **)
type game_state= {level_number: int;
                  game_progress: game_progress;
                  player_position: position;
                  monster_postiion: position;
                  time: int}

let update_play game =
  let master_b = Constants.get_master game.level_number in
  let level_b = Constants.get_level game.level_number in
  let p_pos = Player.update_player_position game.player_position
                                           (Input.get_keypresses)
                                            master_b in
  let m_pos = Monster.update_monster_position game.monster_postiion
                                              p_pos (*maybe game.player_position*)
                                              game.level_number
                                              level_b
                                              game.level_number in
  let t = game.time - 1 in
  let state = if t <= 0 then Lost else In_progress in

  {game with game_progress = state;
             player_position = p_pos;
             monster_position = m_pos;
             time = t}

let update_won game =
  match List.mem 'r' Input.get_keypresses with
  | false -> game
  | true -> Constants.init_game 0

let update_lost game =
    match List.mem 'r' Input.get_keypresses with
  | false -> game
  | true -> Constants.init_game game.level_number

let update_paused game =
  match List.mem ' ' Input.get_keypresses with
  | false -> game
  | true -> {game with game_progress = In_progress}


let main_update game =
  match game.game_progress with
  | In_progress -> update_play game
  | Won -> update_won game
  | Lost -> update_lost game
  | Unstarted -> update_paused game