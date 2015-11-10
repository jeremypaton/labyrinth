open player
open monster

(** position is a tuple of ints- the coordinates **)
type position= (int*int)

(** game progress is a variant of strings: in_progress, win, lose, unstarted **)
type game_progress= | In_progress | Won | Lost | Unstarted

(** game_state is a record that holds the level number, game progress, monster
 **  position, player position, and time **)
type game_state= {level_number: int;
                  game_progress: game_progress;
                  player_position: position;
                  monster_postiion: position;
                  time: int}

(** main update function that takes in the current game state and returns a new
 ** game state **)
val main_update: game_state -> game_state

(** also included in the main .ml file will be helper functions to get game
 ** progress **)
