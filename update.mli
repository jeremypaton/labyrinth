open Constants
open Player
open Monster

(** position is a tuple of ints- the coordinates **)
type position= (int*int)
type key = char

(** game progress is a variant of strings: in_progress, win, lose, unstarted **)
type game_progress= Constants.game_progress

(** game_state is a record that holds the level number, game progress, monster
 **  position, player position, and time **)
type game_state= Constants.game_state

(** main update function that takes in the current game state and returns a new
 ** game state **)
val main_update: game_state -> key list -> game_state


(** also included in the main .ml file will be helper functions to get game
 ** progress **)
