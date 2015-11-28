open Constants
open Dijkstra

(** type wait_left is the duration of time left before the monster can move
 ** again - is an int ref, defined in .ml **)

(** decision function that takes in move_type and time left and returns the
 ** type of the next move done by the monster- helper for update
val decision: unit -> unit **)

(** a function update_monster_position that takes in current monster position,
 ** player position, move type, and returns a new monster position **)
val update_monster_position: monster -> position -> master_board ->
                             levels_board ->  monster
