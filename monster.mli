open constants

(** define variant type move_type: chasing, random. This designates whether
 ** whether the monster is moving randomly or actively chasing the player **)
type move_type: Chasing | Random

(** type wait_left is the duration of time left before the monster can move
 ** again **)
type wait_left: ref int

(** decision function that takes in move_type and time left and returns the
 ** type of the next move done by the monster- helper for update
val decision: move_type -> int -> move_type **)

(** a function update_monster_position that takes in current monster position,
 ** player position, move type, and returns a new monster position **)
val update_monster_position: position -> position -> move_type -> position
