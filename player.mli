open Constants
open Input

(** the player module updates the player location based on keypresses **)

(** a key is a char representing a key pressed by the keyboard **)
type key= char

(** function update_player_position takes in player position, movement
 ** direction, global master board, and returns a new player position **)
val update_player_position: position  ->key list ->  master_board ->
                            position

(** takes in input key_list from update **)
