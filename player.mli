open constants
open Graphics

(** the player module contains the update_player_position function **)

(** variant movement_direction is the movement that the player will go in **)
type movement_direction= Left | Right | Up | Down | None

(** function update_player_postiion takes in player position, movement
 ** direction, global master board, and returns a new player position **)
val update_player_postiion: position -> movement_direction -> master_board ->
                            position

(** in the .ml, there will be a helper function that takes in input from I/O
 ** and outputs movement direction based on key press **)
