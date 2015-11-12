(** module input determines which keys are pressed at any given frame **)
open Graphics

(** a key is a char representing a key pressed by the keyboard **)
type key= char

(** Returns all the pressed keys in the current frame as a key list **)
val get_keypresses: unit -> key list