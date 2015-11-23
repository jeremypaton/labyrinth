(** module display creates a GTK window (our GUI) and updates it according to
 ** what needs to be displayed at any given frame. **)
open GMain
open constants

(** position is a tuple of ints- the coordinates **)
type position= (int*int)

(** game progress is a variant of strings: in_progress, win, lose, unstarted **)
type game_progress= | In_progress | Won | Lost | Unstarted

(** game_state is a record that holds the level number, game progress, monster
 **  position, player position, and time **)
type game_state= {level_number: int;
                  game_progress: game_progress;
                  player_position: position;
                  monster_postiion: position list;
                  time: int}

(** [redraw] takes a drawing_area and a game_state as input, and
 ** accesses the appropriate board information from the constants module to
 ** determine what to draw on the drawing_area **)
val redraw: drawing_area -> game_state -> unit

(** [create_screen] returns a new drawing_area for a new vbox for a new
 ** GWindow. To be called once upon program initialization **)
val create_screen: unit -> drawing_area


(* Note: these function types might change as more is learned about
 * lablgtk *)
