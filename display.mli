(** module display creates a Graphics window (our GUI) and updates it
 ** according to what needs to be displayed at any given frame. **)
open Constants
open Update

(* Exception to be raised in order to close the graph *)
exception End


(* stores information for a box to be drawn on screen, including x and y
 * position, height, border width, and colors *)
type box_data = { x:int; y:int; w:int; h:int; bw:int;
  cmid:Graphics.color; ctop:Graphics.color; cbot:Graphics.color;
  cleft:Graphics.color; cright:Graphics.color}


(* stores info about the displayed image, including resolution (screen size),
 * background color, the current board grid, and the x and y separations of
 * said grid *)
type display = {mutable grid:box_data array; maxx:int; maxy:int; x_sep:int;
                y_sep:int; bg_col:Graphics.color}


(* Begins GUI drawing by setting up a display for the level [start_level] and
 * opening a graphics library graph, and then calling the main_loop *)
val launch_game: int -> unit


(* Function to be called every frame to draw the GUI related to [disp].
 * Processes any input keys to determine what to draw, and gets the game info
 * from [game].
 * Note the function is NOT recursive. *)
val main_loop: Constants.game_state ref -> display ref -> unit