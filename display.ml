(* Took example of http://caml.inria.fr/pub/docs/oreilly-book/pdf/chap5.pdf. The functions
 * that have been directly copy-pasted from that tutorial are:
   draw_rect, draw_poly, draw_box_outline, draw_box, set_grey *)
open Constants
open Update


(* Exception to be raised in order to close the graph *)
exception End


(* stores information for a box to be drawn on screen, including x and y position,
 * height, border width, and colors *)
type box_data = { x:int; y:int; w:int; h:int; bw:int;
  cmid:Graphics.color; ctop:Graphics.color; cbot:Graphics.color;
  cleft:Graphics.color; cright:Graphics.color}


(* stores info about the displayed image, including resolution (screen size),
 * the scaling of these positions for viewability, and the current
 * x and y positions of the mouse *)
type display = {mutable grid:box_data array; maxx:int; maxy:int; x_sep:int; y_sep:int;
                mutable x : int; mutable y :int; bc : Graphics.color;
                fc: Graphics.color; pc : Graphics.color}


(* draws a rectangle on screen with bottom left point at
 * (x0, y0) and with width and height w and h *)
let draw_rect x0 y0 w h =
  let (a,b) = Graphics.current_point()
  and x1 = x0+w and y1 = y0+h
  in
  Graphics.moveto x0 y0;
  Graphics.lineto x0 y1; Graphics.lineto x1 y1;
  Graphics.lineto x1 y0; Graphics.lineto x0 y0;
  Graphics.moveto a b


(* draws a polygon on screen. The points are defined in the
 * (int*int) array [r] *)
let draw_poly r =
  let (a,b) = Graphics.current_point () in
  let (x0,y0) = r.(0) in Graphics.moveto x0 y0;
  for i = 1 to (Array.length r)-1 do
  let (x,y) = r.(i) in Graphics.lineto x y
  done;
  Graphics.lineto x0 y0;
  Graphics.moveto a b


(* draw the outline of the box in the box_data [bcf] *)
let draw_box_outline (bcf:box_data) col =
  Graphics.set_color col;
  draw_rect bcf.x bcf.y bcf.w bcf.h


(* draw the fill of the box in box_data [bcf] *)
let draw_box (bcf:box_data) =
  Graphics.set_color bcf.cmid;
  Graphics.fill_rect bcf.x bcf.y bcf.w bcf.h;
  draw_box_outline bcf Graphics.black


(* returns a grey Graphics.rgb. The lower [x], the darker
 * the image. 0 is black, 255 is white *)
let set_gray x = (Graphics.rgb x x x)
let gray1= set_gray 140 and gray2= set_gray 70 and gray3= set_gray 200


(* draw a block on the screen using the given box_data [bcf]. A block
 * has a "bevel" border which makes it look 2.5D. To be used for
 * walls, monsterrs, and the player. *)
let draw_block (bcf:box_data) =
  let x1 = bcf.x and y1 = bcf.y in
  let x2 = x1+bcf.w and y2 = y1+bcf.h in
  let ix1 = x1+bcf.bw and ix2 = x2-bcf.bw
  and iy1 = y1+bcf.bw and iy2 = y2-bcf.bw in

  Graphics.set_color bcf.cbot;
  Graphics.fill_poly
  [| (x1,y1);(ix1,iy1);(ix2,iy1);(ix2,iy2);(x2,y2);(x2,y1) |];

  Graphics.set_color bcf.ctop;
  Graphics.fill_poly
  [| (x1,y1);(ix1,iy1);(ix1,iy2);(ix2,iy2);(x2,y2);(x1,y2) |];

  Graphics.set_color bcf.cmid;
  Graphics.fill_rect ix1 iy1 (ix2-ix1) (iy2-iy1);

  draw_box_outline bcf Graphics.black


(* draw a block-like object with an arrow tip pointing up/down/right/left.
 * To be used for the zombie monsters (walk in a straight path back
 * and forth. Uses the box_data [bcf]. [invis] is used to determine which
 * side the arrow is on: the arrows whose data is of color [invis] in the bcf
 * will not be drawn. *)
let draw_pointer (bcf:box_data) invis =
  let x1 = bcf.x and y1 = bcf.y in
  let x2 = x1+bcf.w and y2 = y1+bcf.h in
  let ix1 = x1+bcf.bw and ix2 = x2-bcf.bw
  and iy1 = y1+bcf.bw and iy2 = y2-bcf.bw
  and mx = x1+((x2-x1)/2) and my = y1+((y2-y1)/2) in

  let w' = (ix2-ix1)/3 and h' = (iy2-iy1)/3 in
  let x1' = ix1 + w' and x2' = ix2 - w' and y1' = iy1 + h' and y2' = iy2 - h' in
  Graphics.set_color gray3;
  Graphics.fill_poly [|(ix1, iy1); (ix1, iy2); (ix2, iy2)|];
  Graphics.set_color gray2;
  Graphics.fill_poly [|(ix1, iy1); (ix2, iy2); (ix2, iy1)|];
  Graphics.set_color bcf.cmid;
  Graphics.fill_rect x1' y1' (x2'-x1') (y2'-y1');
  draw_box_outline bcf (Graphics.rgb 130 40 40);

  if bcf.ctop <> invis then
    let _ = Graphics.set_color bcf.ctop in
    Graphics.fill_poly [|(ix1, iy2); (ix2, iy2); (mx, y2)|]

  else if bcf.cbot <> invis then
    let _ = Graphics.set_color bcf.cbot in
     Graphics.fill_poly [|(ix1, iy1); (ix2, iy1); (mx, y1)|]

  else if bcf.cleft <> invis then
    let _ = Graphics.set_color bcf.cleft in
     Graphics.fill_poly [|(x1, my); (ix1, iy1); (ix1, iy2)|]

  else if bcf.cright <> invis then
    let _ = Graphics.set_color bcf.cright in
    Graphics.fill_poly [|(x2, my); (ix2, iy1); (ix2, iy2)|]


(* takes a position [pos] to create a box_data to be drawn on display [s].
 * The box_data's size is determined by [scaling] (value of 1 makes box
 * take up an entire space on the grid. value > 1 takes more space. < 1 takes
 * less space). [border] determines the border size of the box_data
 * (drawn differently if it is ultimately drawn using draw_point, draw_block, etc).
 * (value of 2 makes entire box/block a border. Higher is less border). *)
let pos_to_boxdata (s:display) (pos: int*int) (scaling: float) (border: float) =
  let w' = int_of_float ((float_of_int s.x_sep)/.scaling) in
  let h' = int_of_float ((float_of_int s.y_sep)/.scaling) in
  let float_y = float_of_int (fst pos) in
  let float_x = float_of_int (snd pos) in
  let cent_x = int_of_float ((float_x+.0.5)*.(float_of_int s.x_sep)) in
  let cent_y = int_of_float ((float_y+.0.5)*.(float_of_int s.y_sep)) in
  let x' = cent_x - (w'/2) in
  let y' = cent_y - (h'/2) in
  let bw' = int_of_float (float_of_int ((min w' h'))/.border) in
  {x=x'; y=y'; w=w'; h=h'; bw=bw';
  cmid=Graphics.black; ctop=Graphics.black; cbot=Graphics.black; cleft=Graphics.black; cright=Graphics.black}


(* draws the player as a small green bloxk on display [s] at position [pos]. *)
let draw_player (s:display) (pos: int*int) =
  let scaling = 2.75 in
  let border = 2.5 in
  let tempbcf = pos_to_boxdata s pos scaling border in
  let bcf = {tempbcf with cmid=Graphics.rgb 100 200 100;
                          ctop=Graphics.rgb 140 240 170;
                          cbot=Graphics.rgb 80 120 80} in
  draw_block bcf


(* draws a monster on display [s] at position [pos]. The monster
 * is drawn using draw_pointer or draw_block, and with different colors,
 * depending on the move_type [m_type] *)
let draw_monster (s:display) (pos: int*int) (m_type: move_type)=
  let scaling = 1.5 in
  let border = 4. in

  let invis = gray1 in
  let visible = Graphics.blue in
  let tempbcf = pos_to_boxdata s pos scaling border in
  let no_edge = {tempbcf with cmid = gray1; ctop = invis; cbot = invis;
                                cleft = invis; cright = invis} in
  match m_type with
          | Chasing -> draw_block {tempbcf with cmid=Graphics.red; ctop=gray1; cbot=gray2}
          | Random -> draw_block {tempbcf with cmid=Graphics.magenta; ctop = gray1; cbot=gray2}
          | Circle _-> draw_block {tempbcf with cmid=Graphics.cyan; ctop = gray1; cbot=gray2}
          | Radius _-> draw_block {tempbcf with cmid=Graphics.green; ctop = gray1; cbot=gray2}
          | Up -> draw_pointer {no_edge with ctop=visible} invis
          | Down -> draw_pointer {no_edge with cbot=visible} invis
          | Left -> draw_pointer {no_edge with cleft=visible} invis
          | Right -> draw_pointer {no_edge with cright=visible} invis


(* Initialize the board in the display (i.e. draw the grid). Player and
 * monsters are not drawn *)
let draw_board (disp_ref:display ref) () =
  let disp = !disp_ref in
  (* draw background *)
  Graphics.set_color disp.bc;
  Graphics.fill_rect 0 0 (disp.maxx+1) (disp.maxy+1);
  (* draw grid *)
  Array.iter draw_block disp.grid


(* flip the given y position according to the number of rows in
 * [board]. This is because the first row in a board array, which is
 * to be drawn on the upper part of the window, needs to have a y-value
 * close to the window height instead of 0 (since y increases from
 * bottom to top in the GUI). *)
let flip_y pos board=
  let rows = (List.length board) in
  ((rows-fst pos)-1, snd pos)


(* return a list of box_data for the walls of the current level.
 * The positions of the walls are given in [board]: false means a wall
 * is at the current index. *)
let create_grid (s:display) (board: bool list list) (data:box_data) =
  (* determine grid info *)
  let rows = (List.length board) in
  let columns = (List.length (List.hd board)) in
  let bw' = (min s.x_sep s.y_sep)/4 in
  (* loop through all positions on the board *)
  let acc = ref [] in
  for r' = 0 to (rows-1) do
    (* because graphics has y=0 on bottom, but board has y=0 on top *)
    let r = (rows-1)-r' in
    for c = 0 to (columns-1) do
      if (List.nth (List.nth board r) c)=false then
        let new_box = {data with x=s.x_sep*c; y=s.y_sep*r'; w=s.x_sep;
                       h=s.y_sep; bw=bw'} in
        acc := !acc@[new_box]
    done
  done;
  !acc


(* Create the GUI which will follow the display ref s'.
 * All other information is in Constants, including window size *)
let initialize_display (s':display ref) board =
  let maxx' = Constants.resolution_x in
  let maxy' = Constants.resolution_y in
  let rows = (List.length board) in
  let columns = (List.length (List.hd board)) in
  let x_sep' = (maxx')/columns in
  let y_sep' = (maxy')/rows in

  let temp_disp = {grid=[||]; maxx=maxx'; maxy=maxy'; x=60; y=60;
                   bc=Graphics.rgb 130 130 130; fc=Graphics.black;
                   pc=Graphics.red; x_sep=x_sep'; y_sep=y_sep'} in
  let wall_data = {x=0; y=0; w=0;h=0; bw=5; cmid=gray1; ctop=gray3;
                   cbot=gray2; cleft=gray1; cright=gray1} in
  let grid' = Array.of_list (create_grid temp_disp board wall_data) in
  s' := {temp_disp with grid=grid'}


(* Update the display ref [s'] and the game_state [game]
 * to start the level [lvl], as defined in Constants. *)
let begin_the_level (s':display ref) lvl game =
  game := (Constants.init_level lvl);
  initialize_display s' (Constants.get_master lvl)


(* draw the "new" positions of the player and monster.
 * This function is to be called after updating the game_state, hence
 * "new" positions. Always call this after draw_board *)
let draw_new_positions (disp:display) game=
         (* draw player *)
         let board = Constants.get_master !game.level_number in
         let pp = flip_y !game.player_position board in
         draw_player disp pp;
         (* draw monsters *)
         for i = 0 to (List.length !game.monster_position)-1 do
           let monster = List.nth !game.monster_position i in
           let mp = flip_y (snd monster) board in
           draw_monster disp mp (fst monster);
         done


(* Loop function to be called every frame and after each key press
 * initialization. Updates player and monsters positions *)
let draw_loop (disp_ref:display ref) (game:game_state ref) (keys:char list) =
  match keys with
  (* exit program if 'e'/'E' is pressed *)
  | 'e'::_ | 'E'::_ -> raise End
  (* otherwise, run the main update function on key input *)
  | _ -> (* update game by one frame, and change level if appropriate *)
         let cur_lvl = !game.level_number in
         game := Update.main_update !game keys;
         if !game.level_number <> cur_lvl then
            begin_the_level disp_ref !game.level_number game;
         (* 1. draw background and board *)
         draw_board disp_ref ();
         (* 2. drawn player and monsters at their new positions *)
         draw_new_positions !disp_ref game;
         (* 3. draw text for game state, or time left if game in progress *)
         Graphics.moveto (!disp_ref.maxx/2) (!disp_ref.maxy-20);
         let text = ref ("Time Left: "^(string_of_int !game.time)^" - ") in
         let descr = (Constants.get_level_description !game.level_number) in
         let _ = match !game.game_progress with
                 | In_progress -> text:= !text^descr
                 | Won -> text:= !text^"YOU WON! :D   "^descr
                 | Lost -> text:= !text^"YOU LOST D:   "^descr
                 | Unstarted -> text:= !text^"Paused. "^descr
         in
         Graphics.set_color Graphics.black;
         Graphics.moveto 1 1;
         Graphics.draw_string !text;
         Graphics.set_color Graphics.white;
         Graphics.moveto 0 2;
         Graphics.draw_string !text


(* Function to be called every frame to draw the GUI by managing each frame's
 * input (keys, mouse). Note the function is NOT recursive. *)
let main_loop game (disp:display ref) =
  try
    while true do
        let s = Graphics.wait_next_event [Graphics.Key_pressed; Graphics.Poll] in
        if s.Graphics.keypressed then
          let k = Graphics.read_key () in
          draw_loop disp game [k]
    done
  with
    | End -> Graphics.close_graph(); print_string "Game ended by user\n"
    | _ -> () (*Printf.printf "%s\n%!" ("EXCEPTION IN DISPLAY")*)


(* Begins GUI drawing by setting up a display for the first level and opening
 * a graphics library graph, and then calling the main_loop *)
let launch_game (start_level:int) =
  (* setup display for first level *)
  let board = Constants.get_master start_level in
  let disp = ref {grid=[||]; maxx=100; maxy=100; x=60; y=60;
                   bc=Graphics.rgb 130 130 130; fc=Graphics.black;
                   pc=Graphics.red; x_sep=1; y_sep=1} in
  initialize_display disp board;
  (* open GUI *)
  Graphics.open_graph (" " ^ (string_of_int (!disp.maxx)) ^
                       "x" ^ (string_of_int (!disp.maxy)));
  (* draw first frame *)
  let game = ref (Constants.init_level start_level) in
  draw_loop disp game [];
  (* being main drawing loop *)
  main_loop game disp


(* begin the game *)
let _ = launch_game Constants.start_level
