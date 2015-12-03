(* heavily took example of http://caml.inria.fr/pub/docs/oreilly-book/pdf/chap5.pdf *)
open Constants
open Update
exception End


type relief = Top | Bot | Flat
type box_data = { x:int; y:int; w:int; h:int; bw:int; mutable r:relief;
  cmid:Graphics.color; ctop:Graphics.color; cbot:Graphics.color;
  cleft:Graphics.color; cright:Graphics.color}
(* stores info about the displayed image, including number of possible x and
 * y positions, the scaling of these positions for viewability, and the current
 * x and y positions of the mouse *)
type display = {mutable grid:box_data array; maxx:int; maxy:int; x_sep:int; y_sep:int;
                mutable x : int; mutable y :int; scale:int; bc : Graphics.color;
                fc: Graphics.color; pc : Graphics.color}

let draw_rect x0 y0 w h =
  let (a,b) = Graphics.current_point()
  and x1 = x0+w and y1 = y0+h
  in
  Graphics.moveto x0 y0;
  Graphics.lineto x0 y1; Graphics.lineto x1 y1;
  Graphics.lineto x1 y0; Graphics.lineto x0 y0;
  Graphics.moveto a b


let draw_poly r =
  let (a,b) = Graphics.current_point () in
  let (x0,y0) = r.(0) in Graphics.moveto x0 y0;
  for i = 1 to (Array.length r)-1 do
  let (x,y) = r.(i) in Graphics.lineto x y
  done;
  Graphics.lineto x0 y0;
  Graphics.moveto a b


let draw_box_outline (bcf:box_data) col =
  Graphics.set_color col;
  draw_rect bcf.x bcf.y bcf.w bcf.h


let draw_box (bcf:box_data) =
  Graphics.set_color bcf.cmid;
  Graphics.fill_rect bcf.x bcf.y bcf.w bcf.h;
  draw_box_outline bcf Graphics.black





let set_gray x = (Graphics.rgb x x x)
let gray1= set_gray 140 and gray2= set_gray 70 and gray3= set_gray 200


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
  {x=x'; y=y'; w=w'; h=h'; bw=bw'; r=Top;
  cmid=Graphics.black; ctop=Graphics.black; cbot=Graphics.black; cleft=Graphics.black; cright=Graphics.black}


let draw_player (s:display) (pos: int*int) =
  let scaling = 2.75 in
  let border = 2.5 in
  let tempbcf = pos_to_boxdata s pos scaling border in
  let bcf = {tempbcf with cmid=Graphics.rgb 100 200 100;
                          ctop=Graphics.rgb 140 240 170;
                          cbot=Graphics.rgb 80 120 80} in
  (*Printf.printf "%s\n%!" ("player pos is "^(string_of_int (fst pos))^", "^(string_of_int (snd pos)));*)
  draw_block bcf

let draw_monster (s:display) (pos: int*int) (m_type: move_type)=
  let scaling = 1.5 in
  let border = 4. in
  let tempbcf = pos_to_boxdata s pos scaling border in
  match m_type with
          | Chasing -> draw_block {tempbcf with cmid=Graphics.red; ctop=gray1; cbot=gray2}
          | Random -> draw_block {tempbcf with cmid=Graphics.magenta; ctop = gray1; cbot=gray2}
          | Circle x -> draw_block {tempbcf with cmid=Graphics.green; ctop = gray1; cbot=gray2}
          | Radius x-> draw_block {tempbcf with cmid=Graphics.cyan; ctop = gray1; cbot=gray2}
          | x -> let invis = gray1 in
                 let visible = Graphics.blue in
                 let tempbcf = pos_to_boxdata s pos 1.1 3.5 in
                 let no_edge = {tempbcf with cmid = gray1; ctop = invis; cbot = invis;
                                cleft = invis; cright = invis} in
                 match x with
                 | Up -> draw_pointer {no_edge with ctop=visible} invis
                 | Down -> draw_pointer {no_edge with cbot=visible} invis
                 | Left -> draw_pointer {no_edge with cleft=visible} invis
                 | Right -> draw_pointer {no_edge with cright=visible} invis







let skel (lvl:int) f_draw_board f_loop f_end f_key f_mouse f_except =
  let game = ref (match (Constants.init_level lvl) with
                  | Some x -> x
                  | None -> failwith "display.ml : cannot access level") in
  f_loop game [];
  (*Printf.printf "%s\n%!" ("there are "^(string_of_int (List.length !game.monster_position))^" monsters");*)
  try
    while true do
      (*try*)
        let s = Graphics.wait_next_event [Graphics.Button_down;
                                          Graphics.Key_pressed; Graphics.Poll] in
        if s.Graphics.keypressed then
          let k = Graphics.read_key () in
          f_loop game [k]
        else if s.Graphics.button then
          f_mouse s.Graphics.mouse_x s.Graphics.mouse_y
        (*else
          f_loop game []*)
      (*with
        End -> raise End
        | e -> f_except e*)
    done
  with
  End -> f_end ()


let next_line () =
  let (x,y) = Graphics.current_point()
  in if y>12 then Graphics.moveto 0 (y-12)
  else Graphics.moveto 0 y


let handle_char c = match c with
  |'&' -> raise End
  | '\n' -> next_line ()
  | '\r' -> next_line ()
  | _ -> Graphics.draw_char c

let go () = skel 0
  (fun () -> Graphics.clear_graph (); Graphics.moveto 0 (Graphics.size_y() -12) )
  (fun x y -> ())
  (fun () -> Graphics.clear_graph() )
  handle_char
  (fun x y -> Graphics.moveto x y)
  (fun e -> () )






(* Draw a rectangle with coordinates [(s*x) (s*y) s s] and color c *)
let draw_point x y s c =
  Graphics.set_color c;
  Graphics.fill_rect (s*x) (s*y) s s


(* Initialize the board in the display (i.e. draw the grid). Player and
 * monsters are not drawn *)
let t_draw_board (s':display ref) () =
  let s = !s' in
  (* draw background *)
  Graphics.set_color s.bc;
  Graphics.fill_rect 0 0 (s.scale*s.maxx+1) (s.scale*s.maxy+1);
  (* draw grid *)
  Array.iter draw_block s.grid


let get_master_board lvl=
  match (Constants.get_master lvl) with
  | Some x -> x
  | _ -> failwith "no board"

let flip_y pos board=
  let rows = (List.length board) in
  ((rows-fst pos)-1, snd pos)




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
  (*let _ = print_string "\ncreate_grid personalized is running\n" in
  let _ = print_int (List.length !acc) in*)
  !acc



let initialize_display (s':display ref) board =
  let maxx' = Constants.resolution_x in
  let maxy' = Constants.resolution_y in
  let scale' = 1 in
  let rows = (List.length board) in
  let columns = (List.length (List.hd board)) in
  let x_sep' = (scale'*maxx')/columns in
  let y_sep' = (scale'*maxy')/rows in

  let temp_disp = {grid=[||]; maxx=maxx'; maxy=maxy'; x=60; y=60; scale=scale';
                   bc=Graphics.rgb 130 130 130; fc=Graphics.black;
                   pc=Graphics.red; x_sep=x_sep'; y_sep=y_sep'} in
  let wall_data = {x=0; y=0; w=0;h=0; bw=5; cmid=gray1; ctop=gray3;
                   cbot=gray2; cleft=gray1; cright=gray1; r=Top} in
  let grid' = Array.of_list (create_grid temp_disp board wall_data) in
  s' := {temp_disp with grid=grid'}


let start_level (s':display ref) lvl game =
  game := (match (Constants.init_level lvl) with
          | Some x -> x
          | None -> failwith "display.ml : cannot access level");
  initialize_display s' (get_master_board lvl)

(* Loop function to be called every frame and after each key press after
 * initialization. Updates player and monsters positions *)
let t_loop (s':display ref) (game:game_state ref) (keys:char list) =
  match keys with
  (* exit program if 'e'/'E' is pressed *)
  | 'e'::_ | 'E'::_ -> raise End
  (* otherwise, run the main update function on key input *)
  | _ -> (* update game by one frame, and change level if appropriate *)
         let cur_lvl = !game.level_number in
         game := Update.main_update !game keys;
         if !game.level_number <> cur_lvl then
           start_level s' !game.level_number game;
         (* 1. draw background and board *)
         t_draw_board s' ();
         (* 2. draw text for game state, or time left if game in progress *)
         Graphics.moveto (!s'.maxx/2) (!s'.maxy-20);
         let text = ref ("Level "^(string_of_int !game.level_number)^" ") in
         let _ = match !game.game_progress with
                 | In_progress -> text:= !text^("Time Left: "^
                                               (string_of_int !game.time))
                 | Won -> text:= !text^"Won!"
                 | Lost -> text:= !text^"Lost :("
                 | Unstarted -> text:= !text^"Paused."
         in
         Graphics.draw_string !text;
         (* 3. draw player *)
         let board = get_master_board !game.level_number in
         let pp = flip_y !game.player_position board in
         draw_player !s' pp;
         (* 4. draw monsters *)
         for i = 0 to (List.length !game.monster_position)-1 do
           let monster = List.nth !game.monster_position i in
           let mp = flip_y (snd monster) board in
           draw_monster !s' mp (fst monster);
         done


(* Exits the program *)
let t_end s () =
  Graphics.close_graph();
  print_string "Game ended by user"; print_newline()


(* Function to execute on mouse clicks *)
let t_mouse s x y = ()


(* Function to execute on exceptions, unless exceptions is [End], i.e close *)
let t_except s ex = Printf.printf "%s\n%!" ("EXCEPTION IN DISPLAY")





(*let rec create_grid' nb_col n sep (b:box_data) =
  if n < 0 then []
  else
  let px = n mod nb_col and py = n / nb_col in
  let nx = b.x +sep + px*(b.w+sep)
  and ny = b.y +sep + py*(b.h+sep) in
  let b1 = {b with x=nx; y=ny} in
  b1 :: (create_grid' nb_col (n-1) sep b)*)



let t_key (s':display ref) c =
  let s = !s' in
  draw_point s.x s.y s.scale s.fc;

  (match c with
  | '8' -> if s.y < s.maxy then s.y <- s.y + 1;
  | '2' -> if s.y > 0 then s.y <- s.y - 1
  | '4' -> if s.x > 0 then s.x <- s.x - 1
  | '6' -> if s.x < s.maxx then s.x <- s.x + 1
  | 'c' -> Graphics.set_color s.bc;
  Graphics.fill_rect 0 0 (s.scale*s.maxx+1) (s.scale*s.maxy+1);
  Graphics.clear_graph()
  | 'e' -> raise End
  | _ -> () );
  draw_point s.x s.y s.scale s.pc



let slate (lvl:int) (disp:display ref) =
  let s = !disp in
  Graphics.open_graph (" " ^ (string_of_int (s.scale*s.maxx)) ^
  "x" ^ (string_of_int (s.scale*s.maxy)));
  skel lvl (t_draw_board disp) (t_loop disp) (t_end disp) (t_key disp)
  (t_mouse disp) (t_except disp)




let launch_game lvl =
  let board= get_master_board lvl in
  let temp_disp = ref {grid=[||]; maxx=100; maxy=100; x=60; y=60; scale=1;
                   bc=Graphics.rgb 130 130 130; fc=Graphics.black;
                   pc=Graphics.red; x_sep=1; y_sep=1} in
  initialize_display temp_disp board;
  slate lvl temp_disp

let _ = launch_game Constants.start_level
