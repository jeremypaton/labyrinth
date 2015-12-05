(** module constants hold the global master board and the level, board list **)

(** master board is list of Booleans, T/F path/wall and also the (level, board)
 ** list of level and the corresponding board of weights of paths and walls.
 ** Boards are stored in matrix format where indices represent positions **)

(** master_board is a board of Booleans **)
type master_board= bool list list

(** type levels board is a board of ints **)
type levels_board= float list list

(*Types for moving for a random monster*)
type move_list_types =  Mv_up |Mv_dn | Mv_lt | Mv_rt


(** position is a tuple of ints- the column order matrix coordinates (i,j) = (y,x) **)
type position= (int*int)

(** game progress is a variant of strings: in_progress, win, lose, unstarted **)
type game_progress= In_progress | Won | Lost | Unstarted

(** define variant type move_type: chasing, random. This designates whether
 ** whether the monster is moving randomly or actively chasing the player **)
type move_type= Chasing | Random | Up | Down | Left | Right
               | Circle of position | Radius of position

type monster = (move_type * position)

(** game_state is a record that holds the level number, game progress, monster
 **  position, player position, and time **)
type game_state= {previous: game_state option;
                  level_number: int;
                  game_progress: game_progress;
                  player_position: position;
                  monster_position: monster list;
                  time: int}

(** initialize a given level **)
val init_level: int -> game_state

(** get a given level's master board **)
val get_master: int -> master_board
(** get a given level's weight board **)
val get_weights: int -> levels_board

val get_level_description: int -> bytes

(** whether a level exists **)
val is_level: int -> bool

val start_level : int

(*Determines steps of random monster*)
val move_list :move_list_types list

(*Moving counter for random monster*)
val move_ctr: int ref
(** screen resolutions **)
val resolution_x : int
val resolution_y : int

(** monster radii **)
val monster_radius: float
