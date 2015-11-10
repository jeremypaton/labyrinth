(** module constants hold the global master board and the level, board list **)

(** master board is list of Booleans, T/F path/wall and also the (level, board)
 ** list of level and the corresponding board of weights of paths and walls.
 ** Boards are stored in matrix format where indices represent positions **)

(** master_board is a board of Booleans **)
type master_board= bool list list

(** type levels board is a board of ints **)
type levels_board= int list list
