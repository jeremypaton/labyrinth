open Constants
(** module Dijkstra gets the first movmement of the optimal path between two
 ** positions **)

(** function Dijkstra takes in monster position, player position, board, and
 ** returns new monster position **)
val dijkstra: position -> position -> levels_board -> position
