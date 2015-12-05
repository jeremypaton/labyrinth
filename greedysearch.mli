open Constants
(** module greedysearch gets the first movmement of the optimal path between two
 ** positions **)

(** function greedy takes in monster position, player position, board, and
 ** returns new monster position based on greedy search algorithm- moves to
 ** the neighboring square closest to player **)
val greedy: position -> position -> levels_board -> position
