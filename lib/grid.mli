type t
(** The type of a grid *)

val make_grid : int -> t
(** [make_grid size] is a [size] x [size] grid. *)

val is_complete : t -> bool
(** [is_complete grid] is whether the game is complete. *)

val completed_boxes : t -> int
(** [completed_boxes grid] is the number of completed boxes in [grid]. *)

val make_connection : int * int -> int * int -> t -> t
(** [make_connection grid (x1, y1) (x2, y2)] is [grid] with a new connection 
between points [(x1, y1)] and [(x2, y2)]]*)
