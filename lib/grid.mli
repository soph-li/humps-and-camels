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

val check_completed_box : int * int -> int * int -> int -> t -> bool
(** [check_completed_box (x1, y1) (x2, y2) spacing board] checks if a box is
    completed upon adding the line connecting [(x1, y1)] and [(x1, y1)]. If a
    so, it updates [board] with the incremented completed box count and returns
    true. *)

val get_grid : t -> ((int * int) * (int * int) list) list
(** [get_grid board] returns a list of all connections in [board]'s grid. Each
    entry in the list is of the form [(point, neighbors)], where [neighbors] is
    a list of points connected to [point]. *)
