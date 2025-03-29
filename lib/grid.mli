type t
(** The type of a grid. *)

val make_grid : int -> t
(** [make_grid size] is a [size] x [size] grid. *)

val is_game_over : t -> int -> bool
(** [is_game_over grid size] is whether the game is over. A game is considered
    over if the number of completed boxes equals the maximum number of compleetd
    boxes for the grid size. *)

val completed_boxes : t -> int
(** [completed_boxes grid] is the number of completed boxes in [grid]. *)

val make_connection : int * int -> int * int -> t -> t
(** [make_connection grid (x1, y1) (x2, y2)] is [grid] with a new connection 
between points [(x1, y1)] and [(x2, y2)]]. *)

val completed_box_coordinates :
  int * int -> int * int -> int -> t -> (int * int) list
(** [check_completed_box (x1, y1) (x2, y2) spacing grid] checks if a box is
    completed after adding the line connecting [(x1, y1)] and [(x1, y1)] in
    [grid] with [spacing] between dots. If so, it updates [board] with the
    incremented completed box count. It returns a list of the bottom-left
    coordinates of all completed boxes, or [] if no boxes were completed. *)

val get_grid : t -> ((int * int) * (int * int) list) list
(** [get_grid board] returns a list of all connections in [board]'s grid. Each
    entry in the list is of the form [(point, neighbors)], where [neighbors] is
    a list of points connected to [point]. *)

val is_valid_move : int * int -> int * int -> int -> int -> t -> bool
(** [is_valid_move (x1, y1) (x2, y2) spacing size board] returns whether
    [(x2, y2)] is a valid connection from [(x1, y1)]. A connection is considered
    valid if 1) [(x1, y1)] and [(x2, y2)] are different points; 2) the point
    [(x2, y2)] is within the bounds of the grid; 3) the squared distance between
    [x1, y1)] and [(x2, y2)] is less than the squared [spacing] between adjacent
    points on the grid; and 4) the line connecting [(x1, y1)] and [(x2, y2)] has
    not already been drawn. Requires: [(x1, y1)] is within the bounds of the
    grid. *)

val find_nearest_dot : int * int -> int -> int -> (int * int) option
(** [find_nearest_dot (x, y) size spacing] returns the nearest dot to [(x, y)]
    in the grid, if it is within a radius of 10. Otherwise, returns [None]. *)

val has_available_moves : int * int -> int -> int -> t -> bool
(** [has_available_moves (x, y) spacing size board] checks if [(x, y)] has any
    valid moves left. *)
