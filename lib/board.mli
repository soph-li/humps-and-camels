(** [draw_grid size window_size] draws a [size] x [size] grid of dots in a
    [window_size] x [window_size] window. Requires: [size] and [window_size] are
    positive. *)

val draw_grid : int -> int -> unit

(** [draw_x x y color spacing] draws a 'X' with the color [color] by connecting
    the bottom left corner [(x, y)] to the top right corner and the top left
    corner to the bottom right corner where adjacent points are [spacing] apart.
*)

val draw_x : int -> int -> int -> int -> unit

(** [draw_margin_text str grid_size window_h y_pos] draws the given text in the
    allocated score panel of the window. *)

val animate_confetti : int -> int -> unit
(** [animate_confetti window_w window_h] animates a confetti effect until all
    particles fall off the screen. *)

val draw_margin_text : string -> int -> int -> int -> unit

val draw_scores : Game.t -> 'a -> int -> int -> int -> unit
(** [draw_scores board colors grid_size window_h] draws the tallied score of
    each player during gameplay. *)

val draw_game_over : int -> int -> int list -> unit
(** [draw_game_over window_w window_h winners] draws the game over screen
    following the completion of a board with a winners message. *)

val center_align : int -> string -> int -> unit
(**[center_align y str window_width] draws the given string to be center aligned
   in a window with width [widnow_width] at y-coordinate [y]. *)

val redraw_board :
  int ->
  int ->
  int ->
  (int * int * int * int * int) list ->
  ((int * int) * int) list ->
  unit
(** [redraw_board size board_size spacing lines completed_boxes] redraws the
    updated grid with all previous lines and completed boxes. *)
