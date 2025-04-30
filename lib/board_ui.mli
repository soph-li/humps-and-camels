type confetti = {
  mutable x : int;
  mutable y : int;
  dx : int;
  dy : int;
  color : Graphics.color;
}
(** The type of a single confetti particle, including its coordinates and
    velocity. *)

val draw_grid : int -> int -> unit
(** [draw_grid size window_size] draws a [size] x [size] grid of dots in a
    [window_size] x [window_size] window. Requires: [size] and [window_size] are
    positive. *)

val draw_x : int -> int -> int -> int -> unit
(** [draw_x x y color spacing] draws a 'X' with the color [color] by connecting
    the bottom left corner [(x, y)] to the top right corner and the top left
    corner to the bottom right corner where adjacent points are [spacing] apart.
*)

(* val draw_xs : int -> int -> (int * int) list -> 'a -> 'b -> 'c -> 'd ->
   unit *)
(** [draw_xs cur_color spacing new_completed_boxes new_board size window_width]
    draws an 'X' through every completed box with the appropriate color. *)

val draw_button : int -> int -> int -> int -> string -> unit
(** [draw_button x y w h label] draws a rectangular button of the given
    dimensions, labeled by [label]. *)

val draw_margin_text : string -> int -> int -> int -> unit
(** [draw_margin_text str grid_size window_h y_pos] draws the given text in the
    allocated score panel of the window. *)

val draw_turn_indicator : int -> int -> int -> unit
(** [draw_turn_indicator player window_w window_h] displays a message in the
    corner of the window indicating that it is currently [player]'s turn. *)

val generate_confetti : int -> int -> int -> confetti list
(** [generate_confetti n window_w window_h] generates [n] confetti particles
    randomly positioned within the top region of the window of width [window_w]
    and height [window_h]. Each particle has a random color and initial
    velocity. *)

val animate_confetti : int -> int -> unit
(** [animate_confetti window_w window_h] animates a confetti effect until all
    particles fall off the screen. *)

val draw_margin_text : string -> int -> int -> int -> unit

val draw_scores : Grid.t -> 'a -> int -> int -> int -> unit
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

val find_nearest_dot : int * int -> int -> int -> (int * int) option
(** [find_nearest_dot (x, y) size spacing] returns the nearest dot to [(x, y)]
    in the grid, if it is within a radius of 10. Otherwise, returns [None]. *)
