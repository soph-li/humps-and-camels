type confetti
(** The abstract type of a single confetti particle. *)

type click_status
(** The abstract type of a single button click following the end of a game. *)

val create_confetti : int -> int -> int -> int -> Graphics.color -> confetti
(** [make_confetti x y dx dy color] creates a confetti particle. *)

val confetti_x : confetti -> int
(** [confetti_x c] is the current x position of [c] in a window. *)

val confetti_y : confetti -> int
(** [confetti_y c] is the current y position of [c] in a window. *)

val confetti_dx : confetti -> int
(** [confetti_dx c] is the horizontal velocity of [c]. *)

val confetti_dy : confetti -> int
(** [confetti_dy c] is the vertical velocity of [c]. *)

val confetti_color : confetti -> Graphics.color
(** [confetti_color c] is the color of [c]. *)

val replay : click_status
(** [replay] is the status representing a replay click. *)

val quit : click_status
(** [quit] is the status representing a quit click. *)

val noclick : click_status
(** [noclick] is the status representing no button was clicked. *)

val is_replay : click_status -> bool
(** [is_replay click] is true if the user makes a replay click, and false
    otherwise. *)

val is_quit : click_status -> bool
(** [is_quit click] is true if the user makes a quick click, and false
    otherwise. *)

val is_noclick : click_status -> bool
(** [is_noclick click] is true if the user makes an invalid click, and false
    otherwise. *)

val draw_grid : int -> int -> unit
(** [draw_grid size window_size] draws a [size] x [size] grid of dots in a
    [window_size] x [window_size] window. Requires: [size] and [window_size] are
    positive. *)

val draw_x : int -> int -> int -> int -> unit
(** [draw_x x y color spacing] draws a 'X' with the color [color] by connecting
    the bottom left corner [(x, y)] to the top right corner and the top left
    corner to the bottom right corner where adjacent points are [spacing] apart.
*)

val draw_button : int -> int -> int -> int -> string -> unit
(** [draw_button x y w h label] draws a rectangular button of the given
    dimensions, labeled by [label]. *)

val wait_for_end_choice : int -> int -> click_status -> string
(** [wait_for_end_choice window_w window_h click_status] draws a restart and
    quit button, and is the name of the button clicked by the user. Otherwise,
    the result of the button click corresponds with [click_status]. *)

val generate_confetti : int -> int -> int -> confetti list
(** [generate_confetti n window_w window_h] generates [n] confetti particles
    randomly positioned within the top region of the window of width [window_w]
    and height [window_h]. Each particle has a random color and initial
    velocity. *)

val animate_confetti : int -> int -> unit
(** [animate_confetti window_w window_h] animates a confetti effect until all
    particles fall off the screen. *)

val draw_margin_text : string -> int -> int -> int -> unit
(** [draw_margin_text str grid_size window_h y_pos] draws the given text in the
    allocated score panel of the window. *)

val draw_scores : Grid.t -> int -> int -> int -> unit
(** [draw_scores board grid_size window_h panel_w] draws the tallied score of each
    player during gameplay. *)

val draw_turn_indicator : int -> int -> int -> int -> unit
(** [draw_turn_indicator player grid_size window_ panel_w] displays a message in the
    corner of the window indicating that it is currently [player]'s turn. *)

val draw_game_over : int -> int -> int list -> unit
(** [draw_game_over window_w window_h winners] draws the game over screen
    following the completion of a board with a winners message. *)

val center_align : int -> string -> int -> unit
(** [center_align y str window_width] draws the given string to be center
    aligned in a window with width [window_width] at y-coordinate [y]. *)

val redraw_board :
  int ->
  int ->
  int ->
  (int * int * int * int * int) list ->
  ((int * int) * int) list ->
  unit
(** [redraw_board size board_size spacing lines completed_boxes] redraws the
    updated grid with all previous lines and completed boxes. *)

val draw_rules_screen : int -> int -> unit
(** [draw_rules_scren window_width window_height] displays the rules of Dots and
    Boxes graphically in the window. *)
