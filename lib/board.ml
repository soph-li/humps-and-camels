open Graphics
open Unix
open Game

type confetti = {
  mutable x : int;
  mutable y : int;
  dx : int;
  dy : int;
  color : color;
}
(** The type representing a single confetti particle, including its coordinates
    and velocity. *)

let draw_grid size window_size =
  set_color black;

  let spacing = window_size / size in

  for i = 0 to size - 1 do
    for j = 0 to size - 1 do
      let x = (i * spacing) + (spacing / 2) in
      let y = (j * spacing) + (spacing / 2) in
      fill_circle x y 2
    done
  done

let draw_x x y color spacing =
  set_color color;
  moveto x y;
  lineto (x + spacing) (y + spacing);
  moveto x (y + spacing);
  lineto (x + spacing) y

(** [generate_confetti n window_w window_h] generates [n] confetti particles
    randomly positioned within the top region of the window of width [window_w]
    and height [window_h]. Each particle has a random color and initial
    velocity. *)
let generate_confetti n window_w window_h =
  let rand_color () =
    [| red; green; blue; yellow; cyan; magenta |].(Random.int 6)
  in
  List.init n (fun _ ->
      {
        x = Random.int window_w;
        y = window_h + Random.int 200;
        dx = Random.int 5 - 2;
        dy = -(Random.int 8 + 4);
        color = rand_color ();
      })

let animate_confetti window_w window_h =
  let confetti = generate_confetti 150 window_w window_h in
  auto_synchronize false;
  while List.exists (fun p -> p.y > 0) confetti do
    clear_graph ();
    List.iter
      (fun p ->
        set_color p.color;
        fill_circle p.x p.y 4;
        p.x <- p.x + p.dx;
        p.y <- p.y + p.dy)
      confetti;
    synchronize ();
    Unix.sleepf 0.03
  done;
  auto_synchronize true

let draw_margin_text str grid_size window_h y_pos =
  set_color black;
  set_text_size 20;
  moveto (grid_size + 20) (window_h - y_pos);
  draw_string str

let draw_scores board colors grid_size window_h panel_w =
  (* auto_synchronize false; *)
  (* Draw score panel area *)
  set_color white;
  fill_rect grid_size 0 panel_w window_h;
  set_color black;

  (* Draw title *)
  draw_margin_text "Player Scores" grid_size window_h 50;

  (* Draw scores *)
  set_text_size 20;
  let scores = get_scores board in
  List.iteri
    (fun idx (player, score) ->
      moveto (grid_size + 20) (window_h - 80 - (idx * 30));
      draw_string (Printf.sprintf "Player %d: %d" player score))
    scores
(* synchronize ();

   auto_synchronize true *)

let draw_game_over window_w window_h winners =
  Unix.sleepf 1.;
  clear_graph ();
  set_color black;
  set_text_size 50;

  let end_msg = "Game Over!" in
  let text_width = fst (text_size end_msg) in
  let x = (window_w - text_width) / 2 in
  let y = window_h / 2 in
  moveto x y;
  draw_string end_msg;
  animate_confetti window_w window_h;
  set_color black;
  let sorted_winners = List.sort compare winners in
  let y_winner = y - 50 in
  match sorted_winners with
  | [ winner ] ->
      let win_msg = "Player " ^ string_of_int (winner + 1) ^ " wins!" in
      let win_width = fst (text_size win_msg) in
      let x_win = (window_w - win_width) / 2 in
      moveto x_win y_winner;
      draw_string win_msg
  | _ ->
      let tie_msg = "It's a tie between:" in
      let text_width = fst (text_size tie_msg) in
      let x_tie = (window_w - text_width) / 2 in
      moveto x_tie y_winner;
      draw_string tie_msg;

      let offset = ref (y_winner - 30) in
      List.iter
        (fun w ->
          let player_str = "Player " ^ string_of_int (w + 1) in
          let text_width = fst (text_size player_str) in
          let x_player = (window_w - text_width) / 2 in
          moveto x_player !offset;
          draw_string player_str;
          offset := !offset - 25)
        sorted_winners

let center_align y str window_width =
  (* Prompted ChaptGPT-4o "Is there pre-set alignment in OCaml Graphics"
     accessed 4/8/25. *)
  let str_len = text_size str in
  let text_width = fst str_len in
  let x = (window_width - text_width) / 2 in
  moveto x y;
  draw_string str

let redraw_board size board_size spacing lines completed_boxes =
  auto_synchronize false;

  (* Prompted ChaptGPT-4o "How to fix flickering screen with clear_graph for
     display," accessed 4/4/25. *)
  clear_graph ();

  (* draw_scores board color_list board_size window_height score_panel_width; *)
  draw_grid size board_size;

  (* Draw previous line segments *)
  List.iter
    (fun (x1, y1, x2, y2, player_color) ->
      set_color player_color;
      (* color of player who drew the line *)
      moveto x1 y1;
      lineto x2 y2;
      set_color black;
      (* draw dots *)
      fill_circle x1 y1 5;
      fill_circle x2 y2 5)
    lines;

  (* Redraw X's for completed boxes. *)
  List.iter
    (fun ((x, y), player_color) -> draw_x x y player_color spacing)
    completed_boxes;
  synchronize ();
  auto_synchronize true
