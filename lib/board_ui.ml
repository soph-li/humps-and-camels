open Graphics
open Unix
open Grid

type confetti = {
  mutable x : int;
  mutable y : int;
  dx : int;
  dy : int;
  color : color;
}

type click_status =
  | ReplayClick
  | QuitClick
  | NoClick

let create_confetti x y dx dy color = { x; y; dx; dy; color }
let confetti_x c = c.x
let confetti_y c = c.y
let confetti_dx c = c.dx
let confetti_dy c = c.dy
let confetti_color c = c.color
let replay = ReplayClick
let quit = QuitClick
let noclick = NoClick

let is_replay = function
  | ReplayClick -> true
  | _ -> false

let is_quit = function
  | QuitClick -> true
  | _ -> false

let is_noclick = function
  | NoClick -> true
  | _ -> false

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

let draw_button x y w h label =
  let btn_font = "-*-helvetica-medium-r-normal--16-*-*-*-*-*-*-*" in
  set_font btn_font;

  set_color black;
  fill_rect x y w h;
  set_color white;
  let text_w, text_h = text_size label in
  moveto (x + ((w - text_w) / 2)) (y + ((h - text_h) / 2));
  draw_string label

let wait_for_end_choice window_w window_h pre_status =
  let padding = 10 in
  let label1 = "Replay" in
  let label2 = "Quit" in
  let text_w1, text_h1 = text_size label1 in
  let text_w2, _ = text_size label2 in
  let button_w1 = text_w1 + padding in
  let button_w2 = text_w2 + padding in
  let button_h = text_h1 + padding in

  let spacing = 20 in
  let total_width = button_w1 + button_w2 + spacing in
  let start_x = (window_w - total_width) / 2 in
  let y = 100 in

  let replay_x = start_x in
  let quit_x = start_x + button_w1 + spacing in

  draw_button replay_x y button_w1 button_h label1;
  draw_button quit_x y button_w2 button_h label2;

  let rec wait () =
    let status = wait_next_event [ Button_down ] in
    let mx = status.mouse_x in
    let my = status.mouse_y in
    if
      mx >= replay_x
      && mx <= replay_x + button_w1
      && my >= y
      && my <= y + button_h
    then (
      close_graph ();
      "replay")
    else if
      mx >= quit_x && mx <= quit_x + button_w2 && my >= y && my <= y + button_h
    then (
      close_graph ();
      "quit")
    else wait ()
  in
  match pre_status with
  | ReplayClick -> "replay"
  | QuitClick -> "quit"
  | _ -> wait ()

let generate_confetti n window_w window_h =
  let rand_color () =
    [| red; green; blue; yellow; cyan; magenta |].(Random.int 6)
  in
  List.init n (fun _ ->
      {
        x = Random.int window_w;
        y = window_h + Random.int 200;
        dx = Random.int 5 - 2;
        dy = -(Random.int 12 + 8);
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
  clear_graph ();
  auto_synchronize true

let draw_margin_text str grid_size window_h y_pos =
  set_color black;
  set_text_size 20;
  moveto (grid_size + 20) (window_h - y_pos);
  draw_string str

let draw_scores board grid_size window_h panel_w =
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
  let y = window_h * 2 / 3 in
  moveto x y;
  draw_string end_msg;
  animate_confetti window_w window_h;
  set_color black;
  let sorted_winners = List.sort compare winners in
  let y_winner = y - 50 in

  match sorted_winners with
  | [ winner ] ->
      let win_font = "-*-helvetica-medium-r-normal--36-*-*-*-*-*-*-*" in
      set_font win_font;

      let win_msg = "Player " ^ string_of_int (winner + 1) ^ " wins!" in
      let win_width = fst (text_size win_msg) in
      let x_win = (window_w - win_width) / 2 in
      moveto x_win y_winner;
      draw_string win_msg
  | _ ->
      let tie_font = "-*-helvetica-medium-r-normal--36-*-*-*-*-*-*-*" in
      set_font tie_font;

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

let draw_rules_screen window_width window_height =
  clear_graph ();
  set_color black;

  (* Prompted ChatGPT-4o, "How to change font in OCaml Graphics", accessed
     5/5/25. *)
  let title_font = "-*-helvetica-bold-r-normal--18-*-*-*-*-*-*-*" in
  let line_font = "-*-helvetica-medium-r-normal--14-*-*-*-*-*-*-*" in

  let rules =
    [
      "1. Players take turns connecting two adjacent dots with a line.";
      "2. Complete the fourth side of a box to claim it and get an extra turn.";
      "3. The game ends when all boxes have been claimed.";
      "4. The player with the most boxes at the end wins.";
      "5. Click two adjacent dots to draw a line.";
      "";
      "Click anywhere to start the game.";
    ]
  in

  (* Set up drawing. *)
  set_font line_font;
  let max_line_width = window_width - 40 in
  let line_spacing = 25 in

  (* Prompted ChatGPT-4o, "How to wrap text from rules based on window width and
     window height in OCaml", Adapted lines 266-308 from ChatGPT, accessed
     5/5/25. *)
  let wrap_text line =
    let words = String.split_on_char ' ' line in
    let rec build_lines current_line lines = function
      | [] -> List.rev (current_line :: lines)
      | h :: t ->
          let tentative =
            if current_line = "" then h else current_line ^ " " ^ h
          in
          if fst (text_size tentative) > max_line_width then
            build_lines h (current_line :: lines) t
          else build_lines tentative lines t
    in
    build_lines "" [] words
  in

  let wrapped_lines = List.flatten (List.map wrap_text rules) in
  let total_height = line_spacing * List.length wrapped_lines in
  let start_y = ((window_height + total_height) / 2) - line_spacing in

  (* Draw title. *)
  set_font title_font;
  let title = "RULES OF DOTS AND BOXES" in
  let title_width = fst (text_size title) in
  let title_y = start_y + line_spacing in
  moveto ((window_width - title_width) / 2) title_y;
  draw_string title;

  (* Draw rules with word wrap and center align. *)
  set_font line_font;
  let rec draw_lines y_offset = function
    | [] -> ()
    | line :: rest ->
        let line_width = fst (text_size line) in
        let x = (window_width - line_width) / 2 in
        moveto x y_offset;
        draw_string line;
        draw_lines (y_offset - line_spacing) rest
  in

  draw_lines start_y wrapped_lines;
  ignore (wait_next_event [ Button_down ])
