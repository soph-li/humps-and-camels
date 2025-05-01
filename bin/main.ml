(* Add ending dot and one box option for game!!!!!!*)

open Graphics
open Unix
open Cs3110_final_project.Grid
open Cs3110_final_project.Board_ui

(** Prompted ChatGPT-4o, "How to install OCaml Graphics", accessed 3/22/25. *)

(** Prompted ChatGPT-4o, "What should I do if I encountered Fatal error:
    exception Graphics.Graphic_failure("Cannot open display ")", accessed
    3/22/25. *)

(** Prompted ChatGPT-4o, "How to install Xvfb", accessed 3/22/25. *)

(** Prompted ChatGPT-4o, "How to use OCaml Graphics", accessed 3/22/25. *)

(** Basic board setup interface adapted from
    "https://ocaml.org/manual/4.03/libref/Graphics.html", accessed 3/22/25. *)

(** Adapted from "https://ocaml.org/manual/4.03/libref/Graphics.html", accessed
    3/25/25. *)

(** Prompted ChatGPT-4o, "Why are my mouse clicks not working in Ocaml using
    XQuartz, accessed 3/23/25." *)

(** Prompted ChatGPT-4o, "How to handle window closure in OCaml Graphics",
    accessed 3/25/25. *)

(** Prompted ChaptGPT-4o "Is there pre-set alignment in OCaml Graphics" accessed
    4/8/25. *)

exception Quit
(** Raised if user quits the program. *)

exception Restart

(** [determine_winners score] returns a list of players who have the most
    points. *)
let determine_winners score =
  let max_points =
    List.fold_left
      (fun acc (_, points) -> if points > acc then points else acc)
      0 score
  in
  List.fold_right
    (fun (candidate, points) acc ->
      if points = max_points then candidate :: acc else acc)
    score []

(** [check_if_game_over board size window_width window_height] checks if the
    game is over and if so, it end game properly. *)
let check_if_game_over board size window_width window_height =
  if is_game_over board size then (
    let final_scores = get_scores board in
    let winners = determine_winners final_scores in
    print_endline "Game over";
    draw_game_over window_width window_height winners)
  else ""

(** [wait_for_valid_fst_dot player_idx board size board_size spacing] waits for
    player to click a valid first dot. *)
let rec wait_for_valid_fst_dot player_idx board size board_size spacing () =
  print_endline ("Player " ^ string_of_int (player_idx + 1) ^ "'s turn");
  let event = wait_next_event [ Button_down ] in
  let x, y = (event.mouse_x, event.mouse_y) in
  match find_nearest_dot (x, y) size board_size with
  | Some (x, y) ->
      if has_available_moves (x, y) spacing size board then Some (x, y)
      else wait_for_valid_fst_dot player_idx board size board_size spacing ()
  | _ -> wait_for_valid_fst_dot player_idx board size board_size spacing ()

(* Prompted ChatGPT-40 "How to draw line leaving point, following user mouse
   position, Ocaml graphics.", accesssed 4/1/25. *)

(* Draw X's to mark boxes that were just completed. *)

(** [update_board (start_x, start_y) (dot2_x, dot2_y) board spacing player_idx
     cur_color completed_boxes_lst size window_width window_height] returns the
    updated_board, updated_completed_boxes, and the other unchanged variables.
*)
let rec update_board (start_x, start_y) (dot2_x, dot2_y) board spacing
    player_idx cur_color completed_boxes_lst size window_width window_height =
  (* Update board connections *)
  let new_board = make_connection (start_x, start_y) (dot2_x, dot2_y) board in
  (* Update list of completed boxes with coordinates and color of player who
     made the move. *)
  let new_completed_boxes =
    get_box_coordinates (start_x, start_y) (dot2_x, dot2_y) spacing new_board
      player_idx
  in
  let updated_completed_boxes =
    List.fold_left
      (fun acc (x, y) -> ((x, y), cur_color) :: acc)
      completed_boxes_lst new_completed_boxes
  in
  (new_board, updated_completed_boxes, size)

(** [draw_livewire color_list player_idx start_x start_y x2 y2] draws a line
    from [(start_x, start_x)] to [(x2, y2)] with the color in [color_list] at
    [player_idx]. *)
let rec draw_livewire color_list player_idx start_x start_y x2 y2 =
  let cur_color = List.nth color_list player_idx in
  set_color cur_color;
  set_line_width 3;
  moveto start_x start_y;
  lineto x2 y2

(** Draw live line from first dot to mouse position. *)
let rec follow_mouse size board_size spacing board cur_color color_list
    player_idx lines_lst completed_boxes_lst window_width window_height
    (start_x, start_y) =
  let event = wait_next_event [ Mouse_motion; Button_down ] in
  let x2, y2 = (event.mouse_x, event.mouse_y) in
  redraw_board size board_size spacing lines_lst completed_boxes_lst;
  (* Redraw start point *)
  set_color black;
  fill_circle start_x start_y 5;
  draw_livewire color_list player_idx start_x start_y x2 y2;
  (* Draw live wire *)
  (* Prompted ChatGPT-4o, "How to tell if mouse button pressed," accessed
     4/2/25. Referenced
     https://ocaml.org/p/graphics/5.1.1/doc/Graphics/index.html for mouse
     events, accessed 4/2/25. *)
  if event.button then
    match find_nearest_dot (x2, y2) size board_size with
    | Some (dot2_x, dot2_y) ->
        if
          is_valid_move (start_x, start_y) (dot2_x, dot2_y) spacing size
            board (* Check if second point is valid *)
        then (
          set_color black;
          fill_circle dot2_x dot2_y 5;
          (* Add new line segment to list of lines. *)
          let updated_lines =
            (start_x, start_y, dot2_x, dot2_y, cur_color) :: lines_lst
          in
          let prev_completed_boxes = completed_boxes board in
          (* Get previous box count to compare to new box count *)
          let new_board, updated_completed_boxes, size =
            update_board (start_x, start_y) (dot2_x, dot2_y) board spacing
              player_idx cur_color completed_boxes_lst size window_width
              window_height
          in
          redraw_board size board_size spacing updated_lines
            updated_completed_boxes;
          let choice =
            check_if_game_over new_board size window_width window_height
          in
          match choice with
          | "replay" ->
              Unix.sleepf 0.5;
              raise Restart
          | "quit" ->
              Unix.sleepf 0.5;
              raise Quit
          | "" ->
              (* Game continues, proceed with the next turn *)
              let next_player_idx =
                if prev_completed_boxes < List.length updated_completed_boxes
                then player_idx (* Same player if they completed a box *)
                else (player_idx + 1) mod List.length color_list
              in
              play size board_size spacing new_board updated_lines
                updated_completed_boxes next_player_idx color_list window_width
                window_height
          | _ -> raise Quit)
        else
          follow_mouse size board_size spacing board cur_color color_list
            player_idx lines_lst completed_boxes_lst window_width window_height
            (start_x, start_y)
    | None ->
        follow_mouse size board_size spacing board cur_color color_list
          player_idx lines_lst completed_boxes_lst window_width window_height
          (start_x, start_y)
  else
    follow_mouse size board_size spacing board cur_color color_list player_idx
      lines_lst completed_boxes_lst window_width window_height (start_x, start_y)

(* Play turn of a player *)
(* Prompted ChaptGPT-4o "how to connect mutually recursive functions" alonmg
   with follow_mouse and play to figure out to use "and," accessed 4/14/25. *)
and play size board_size spacing board lines_lst completed_boxes_lst player_idx
    color_list window_width window_height =
  (* Wait for user to put down a first dot. *)
  let first_dot =
    wait_for_valid_fst_dot player_idx board size board_size spacing ()
  in
  (* Draw live line from valid first dot to mouse position. *)
  match first_dot with
  | None -> ()
  | Some (dot1_x, dot1_y) ->
      set_color black;
      (* draw dot *)
      fill_circle dot1_x dot1_y 5;
      let cur_color = List.nth color_list player_idx in
      follow_mouse size board_size spacing board cur_color color_list player_idx
        lines_lst completed_boxes_lst window_width window_height (dot1_x, dot1_y)

(** [draw_line size window_size color] draws a [color] line connecting the two
    dots that are closest to the positions where the user clicked in the grid.
*)
let draw_line size board_size color board player color_list window_width
    window_height score_panel_width =
  let spacing = board_size / size in
  (* start gameplay *)
  play size board_size spacing board [] [] (player - 1) color_list window_width
    window_height

(** [get_valid_players ()] prompts the user until a valid number of players is
    entered or the user decides to quit. Raises [Quit] if the user enters
    'quit'. *)
let rec get_valid_players () =
  print_endline "Enter number of players (2-4): ";
  try
    let input = String.trim (read_line ()) in
    if input = "quit" then raise Quit
    else
      let player_num = int_of_string input in
      if player_num < 2 || player_num > 4 then (
        (* if player_num < 1 || player_num > 4 then ( *)
        print_endline
          "\n\
           Please try again with a valid number from 2-4, or type 'quit' to \
           quit. \n";
        get_valid_players ())
      else player_num
  with Failure _ ->
    print_endline
      "\n\
       Please try again with a valid number from 2-4, or type 'quit' to quit. \n";
    get_valid_players ()

(** [color_of_string color] returns the Graphics color corresponding to the
    given string. Raises: [Failure "Invalid Color"] if [color] is not a valid
    Graphics color. *)
let color_of_string color =
  match color with
  | "black" -> black
  | "red" -> red
  | "green" -> green
  | "blue" -> blue
  | "yellow" -> yellow
  | "cyan" -> cyan
  | "magenta" -> magenta
  | _ -> failwith "Invalid color."

(** [select_player_color ind player_num selected_colors] returns the list of
    colors selected by player input starting from [ind] to [player_num]. *)
let rec select_player_color ind player_num selected_colors =
  if ind = player_num then List.rev selected_colors
  else (
    print_endline
      ("\nPlayer " ^ string_of_int (ind + 1) ^ ", enter your color choice: ");

    let input = String.trim (String.lowercase_ascii (read_line ())) in
    try
      let colorized_input = color_of_string input in
      if List.mem colorized_input selected_colors then (
        print_endline "\nColor already taken! Please choose another color.";
        select_player_color ind player_num selected_colors)
      else
        select_player_color (ind + 1) player_num
          (colorized_input :: selected_colors)
    with Failure _ ->
      print_endline "\nInvalid color! Please try again with a valid choice.";
      select_player_color ind player_num selected_colors)

(* Main *)
let rec start_game () =
  try
    (* Get valid number of players. *)
    let player_num = get_valid_players () in
    let size =
      match player_num with
      (* | 1 -> 2 one box case for testing end_screen *)
      | 2 -> 4
      | 3 -> 6
      | 4 -> 8
      | _ -> failwith "\nInvalid player count."
    in

    (* Specify available colors. *)
    print_endline "\nColors available:";
    print_endline " - black";
    print_endline " - red";
    print_endline " - green";
    print_endline " - blue";
    print_endline " - yellow";
    print_endline " - cyan";
    print_endline " - magenta";

    let color_list = select_player_color 0 player_num [] in
    print_endline
      ("\nStarting a game for "
      ^ string_of_int (List.length color_list)
      ^ " players...");

    let board = make_grid size player_num in
    let spacing = 100 in
    let grid_size = size * spacing in
    let score_panel_width = 150 in
    let window_width = grid_size in
    (* let window_width = grid_size + score_panel_width in *)
    let window_height = grid_size in

    open_graph
      (" " ^ string_of_int window_width ^ "x" ^ string_of_int window_height);

    (* Display initial board. *)
    draw_grid size grid_size;

    (* draw_scores board color_list grid_size window_height
       score_panel_width; *)

    (* Main game loop *)
    let rec play_game color_list player_idx =
      let current_color = List.nth color_list player_idx in

      (* print_endline ("Player " ^ string_of_int (player_idx + 1) ^ "'s
         turn"); *)
      let prev_completed_boxes = completed_boxes board in

      draw_line size grid_size current_color board (player_idx + 1) color_list
        window_height window_width score_panel_width;

      (* draw_scores board color_list grid_size window_height
         score_panel_width; *)
      let new_completed_boxes = completed_boxes board in

      let completed_box = new_completed_boxes > prev_completed_boxes in

      if not (is_game_over board size) then (
        print_endline "Game continues...";
        let next_player_idx =
          if completed_box then player_idx
          else (player_idx + 1) mod List.length color_list
        in

        play_game color_list next_player_idx)
      else
        let final_scores = get_scores board in
        let winners = determine_winners final_scores in
        ignore (draw_game_over window_width window_height winners);

        match winners with
        | [ winner ] ->
            print_endline
              ("\nGame over! Player " ^ string_of_int winner ^ " won.")
        | _ ->
            print_endline
              "\nGame over! It's a tie between the following players:";
            List.iter
              (fun winner ->
                print_endline (" - Player " ^ string_of_int winner))
              winners
      (* Prompted ChatGPT -4o, "How to introduce delay in OCaml to allow the
           final image in graphics show up before the program exits", accessed
           3/29/25. *)
      (* Unix.sleepf 3. *)
    in
    play_game color_list 0
    (* Handle closing of game. *)
    (* close_graph () *)
  with
  | Failure e ->
      print_endline e;
      close_graph ()
  | Quit -> print_endline "\nExited game."
  | Restart ->
      print_endline "\nRestarting game.";
      close_graph ();
      start_game ()
  | Graphics.Graphic_failure _ ->
      print_endline "\nThank you for playing!";
      close_graph ()
  | _ ->
      print_endline "\nError: An unexpected error occured.";
      close_graph ()

let () = start_game ()
