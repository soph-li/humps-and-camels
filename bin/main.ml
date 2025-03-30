open Graphics
open Unix

open Cs3110_final_project.Grid
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

exception Quit
(** Raised if user quits the program. *)

(** [draw_grid size window_size] draws a [size] x [size] grid of dots in a
    [window_size] x [window_size] window. Requires: [size] and [window_size] are
    positive. *)
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

(** [draw_x x y color spacing] draws a 'X' with the color [color] by connecting
    the bottom left corner [(x, y)] to the top right corner and the top left
    corner to the bottom right corner where adjacent points are [spacing] apart.
*)
let draw_x x y color spacing =
  set_color color;
  moveto x y;
  lineto (x + spacing) (y + spacing);
  moveto x (y + spacing);
  lineto (x + spacing) y

(** [draw_line size window_size color] draws a [color] line connecting the two
    dots that are closest to the positions where the user clicked in the grid.
*)
let draw_line size window_size color board player =
  let spacing = window_size / size in

  (* Ensure the user picks a dot that has available moves. *)
  let rec wait_for_valid_fst_dot () =
    let event = wait_next_event [ Button_down ] in
    let x, y = (event.mouse_x, event.mouse_y) in
    match find_nearest_dot (x, y) size window_size with
    | Some (x, y) ->
        if has_available_moves (x, y) spacing size board then Some (x, y)
        else wait_for_valid_fst_dot ()
    | _ -> wait_for_valid_fst_dot ()
  in

  let first_dot = wait_for_valid_fst_dot () in
  match first_dot with
  | None -> ()
  | Some (dot1_x, dot1_y) -> (
      set_color black;
      fill_circle dot1_x dot1_y 5;

      let rec wait_for_valid_snd_dot () =
        let event = wait_next_event [ Button_down ] in
        let x, y = (event.mouse_x, event.mouse_y) in
        match find_nearest_dot (x, y) size window_size with
        | Some (dot2_x, dot2_y) ->
            if
              is_valid_move (dot1_x, dot1_y) (dot2_x, dot2_y) spacing size board
            then Some (dot2_x, dot2_y)
            else wait_for_valid_snd_dot ()
        | _ -> wait_for_valid_snd_dot ()
      in

      let second_dot = wait_for_valid_snd_dot () in
      match second_dot with
      | None -> ()
      | Some (dot2_x, dot2_y) ->
          fill_circle dot2_x dot2_y 5;
          set_color color;
          set_line_width 5;
          moveto dot1_x dot1_y;
          lineto dot2_x dot2_y;

          (* Update board *)
          let new_board =
            make_connection (dot1_x, dot1_y) (dot2_x, dot2_y) board
          in

          let completed_boxes =
            completed_box_coordinates (dot1_x, dot1_y) (dot2_x, dot2_y) spacing
              new_board player
          in

          (* Draw X's to mark completed boxes. *)
          List.iter (fun (x, y) -> draw_x x y color spacing) completed_boxes)

(** [get_valid_players ()] prompts the user until a valid number of players is
    entered or the user decides to quit. Raises [Quit] if the user enters
    'quit'. *)
let rec get_valid_players () =
  print_endline "Enter number of players (2-4): ";
  try
    let input = read_line () in
    if input = "quit" then raise Quit
    else
      let player_num = int_of_string input in
      if player_num < 2 || player_num > 4 then (
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

    let input = read_line () in
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
let () =
  try
    (* Get valid number of players. *)
    let player_num = get_valid_players () in
    let size =
      match player_num with
      | 2 -> 4
      | 3 -> 6
      | 4 -> 10
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
    let window_size = size * 100 in
    open_graph
      (" " ^ string_of_int window_size ^ "x" ^ string_of_int window_size);

    (* Display initial board. *)
    draw_grid size window_size;

    (* Main game loop *)
    let rec play_game color_list player_idx =
      let current_color = List.nth color_list player_idx in

      print_endline ("Player " ^ string_of_int (player_idx + 1) ^ "'s turn");

      let prev_completed_boxes = completed_boxes board in

      draw_line size window_size current_color board (player_idx + 1);

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

        (* [determine_winners score] returns a list of players who have the most
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
        in

        let winners = determine_winners final_scores in

        (match winners with
        | [ winner ] ->
            print_endline
              ("\nGame over! Player " ^ string_of_int winner ^ " won.")
        | _ ->
            print_endline
              "\nGame over! It's a tie between the following players:";
            List.iter
              (fun winner ->
                print_endline (" - Player " ^ string_of_int winner))
              winners);

        (* Prompted ChatGPT -4o, "How to introduce delay in OCaml to allow the
           final image in graphics show up before the program exits", accessed
           3/29/25. *)
        Unix.sleepf 2.
    in
    play_game color_list 0;

    (* Handle closing of game. *)
    close_graph ()
  with
  | Failure e ->
      print_endline e;
      close_graph ()
  | Quit -> print_endline "\nExited game."
  | Graphics.Graphic_failure _ ->
      print_endline "\nThank you for playing!";
      close_graph ()
  | _ ->
      print_endline "\nError: An unexpected error occured.";
      close_graph ()
