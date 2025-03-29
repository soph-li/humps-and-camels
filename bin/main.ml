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
let draw_line size window_size color board =
  let rec wait_for_valid_click () =
    let event = wait_next_event [ Button_down ] in
    let x, y = (event.mouse_x, event.mouse_y) in
    match find_nearest_dot (x, y) size window_size with
    | None -> wait_for_valid_click ()
    | Some (dot_x, dot_y) -> Some (dot_x, dot_y)
  in

  let first_dot = wait_for_valid_click () in
  match first_dot with
  | None -> ()
  | Some (dot1_x, dot1_y) ->
      set_color black;
      fill_circle dot1_x dot1_y 5;

      let rec wait_for_valid_snd_click () =
        let second_dot = wait_for_valid_click () in
        match second_dot with
        | None -> ()
        | Some (dot2_x, dot2_y) ->
            if
              is_valid_move (dot1_x, dot1_y) (dot2_x, dot2_y)
                (window_size / size)
            then (
              fill_circle dot2_x dot2_y 5;
              set_color color;
              set_line_width 5;
              moveto dot1_x dot1_y;
              lineto dot2_x dot2_y;

              (* Update board *)
              let new_board =
                make_connection (dot1_x, dot1_y) (dot2_x, dot2_y) board
              in
              let spacing = window_size / size in
              let completed_boxes =
                completed_box_coordinates (dot1_x, dot1_y) (dot2_x, dot2_y)
                  spacing new_board
              in
              (* Draw X's to mark completed boxes. *)
              List.iter (fun (x, y) -> draw_x x y color spacing) completed_boxes)
            else wait_for_valid_snd_click ()
      in
      wait_for_valid_snd_click ()

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
    (* Specify availible colors. *)
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

    let board = make_grid size in
    let window_size = size * 100 in
    open_graph
      (" " ^ string_of_int window_size ^ "x" ^ string_of_int window_size);
    (* display initial board *)
    draw_grid size window_size;

    (* Main game loop *)
    let rec play_game color_list =
      let current_color =
        match color_list with
        | [] -> failwith "Error: No colors available"
        | h :: _ -> h
      in

      draw_line size window_size current_color board;

      if not (is_game_over board size) then (
        print_endline "Game continues...";
        let new_color_list = List.tl color_list @ [ List.hd color_list ] in
        play_game new_color_list)
      else (
        (* Prompted ChatGPT -4o, "How to introduce delay in OCaml to allow the
           final image in graphics show up before the program exits", accessed
           3/29/25. *)
        Unix.sleepf 0.5;
        print_endline "\nGame Over! The winner is Player _ !")
    in
    play_game color_list;

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
