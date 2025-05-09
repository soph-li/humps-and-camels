Graphics.open_graph ""

open OUnit2
open Cs3110_final_project.Grid
open Cs3110_final_project.Board_ui
open Graphics

(** [print_list lst] returns a string representation of [lst]. *)
let rec print_list lst =
  match lst with
  | [] -> ""
  | [ (x, y) ] -> "(" ^ string_of_int x ^ ", " ^ string_of_int y ^ ")"
  | (x, y) :: t ->
      "(" ^ string_of_int x ^ ", " ^ string_of_int y ^ ")" ^ "; " ^ print_list t

(*****************************************************************************
 * Tests for grid.
 *****************************************************************************)

(** [make_check_completed_box_test test_name (x1, y1) (x2, y2) spacing board
     expected_output] creates a test case named [test_name]. It tests whether
    adding a line connecting [(x1,y1)] and [(x2,y2)] forms a box in [grid] with
    [spacing]. *)
let make_check_completed_box_test test_name (x1, y1) (x2, y2) spacing board
    expected_coordinates =
  test_name >:: fun _ ->
  assert_equal expected_coordinates
    (get_box_coordinates (x1, y1) (x2, y2) spacing board 0)
    ~printer:print_list

(** [board_2x2] is a 2x2 grid of dots. *)
let board_2x2 = make_grid 2 0

(** [board_2x2_one_box] is a 2x2 grid of dots with a box formed by the
    coordinates (0,0), (0,1), (1,0), and (1,1). *)
let board_2x2_one_box =
  let board_1 = make_connection (0, 0) (0, 1) board_2x2 in
  let board_2 = make_connection (0, 1) (1, 1) board_1 in
  let board_3 = make_connection (1, 1) (1, 0) board_2 in
  let board_4 = make_connection (0, 0) (1, 0) board_3 in
  board_4

(** [board_4x4_two_box_vertical] is a 4x4 grid of dots with 2 boxes - one formed
    by the coordinates (0,0), (0,1), (1,1), and (1,0), and another formed by the
    coordinates (1,0), (1,1), (2,1), and (2,0). *)
let board_4x4_two_box_vertical =
  let board_4x4 = make_grid 4 0 in
  let board_1 = make_connection (0, 0) (0, 1) board_4x4 in
  let board_2 = make_connection (0, 1) (1, 1) board_1 in
  let board_3 = make_connection (1, 1) (2, 1) board_2 in
  let board_4 = make_connection (2, 1) (2, 0) board_3 in
  let board_5 = make_connection (2, 0) (1, 0) board_4 in
  let board_6 = make_connection (0, 0) (1, 0) board_5 in
  let board_7 = make_connection (1, 0) (1, 1) board_6 in
  board_7

(** [board_4x4_two_box_horizontal] is a 4x4 grid of dots with 2 boxes - one
    formed by the coordinates (0,0), (0,1), (1,1), and (1,0), and another formed
    by the coordinates (0,1), (0,2), (1,2), and (1,1). *)
let board_4x4_two_box_horizontal =
  let board_4x4 = make_grid 4 0 in
  let board_1 = make_connection (0, 0) (0, 1) board_4x4 in
  let board_2 = make_connection (0, 1) (0, 2) board_1 in
  let board_3 = make_connection (0, 2) (1, 2) board_2 in
  let board_4 = make_connection (1, 2) (1, 1) board_3 in
  let board_5 = make_connection (1, 1) (1, 0) board_4 in
  let board_6 = make_connection (1, 0) (0, 0) board_5 in
  let board_7 = make_connection (0, 1) (1, 1) board_6 in
  board_7

let check_completed_box_tests =
  "Test suite for check_completed_box"
  >::: [
         make_check_completed_box_test "Box is not completed" (0, 0) (0, 1) 0
           board_2x2 [];
         make_check_completed_box_test "Box completed above a horizontal line"
           (0, 0) (1, 0) 1 board_2x2_one_box
           [ (0, 0) ];
         make_check_completed_box_test "Box completed below a horizontal line"
           (0, 1) (1, 1) 1 board_2x2_one_box
           [ (0, 0) ];
         make_check_completed_box_test
           "Box completed to the right of a vertical line" (0, 0) (0, 1) 1
           board_2x2_one_box
           [ (0, 0) ];
         make_check_completed_box_test
           "Box completed to the left of a vertical line" (1, 0) (1, 1) 1
           board_2x2_one_box
           [ (0, 0) ];
         make_check_completed_box_test
           "Two boxes completed after adding a vertical line" (1, 0) (1, 1) 1
           board_4x4_two_box_vertical
           [ (0, 0); (1, 0) ];
         make_check_completed_box_test
           "Two boxes completed after adding a horizontal line" (0, 1) (1, 1) 1
           board_4x4_two_box_horizontal
           [ (0, 0); (0, 1) ];
       ]

(** [make_is_valid_move test_name (x1, y1) (x2, y2) spacing size board
     expected_output] creates a test case to check whether [is_valid_move]
    correctly identifies if a move is valid. *)
let make_is_valid_move_test test_name (x1, y1) (x2, y2) spacing size board
    expected_output =
  test_name >:: fun _ ->
  assert_equal expected_output
    (is_valid_move (x1, y1) (x2, y2) spacing size board)
    ~printer:(fun boolean ->
      Printf.sprintf "Move from (%d,%d) to (%d,%d) is %s" x1 y1 x2 y2
        (if boolean then "valid" else "invalid"))

let is_valid_move_tests =
  "Test suite for make_is_valid_move"
  >::: [
         make_is_valid_move_test "Not valid if p1 and p2 are the same points"
           (0, 0) (0, 0) 100 2 (make_grid 2 0) false;
         make_is_valid_move_test "Not valid if p2 is out of bounds of the grid"
           (0, 0) (500, 500) 100 2 (make_grid 2 0) false;
         make_is_valid_move_test "Not valid if points are non-adjacent" (0, 0)
           (200, 200) 100 2 (make_grid 4 0) false;
         make_is_valid_move_test "Not valid if points are diagonal" (0, 0)
           (100, 100) 100 2 (make_grid 2 0) false;
         make_is_valid_move_test
           "Not valid if line connecting p1 and p2 has already been drawn"
           (0, 0) (0, 1) 100 2 board_2x2_one_box false;
         make_is_valid_move_test
           "Valid if p1 and p2 are nonequal, in bounds, adjacent, and \
            currently non-connected"
           (0, 0) (0, 100) 100 2 (make_grid 2 0) true;
       ]

(** [print_point_opt opt_point] is the string representation of [opt_point]. *)
let print_point_opt opt_point =
  match opt_point with
  | Some (x, y) -> "Some (" ^ string_of_int x ^ ", " ^ string_of_int y ^ ")"
  | None -> "None"

(** [make_find_nearest_point_test test_name (x_in, y_in, size, window_size)
     expected_output] makes a test case with name [test_name] to check if
    [find_nearest dot x_in x_out size window_size] equals the appropriate
    [expected_output]*)
let make_find_nearest_point_test test_name (x_in, y_in, size, window_size)
    expected_output =
  test_name >:: fun _ ->
  assert_equal expected_output
    (find_nearest_dot (x_in, y_in) size window_size)
    ~printer:print_point_opt

let find_nearest_point_tests =
  "Test suite for find_nearest_dot"
  >::: [
         make_find_nearest_point_test "No nearest dots detected" (5, 5, 4, 400)
           None;
         make_find_nearest_point_test "Clicked just outside radius 10"
           (111, 100, 4, 400) None;
         make_find_nearest_point_test "Clicked right on border" (0, 0, 10, 400)
           None;
         make_find_nearest_point_test "Clicked just inside radius 10"
           (141, 150, 4, 400)
           (Some (150, 150));
         make_find_nearest_point_test "Nearest dot found well within radius 10"
           (105, 105, 10, 400)
           (Some (100, 100));
         make_find_nearest_point_test "Clicked right on dot" (50, 50, 4, 400)
           (Some (50, 50));
       ]

let all_grid_tests =
  "All test cases for grid"
  >::: [
         check_completed_box_tests;
         is_valid_move_tests;
         find_nearest_point_tests;
       ]

(*****************************************************************************
 * Simulate playing a game until completion.
 *****************************************************************************)

(** [get_all_points size spacing] returns all points in a [size] x [size] grid.
*)
let get_all_points size spacing =
  List.flatten
    (List.init size (fun i ->
         List.init size (fun j ->
             ((i * spacing) + (spacing / 2), (j * spacing) + (spacing / 2)))))

(** [get_valid_moves_from_point (x, y) spacing size grid] is all possible valid
    moves from [(x, y)] *)
let get_valid_moves_from_point (x, y) spacing size grid =
  let potential_moves =
    [ (x + spacing, y); (x - spacing, y); (x, y + spacing); (x, y - spacing) ]
  in
  List.filter
    (fun (x2, y2) -> is_valid_move (x, y) (x2, y2) spacing size grid)
    potential_moves

(** [make_random_move] make a random valid move on the grid. *)
let make_random_move grid size spacing player =
  let all_points = get_all_points size spacing in
  let points_with_moves =
    List.filter (fun p -> has_available_moves p spacing size grid) all_points
  in
  if points_with_moves = [] then None
  else
    let from_point =
      List.nth points_with_moves (Random.int (List.length points_with_moves))
    in
    let moves = get_valid_moves_from_point from_point spacing size grid in
    let to_point = List.nth moves (Random.int (List.length moves)) in
    let new_board = make_connection from_point to_point grid in
    let completed_boxes =
      get_box_coordinates from_point to_point spacing new_board player
    in
    Some (new_board, completed_boxes <> [])

(** [play_random_game grid size num_players] plays [grid] with [num_players]
    until completion. *)
let play_random_game grid spacing size num_players =
  let player_sequence = ref [] in
  let box_completions = ref [] in
  let rec play player_idx moves board =
    if is_game_over board size then
      (board, moves, true, List.rev !player_sequence, List.rev !box_completions)
    else
      match make_random_move board size spacing player_idx with
      | None ->
          ( board,
            moves,
            true,
            List.rev !player_sequence,
            List.rev !box_completions )
      | Some (new_board, completed_box) ->
          player_sequence := player_idx :: !player_sequence;
          box_completions := completed_box :: !box_completions;
          let next_player =
            if completed_box then player_idx
            else (player_idx + 1) mod num_players
          in
          play next_player (moves + 1) new_board
  in
  play 0 0 grid

(** [validate_turn_sequence player_sequence box_completions player_count] checks
    if the sequence of player turns follows the rules of Dots and Boxes - if a
    player completes a box, they get another turn; otherwise, the turn passes to
    the next player. *)
let validate_turn_sequence player_sequence box_completions player_count =
  let rec check_remaining_turns players boxes expected_player =
    match (players, boxes) with
    | [], [] -> true
    | current_player :: remaining_players, box_completed :: remaining_boxes ->
        current_player = expected_player
        && check_remaining_turns remaining_players remaining_boxes
             (if box_completed then current_player
              else (current_player + 1) mod player_count)
    | _ -> false
  in
  match (player_sequence, box_completions) with
  | [], _ | _, [] -> true
  | first_player :: remaining_players, first_box :: remaining_boxes ->
      check_remaining_turns remaining_players remaining_boxes
        (if first_box then first_player else (first_player + 1) mod player_count)

(** [assert_initial_moves_exist grid size spacing] verifies that the initial
    game board has available moves. *)
let assert_initial_moves_exist grid size spacing =
  let all_points = get_all_points size spacing in
  let points_with_moves =
    List.filter (fun p -> has_available_moves p spacing size grid) all_points
  in
  assert_equal true (points_with_moves <> []) ~printer:(fun boolean ->
      if boolean then "Initial moves exist" else "No initial moves found")

(** [assert_game_completes_properly game_over] verifies that the game reached a
    completed state. *)
let assert_game_completes_properly game_over =
  assert_equal true game_over ~printer:(fun boolean ->
      if boolean then "Game ended" else "Game has not ended")

(** [assert_valid_turn_sequence player_sequence box_completions num_players]
    verifies players took turns correctly. *)
let assert_valid_turn_sequence player_sequence box_completions num_players =
  assert_equal true
    (validate_turn_sequence player_sequence box_completions num_players)
    ~printer:(fun boolean ->
      let status =
        if boolean then "Valid turn sequence" else "Invalid turn sequence"
      in
      Printf.sprintf "%s\nPlayer sequence: %s" status
        (String.concat " " (List.map string_of_int player_sequence)))

(** [assert_completed_boxes_count final_board size] verifies all boxes were
    completed. *)
let assert_completed_boxes_count final_board size =
  assert_equal
    ((size - 1) * (size - 1))
    (completed_boxes final_board)
    ~printer:(fun i -> Printf.sprintf "%d completed boxes" i)

(** [assert_score_count_matches_players scores num_players] verifies score count
    matches player count. *)
let assert_score_count_matches_players scores num_players =
  assert_equal num_players (List.length scores) ~printer:(fun i ->
      Printf.sprintf "Scores for %d players" i)

(** [assert_total_score_matches_boxes scores size] verifies total points match
    boxes. *)
let assert_total_score_matches_boxes scores size =
  let total_score =
    List.fold_left (fun acc (_, score) -> acc + score) 0 scores
  in
  assert_equal
    ((size - 1) * (size - 1))
    total_score
    ~printer:(fun i -> Printf.sprintf "%d points across all players" i)

(** [assert_no_remaining_moves final_board size spacing] verifies no moves
    remain. *)
let assert_no_remaining_moves final_board size spacing =
  let all_points = get_all_points size spacing in
  let final_points_with_moves =
    List.filter
      (fun p -> has_available_moves p spacing size final_board)
      all_points
  in
  assert_equal 0 (List.length final_points_with_moves)
    ~printer:(fun num_of_moves ->
      let status =
        if num_of_moves = 0 then "No moves remaining"
        else Printf.sprintf "%d moves remaining" num_of_moves
      in
      let problem_points =
        if num_of_moves > 0 then
          let point_strings =
            List.map
              (fun (x, y) -> Printf.sprintf "(%d,%d)" x y)
              final_points_with_moves
          in
          "\nPoints with moves: " ^ String.concat ", " point_strings ^ ""
        else ""
      in
      Printf.sprintf "%s%s" status problem_points)

(** [make_play_random_game_test] creates a test case named [test_name],
    simulating a game with [num_players] players on a [size] x [size] grid. *)
let make_play_random_game_test test_name size num_players =
  test_name >:: fun _ ->
  let spacing = 100 in
  let grid = make_grid size num_players in

  (* Initial grid should have available moves. *)
  assert_initial_moves_exist grid size spacing;

  let final_board, moves, game_over, player_sequence, box_completions =
    play_random_game grid spacing size num_players
  in

  (* The game completes properly. *)
  assert_game_completes_properly game_over;

  (* Players take turns. *)
  assert_valid_turn_sequence player_sequence box_completions num_players;
  assert_completed_boxes_count final_board size;

  (* The number of completed boxes is equal to the total number of boxes. *)
  let scores = get_scores final_board in

  (* The number of score entries matches the player count. *)
  assert_score_count_matches_players scores num_players;

  (* The sum of all players' scores is equal to the total number of boxes. *)
  assert_total_score_matches_boxes scores size;

  (* There should be no more available moves. *)
  assert_no_remaining_moves final_board size spacing

let play_random_game_tests =
  "Test suite for simulating playing a game until completion"
  >::: [
         make_play_random_game_test "4x4 game with 2 players" 4 2;
         make_play_random_game_test "6x6 game with 3 players" 6 3;
         make_play_random_game_test "8x8 game with 5 players" 8 5;
         make_play_random_game_test "8x8 game with 10 players" 8 10;
       ]

(*****************************************************************************
 * Tests for board_ui.
 *****************************************************************************)

(** [make_function_executes_test test_name input expected_output] opnes a
    grapics window, then checks if evaluating [input] returns [()] or raises an
    exception. *)
let make_function_executes_test test_name input expected_output =
  test_name >:: fun _ ->
  try
    let output = input in
    assert_equal expected_output output
  with e ->
    (try close_graph () with _ -> ());
    raise e

let draw_grid_tests =
  "Test suite for draw_grid"
  >::: [
         make_function_executes_test "Draws 4x4 grid" (draw_grid 4 4) ();
         make_function_executes_test "Draws 6x6 grid" (draw_grid 6 6) ();
         make_function_executes_test "Draws 6x6 grid" (draw_grid 8 8) ();
       ]

let draw_x_tests =
  "Test suite for draw_x"
  >::: [
         make_function_executes_test "Draws red X"
           (draw_x 2 2 (rgb 255 0 0) 20)
           ();
         (* Referenced https://ocaml.org/manual/4.05/libref/Graphics.html for
                       how to define Ocaml Graphics colors, accessed 4/30/25. *)
         make_function_executes_test "Draws blue X"
           (draw_x 2 2 (rgb 0 0 255) 20)
           ();
       ]

let draw_button_tests =
  "Test suite for draw_button"
  >::: [
         make_function_executes_test "Draws 'replay' button"
           (draw_button 1 1 1 2 "Replay")
           ();
         make_function_executes_test "Draws 'quit' button"
           (draw_button 1 1 1 2 "Quit")
           ();
       ]

let draw_margin_text_tests =
  "Test suite for draw_margin_text"
  >::: [
         make_function_executes_test "draw_margin_text"
           (draw_margin_text "margin" 100 100 20)
           ();
       ]

let center_align_tests =
  "Test suite for center_align"
  >::: [
         make_function_executes_test "center_align"
           (center_align 100 "txt" 200)
           ();
       ]

let draw_game_over_tests =
  "Test suite for draw_game_over"
  >::: [
         make_function_executes_test "draw_scores"
           (draw_scores (make_grid 4 2) 100 100 20)
           ();
         make_function_executes_test "Draw game over screen with one winner"
           (draw_game_over 100 100 [ 1 ])
           ();
         make_function_executes_test
           "Draw game over screen with multiple winners"
           (draw_game_over 100 100 [ 1; 2; 3 ])
           ();
       ]

let draw_rules_screen_test =
  "Test suite for draw_rules_screen"
  >::: [
         make_function_executes_test "Draw rules"
           (draw_rules_screen 1000 100)
           ();
       ]

let redraw_board_tests =
  "Test suite for redraw_board"
  >::: [
         make_function_executes_test "Redraw board with lines"
           (redraw_board 4 100 20
              [ (0, 1, 2, 3, 4); (10, 9, 8, 7, 6) ]
              [ ((1, 2), 3); ((5, 6), 8) ])
           ();
       ]

let make_create_confetti_test test_name x y dx dy color =
  test_name >:: fun _ ->
  let c = create_confetti x y dx dy color in
  assert_equal x (confetti_x c);
  assert_equal y (confetti_y c);
  assert_equal dx (confetti_dx c);
  assert_equal dy (confetti_dy c);
  assert_equal color (confetti_color c)

let create_confetti_tests =
  "Test suite for create_confetti and getter functions"
  >::: [
         make_create_confetti_test "Creates standard particle" 100 100 100 100
           Graphics.blue;
         make_create_confetti_test "Creates standard particle" 400 600 8 4
           Graphics.cyan;
         make_create_confetti_test "Creates static particle" 100 100 0 0
           Graphics.white;
       ]

let generate_confetti_tests =
  "Test suite for generate_confetti"
  >::: [
         ( "Draws confetti" >:: fun _ ->
           let n = 100 in
           let window_w = 800 in
           let window_h = 600 in
           let confetti = generate_confetti n window_w window_h in

           (* The correct number is generated *)
           assert_equal n (List.length confetti) ~printer:string_of_int;

           (* The confetti is within bounds. *)
           List.iter
             (fun c ->
               assert_equal true
                 (confetti_x c >= 0 && confetti_x c <= window_w)
                 ~printer:string_of_bool)
             confetti;

           List.iter
             (fun c ->
               assert_equal true
                 (confetti_y c >= window_h && confetti_y c <= window_h + 200)
                 ~printer:string_of_bool)
             confetti;

           (* The confetti falls within a specified range of velocities. *)
           List.iter
             (fun c ->
               assert_equal true
                 (confetti_dx c >= -2 && confetti_dx c <= 2)
                 ~printer:string_of_bool)
             confetti;

           List.iter
             (fun c ->
               assert_equal true
                 (confetti_dy c <= -8 && confetti_dy c >= -19)
                 ~printer:string_of_bool)
             confetti );
       ]

let make_wait_for_end_choice_test test_name status_str =
  let status =
    match status_str with
    | "replay" -> replay
    | "quit" -> quit
    | _ -> noclick
  in
  test_name >:: fun _ ->
  let window_w = 800 in
  let window_h = 600 in
  let choice_end_func = wait_for_end_choice window_w window_h status in

  assert_equal status_str choice_end_func ~printer:(fun x -> x)

let wait_for_end_choice_test_tests =
  "Test suite for wait_for_end_choice_test"
  >::: [
         make_wait_for_end_choice_test
           "wait_for_end_choice gives 'replay' signal" "replay";
         make_wait_for_end_choice_test "wait_for_end_choice gives 'quit' signal"
           "quit";
       ]

let all_board_ui_tests =
  "All test cases for board_ui"
  >::: [
         draw_grid_tests;
         draw_x_tests;
         draw_button_tests;
         draw_margin_text_tests;
         center_align_tests;
         draw_game_over_tests;
         redraw_board_tests;
         draw_rules_screen_test;
         create_confetti_tests;
         generate_confetti_tests;
         (* wait_for_end_choice_test_tests; *)
       ]

let () =
  run_test_tt_main
    ("Tests for Dots and Boxes"
    >::: [ all_grid_tests; play_random_game_tests; all_board_ui_tests ])
(* try Graphics.close_graph () with Graphics.Graphic_failure _ -> () *)
(* Prompted ChatGPT-4o with "Fatal error: exception
   Graphics.Graphic_failure("graphic screen not opened")" to figure out to open
   and close Graphics for tests, accessed 4/30/25. *)
