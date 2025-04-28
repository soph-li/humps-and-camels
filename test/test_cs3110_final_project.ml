open OUnit2
open Cs3110_final_project.Grid
open Cs3110_final_project.Board_ui

(** [board_2x2] is a 2x2 grid of dots. *)
let board_2x2 = make_grid 2 0

(** [print_list lst] returns a string representation of [lst]. *)
let rec print_list lst =
  match lst with
  | [] -> ""
  | [ (x, y) ] -> "(" ^ string_of_int x ^ ", " ^ string_of_int y ^ ")"
  | (x, y) :: t ->
      "(" ^ string_of_int x ^ ", " ^ string_of_int y ^ ")" ^ "; " ^ print_list t

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
         make_check_completed_box_test "box is not completed" (0, 0) (0, 1) 0
           board_2x2 [];
         make_check_completed_box_test "box completed above a horizontal line"
           (0, 0) (1, 0) 1 board_2x2_one_box
           [ (0, 0) ];
         make_check_completed_box_test "box completed below a horizontal line"
           (0, 1) (1, 1) 1 board_2x2_one_box
           [ (0, 0) ];
         make_check_completed_box_test
           "box completed to the right of a vertical line" (0, 0) (0, 1) 1
           board_2x2_one_box
           [ (0, 0) ];
         make_check_completed_box_test
           "box completed to the left of a vertical line" (1, 0) (1, 1) 1
           board_2x2_one_box
           [ (0, 0) ];
         make_check_completed_box_test
           "two boxes completed after adding a vertical line" (1, 0) (1, 1) 1
           board_4x4_two_box_vertical
           [ (0, 0); (1, 0) ];
         make_check_completed_box_test
           "two boxes completed after adding a horizontal line" (0, 1) (1, 1) 1
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

let is_valid_move_test =
  "test suite for make_is_valid_move"
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

(*****************************************************************************)
(*****************************************************************************)

(** [get_all_points size spacing] returns all points in a [size x size] grid. *)
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
  let rec play player_idx moves board =
    if is_game_over board size then (board, moves, true)
    else
      match make_random_move board size spacing player_idx with
      | None -> (board, moves, true)
      | Some (new_board, completed_box) ->
          let next_player =
            if completed_box then player_idx
            else (player_idx + 1) mod num_players
          in
          play next_player (moves + 1) new_board
  in
  play 0 0 grid

(** [make_play_random_game_test] creates a test case named [test_name],
    simulating a game with [num_players] players on a [size x size] grid. *)
let make_play_random_game_test test_name size num_players =
  test_name >:: fun _ ->
  let spacing = 100 in
  let grid = make_grid size num_players in

  (* Initial board should have available moves. *)
  let all_points = get_all_points size spacing in
  let points_with_moves =
    List.filter (fun p -> has_available_moves p spacing size grid) all_points
  in
  assert_equal true (points_with_moves <> []);

  let final_board, moves, game_over =
    play_random_game grid spacing size num_players
  in
  (* The game completes properly. *)
  assert_equal true game_over;
  (* The number of completed boxes is equal to the total number of boxes. *)
  assert_equal
    ((size - 1) * (size - 1))
    (completed_boxes final_board)
    ~printer:string_of_int;
  (* The sum of all players' scores is equal to the total number of boxes. *)
  let scores = get_scores final_board in
  let total_score =
    List.fold_left (fun acc (_, score) -> acc + score) 0 scores
  in
  assert_equal ((size - 1) * (size - 1)) total_score ~printer:string_of_int;
  (* There should be no more available moves. *)
  let final_points_with_moves =
    List.filter
      (fun p -> has_available_moves p spacing size final_board)
      all_points
  in
  assert_equal 0 (List.length final_points_with_moves) ~printer:string_of_int

let play_random_game_test =
  "Simulate playing a game until completion"
  >::: [
         make_play_random_game_test "4x4 game" 4 2;
         make_play_random_game_test "6x6 game" 6 3;
         make_play_random_game_test "8x8 game" 8 5;
       ]

let _ =
  run_test_tt_main
    ("all tests"
    >::: [
           check_completed_box_tests; is_valid_move_test; play_random_game_test;
         ])
