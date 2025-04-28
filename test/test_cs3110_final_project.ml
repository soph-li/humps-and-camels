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

let make_connection_test =
  "connect (0,0) and (0,1)" >:: fun _ ->
  let new_board = make_connection (0, 0) (0, 1) board_2x2 in
  let lst = get_grid new_board in

  assert_bool "(0,0) should be connected to (0,1)"
    (List.exists
       (fun ((x, y), neighbors) -> (x, y) = (0, 0) && List.mem (0, 1) neighbors)
       lst);

  assert_bool "(0,1) should be connected to (0,0)"
    (List.exists
       (fun ((x, y), neighbors) -> (x, y) = (0, 1) && List.mem (0, 0) neighbors)
       lst)

let make_connection_tests =
  "test suite for make_connection" >::: [ make_connection_test ]

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
  "test suite for check_completed_box"
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

(** [make_completed_boxes_test test_name board expected_output] creates a test
    case to check whether [completed_boxes grid] matches the actual number of
    completed boxes in the game. *)
let make_completed_boxes_test test_name board expected_output =
  test_name >:: fun _ ->
  (* print_endline (string_of_int (completed_boxes board)); *)
  print_endline "\n Expected ";
  print_string (string_of_int expected_output);
  print_endline "Actual ";
  print_string (string_of_int (completed_boxes board));
  assert_equal expected_output (completed_boxes board)

let board_2x2_one_box_comp =
  let board_1 = make_connection (0, 0) (0, 1) board_2x2 in
  let board_2 = make_connection (0, 1) (1, 1) board_1 in
  let board_3 = make_connection (1, 1) (1, 0) board_2 in
  let _ = get_box_coordinates (0, 0) (0, 1) 100 board_3 0 in
  board_3

let completed_boxes_tests =
  "test suite for make_completed_boxes"
  >::: [
         make_completed_boxes_test "no completed boxes" (make_grid 2 0) 0;
         (* make_completed_boxes_test "no completed boxes" (let _ =
            completed_box_coordinates (0, 0) (0, 1) 100 board_2x2_one_box_comp
            in board_2x2_one_box) 1; *)
       ]

(** [make_is_game_over test_name board size expected_output] creates a test case
    to check whether [is_game_over grid size] correctly identifies if the game
    is complete. *)
let make_is_game_over_test test_name board size expected_output =
  test_name >:: fun _ ->
  (* print_endline (string_of_int (completed_boxes board)); *)
  assert_equal expected_output (is_game_over board size)

let is_game_over_tests =
  "test suite for make_completed_boxes"
  >::: [
         make_is_game_over_test "a game with no completed boxes is not over"
           (make_grid 2 0) 2 false;
         (* make_is_game_over_test "a game with a 2x2 grid with 1 completed box
            is over" (board_2x2_one_box) 2 true; *)
         make_is_game_over_test
           "a game with a 4x4 grid with 2 completed boxes is over"
           board_4x4_two_box_vertical 4 false;
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
(*****************************************************************************)

(** Generate a list of all possible points on a board of given size *)
let generate_all_points size spacing =
  let rec gen_x x acc =
    if x >= size * spacing then acc else gen_x (x + spacing) (x :: acc)
  in
  let rec gen_y y acc =
    if y >= size * spacing then acc else gen_y (y + spacing) (y :: acc)
  in
  let xs = gen_x 0 [] in
  let ys = gen_y 0 [] in
  List.concat (List.map (fun x -> List.map (fun y -> (x, y)) ys) xs)

let _ =
  run_test_tt_main
    ("all tests"
    >::: [
           make_connection_tests;
           check_completed_box_tests;
           completed_boxes_tests;
           is_game_over_tests;
           is_valid_move_test;
         ])
