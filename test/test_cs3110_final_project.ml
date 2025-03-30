open OUnit2
open Cs3110_final_project.Grid

(** [board_2x2] is a 2x2 grid of dots. *)
let board_2x2 = make_grid 2

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
    (completed_box_coordinates (x1, y1) (x2, y2) spacing board)
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
  let board_4x4 = make_grid 4 in
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
  let board_4x4 = make_grid 4 in
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

(* let board_2x2_one_box_comp =
  let board_1 = make_connection (0, 0) (0, 1) board_2x2 in
  let board_2 = make_connection (0, 1) (1, 1) board_1 in
  let board_3 = make_connection (1, 1) (1, 0) board_2 in
  let _ = completed_box_coordinates (0, 0) (0, 1) 100 board_3 in
  board_3 *)

let completed_boxes_tests =
  "test suite for make_completed_boxes"
  >::: [
         make_completed_boxes_test "no completed boxes" (make_grid 2) 0;
         (* make_completed_boxes_test "no completed boxes"
           (let _ =
              completed_box_coordinates (0, 0) (0, 1) 100 board_2x2_one_box_comp
            in
            board_2x2_one_box)
           1; *)
       ]

let _ =
  run_test_tt_main
    ("all tests"
    >::: [
           make_connection_tests;
           check_completed_box_tests;
           completed_boxes_tests;
         ])
