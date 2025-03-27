open OUnit2
open Cs3110_final_project.Grid

let make_connection_test' =
  "connect (0,0) and (0,1)" >:: fun _ ->
  let board = make_grid 2 in
  let new_board = make_connection (0, 0) (0, 1) board in
  let lst = get_grid new_board in

  assert_bool "(0,0) should be connected to (0,1)"
    (List.exists
       (fun ((x, y), neighbors) -> (x, y) = (0, 0) && List.mem (0, 1) neighbors)
       lst);

  assert_bool "(0,1) should be connected to (0,0)"
    (List.exists
       (fun ((x, y), neighbors) -> (x, y) = (0, 1) && List.mem (0, 0) neighbors)
       lst)

let make_connection_test =
  "test suite for make_connection" >::: [ make_connection_test' ]

let _ = run_test_tt_main ("all tests" >::: [ make_connection_test ])
