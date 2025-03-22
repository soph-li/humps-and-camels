open Graphics

(** Prompted ChatGPT-4o, "How to install OCaml Graphics", accessed 3/25/25. *)

(** Prompted ChatGPT-4o, "What should I do if I encountered Fatal error:
    exception Graphics.Graphic_failure("Cannot open display ")", accessed
    3/25/25. *)

(** Prompted ChatGPT-4o, "How to install Xvfb", accessed 3/25/25. *)

(** Prompted ChatGPT-4o, "How to use OCaml Graphics", accessed 3/25/25. *)

(** Adapted from "https://ocaml.org/manual/4.03/libref/Graphics.html", accessed
    3/25/25. *)

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

(** [distance_sq (x1, y1) (x2, y2)] returns the squared distance between
    [(x1, y1)] and [(x2, y2)]. *)
let distance_sq (x1, y1) (x2, y2) =
  ((x2 - x1) * (x2 - x1)) + ((y2 - y1) * (y2 - y1))

(** [get_all_dots] returns all dots in a [size] x [size] grid. *)
let get_all_dots size window_size =
  let spacing = window_size / size in
  List.flatten
    (List.init size (fun i ->
         List.init size (fun j ->
             ((i * spacing) + (spacing / 2), (j * spacing) + (spacing / 2)))))

(** [find_nearest_dot x y size window_size] returns the nearest dot to [(x, y)]
    in the grid. *)
let find_nearest_dot x y size window_size =
  let radius = 10 in

  let dots = get_all_dots size window_size in

  let nearest_dot =
    List.fold_left
      (fun acc (dot_x, dot_y) ->
        let dist_sq = distance_sq (x, y) (dot_x, dot_y) in
        match acc with
        | None -> Some (dot_x, dot_y, dist_sq)
        | Some (_, _, min_dist_sq) ->
            if dist_sq < min_dist_sq then Some (dot_x, dot_y, dist_sq) else acc)
      None dots
  in

  match nearest_dot with
  | Some (dot_x, dot_y, dist_sq) ->
      if dist_sq <= radius * radius then Some (dot_x, dot_y) else None
  | _ -> None

(** [draw_line size window_size color] draws a [color] line connecting the two
    dots that are closest to the positions where the user clicked in the grid.
*)
let draw_line size window_size color =
  let rec wait_for_valid_click () =
    let event = wait_next_event [ Button_down ] in
    let x, y = (event.mouse_x, event.mouse_y) in
    match find_nearest_dot x y size window_size with
    | None -> wait_for_valid_click ()
    | Some (dot_x, dot_y) -> Some (dot_x, dot_y)
  in

  let first_dot = wait_for_valid_click () in
  match first_dot with
  | None -> ()
  | Some (dot1_x, dot1_y) -> (
      let second_dot = wait_for_valid_click () in
      match second_dot with
      | None -> ()
      | Some (dot2_x, dot2_y) ->
          set_color color;
          moveto dot1_x dot1_y;
          lineto dot2_x dot2_y)

let () =
  print_endline "Enter board size: ";

  let size = read_int () in
  let window_size = size * 100 in
  open_graph (" " ^ string_of_int window_size ^ "x" ^ string_of_int window_size);

  draw_grid size window_size;
  draw_line size window_size blue;

  ignore (read_key ());
  close_graph ()
