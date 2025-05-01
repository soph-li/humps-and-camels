open Hashtbl
(** Prompted ChatGPT-4o, "how to make Hashtbl custom type in OCaml", accessed
    3/23/35. *)

type point = int * int

(** Referenced code under functional interface for special Hash table for keys,
    https://ocaml.org/manual/5.3/api/Hashtbl.html, accessed 3/23/25. *)
module PointHash = struct
  type t = point

  let equal (x1, y1) (x2, y2) = x1 = x2 && y1 = y2

  (** Used large primes for hashing from
      https://planetmath.org/goodhashtableprimes *)
  let hash (x, y) = ((x * 53) + (y * 97)) land max_int
end

module PointHashtbl = Hashtbl.Make (PointHash)
(** Make Hashtbl where keys are points. *)

(** Point module used to create a set of points. *)
module OrderedPairPoint = struct
  type t = point

  (* Points are compared by their first coordinate, then the second if the
     firsts are the same *)
  let compare (x1, y1) (x2, y2) =
    let x_res = compare x1 x2 in
    if x_res = 0 then compare y1 y2 else x_res
end

module PointSet = Set.Make (OrderedPairPoint)
(** The values of the Hashtabl are sets of points. *)

type t = {
  grid : (point, PointSet.t) Hashtbl.t;
  mutable completed_boxes : int;
  scores : (int, int) Hashtbl.t;
}
(** A Grid is composed of [grid] and [completed boxes]. [grid] has keys that are
    points and values of sets of points. *)

let make_grid size num_players =
  let scores = Hashtbl.create num_players in
  for i = 0 to num_players - 1 do
    Hashtbl.add scores i 0
  done;
  { grid = Hashtbl.create (size * size); completed_boxes = 0; scores }

let is_game_over { grid; completed_boxes } size =
  completed_boxes = (size - 1) * (size - 1)

let completed_boxes { grid; completed_boxes } = completed_boxes

(** [add_to_set key neighbor g spacing] adds [neighbor] to the set of connected
    points for [key] in [grid]. If [key] is not in [grid], it initializes an
    empty set before adding [neighbor]. *)
let add_to_set key neighbor grid =
  let neighbors =
    try Hashtbl.find grid key with Not_found -> PointSet.empty
  in
  Hashtbl.replace grid key (PointSet.add neighbor neighbors)

(** [is_box_closed bottom_left bottom_right top_left top_right grid] checks
    whether [bottom_left], [bottom_right], [top_left], [top_right] are in [grid]
    and form a box. *)
let is_box_closed bottom_left bottom_right top_left top_right grid =
  if
    Hashtbl.mem grid bottom_left
    && Hashtbl.mem grid bottom_right
    && Hashtbl.mem grid top_left && Hashtbl.mem grid top_right
  then
    PointSet.mem bottom_right (Hashtbl.find grid bottom_left)
    && PointSet.mem bottom_right (Hashtbl.find grid top_right)
    && PointSet.mem bottom_left (Hashtbl.find grid bottom_right)
    && PointSet.mem bottom_left (Hashtbl.find grid top_left)
    && PointSet.mem top_right (Hashtbl.find grid bottom_right)
    && PointSet.mem top_right (Hashtbl.find grid top_left)
    && PointSet.mem top_left (Hashtbl.find grid bottom_left)
    && PointSet.mem top_left (Hashtbl.find grid top_right)
  else false

(** [order_coordinates (x1, y1) (x2, y2)] orders the two pairs of coordinates
    lexicographically. It returns [(x_low, y_low, x_high, y_high)] where
    [x_low < x_high] or [x_high < y_high]. *)
let order_coordinates (x1, y1) (x2, y2) =
  if x1 > x2 || (x1 = x2 && y1 > y2) then (x2, y2, x1, y1) else (x1, y1, x2, y2)

let get_box_coordinates (x1, y1) (x2, y2) spacing board player =
  let x1, y1, x2, y2 = order_coordinates (x1, y1) (x2, y2) in
  let grid = board.grid in
  let completed_boxes = ref [] in

  let update_points bottom_left =
    completed_boxes := bottom_left :: !completed_boxes;
    board.completed_boxes <- board.completed_boxes + 1;
    let current_score =
      try Hashtbl.find board.scores player with Not_found -> 0
    in
    Hashtbl.replace board.scores player (current_score + 1)
  in

  (* Check for a completed box above the current horizontal line. *)
  if y1 = y2 then
    let bottom_left = (x1, y1) in
    let bottom_right = (x2, y2) in
    let top_left = (x1, y1 + spacing) in
    let top_right = (x2, y2 + spacing) in
    if is_box_closed bottom_left bottom_right top_left top_right grid then
      update_points bottom_left
    else ()
  else ();

  (* Check for a completed box below the current horizontal line. *)
  if y1 = y2 then
    let bottom_left = (x1, y1 - spacing) in
    let bottom_right = (x2, y2 - spacing) in
    let top_left = (x1, y1) in
    let top_right = (x2, y2) in
    if is_box_closed bottom_left bottom_right top_left top_right grid then
      update_points bottom_left
    else ()
  else ();

  (* Check for a completed box to the right of the current vertical line. *)
  if x1 = x2 then
    let bottom_left = (x1, y1) in
    let bottom_right = (x1 + spacing, y1) in
    let top_left = (x2, y2) in
    let top_right = (x2 + spacing, y2) in
    if is_box_closed bottom_left bottom_right top_left top_right grid then
      update_points bottom_left
    else ()
  else ();

  (* Check for a completed box to the left of the current vertical line. *)
  if x1 = x2 then
    let bottom_left = (x1 - spacing, y1) in
    let bottom_right = (x1, y1) in
    let top_left = (x2 - spacing, y2) in
    let top_right = (x2, y2) in
    if is_box_closed bottom_left bottom_right top_left top_right grid then
      update_points bottom_left
    else ()
  else ();

  !completed_boxes

let make_connection (x1, y1) (x2, y2) board =
  let grid = board.grid in
  add_to_set (x1, y1) (x2, y2) grid;
  add_to_set (x2, y2) (x1, y1) grid;
  board

let get_grid board =
  Hashtbl.fold
    (fun key neighbors acc -> (key, PointSet.elements neighbors) :: acc)
    board.grid []

(** [distance_sq (x1, y1) (x2, y2)] returns the squared distance between
    [(x1, y1)] and [(x2, y2)]. *)
let distance_sq (x1, y1) (x2, y2) =
  ((x2 - x1) * (x2 - x1)) + ((y2 - y1) * (y2 - y1))

let is_valid_move (x1, y1) (x2, y2) spacing size board =
  let dist_sq = distance_sq (x1, y1) (x2, y2) in
  if x2 < 0 || x2 >= size * 100 || y2 < 0 || y2 >= size * 100 then false
  else if (x1, y1) = (x2, y2) then false
  else if dist_sq > spacing * spacing then false
  else
    match Hashtbl.find_opt board.grid (x1, y1) with
    | Some neighbors -> not (PointSet.mem (x2, y2) neighbors)
    | None -> true

let has_available_moves (x, y) spacing size board =
  let potential_moves =
    [ (x + spacing, y); (x - spacing, y); (x, y + spacing); (x, y - spacing) ]
  in
  List.exists
    (fun (x2, y2) -> is_valid_move (x, y) (x2, y2) spacing size board)
    potential_moves

let get_scores board =
  Hashtbl.fold (fun player score acc -> (player, score) :: acc) board.scores []

(** [get_all_points size spacing] returns all points in a [size x size] grid. *)
let get_all_dots size spacing =
  List.flatten
    (List.init size (fun i ->
         List.init size (fun j ->
             ((i * spacing) + (spacing / 2), (j * spacing) + (spacing / 2)))))

let find_nearest_dot (x, y) size window_size =
  let radius = 10 in

  let spacing = window_size / size in
  let dots = get_all_dots size spacing in

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
