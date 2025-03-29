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
}
(** A Grid is composed of [grid] and [completed boxes]. [grid] has keys that are
    points and values of sets of points*)

let make_grid size =
  { grid = Hashtbl.create (size * size); completed_boxes = 0 }

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

let completed_box_coordinates (x1, y1) (x2, y2) spacing board =
  let x1, y1, x2, y2 = order_coordinates (x1, y1) (x2, y2) in
  let grid = board.grid in
  let completed_boxes = ref [] in

  (* Check for a completed box above the current horizontal line. *)
  if y1 = y2 then
    let bottom_left = (x1, y1) in
    let bottom_right = (x2, y2) in
    let top_left = (x1, y1 + spacing) in
    let top_right = (x2, y2 + spacing) in
    if is_box_closed bottom_left bottom_right top_left top_right grid then (
      completed_boxes := bottom_left :: !completed_boxes;
      board.completed_boxes <- board.completed_boxes + 1)
    else ()
  else ();

  (* Check for a completed box below the current horizontal line. *)
  if y1 = y2 then
    let bottom_left = (x1, y1 - spacing) in
    let bottom_right = (x2, y2 - spacing) in
    let top_left = (x1, y1) in
    let top_right = (x2, y2) in
    if is_box_closed bottom_left bottom_right top_left top_right grid then (
      completed_boxes := bottom_left :: !completed_boxes;
      board.completed_boxes <- board.completed_boxes + 1)
    else ()
  else ();

  (* Check for a completed box to the right of the current vertical line. *)
  if x1 = x2 then
    let bottom_left = (x1, y1) in
    let bottom_right = (x1 + spacing, y1) in
    let top_left = (x2, y2) in
    let top_right = (x2 + spacing, y2) in
    if is_box_closed bottom_left bottom_right top_left top_right grid then (
      completed_boxes := bottom_left :: !completed_boxes;
      board.completed_boxes <- board.completed_boxes + 1)
    else ()
  else ();

  (* Check for a completed box to the left of the current vertical line. *)
  if x1 = x2 then
    let bottom_left = (x1 - spacing, y1) in
    let bottom_right = (x1, y1) in
    let top_left = (x2 - spacing, y2) in
    let top_right = (x2, y2) in
    if is_box_closed bottom_left bottom_right top_left top_right grid then (
      completed_boxes := bottom_left :: !completed_boxes;
      board.completed_boxes <- board.completed_boxes + 1)
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
