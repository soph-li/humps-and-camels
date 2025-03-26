open Hashtbl
(** Prompted ChatGPT-4o, "how to make Hashtbl custom type in Ocaml", accessed
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
  mutable grid : (point, PointSet.t) Hashtbl.t;
  completed_boxes : int;
}
(** A Grid is composed of [grid] and [completed boxes]. [grid] has keys that are
    points and values of sets of points*)

(** Function stubs *)
let make_grid size = { grid = Hashtbl.create size; completed_boxes = 0 }

let is_complete { grid; completed_boxes } =
  completed_boxes = Hashtbl.length grid * Hashtbl.length grid

let completed_boxes { grid; completed_boxes } = completed_boxes

(** [add_to_set key neighbor g spacing] adds [neighbor] to the set of connected
    points for [key] in [grid]. If [g] is not in [grid], it initializes an empty
    set before adding [neighbor]. *)
let add_to_set key neighbor grid =
  let neighbors =
    try Hashtbl.find grid key with Not_found -> PointSet.empty
  in
  Hashtbl.replace grid key (PointSet.add neighbor neighbors)

let check_completed_box board spacing = failwith "not implemented"

let make_connection (x1, y1) (x2, y2) board =
  let grid = board.grid in
  add_to_set (x1, y1) (x2, y2) grid;
  add_to_set (x2, y2) (x1, y1) grid;

  board
