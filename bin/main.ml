open Graphics

(** Prompted ChatGPT-4o, "How to install OCaml Graphics", accessed 3/25/25. *)

(** Prompted ChatGPT-4o, "What should I do if I encountered Fatal error:
    exception Graphics.Graphic_failure("Cannot open display ")", accessed
    3/25/25. *)

(** Prompted ChatGPT-4o, "How to install Xvfb"*)

(** Prompted ChatGPT-4o, "How to use OCaml Graphics", accessed 3/25/25. *)

(** Used "https://ocaml.org/manual/4.03/libref/Graphics.html", accessed 3/25/25.
*)

(** [draw_grid size window_size] draws a grid of dots with [size] rows and
    [size] columns. Requires: [size] and [window_size] are positive. *)
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

let () =
  open_graph " 400x400";
  draw_grid 4 400;
  ignore (read_key ());
  close_graph ()
