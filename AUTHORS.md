Yanwei Liu (ycl6), Sophia Li (syl57), Cheryl Lam (cwl92)

------
main.ml

Prompted ChatGPT-4o, "How to install OCaml Graphics", accessed 3/22/25.

Prompted ChatGPT-4o, "How to install Xvfb", accessed 3/22/25.

Prompted ChatGPT-4o, "How to use OCaml Graphics", accessed 3/22/25.

Basic board setup interface adapted from
   "https://ocaml.org/manual/4.03/libref/Graphics.html", accessed 3/22/25.

Prompted ChatGPT-4o, "Why are my mouse clicks not working in Ocaml using
   XQuartz, used to debug follow_mouse accessed 3/23/25."

Prompted ChatGPT-4o, "How to handle window closure in OCaml Graphics", for close_graph,
   accessed 3/25/25.

Prompted ChaptGPT-4o "Is there pre-set alignment in OCaml Graphics" accessed
   4/8/25.

Prompted ChatGPT-4o with main function and line 19 "what type is
   color_list", accessed 5/10/25.
   
Prompted ChatGPT-4o, "How to draw line leaving point, following user mouse
     position, Ocaml graphics.", for lines 135,136, accesssed 4/1/25.

Prompted ChatGPT-4o, "How to tell if mouse button pressed," original code
     for lines 150-175 logic, accessed 4/2/25. 

Referenced https://ocaml.org/p/graphics/5.1.1/doc/Graphics/index.html for mouse events
     in "if event.button" branch, lines 150-175, accessed 4/2/25.

Prompted ChaptGPT-4o "how to connect mutually recursive functions" along with
   follow_mouse and play to figure out to use "and," lines 130-198, accessed 4/14/25.


------
board_ui.ml

Prompted ChaptGPT-4o "Is there pre-set alignment in OCaml Graphics," lines 229-236, 
    accessed 4/8/25.

Prompted ChaptGPT-4o "How to fix flickering screen with clear_graph for
     display," lines 238-264, accessed 4/4/25.

Prompted ChatGPT-4o, "How to wrap text from rules based on window width and
     window height in OCaml", Adapted lines 270-281 from ChatGPT, accessed
     5/5/25.

Prompted ChatGPT-4o, "How to change font in OCaml Graphics", lines 292,293, accessed
     5/5/25.


Prompted ChatGPT-4o "Animation physics guide in OCaml Graphics," lines 18-26, 117-146, accessed
        4/20/25.

Prompted ChatGPT-4o "How to have OCaml Graphics animation while not entirely clearing board," 
        lines 18-26, 117-146, accessed 4/22/25.

Prompted ChatGPT-4o "How to generate random particles in OCaml Graphics," lines 18-26, 117-146, 
    accessed 4/22/25.

Referenced https://youtu.be/GiA6ls9mOL4?si=L-gneW9pyqKtq-Dh for confetti animation, accessed 
    4/20/25.

Referenced 
        https://adarsh-gupta.medium.com/animating-confetti-in-javascript-a-step-by-step-guide-34b23c31d8e0
        for confetti animation, lines 18-26, 117-146, accessed 4/21/25.

------
grid.ml

Prompted ChatGPT-4o, "how to make Hashtbl custom type in OCaml", line 1, accessed
    3/23/35.

Referenced code under functional interface for special Hash table for keys,
    https://ocaml.org/manual/5.3/api/Hashtbl.html, lines 9-17, accessed 3/23/25.

Used large primes for hashing from https://planetmath.org/goodhashtableprimes, line 17, accessed 
    3/23/25. 

------
test.ml

Prompted ChatGPT-4o, "What should I do if I encountered Fatal error:
   exception Graphics.Graphic_failure("Cannot open display ")", lines 1, 401, accessed
   3/22/25. 

Referenced https://ocaml.org/manual/4.05/libref/Graphics.html for
    how to define Ocaml Graphics colors, lines 416, 422, accessed 4/30/25.

Prompted ChatGPT-4o with "How to test function that returns a variant type or
        test return type is string OCaml", lines 592-599, accessed 5/1/25.