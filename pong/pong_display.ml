open Graphics


let width = 800
let height = 400


let draw_left_wall () =
  fill_rect 20 20 5 (5 + height + 5)

let draw_right_wall () =
  fill_rect (25 + width) 20 5 (5 + height + 5)

let draw_arena () =
  fill_rect 20 20 (5 + width + 5) 5 ;
  fill_rect 20 (25 + height) (5 + width + 5) 5 ;
  draw_left_wall () ;
  draw_right_wall () ;
  ()

let draw_ball, erase_ball =
  let draw c x y =
    set_color c ;
    fill_circle (25 + x) (25 + y) 5 ;
    set_color foreground in
  draw red, draw white


let init () =
  open_graph "" ;
  auto_synchronize false ;
  set_window_title "Pong" ;
  resize_window (width + 50) (height + 50) ;
  draw_arena ()


let rec loop prev_x prev_y =
  let line =
    try input_line stdin
    with End_of_file -> exit 0
  in
  let x, y =
    try Scanf.sscanf line "(%d, %d, %d)" (fun x y _ -> x, y)
    with Scanf.Scan_failure msg ->
      Format.eprintf "Ill formed input: %s\nExiting@." msg ;
      exit 1
  in
  erase_ball prev_x prev_y ;
  draw_arena () ;
  draw_ball x y ;
  synchronize () ;
  loop x y


let () =
  init () ;
  loop 10 10
