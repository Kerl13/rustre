open Graphics


let width = 1000
let height = 400
let paddle_length = 30
let ball_radius = 8


let draw_left_wall () =
  fill_rect 20 20 5 (5 + height + 5)

let draw_right_wall () =
  set_color white ;
  fill_rect (25 + width) 0 5 (height + 50 + 50) ;
  set_color (rgb 230 230 230) ;
  fill_rect (25 + width) 20 5 (5 + height + 5) ;
  set_color foreground

let write_score score =
  moveto 100 (height + 50 + 10) ;
  set_color white ;
  fill_rect 100 (height + 50 + 10) 200 40 ;
  set_color foreground ;
  draw_string ("Score = " ^ (string_of_int score))

let draw_arena score =
  write_score score ;
  draw_left_wall () ;
  draw_right_wall () ;
  fill_rect 20 20 (5 + width + 5) 5 ;
  fill_rect 20 (25 + height) (5 + width + 5) 5 ;
  ()

let draw_ball, erase_ball =
  let draw c x y =
    set_color c ;
    fill_circle (25 + x) (25 + y) ball_radius;
    set_color foreground in
  draw red, draw white

let draw_paddle y =
  set_color blue ;
  fill_rect (25 + width) (25 + y - paddle_length / 2) 5 paddle_length ;
  set_color foreground


let init () =
  open_graph "" ;
  auto_synchronize false ;
  set_window_title "Pong" ;
  resize_window (width + 50) (height + 50 + 50) ;
  draw_arena 0


type state = {
  x : int ;
  y : int ;
  paddle : int ;
  score : int
}

let rec loop state =
  let line =
    try input_line stdin
    with End_of_file -> exit 0
  in
  let x, y, paddle, score =
    try Scanf.sscanf line "(%d, %d, %d, %d)" (fun x y p s -> x, y, p, s)
    with Scanf.Scan_failure msg ->
      Format.eprintf "Ill formed input: %s\nExiting@." msg ;
      exit 1
  in
  erase_ball state.x state.y ;
  let state = {
    x = x ; y = y ;
    paddle = paddle ;
    score = score
  } in
  draw_arena state.score ;
  draw_paddle paddle ;
  draw_ball x y ;
  synchronize () ;
  loop state


let () =
  init () ;
  loop { x = 10 ; y = 10 ; paddle = 100 ; score = 0 }
