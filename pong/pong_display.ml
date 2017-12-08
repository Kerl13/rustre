open Graphics

let () =
  open_graph "" ;
  set_window_title "Pong" ;
  let _ = wait_next_event [Key_pressed] in
  ()
