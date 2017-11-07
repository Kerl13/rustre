(* TODO : add type annotations *)

let node chrono (StSt, Rst) = (disp_1, disp_2) where
  automaton
    CHRONO ->
      do automaton
        | STOP ->
            do s = 0 -> last s
            and m = 0 -> last m
            and run = false
            unless StSt continue START
        | START ->
            let d = 0 -> (pre d + 1) mod 100 in
            do s = if d < pre d
                   then (last s + 1) mod 60
                   else last s
            and m = if s < last s
                    then (last m + 1) mod 60
                    else last m
            and run = true
            unless StSt continue STOP
         end
       until Rst and not run then CHRONO
    end and
    automaton
    | TIME ->
      do disp_1 = s
        and disp_2 = m
        until Rst and run then LAP
    | LAP ->
      do until Rst then TIME
  end
