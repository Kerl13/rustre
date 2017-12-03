
let state = { Int_accumulation__Nodeswitcher.inst1 = Int_accumulation__Nodeaccumulation.{ var1 = 0; } }

let () =
  for i = 0 to 100 do
    Int_accumulation__Nodeswitcher.step state i true |> Format.printf "%d@."
  done;
