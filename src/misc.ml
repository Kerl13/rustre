let pp_string fmt s =
  Format.fprintf fmt "%s" s

let rec pp_list sep pp ppf = function
  | [] -> Format.fprintf ppf ""
  | [x] -> Format.fprintf ppf "%a" pp x
  | x :: xs -> Format.fprintf ppf "%a%s%a" pp x sep (pp_list sep pp) xs
