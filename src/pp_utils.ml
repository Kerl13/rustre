let fprintf = Format.fprintf

let rec pp_list sep pp ppf = function
 | [] -> fprintf ppf ""
 | [x] -> fprintf ppf "%a" pp x
 | x :: xs -> fprintf ppf "%a%s%a" pp x sep (pp_list sep pp) xs

let rec pp_list_n sep pp ppf = function
  | [] -> fprintf ppf ""
  | [x] -> fprintf ppf "%a" pp x
  | x :: xs -> fprintf ppf "%a%s@\n%a" pp x sep (pp_list_n sep pp) xs

 let rec pp_list_brk sep pp ppf = function
   | [] -> fprintf ppf ""
   | [x] -> fprintf ppf "%a" pp x
   | x :: xs -> fprintf ppf "%a%s@ %a" pp x sep (pp_list_brk sep pp) xs
