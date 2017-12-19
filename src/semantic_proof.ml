open Ast_parsing
open Pp_utils

let rec spec_print_state_flat pref states ppf s =
  let st = List.assoc s states in
  let st = List.filter (function
      | Ast_object.Mach_var (_, target) -> List.assoc target states <> []
      | _ -> true) st in
  fprintf ppf "%a" (pp_list_brk ", " (fun ppf sc ->
      match sc with
      | Ast_object.State_var(i, ty) -> fprintf ppf "%s%s: stream %a" pref i pp_ty ty
      | Ast_object.Mach_var (i, target) -> fprintf ppf "%a" (spec_print_state_flat (pref ^ i) states) target)) st

let spec_count = ref 0
let node_ind = ref []

let rec spec_print_intro pref states ppf s =
  let st = List.assoc s states in
  let st = List.filter (function
      | Ast_object.Mach_var (_, target) -> List.assoc target states <> []
      | _ -> true) st in
  fprintf ppf "%a" (pp_list_brk "" (fun ppf sc ->
      match sc with
      | Ast_object.State_var(i, _) -> fprintf ppf "%s%s" pref i
      | Ast_object.Mach_var (i, target) -> fprintf ppf "%a" (spec_print_intro (pref ^ i) states) target)) st

let rec extract_apps = function
  | Ast_object.SSeq(a, b) -> extract_apps a @ extract_apps b
  | Ast_object.SCall(var_in, inst, id, var_out) ->
    [var_in, inst, id, var_out]
  | Ast_object.SAssign _ -> []
  | Ast_object.SCase _ -> []
  | Ast_object.SSkip -> []
  | Ast_object.SReset _ -> []

let spec_node states f_obj ppf node =
  let node_obc = List.find (fun n -> n.Ast_object.name = node.n_name) f_obj.Ast_object.objf_machines in
  let _, _, _, step = node_obc.Ast_object.step in
  let apps = extract_apps step in
  let f ppf () =
    fprintf ppf "%a %a"
      (pp_list_brk "" (fun ppf (s, _) -> fprintf ppf "%s" s)) (node.n_input @ node.n_output)
      (spec_print_intro "st" states) node.n_name
  in
  fprintf ppf "intros %a %a.@\n"
    (pp_list_brk "" (fun ppf (s, _) -> fprintf ppf "%s" s)) (node.n_input @ node.n_output)
    (spec_print_intro "st" states) node.n_name;

  let st = List.assoc node.n_name states in
  if st <> [] then
    fprintf ppf "intros (h_init, h_rec).@\ninversion h_init.@\n"
  else
    fprintf ppf "intros h_rec.@\n";
  if !spec_count = 0 then
    fprintf ppf "unfold spec.@\n"
  else
    fprintf ppf "unfold spec%d.@\n" !spec_count;
  fprintf ppf "Ltac simpl_all u1 := repeat match goal with \
  | [ H: context[u1] |- _ ] => unfold u1 in H; simpl in H \
  end. \
 \
  Ltac s h2 %a := match goal with \
  | [ |- _ /\\ _] => split \
  | [H: _ /\\ _ |- _] => destruct H \
  | [ |-  _ = sfby _ _] => \
    apply sext \
  | [ |-  _ = splus ?A ?B] => \
    apply sext \
  | [ |-  _ = sminus ?A ?B] => \
    apply sext \
  | [ |-  _ = sgt ?A ?B] => \
    apply sext \
  | [ |-  _ = seq ?A ?B] => \
    apply sext \
  | [ |-  _ = sconst _] => \
    apply sext \
  | [ |- forall n:nat, ?C] => \
      intros nk; \
      pose (h2 nk) as nk2; \
      try match goal with \
      | [ |- context [S ?n]] => pose (h2 (S n)) as nk3 \
      end \
  | [ H : ?A = ?C, K : ?C = ?D |- ?A = ?D] => rewrite H; eauto \
  | [ H: context[get (splus _ _)] |- _] => rewrite splus_rw in H \
  | [ H: context[get (sconst _)] |- _] => rewrite sconst_rw in H \
  | [ |- context[get (sgt ?a ?b) ?n] ] => \
    let dec_bool := fresh \"dec_bool\" in \
    pose (gt_dec (get a n) (get b n)) as dec_bool; \
    let dec1 := fresh dec_bool in \
    let dec2 := fresh dec_bool in \
    destruct dec_bool as [dec1 | dec2]; \
    let c := fresh \"c\" in \
    ((pose (sgt_rw_true a b n dec1) as c; clearbody c; rewrite c; clear c) \
    || (pose (sgt_rw_false a b n dec2) as c; clearbody c; rewrite c; clear c)) \
  | [ |- context[get (seq ?a ?b) ?n]] => \
    let dec_bool := fresh \"dec_bool\" in \
    pose (Z.eq_dec (get a n) (get b n)) as dec_bool; \
    let dec1 := fresh dec_bool in \
    let dec2 := fresh dec_bool in \
    destruct dec_bool as [dec1 | dec2]; \
    let c := fresh \"c\" in \
    ((pose (seq_rw_true a b n dec1) as c; clearbody c; rewrite c; clear c) \
    || (pose (seq_rw_false a b n dec2) as c; clearbody c; rewrite c; clear c)) \
  | [ |- context[get (splus _ _) _]] => rewrite splus_rw \
  | [ |- context[get (sconst _) _]] => rewrite sconst_rw \
  | [ |- context[get (sfby _ _) (O)]] => rewrite sfby_rw_o \
  | [ |- context[get (sfby _ _) (S _)]] => rewrite sfby_rw_s \
  | [ |- context[get (sfby _ _) ?n]] => \
    let n2 := fresh \"n\" in \
    destruct n as [ _ | n2 ]; \
    let h2n := fresh \"h2pn\" in \
    try pose (h2 n2) as h2n; try inversion h2n \
  | [ H: ?A <> ?B, H2: ?A = ?B -> ?C |- _ ] => clear H2 \
  | [ H: ?A = ?B, H2: ?A <> ?B -> ?C |- _ ] => clear H2 \
  | [ H : (?A = ?B -> ?C) /\\ (?A <> ?B -> ?D) |- _ ] => \
    let h := fresh H in \
    let h' := fresh H in \
    destruct H as (h & h'); \
    let u := fresh \"dec\" in \
    pose (Z.eq_dec A B) as u; \
    let u1 := fresh u in \
    let u2 := fresh u in \
    destruct u as [u1 | u2]; \
    (let h'' := fresh h in \
    pose (h u1) as h'';  clearbody h''; clear h) || \
    (let h'' := fresh h in \
    pose (h' u2) as h''; clearbody h''; clear h) \
  | [ |- ?a <-> ?b ] => split; intro \
%a \
%a \
  | [H: ?A -> _, H2:?A |- _] => \
    let h := fresh H in \
    pose (H H2) as h; \
    clearbody h; \
    clear H \
  | [H: (?c = true) -> False |- _ ] => \
    let c2 := fresh \"c\" in \
    case_eq c; intro c2; (destruct (H c2) || clear H) \
  end. \
 \
  Ltac unfold_eq := match goal with \
  | [ H: ?A = ?E |- _] => \
    match E with \
    | context[is_eq ?C ?D] => \
      let y := fresh \"y\" in \
      pose (is_eq_def C D) as y; \
      clearbody y; \
      symmetry in H \
      end \
  end. \
 \
  Ltac simpl_pairs := repeat match goal with \
  | [ H: pair _ _ = pair _ _ |- _ ] => inversion H; clear H \
  end. \
 \
Ltac solve u1 h_rec %a := simpl_all u1; repeat (repeat (s h_rec %a); simpl_pairs; try congruence; unfold_eq). \
solve step_fonct_full%a h_rec %a.@\n"
    f ()
    (fun ppf () ->
       if st <> [] then
         fprintf ppf "  | [ |- context[reset_state]] => unfold reset_state"
    ) ()
    (pp_list_n "" (fun ppf (var_in, id, machine_name, var_out) ->
         let i = List.assoc machine_name !node_ind in
         fprintf ppf "| [ |- spec%a %a ] => "
           (fun ppf () ->
              if i <> 0 then fprintf ppf "%d" i
           ) ()
           (pp_list " " (fun ppf (s) ->
                let s = match s with
                    Ast_object.Var s | Ast_object.Loc s -> s
                  | Ast_object.State _ -> assert false in
                fprintf ppf "%s" s)) (var_in @ var_out);
         fprintf ppf "apply valid%a with %a@\n"
           (fun ppf () ->
              if i <> 0 then fprintf ppf "%d" i
           ) ()
           (spec_print_intro ("st" ^ id) states) machine_name;
       )) apps
    f () f ()
    (fun ppf () ->
       if !spec_count <> 0 then fprintf ppf "%d" !spec_count
    ) () f ();

  fprintf ppf "Qed.@\n@.";

  node_ind := (node.n_name, !spec_count)::!node_ind;
  incr spec_count

(* award winning hack *)
let find_offset f =
  let a = open_in f in
  let lo0 = ref 0 in
  try
    while true do
      let l = pos_in a in
      let s = input_line a in
      let i = "intros" in
      if String.length s >= String.length i && String.sub s 0 (String.length i) = i then
        lo0 := l;
    done; !lo0
  with
  | End_of_file -> !lo0


let spec_file ppf (states, f, f_obj) =
  spec_count := 0;
  node_ind := [];
  List.iter (fun n ->
      let f = (Format.sprintf "spec/spec_Spec%s_valid_1.v" n.n_name) in
      let a = open_out_gen [Open_wronly] 0 f in
      seek_out a (find_offset f);
      spec_node states f_obj (Format.formatter_of_out_channel a) n;
      close_out a;
    ) f.f_nodes;
  spec_count := 0;
  node_ind := [];
  fprintf ppf "%a" (pp_list_n "\n" (spec_node states f_obj)) f.f_nodes
