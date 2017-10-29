Require Import ZArith.

Inductive ty := | Int | Bool | TPair (A B: ty).

Inductive ident :=
| Var (id: nat)
| State (id: nat).

Definition obj := nat.

Inductive expr: (ident -> ty) -> ty -> Type :=
  | EVar E t (i: ident): E i = t -> expr E t
  | EInt E (i: Z): expr E Int
  | EBool E (b: bool): expr E Bool
  | EAdd E: expr E Int -> expr E Int -> expr E Int
  | EMinus E: expr E Int -> expr E Int -> expr E Int
  | EMult E: expr E Int -> expr E Int -> expr E Int
  | ENot E: expr E Bool -> expr E Bool.

Inductive pat :=
| PElt (_: ident)
| PPair (_ _: pat).

Definition machine_id := nat.

Fixpoint ty_for_pat E p := match p with
| PElt a => E a
| PPair a b => TPair (ty_for_pat E a) (ty_for_pat E b)
end.

Inductive statement (E:ident -> ty) (M: machine_id -> ty*ty) :=
  | SAssign (x: ident) (e: expr E (E x))
  | SSeq: statement E M -> statement E M -> statement E M
  | SSkip
  | SCall (p: pat) (m: machine_id) (p2: pat): (ty_for_pat E p, ty_for_pat E p2) = M m -> statement E M
  | SReset (o: machine_id)
  | SIf (e: expr E Bool) (s1 s2: statement E M).