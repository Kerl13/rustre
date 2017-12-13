(* This file is generated by Why3's Coq driver *)
(* Beware! Only edit allowed sections below    *)
Require Import BuiltIn.
Require BuiltIn.
Require int.Int.
Require int.Abs.
Require int.ComputerDivision.

(* Why3 assumption *)
Definition unit := unit.

(* Why3 assumption *)
Inductive state :=
  | mk_state : bool -> Z -> Z -> state.
Axiom state_WhyType : WhyType state.
Existing Instance state_WhyType.

(* Why3 assumption *)
Definition var3 (v:state): Z := match v with
  | (mk_state x x1 x2) => x2
  end.

(* Why3 assumption *)
Definition var1 (v:state): Z := match v with
  | (mk_state x x1 x2) => x1
  end.

(* Why3 assumption *)
Definition usTfbyF (v:state): bool :=
  match v with
  | (mk_state x x1 x2) => x
  end.

(* Why3 goal *)
Theorem prop_ind : forall (out__1:Z) (out__2:Z) (uss:state) (uss2:state)
  (uss3:state),
  (match match (usTfbyF uss) with
  | false => let var4 := (var3 uss) in let var2 := (var1 uss) in (var2, var4)
  | true => let var4 := 0%Z in let var2 := 0%Z in (var2, var4)
  end with
  | (x, x1) => let cpt := (x + 1%Z)%Z in ((out__1 = (x1 + cpt)%Z) /\
      ((((ZArith.BinInt.Z.quot (cpt * (cpt + 1%Z)%Z)%Z 2%Z) = out__1) /\
      (((var3 uss2) = out__1) /\ (((var1 uss2) = cpt) /\
      (((usTfbyF uss2) = false) /\
      (((ZArith.BinInt.Z.quot (cpt * (cpt + 1%Z)%Z)%Z 2%Z) = out__1) \/
      ((~ ((ZArith.BinInt.Z.quot (cpt * (cpt + 1%Z)%Z)%Z 2%Z) = out__1)) /\
      (false = true))))))) \/
      ((~ ((ZArith.BinInt.Z.quot (cpt * (cpt + 1%Z)%Z)%Z 2%Z) = out__1)) /\
      (((var3 uss2) = out__1) /\ (((var1 uss2) = cpt) /\
      (((usTfbyF uss2) = false) /\
      (((ZArith.BinInt.Z.quot (cpt * (cpt + 1%Z)%Z)%Z 2%Z) = out__1) \/
      ((~ ((ZArith.BinInt.Z.quot (cpt * (cpt + 1%Z)%Z)%Z 2%Z) = out__1)) /\
      (false = true)))))))))
  end /\
  match match (usTfbyF uss2) with
  | false => let var4 := (var3 uss2) in let var2 := (var1 uss2) in (var2,
      var4)
  | true => let var4 := 0%Z in let var2 := 0%Z in (var2, var4)
  end with
  | (x, x1) => let cpt := (x + 1%Z)%Z in ((out__2 = (x1 + cpt)%Z) /\
      ((((ZArith.BinInt.Z.quot (cpt * (cpt + 1%Z)%Z)%Z 2%Z) = out__2) /\
      (((var3 uss3) = out__2) /\ (((var1 uss3) = cpt) /\
      ((usTfbyF uss3) = false)))) \/
      ((~ ((ZArith.BinInt.Z.quot (cpt * (cpt + 1%Z)%Z)%Z 2%Z) = out__2)) /\
      (((var3 uss3) = out__2) /\ (((var1 uss3) = cpt) /\
      ((usTfbyF uss3) = false))))))
  end) -> forall (x:Z) (x1:Z),
  (match (usTfbyF uss2) with
  | false => let var4 := (var3 uss2) in let var2 := (var1 uss2) in (var2,
      var4)
  | true => let var4 := 0%Z in let var2 := 0%Z in (var2, var4)
  end = (x, x1)) -> let cpt := (x + 1%Z)%Z in
  ((((ZArith.BinInt.Z.quot (cpt * (cpt + 1%Z)%Z)%Z 2%Z) = out__2) ->
  ((~ ((ZArith.BinInt.Z.quot (cpt * (cpt + 1%Z)%Z)%Z 2%Z) = out__2)) ->
  (false = true))) /\
  ((~ ((ZArith.BinInt.Z.quot (cpt * (cpt + 1%Z)%Z)%Z 2%Z) = out__2)) ->
  ((~ ((ZArith.BinInt.Z.quot (cpt * (cpt + 1%Z)%Z)%Z 2%Z) = out__2)) ->
  (false = true)))).
intros out__1 out__2 uss uss2 uss3 (h1,h2) x x1 h3 cpt.

simpl in * |- *.
split.
intro.
intro.
eauto.
intros.
case_eq (usTfbyF uss2).
intro.
rewrite H1 in h2.
simpl in h2.
case_eq (usTfbyF uss).
intro.
rewrite H2 in h1.
simpl in h1.
rewrite H1 in h1.
eauto.
destruct h1.
intuition.
intuition.
rewrite H2 in h1.
simpl in h1.
intuition.
eauto.
intuition.
rewrite H1 in h1.
simpl in h1.
intuition.
eauto.
case_eq (usTfbyF uss).
intros.
rewrite H2 in h1.
simpl in h1.
intuition.
rewrite H1 in h2.
simpl in h2.
rewrite H1 in h3.
inversion h3.
rewrite H10 in h2.
rewrite H11 in h2.
simpl in h2.
destruct h2.
destruct H12.
destruct H12.
intuition.
destruct H12.
destruct H13.
unfold cpt in H.
unfold cpt in H0.
destruct
intuition.
intuition.
intuition.

rewrite H8 in h3.

Qed.
