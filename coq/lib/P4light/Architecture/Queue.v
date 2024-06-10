Require Import Coq.Lists.List.
Require Import Coq.micromega.Lia.
Require Import Coq.ZArith.ZArith.
Require Import VST.zlist.Zlist.
Import ListNotations.

Open Scope Z_scope.

Section Queue.

  Context {A : Type}.

  Inductive queue := empty_queue | nonempty_queue: list A -> A -> list A -> queue.

  Definition enque (x: A) (que: queue): queue :=
    match que with
    | empty_queue => nonempty_queue [] x []
    | nonempty_queue front mid rear => nonempty_queue front mid (x :: rear)
    end.

  Definition front (que: queue): option A :=
    match que with
    | empty_queue => None
    | nonempty_queue [] mid _ => Some mid
    | nonempty_queue (v :: _) _ _ => Some v
    end.

  Definition deque (que: queue): queue :=
    match que with
    | empty_queue => empty_queue
    | nonempty_queue [] _ [] => empty_queue
    | nonempty_queue [] _ (v :: rear) => nonempty_queue (rev' rear) v []
    | nonempty_queue (_ :: front) mid rear => nonempty_queue front mid rear
    end.

  Definition is_empty (que: queue): bool :=
    match que with
    | empty_queue => true
    | _ => false
    end.

  Definition list_rep (que: queue): list A :=
    match que with
    | empty_queue => nil
    | nonempty_queue front mid rear => front ++ mid :: rev' rear
    end.

  Definition list_enque (l: list A) (que: queue): queue :=
    fold_left (Basics.flip enque) l que.

  Definition list_to_queue (l: list A): queue := list_enque l empty_queue.

  Definition qlength (que: queue): Z :=
    match que with
    | empty_queue => 0
    | nonempty_queue front _ rear => Zlength front + Zlength rear + 1
    end.

  Definition concat_queue (q1 q2: queue): queue := list_enque (list_rep q2) q1.

  Inductive Sublist: list A -> list A -> Prop :=
  | SubNil: forall l, Sublist nil l
  | SubAbsent: forall x l1 l2, Sublist l1 l2 -> Sublist l1 (x :: l2)
  | SubCons: forall x l1 l2, Sublist l1 l2 -> Sublist (x :: l1) (x :: l2).

  Lemma Sublist_incl: forall l1 l2, Sublist l1 l2 -> incl l1 l2.
  Proof.
    intros. induction H.
    - apply incl_nil.
    - apply incl_tl. assumption.
    - apply incl_cons.
      + simpl. left. reflexivity.
      + apply incl_tl. assumption.
  Qed.

  Lemma Sublist_refl: forall l, Sublist l l.
  Proof.
    intros l. induction l. constructor.
    apply SubCons. assumption.
  Qed.

  Definition SubQueue (q1 q2: queue) := Sublist (list_rep q1) (list_rep q2).

  Lemma SubQueue_refl: forall q, SubQueue q q. Proof. intros. apply Sublist_refl. Qed.

  Lemma empty_queue_rep_nil: list_rep empty_queue = [].
  Proof. reflexivity. Qed.

  Lemma is_empty_true_iff: forall q, is_empty q = true <-> q = empty_queue.
  Proof.
    intros; split; intros.
    - destruct q; auto. simpl in H. discriminate.
    - subst. reflexivity.
  Qed.

  Lemma queue_rep_nil_iff: forall q, list_rep q = [] <-> q = empty_queue.
  Proof.
    intros; split; intros.
    - destruct q; auto. simpl in H. symmetry in H.
      apply app_cons_not_nil in H. contradiction.
    - subst. reflexivity.
  Qed.

  Lemma queue_rep_is_empty_iff: forall q, is_empty q = true <-> list_rep q = [].
  Proof. intros. rewrite is_empty_true_iff. symmetry. apply queue_rep_nil_iff. Qed.

  Lemma queue_front: forall q, front q = hd_error (list_rep q).
  Proof. intros. destruct q; simpl; [|destruct l; simpl]; reflexivity. Qed.

  Lemma queue_front_some_iff: forall q x, front q = Some x <-> exists l, list_rep q = x :: l.
  Proof.
    intros; split; intros.
    - destruct q as [|front mid rear]; simpl in H. discriminate.
      destruct front as [|a front]; inversion H.
      + exists (rev' rear). reflexivity.
      + exists (front ++ mid :: rev' rear). reflexivity.
    - destruct H as [l H]. rewrite queue_front, H. reflexivity.
  Qed.

  Lemma enque_not_eq_empty: forall x q, enque x q <> empty_queue.
  Proof. intros. destruct q; simpl; discriminate. Qed.

  Lemma rev'_eq: forall {A} (l: list A), rev' l = rev l.
  Proof. intros. unfold rev'. rewrite rev_alt. reflexivity. Qed.

  Lemma enque_eq: forall q x, list_rep (enque x q) = list_rep q ++ [x].
  Proof.
    intros. destruct q; simpl in *.
    - reflexivity.
    - rewrite <- app_assoc, <- app_comm_cons, !rev'_eq. reflexivity.
  Qed.

  Lemma deque_eq: forall q, list_rep (deque q) = tl (list_rep q).
  Proof.
    intros. destruct q as [|front mid rear]; simpl; auto.
    destruct front, rear; simpl; rewrite !rev'_eq; reflexivity.
  Qed.

  Lemma list_enque_eq: forall l q,
      list_rep (list_enque l q) = list_rep q ++ l.
  Proof.
    induction l; intros; simpl.
    - rewrite app_nil_r. reflexivity.
    - rewrite IHl. unfold Basics.flip. rewrite enque_eq, <- app_assoc. reflexivity.
  Qed.

  Lemma list_to_queue_eq: forall l, list_rep (list_to_queue l) = l.
  Proof. intros. unfold list_to_queue. rewrite list_enque_eq. reflexivity. Qed.

  Lemma qlength_eq: forall que, qlength que = Zlength (list_rep que).
  Proof.
    intros. destruct que as [|front mid rear]; simpl; auto.
    rewrite Zlength_app, Zlength_cons, rev'_eq, Zlength_rev. lia.
  Qed.

  Lemma qlength_nonneg: forall que, 0 <= qlength que.
  Proof. intros. rewrite qlength_eq. apply Zlength_nonneg. Qed.

  Lemma qlength_enque: forall a que, qlength (enque a que) = qlength que + 1.
  Proof.
    intros. rewrite !qlength_eq, enque_eq, Zlength_app, Zlength_cons, Zlength_nil. lia.
  Qed.

  Lemma concat_queue_eq: forall q1 q2, list_rep (concat_queue q1 q2) = list_rep q1 ++ list_rep q2.
  Proof.
    intros. unfold concat_queue.
    destruct q1 as [|f1 m1 r1]; destruct q2 as [|f2 m2 r2]; simpl; auto.
    - rewrite list_enque_eq. simpl. reflexivity.
    - rewrite app_nil_r. reflexivity.
    - rewrite list_enque_eq. simpl. reflexivity.
  Qed.

  Lemma qlength_concat: forall q1 q2, qlength (concat_queue q1 q2) = qlength q1 + qlength q2.
  Proof. intros. rewrite !qlength_eq, concat_queue_eq, Zlength_app. reflexivity. Qed.

  Lemma qlength_0_iff: forall que, qlength que = 0 <-> que = empty_queue.
  Proof.
    intros. split; intros.
    - destruct que; auto. rewrite qlength_eq in H. simpl in H.
      rewrite rev'_eq in H. list_solve.
    - subst. simpl. reflexivity.
  Qed.

  Lemma list_enque_not_empty1: forall l q, q <> empty_queue -> list_enque l q <> empty_queue.
  Proof.
    induction l; intros.
    - destruct q. 1: contradiction. simpl. discriminate.
    - simpl. unfold Basics.flip. apply IHl. destruct q; simpl; discriminate.
  Qed.

  Lemma list_enque_not_empty2: forall l q, l <> [] -> list_enque l q <> empty_queue.
  Proof.
    intros. destruct l. 1: contradiction. simpl. unfold Basics.flip.
    apply list_enque_not_empty1. destruct q; simpl; discriminate.
  Qed.

  Lemma concat_queue_eq_empty: forall q1 q2,
      concat_queue q1 q2 = empty_queue -> q1 = empty_queue /\ q2 = empty_queue.
  Proof.
    unfold concat_queue. intros. destruct q1, q2.
    - split; reflexivity.
    - simpl in H. exfalso. revert H. apply list_enque_not_empty2.
      destruct l; simpl; discriminate.
    - simpl in H. inversion H.
    - simpl in H. exfalso. revert H. apply list_enque_not_empty1. discriminate.
  Qed.

  Lemma empty_queue_dec: forall q, {q = empty_queue} + {q <> empty_queue}.
  Proof. intros. destruct q; [left; reflexivity | right; discriminate]. Qed.

  Lemma list_enque_app: forall l1 l2 q, list_enque (l1 ++ l2) q = list_enque l2 (list_enque l1 q).
  Proof. intros. unfold list_enque. rewrite fold_left_app. reflexivity. Qed.

  Lemma enque_eq_inv: forall e1 e2 q1 q2, enque e1 q1 = enque e2 q2 -> e1 = e2 /\ q1 = q2.
  Proof. intros. destruct q1, q2; simpl in H; inversion H; split; auto. Qed.

  Lemma enque_eq_concat: forall p ps q1 q2,
      enque p ps = concat_queue q1 q2 -> q2 <> empty_queue -> exists q3, ps = concat_queue q1 q3.
  Proof.
    intros. revert dependent p. revert ps q1. destruct q2. 1: contradiction.
    clear H0. unfold concat_queue; intros. simpl in *.
    destruct l, l0; simpl in *; unfold Basics.flip in H; rewrite ?rev'_eq in *; simpl in *.
    - exists empty_queue. apply enque_eq_inv in H. destruct H. simpl. assumption.
    - exists (nonempty_queue [] a l0). simpl. unfold Basics.flip. rewrite rev'_eq.
      rewrite list_enque_app in H. simpl in H. unfold Basics.flip in H.
      apply enque_eq_inv in H. destruct H. assumption.
    - rewrite list_enque_app in H. simpl in H. unfold Basics.flip in H.
      apply enque_eq_inv in H. destruct H. exists (nonempty_queue [] a0 (rev l)). simpl.
      unfold Basics.flip. rewrite rev'_eq, rev_involutive. assumption.
    - rewrite app_comm_cons, app_assoc, list_enque_app in H. simpl in H.
      unfold Basics.flip in H. apply enque_eq_inv in H. destruct H.
      exists (nonempty_queue (a0 :: l) a l0). simpl. unfold Basics.flip. rewrite rev'_eq.
      assumption.
  Qed.

  Lemma concat_queue_empty: forall q, concat_queue q empty_queue = q.
  Proof. destruct q; simpl; reflexivity. Qed.

  Lemma enque_eq_concat_same_prefix: forall p ps q,
      enque p ps = concat_queue ps q -> q = enque p empty_queue.
  Proof.
    intros. pose proof (qlength_concat ps q). pose proof (qlength_enque p ps).
    rewrite H, H0 in H1. assert (qlength q = 1) by lia. clear H0 H1.
    unfold concat_queue in H. destruct q; simpl in *. 1: discriminate.
    pose proof (Zlength_nonneg l). pose proof (Zlength_nonneg l0).
    assert (Zlength l = 0) by lia. assert (Zlength l0 = 0) by lia.
    apply Zlength_nil_inv in H3, H4. subst. rewrite rev'_eq in H. simpl in H.
    unfold Basics.flip in H. apply enque_eq_inv in H. destruct H. subst. reflexivity.
  Qed.

  Lemma concat_queue_assoc: forall q1 q2 q3,
      concat_queue (concat_queue q1 q2) q3 = concat_queue q1 (concat_queue q2 q3).
  Proof.
    intros. unfold concat_queue. rewrite list_enque_eq, <- list_enque_app. reflexivity.
  Qed.

  Lemma enque_concat_queue: forall p q1 q2,
      enque p (concat_queue q1 q2) = concat_queue q1 (enque p q2).
  Proof.
    intros. unfold concat_queue. rewrite enque_eq, list_enque_app. simpl.
    unfold Basics.flip. reflexivity.
  Qed.

  Lemma list_enque_nonempty: forall l fr mid rear,
      list_enque l (nonempty_queue fr mid rear) =
        nonempty_queue fr mid (rev l ++ rear).
  Proof.
    induction l; intros; simpl; auto. unfold Basics.flip. simpl. rewrite IHl.
    rewrite <- app_assoc. reflexivity.
  Qed.

End Queue.

(** Test Begin

Definition test := Eval cbv in repeat 0 30000.

Time Compute (let s := (rev' test) in length s).
(* Finished transaction in 0.137 secs (0.135u,0.001s) (successful) *)

Time Compute (let s := (rev test) in length s).
(* Finished transaction in 8.533 secs (8.518u,0.015s) (successful) *)

Test End *)

Arguments queue _ : clear implicits.

Definition qmap {A B: Type} (f: A -> B) (que: queue A) : queue B :=
  match que with
  | empty_queue => empty_queue
  | nonempty_queue front mid rear => nonempty_queue (map f front) (f mid) (map f rear)
  end.

Lemma qmap_map: forall {A B} (f: A -> B) (que: queue A),
    list_rep (qmap f que) = map f (list_rep que).
Proof.
  intros. destruct que; simpl; auto. rewrite !rev'_eq, map_app. simpl.
  rewrite map_rev. reflexivity.
Qed.

(* Goal Sublist [1; 3] [1; 2; 3]. *)
(* Proof. *)
(*   do 4 econstructor. *)
(* Qed. *)

(* Goal Sublist [1; 2] [1; 2; 3]. *)
(* Proof. *)
(*   do 4 econstructor. *)
(* Qed. *)

(* Goal Sublist [2] [1; 2; 3]. *)
(* Proof. *)
(*   do 4 econstructor. *)
(* Qed. *)

(* Goal ~ Sublist [1; 2] [2; 1]. *)
(* Proof. *)
(*   intro H. inversion_clear H. inversion_clear H0; inversion_clear H. *)
(* Qed. *)
