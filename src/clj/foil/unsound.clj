(ns foil.unsound
  (:require [clojure.test :as t]
            [clojure.walk :as w])
  (:refer-clojure :exclude [gensym])
  (:import [clojure.lang IAtom]))

;; http://okmij.org/ftp/ML/generalization.html

;; (*
;;   Simple Hindley-Milner type checker for pure lambda-calculus with let
;;   Explanation of efficient generalization -- Remy algorithm

;;   This code is written to illustrate the need to check the type environment
;;   when generalizing.

;;   The generalization function below is unsound: it quantifies free type
;;   variables in a type with no regard for the type environment.
;;   The problem is fixed in sound_*.ml files.
;; *)

;; (* The language: lambda-calculus with let *)
;; type varname = string

;; type exp =
;;   | Var of varname                      (* variable                *)
;;   | App of exp * exp                    (* application: e1 e2      *)
;;   | Lam of varname * exp                (* abstraction: fun x -> e *)
;;   | Let of varname * exp * exp          (* let x = e in e2         *)
;; ;;

;; (* The types to infer *)
;; (* Types without QVar (quantified variables) are simple types;
;;    those containing QVar are type schemas.
;;    Since quantifiers are always on the outside in the HM system,
;;    they are implied and not explicitly represented.
;; *)
;; type qname = string
;; type typ =
;;   | TVar of tv ref               (* type (schematic) variable *)
;;   | QVar of qname                (* quantified type variable *)
;;   | TArrow of typ * typ
;; and tv = Unbound of string | Link of typ
;; ;;

;; let gensym_counter = ref 0
;; let reset_gensym : unit -> unit =
;;   fun () -> gensym_counter := 0
;; ;;

(def gensym-counter (atom 0))

(defn reset-gensym []
  (reset! gensym-counter 0))

;; let gensym : unit -> string = fun () ->
;;   let n = !gensym_counter in
;;   let () = incr gensym_counter in
;;   if n < 26 then String.make 1 (Char.chr (Char.code 'a' + n))
;;             else "t" ^ string_of_int n
;; ;;

(defn gensym []
  (let [n @gensym-counter]
    (swap! gensym-counter inc)
    (if (< n 26)
      (symbol (str (char (+ (int \a) n))))
      (symbol (str "t" n)))))

;; (* Make a fresh type variable *)
;; let newvar : unit -> typ =
;;  fun () -> TVar (ref (Unbound (gensym ())))
;; ;;

(defn newvar []
  [:tvar (atom [:unbound (gensym)])])

;; (* check to see if a TVar (the first argument) occurs in the type
;;    given as the second argument. Fail if it does.
;; *)
;; let rec occurs : tv ref -> typ -> unit = fun tvr -> function
;;   | TVar tvr' -> if tvr == tvr' then failwith "occurs check"
;;   | TVar {contents = Link ty} -> occurs tvr ty
;;   | TArrow (t1,t2) -> occurs tvr t1; occurs tvr t2
;;   | _ -> ()
;; ;;

(defn occurs [tvr ty]
  (when (vector? ty)
    (cond
      (= :tvar (first ty))
      (cond
        (= tvr ty)
        (assert false "occurs check")
        (= :link (first @(second ty)))
        (occurs tvr (second @(second tvr))))

      (= :tarrow (first ty))
      (let [[_ t1 t2] ty]
        (occurs tvr t1)
        (occurs tvr t2)))))

;; (* Simplistic.  No path compression *)
;; (* Also, QVar are unexpected: they should've been instantiated *)
;; let rec unify : typ -> typ -> unit = fun t1 t2 ->
;;   if t1 == t2 then ()                   (* t1 and t2 are physically the same *)
;;   else match (t1,t2) with
;;   | (TVar ({contents = Unbound _} as tv),t')
;;   | (t',TVar ({contents = Unbound _} as tv)) -> occurs tv t'; tv := Link t'
;;   | (TVar {contents = Link t1},t2)
;;   | (t1,TVar {contents = Link t2}) -> unify t1 t2
;;   | (TArrow (tyl1,tyl2), TArrow (tyr1,tyr2)) ->
;;       unify tyl1 tyr1;
;;       unify tyl2 tyr2
;;   (* everything else is error *)
;; ;;

(defn unify [t1 t2]
  (when-not (= t1 t2)
    (cond
      (and (= :tvar (first t1))
           (= :unbound (first @(second t1))))
      (do (occurs t1 t2)
          (reset! (second t1) [:link t2]))

      (and (= :tvar (first t2))
           (= :unbound (first @(second t2))))
      (do (occurs t2 t1)
          (reset! (second t2) [:link t1]))

      (and (= :tvar (first t1))
           (= :link (first @(second t1))))
      (unify (second @(second t1)) t2)

      (and (= :tvar (first t2))
           (= :link (first @(second t2))))
      (unify t1 (second @(second t2)))

      (and (= :tarrow (first t1))
           (= :tarrow (first t2)))
      (let [[_ tyl1 tyl2] t1
            [_ tyr1 tyr2] t2]
        (unify tyl1 tyr1)
        (unify tyl2 tyr2))

      :else
      (assert false (str t1 " != " t2)))))

;; (* The type environment *)
;; type env = (varname * typ) list
;; ;;

;; (* Unsound generalization: ignores the environment  *)
;; (* and converts all free TVar in the type into QVar *)
;; let rec gen : typ -> typ = function
;;   | TVar {contents = Unbound name} -> QVar name
;;   | TVar {contents = Link ty}      -> gen ty
;;   | TArrow (ty1,ty2) -> TArrow (gen ty1, gen ty2)
;;   | ty -> ty
;; ;;

(defn gen [ty]
  (cond
    (and (= :tvar (first ty))
         (= :unbound (first @(second ty))))
    [:qvar (second @(second ty))]

    (and (= :tvar (first ty))
         (= :link (first @(second ty))))
    (gen (second @(second ty)))

    (= :tarrow (first ty))
    (let [[_ ty1 ty2] ty]
      [:tarrow (gen ty1) (gen ty2)])

    :else
    ty))

;; (* instantiation: replace schematic variables with fresh TVar
;; *)
;; let inst : typ -> typ =
;;   let rec loop subst = function
;;     | QVar name ->
;;         begin
;;           try (List.assoc name subst, subst)
;;           with Not_found ->
;;             let tv = newvar () in
;;             (tv, (name,tv)::subst)
;;         end
;;     | TVar {contents = Link ty} -> loop subst ty
;;     | TArrow (ty1,ty2) ->
;;         let (ty1,subst) = loop subst ty1 in
;;         let (ty2,subst) = loop subst ty2 in
;;         (TArrow (ty1,ty2), subst)
;;     | ty -> (ty, subst)
;;   in fun ty -> fst (loop [] ty)
;; ;;

(defn inst [ty]
  (first
   ((fn loop [subst ty]
      (cond
        (= :qvar (first ty))
        (if-let [ty (get subst (second ty))]
          [ty subst]
          (let [tv (newvar)]
            [tv (assoc subst (second ty) tv)]))

        (and (= :tvar (first ty))
             (= :link (first @(second ty))))
        (loop subst (second @(second ty)))

        (= :tarrow (first ty))
        (let [[_ ty1 ty2] ty
              [ty1 subst] (loop subst ty1)
              [ty2 subst] (loop subst ty2)]
          [[:tarrow ty1 ty2] subst])

        :else
        [ty subst]))
    {} ty)))


;; (* Trivial type checker. Type checking errors are delivered
;;    as exceptions
;; *)
;; let rec typeof : env -> exp -> typ = fun env -> function
;;   | Var x     -> inst (List.assoc x env)
;;   | Lam (x,e) ->
;;       let ty_x = newvar () in
;;       let ty_e = typeof ((x,ty_x)::env) e in
;;       TArrow(ty_x,ty_e)
;;   | App (e1,e2) ->
;;       let ty_fun = typeof env e1 in
;;       let ty_arg = typeof env e2 in
;;       let ty_res = newvar () in
;;       unify ty_fun (TArrow (ty_arg,ty_res));
;;       ty_res
;;   | Let (x,e,e2) ->
;;       let ty_e = typeof env e in
;;       typeof ((x,gen ty_e)::env) e2
;; ;;

(defn typeof [env exp]
  (cond
    (= :var (first exp))
    (let [[_ x] exp]
      (assert (contains? env x) (str "unknown var: " x))
      (inst (get env x)))

    (= :lam (first exp))
    (let [[_ x e] exp
          ty-x (newvar)
          ty-e (typeof (assoc env x ty-x) e)]
      [:tarrow ty-x ty-e])

    (= :app (first exp))
    (let [[_ e1 e2] exp
          ty-fun (typeof env e1)
          ty-arg (typeof env e2)
          ty-res (newvar)]
      (unify ty-fun [:tarrow ty-arg ty-res])
      ty-res)

    (= :let (first exp))
    (let [[_ x e e2] exp
          ty-e (typeof env e)]
      (typeof (assoc env x (gen ty-e)) e2))))

(defn clean-type [typ]
  (w/prewalk #(if (instance? IAtom %) @% %) typ))

(defn infer [exp]
  (reset-gensym)
  (clean-type (typeof {} exp)))

(t/deftest test-infer
;; let id = Lam("x",Var"x");;
;; let c1 = Lam("x",Lam("y",App (Var"x",Var"y")));;

;; let TArrow (TVar {contents = Unbound "a"}, TVar {contents = Unbound "a"})
;;    = reset_gensym ();
;;      typeof [] id
;; ;;

;; let
;;  TArrow
;;  (TVar
;;    {contents =
;;      Link
;;       (TArrow (TVar {contents = Unbound "b"}, TVar {contents = Unbound "c"}))},
;;  TArrow (TVar {contents = Unbound "b"}, TVar {contents = Unbound "c"}))
;;  =
;;    reset_gensym ();
;;    typeof [] c1
;; ;;
  (let [id (infer '[:lam x [:var x]])
        c1 '[:lam x [:lam y [:app [:var x] [:var y]]]]]
    (t/is (= '[:tarrow [:tvar [:unbound a]] [:tvar [:unbound a]]] id))

    (t/is (= '[:tarrow [:tvar [:link [:tarrow [:tvar [:unbound b]] [:tvar [:unbound c]]]]]
               [:tarrow [:tvar [:unbound b]] [:tvar [:unbound c]]]]
             (infer c1))))

;; let
;;  TArrow (TVar {contents = Unbound "b"}, TVar {contents = Unbound "b"})
;;  =
;;  reset_gensym ();
;;  typeof [] (Let ("y",Lam ("z",Var"z"), Var"y"));;
  (t/is (= '[:tarrow [:tvar [:unbound b]] [:tvar [:unbound b]]]
           (infer '[:let y [:lam z [:var z]] [:var y]])))

;; let
;;  TArrow (TVar {contents = Unbound "a"},
;;   TArrow (TVar {contents = Unbound "c"}, TVar {contents = Unbound "c"}))
;;  =
;;  reset_gensym ();
;;  typeof [] (Lam ("x", Let ("y",Lam ("z",Var"z"), Var"y")));;
  (t/is (= '[:tarrow [:tvar [:unbound a]] [:tarrow [:tvar [:unbound c]] [:tvar [:unbound c]]]]
           (infer '[:lam x [:let y [:lam z [:var z]] [:var y]]])))

;; let
;;  TArrow (TVar {contents = Unbound "a"},
;;    TVar
;;     {contents = Link (TVar {contents = Link (TVar {contents = Unbound "a"})})})
;;  =
;;  reset_gensym ();
;;  typeof [] (Lam ("x", Let ("y",Lam ("z",Var"z"),
;;                                     App (Var"y",Var"x"))));;
  (t/is (= '[:tarrow [:tvar [:unbound a]] [:tvar [:link [:tvar [:link [:tvar [:unbound a]]]]]]]
           (infer '[:lam x [:let y [:lam z [:var z]] [:app [:var y] [:var x]]]])))

;; try
;;  reset_gensym ();
;;  typeof [] (Lam ("x",App (Var"x",Var"x")));
;;  assert false;
;;  with Failure e -> print_endline e
;; ;;
  (t/is (thrown? AssertionError (infer '[:lam x [:app [:var x] [:var x]]])))

;; try
;;  reset_gensym ();
;;  typeof [] (Let ("x",Var"x",Var"x"));
;;  assert false;
;;  with Not_found -> print_endline "unbound var"
;; ;;
  (t/is (thrown? AssertionError (infer '[:let x [:app [:var x] [:var x]]])))

;; (* id can be `self-applied', on the surface of it *)
;; let
;;  TVar
;;  {contents =
;;    Link
;;     (TVar
;;       {contents =
;;         Link
;;          (TArrow (TVar {contents = Unbound "c"},
;;            TVar {contents = Unbound "c"}))})}
;;  =
;;  reset_gensym ();
;;  typeof [] (Let ("id",id, App (Var"id",Var"id")));;
  (t/is (= '[:tvar [:link [:tvar [:link [:tarrow [:tvar [:unbound c]] [:tvar [:unbound c]]]]]]]
           (infer '[:let id [:lam x [:var x]] [:app [:var id] [:var id]]])))

;; let
;;  TArrow (TVar {contents = Unbound "i"}, TVar {contents = Unbound "i"})
;;  =
;;  reset_gensym ();
;;  typeof [] (Let ("x",c1,
;;                     Let ("y",
;;                           Let ("z",App(Var"x",id), Var "z"),
;;                          Var"y")));;
  (t/is (= '[:tarrow [:tvar [:unbound i]] [:tvar [:unbound i]]]
           (infer '[:let x [:lam x [:lam y [:app [:var x] [:var y]]]]
                    [:let y [:let z [:app [:var x] [:lam x [:var x]]]
                             [:var z]]
                     [:var y]]])))

;; (*
;; fun x -> fun y -> let x = x y in fun x -> y x;;
;; - : (('a -> 'b) -> 'c) -> ('a -> 'b) -> 'a -> 'b = <fun>
;; *)
;; let
;;  TArrow
;;  (TVar
;;    {contents =
;;      Link
;;       (TArrow
;;         (TVar
;;           {contents =
;;             Link
;;              (TArrow (TVar {contents = Unbound "d"},
;;                TVar {contents = Unbound "e"}))},
;;         TVar {contents = Unbound "c"}))},
;;  TArrow
;;   (TVar
;;     {contents =
;;       Link
;;        (TArrow (TVar {contents = Unbound "d"}, TVar {contents = Unbound "e"}))},
;;   TArrow (TVar {contents = Unbound "d"}, TVar {contents = Unbound "e"})))
;;  =
;;  reset_gensym ();
;;  typeof [] (Lam ("x", Lam("y",Let ("x",App (Var"x",Var"y"),
;;                                   Lam ("x",App (Var"y",Var"x"))))));;
  (t/is (= '[:tarrow [:tvar [:link [:tarrow [:tvar [:link [:tarrow [:tvar [:unbound d]]
                                                           [:tvar [:unbound e]]]]]
                                    [:tvar [:unbound c]]]]]
             [:tarrow [:tvar [:link [:tarrow [:tvar [:unbound d]] [:tvar [:unbound e]]]]]
             [:tarrow [:tvar [:unbound d]] [:tvar [:unbound e]]]]]
           (infer '[:lam x [:lam y [:let x [:app [:var x] [:var y]]
                                    [:lam x [:app [:var y] [:var x]]]]]])))

;; (* unsound generalization ! *)
;; let
;;  TArrow (TVar {contents = Unbound "a"}, TVar {contents = Unbound "b"})
;;  =
;;  reset_gensym ();
;;  typeof [] (Lam ("x", Let ("y",Var"x", Var"y")));;
  (t/is (= '[:tarrow [:tvar [:unbound a]] [:tvar [:unbound b]]]
           (infer '[:lam x [:let y [:var x] [:var y]]])))

;; (* unsound generalization ! *)
;; let
;;  TArrow (TVar {contents = Unbound "a"},
;;   TArrow (TVar {contents = Unbound "c"}, TVar {contents = Unbound "d"}))
;;  =
;;  reset_gensym ();
;;  typeof [] (Lam ("x", Let ("y",Lam ("z",Var"x"), Var"y")));;
  (t/is (= '[:tarrow [:tvar [:unbound a]]
             [:tarrow [:tvar [:unbound c]] [:tvar [:unbound d]]]]
           (infer '[:lam x [:let y [:lam z [:var x]] [:var y]]])))

;; (* unsound generalization ! *)
;; let
;;  TArrow
;;  (TVar
;;    {contents =
;;      Link
;;       (TArrow (TVar {contents = Unbound "b"}, TVar {contents = Unbound "c"}))},
;;   TArrow (TVar {contents = Unbound "d"}, TVar {contents = Unbound "e"}))
;;  =
;;  reset_gensym ();
;;  typeof [] (Lam ("x", Let ("y",Lam ("z",App (Var"x",Var"z")), Var"y")));;
  (t/is (= '[:tarrow [:tvar [:link [:tarrow [:tvar [:unbound b]] [:tvar [:unbound c]]]]]
             [:tarrow [:tvar [:unbound d]] [:tvar [:unbound e]]]]
           (infer '[:lam x [:let y [:lam z [:app [:var x] [:var z]]] [:var y]]])))

;; (* unsound generalization ! *)
;; let
;;  TArrow
;;  (TVar
;;    {contents =
;;      Link
;;       (TArrow (TVar {contents = Unbound "b"}, TVar {contents = Unbound "c"}))},
;;  TArrow (TVar {contents = Unbound "b"}, TVar {contents = Unbound "e"}))
;;  =
;;  reset_gensym ();
;;  typeof [] (Lam ("x", Lam("y",Let ("x",App (Var"x",Var"y"),
;;                                     App (Var"x",Var"y")))));;
  (t/is (= '[:tarrow [:tvar [:link [:tarrow [:tvar [:unbound b]] [:tvar [:unbound c]]]]]
             [:tarrow [:tvar [:unbound b]] [:tvar [:unbound e]]]]
           (infer '[:lam x [:lam y [:let x [:app [:var x] [:var y]]
                                    [:app [:var x] [:var y]]]]])))

;; (* unsound generalization ! *)
;; let
;;  TArrow (TVar {contents = Unbound "a"}, TVar {contents = Unbound "d"})
;;  =
;;  reset_gensym ();
;;  typeof [] (Lam ("x",Let("y",Var"x", App (Var"y",Var"y"))));;
  (t/is (= '[:tarrow [:tvar [:unbound a]] [:tvar [:unbound d]]]
           (infer '[:lam x [:let y [:var x] [:app [:var y] [:var y]]]])))

;; (* unsound generalization ! *)
;; let
;;  TArrow
;;  (TVar
;;    {contents =
;;      Link
;;       (TArrow
;;         (TArrow (TVar {contents = Unbound "b"},
;;           TVar {contents = Unbound "b"}),
;;         TVar {contents = Unbound "c"}))},
;;   TVar {contents = Unbound "e"})
;;  =
;;  reset_gensym ();
;;  typeof [] (Lam ("x",
;;                     Let ("y",
;;                           Let ("z",App(Var"x",id), Var "z"),
;;                          Var"y")));;
  (t/is (= '[:tarrow [:tvar [:link [:tarrow [:tarrow [:tvar [:unbound b]] [:tvar [:unbound b]] ]
                                    [:tvar [:unbound c]]]]]
             [:tvar [:unbound e]]]
           (infer '[:lam x
                    [:let y
                     [:let z [:app [:var x] [:lam x [:var x]]] [:var z]]
                     [:var y]]]))))

;; print_endline "\nAll Done\n";;
