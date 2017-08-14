(* Example highlighting differences between module alias, constraint,
   and transparent constraint *)

module Maybe = struct
  type 'a t = Nothing | Just of 'a

  let return a = Just a

  let ( >>= ) aa f = match aa with
    | Nothing -> Nothing
    | Just a -> match (f a) with
      | Nothing -> Nothing
      | Just b -> Just b

  let some_other_function (x : 'a t) = x
end

module type Monad = sig
  type 'a t
  val return : 'a -> 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
end

[%%expect {|
module Maybe :
  sig
    type 'a t = Nothing | Just of 'a
    val return : 'a -> 'a t
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
    val some_other_function : 'a t -> 'a t
  end
module type Monad =
  sig
    type 'a t
    val return : 'a -> 'a t
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  end
|}]

(* Alias *)
module M1 = Maybe;;
let open M1 in
(* This works *)
let x = return 3 in
let f x = return (x + 1) in
let y = x >>= f in
(* but this works too - environment pollution :( *)
some_other_function y

[%%expect {|
module M1 = Maybe
- : int M1.t = M1.Just 4
|}]

(* Constraint *)
let someone_gave_us_maybe = Maybe.Nothing;;
module M2 = (Maybe : Monad);;
let open M2 in
let x = return 3 in
let f x = return (x + 1) in
let _y = x >>= f in ()
[%%expect {|
val someone_gave_us_maybe : 'a Maybe.t = Maybe.Nothing
module M2 : Monad
- : unit = ()
|}]
(* this won't work, nice *)
let _ = some_other_function _y
[%%expect {|
Line _, characters 8-27:
Error: Unbound value some_other_function
|}]
(* but this won't too: *)
let _ = let open M2 in
let f x = return (x+1) in
someone_gave_us_maybe >>= f
[%%expect {|
Line _, characters 0-21:
Error: This expression has type 'a Maybe.t
       but an expression was expected of type 'b M2.t
|}]

(* Transparent alias *)
module M3 = (Maybe <: Monad);;
let open M3 in
let x = return 3 in
let f x = return (x + 1) in
let _y = x >>= f in
(* this won't work *)
let _ = some_other_function _y in ()
[%%expect {|
module M3 <: Monad = Maybe
Line _, characters 8-27:
Error: Unbound value some_other_function
|}]
let _ = let open M3 in
let f x = return (x+1) in
someone_gave_us_maybe >>= f (* ok *)
[%%expect {|
- : int M3.t = Maybe.Nothing
|}]
;;

(* Transparent alias using open *)
let _ =
let open (Maybe <: Monad) in
let _x = return 3 in ()
[%%expect {|
- : unit = ()
|}]
