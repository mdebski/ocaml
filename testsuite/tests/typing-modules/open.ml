module type AB = sig
  val a : int
  val b : int
end

module type A = sig
  val a : int
end

module type MAB = sig
  module N : AB
end

module type MA = sig
  module N : A
end

module M = struct
  let a = 1
  let b = 2
end

open (M <: A)
let _ = a
let _ = b

[%%expect {|
module type AB = sig val a : int val b : int end
module type A = sig val a : int end
module type MAB = sig module N : AB end
module type MA = sig module N : A end
module M : sig val a : int val b : int end
- : int = 1
Line _, characters 8-9:
Error: Unbound value b
|}]

module M = struct
  module N = struct
    let a = 1
    let b = 1
  end
end

open (M <: MA)

let _ = N.a
let _ = N.b

[%%expect {|
module M : sig module N : sig val a : int val b : int end end
- : int = 1
Line _, characters 8-11:
Error: Unbound value N.b
|}]

let a, b = (), ()

let _ = (M <: MA).(N.a + (N <: A).(a))
let _ = (M <: MA).(N.a + N.b)

let () = a

[%%expect {|
val a : unit = ()
val b : unit = ()
- : int = 2
Line _, characters 25-28:
Error: Unbound value N.b
|}]

module M = struct
  let a = 1
  let b = 2
  let c = 3
end

let _ = let open (M <: A) in a
let _ = let open M in a
let _ = let open M in b
let _ = let open (M <: AB) in a + b
let _ = M.c
let () = a

[%%expect {|
module M : sig val a : int val b : int val c : int end
- : int = 1
- : int = 1
- : int = 2
- : int = 3
- : int = 3
|}]

module type JustT = sig
  type t = A | B
end

module M = struct
  type t = A | B
  let v = A
  let v2 = [A]
end

let _ = match M.v with
  | (M <: JustT).(A) -> 1
  | (M <: JustT).(B) -> 2

let _ = match M.v2 with
  | (M <: JustT).[A] -> 1
  | _ -> 2
[%%expect {|
module type JustT = sig type t = A | B end
module M : sig type t = A | B val v : t val v2 : t list end
- : int = 1
- : int = 1
|}]

module O = struct
  module M = struct
    module N = struct
      let a = 1
      let b = 2
      let c = 3
    end
  end
end

module M = struct end
module N = struct end

let a = ()
let b = ()
let c = ()

module F (X: A) = struct type t end

let _ = let open O in
  let open (M <: MAB) in
  let module P = N in
  let _f (x : F(P).t) : F(M.N).t = x in
  let _f (x : F(P).t) : F(N).t = x in
  let open (N <: A) in (a, N.a, N.b, M.N.b, M.N.c)

let _ = O.((M <: MAB).((N <: A).(a, N.a, N.b, M.N.b, M.N.c)))

[%%expect {|
module O :
  sig
    module M : sig module N : sig val a : int val b : int val c : int end end
  end
module M : sig  end
module N : sig  end
val a : unit = ()
val b : unit = ()
val c : unit = ()
module F : functor (X : A) -> sig type t end
- : int * int * int * int * int = (1, 1, 2, 2, 3)
- : int * int * int * int * int = (1, 1, 2, 2, 3)
|}]

let _ = let open O in
let open (M <: MAB) in
let open (N <: A) in b

[%%expect {|
- : unit = ()
|}]

let _ = let open O in
let open (M <: MAB) in
let open (N <: A) in N.c

[%%expect {|
Line _, characters 21-24:
Error: Unbound value N.c
|}]

let _ = let open O in
let open (M <: MAB) in
let module P = N in
P.c
[%%expect {|
Line _, characters 0-3:
Error: Unbound value P.c
|}]

module XYZ = struct let f x = x end
module rec R :
sig
  open O
  module XYZ : sig val f : 'a -> 'a end
end = struct
  module XYZ = XYZ
end;;
let _ = R.XYZ.f "abc"

[%%expect{|
module XYZ : sig val f : 'a -> 'a end
module rec R : sig module XYZ : sig val f : 'a -> 'a end end
- : string = "abc"
|}]
