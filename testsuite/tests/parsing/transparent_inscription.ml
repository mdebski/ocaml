open (M <: S)

let _ = M.(a)
let _ = M.(+)
let _ = M.()
let _ = M.{ abc with def }
let _ = M.{< abc >}
let _ = M.{<>}
let _ = M.[ abc ]
let _ = M.[]
let _ = M.[| abc |]
let _ = M.[||]

let _ = (M <: S).(a)
let _ = (M <: S).()
let _ = (M <: S).{ abc with def }
let _ = (M <: S).{< abc >}
let _ = (M <: S).{<>}
let _ = (M <: S).[ abc ]
let _ = (M <: S).[]
let _ = (M <: S).[| abc |]
let _ = (M <: S).[||]

let _ = let open M in abc
let _ = let open (M <: S) in abc
