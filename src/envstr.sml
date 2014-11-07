signature ENV_STR =
sig
  type 'a t
  exception NotFound of string

  val empty : 'a t
  val extend : string * 'a -> 'a t -> 'a t
  val lookup : string -> 'a t -> 'a
  val member : string -> 'a t -> bool

  val fromList : (string * 'a) list -> 'a t
  val toString : ('a -> string) -> 'a t -> string
end

structure EnvStr : ENV_STR =
struct
  type 'a t = (string * 'a) list
  exception NotFound of string

  val empty = []

  fun extend pr = fn xs => pr :: xs

  fun lookup key [] = raise (NotFound key)
    | lookup key ((k, v)::xs) =
        if key = k
        then v
        else lookup key xs

  fun member key [] = false
    | member key ((k,v)::xs) = (key = k) orelse member key xs

  val fromList = fn xs => xs

  fun toString show xs =
      "[" ^ String.concatWith ", "
              (List.map (fn (x, v) => "(" ^ x ^ "," ^ show v ^ ")") xs)
    ^ "]"
end
