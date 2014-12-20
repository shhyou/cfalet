signature ENV_STR =
sig
  type 'a t
  exception NotFound of string

  val empty : 'a t
  val extend : string * 'a -> 'a t -> 'a t
  val lookup : string -> 'a t -> 'a
  val member : string -> 'a t -> bool

  val collate : ('a * 'a -> order) -> 'a t * 'a t -> order
  val fromList : (string * 'a) list -> 'a t
  val toList : 'a t -> (string * 'a) list
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

  fun collate cmp =
    let
      fun listComp ([], []) = EQUAL
        | listComp ([], _::_) = LESS
        | listComp (_::_, []) = GREATER
        | listComp ((k1,v1)::xs1, (k2,v2)::xs2) =
            case (String.compare (k1,k2), cmp (v1,v2)) of
                (EQUAL, EQUAL) => listComp (xs1, xs2)
              | (EQUAL, unequ) => unequ
              | (unequ, _)     => unequ
    in
      listComp
    end

  val fromList = fn xs => xs
  val toList = fn xs => xs

  fun toString show xs =
      "[" ^ String.concatWith ", "
              (List.map (fn (x, v) => "(" ^ x ^ "," ^ show v ^ ")") xs)
    ^ "]"
end
