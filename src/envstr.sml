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
  structure StringMap = BinaryMapFn(struct
    type ord_key = string
    val compare = String.compare
  end)

  type 'a t = 'a StringMap.map

  exception NotFound of string

  val empty = StringMap.empty

  fun extend (key, value) = fn set => StringMap.insert (set, key, value)

  fun lookup key set = case StringMap.find (set, key) of
      NONE => raise (NotFound key)
    | SOME v => v

  fun member key set = StringMap.inDomain (set, key)

  val collate = StringMap.collate

  fun fromList xs = List.foldl (fn (pr, set) => extend pr set) empty xs

  val toList = StringMap.listItemsi

  fun toString show xs =
      "[" ^ String.concatWith ", "
              (List.map (fn (x, v) => "(" ^ x ^ "," ^ show v ^ ")") (toList xs))
    ^ "]"
end
