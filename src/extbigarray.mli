open Bigarray

module Kind : sig
  val zero : ('o, 'r) kind -> 'o
  val one : ('o, 'r) kind -> 'o
  (** [zero] and [one] values for each {!kind} *)

  val to_add : ('o, 'r) kind -> ('o -> 'o -> 'o)
  val to_sub : ('o, 'r) kind -> ('o -> 'o -> 'o)
  val to_mul : ('o, 'r) kind -> ('o -> 'o -> 'o)
  val to_div : ('o, 'r) kind -> ('o -> 'o -> 'o)
  val to_neg : ('o, 'r) kind -> ('o -> 'o)
  (** [to_(op) kind] returns the given [(op)] matching [kind].

      @raise Invalid_argument when [kind] is [Char]. *)
end

module type S = sig
  type ('o, 'r, 'l) t
  module Op : sig
    val ( + ) : (('o, 'r, 'l) t as 'a) -> 'a -> 'a
    val ( - ) : (('o, 'r, 'l) t as 'a) -> 'a -> 'a
    val ( * ) : (('o, 'r, 'l) t as 'a) -> 'a -> 'a
    val ( / ) : (('o, 'r, 'l) t as 'a) -> 'a -> 'a
    (** Apply the given operation to corresponding elements of two bigarrays *)

    val ( +< ) : (('o, 'r, 'l) t as 'a) -> 'a -> unit
    val ( -< ) : (('o, 'r, 'l) t as 'a) -> 'a -> unit
    val ( *< ) : (('o, 'r, 'l) t as 'a) -> 'a -> unit
    val ( /< ) : (('o, 'r, 'l) t as 'a) -> 'a -> unit
    (** Apply the given operation to corresponding elements of two bigarrays,
        modifying the first bigarray in place with the result. *)

    val ( ~- ) : (('o, 'r, 'l) t as 'a) -> 'a
    (** Negation *)

    val ( +: ) : (('o, 'r, 'l) t as 'a) -> 'o -> 'a
    val ( -: ) : (('o, 'r, 'l) t as 'a) -> 'o -> 'a
    val ( *: ) : (('o, 'r, 'l) t as 'a) -> 'o -> 'a
    val ( /: ) : (('o, 'r, 'l) t as 'a) -> 'o -> 'a
    (** Apply the given operation to each element of a bigarray *)

    val ( ~-< ) : ('o, 'r, 'l) t -> unit
    (** Negation in place *)

    val ( +:< ) : ('o, 'r, 'l) t -> 'o -> unit
    val ( -:< ) : ('o, 'r, 'l) t -> 'o -> unit
    val ( *:< ) : ('o, 'r, 'l) t -> 'o -> unit
    val ( /:< ) : ('o, 'r, 'l) t -> 'o -> unit
    (** Apply the given operation to each element of a bigarray in place *)
  end
end

module Array1 : sig
  include module type of Bigarray.Array1
  with type ('o, 'r, 'l) t = ('o, 'r, 'l) Bigarray.Array1.t

  include S with type ('o, 'r, 'l) t := ('o, 'r, 'l) Bigarray.Array1.t

  val dims : _ t -> int

  val make : ('o, 'r) kind -> 'l layout -> int -> 'o -> ('o, 'r, 'l) t
  (* Equivalent to [create] then [fill]. *)

  val init : ('o, 'r) kind -> 'l layout -> int -> (int -> 'o) -> ('o, 'r, 'l) t

  val to_array : ('o, 'r, 'l) t -> 'o array
  val of_array : ('o, 'r) kind -> 'l layout -> 'o array -> ('o, 'r, 'l) t

  val to_string : (char, 'r, 'l) t -> string
  val to_bytes : (char, 'r, 'l) t -> bytes

  val of_string : ('o, 'r) kind -> 'l layout -> string -> ('o, 'r, 'l) t
  val of_bytes : ('o, 'r) kind -> 'l layout -> bytes -> ('o, 'r, 'l) t

  val reduce : ('o -> 'o -> 'o) -> ('o, 'r, 'l) t -> 'o
  val fold : ('o -> 'a -> 'a) -> ('o, 'r, 'l) t -> 'a -> 'a
  val map : ('o -> 'o2) -> ('o2, 'r2) kind -> ('o, 'r, 'l) t -> ('o2, 'r2, 'l) t
  val iter : ('o -> unit) -> ('o, 'r, 'l) t -> unit

  val reducei : (int -> 'o -> 'o -> 'o) -> ('o, 'r, 'l) t -> 'o
  val foldi : (int -> 'o -> 'a -> 'a) -> ('o, 'r, 'l) t -> 'a -> 'a
  val mapi : (int -> 'o -> 'o2) -> ('o2, 'r2) kind -> ('o, 'r, 'l) t -> ('o2, 'r2, 'l) t
  val iteri : (int -> 'o -> unit) -> ('o, 'r, 'l) t -> unit
end

module Array2 : sig
  include module type of Bigarray.Array2
  with type ('o, 'r, 'l) t = ('o, 'r, 'l) Bigarray.Array2.t

  include S with type ('o, 'r, 'l) t := ('o, 'r, 'l) Bigarray.Array2.t

  val dims : _ t -> int * int

  val make : ('o, 'r) kind -> 'l layout -> int -> int -> 'o -> ('o, 'r, 'l) t

  val init : ('o, 'r) kind -> 'l layout -> int -> int -> (int -> int -> 'o) -> ('o, 'r, 'l) t

  val to_array : ('o, 'r, 'l) t -> 'o array array

  val of_array1 : ('o, 'r, 'l) Array1.t -> int -> int -> ('o, 'r, 'l) t
  val to_array1 : ('o, 'r, 'l) t -> ('o, 'r, 'l) Array1.t

  val sub : ('o, 'r, 'l) t -> int -> int -> ('o, 'r, 'l) t
  val slice : ('o, 'r, 'l) t -> int -> ('o, 'r, 'l) Array1.t

  val reduce : ('o -> 'o -> 'o) -> ('o, 'r, 'l) t -> 'o
  val fold : ('o -> 'a -> 'a) -> ('o, 'r, 'l) t -> 'a -> 'a
  val map : ('o -> 'o2) -> ('o2, 'r2) kind -> ('o, 'r, 'l) t -> ('o2, 'r2, 'l) t
  val iter : ('o -> unit) -> ('o, 'r, 'l) t -> unit

  val reduce_array1 : ((('o, 'r, 'l) Array1.t as 'a) -> 'a -> 'a) -> ('o, 'r, 'l) t -> 'a
  val foldi_array1 : (('o, 'r, 'l) Array1.t -> 'a -> 'a) -> ('o, 'r, 'l) t -> 'a -> 'a
  val map_array1 : (('o, 'r, 'l) Array1.t -> ('o2, 'r2, 'l) Array1.t) -> ('o2, 'r2) kind -> ('o, 'r, 'l) t -> ('o2, 'r2, 'l) t
  val iter_array1 : (('o, 'r, 'l) Array1.t -> unit) -> ('o, 'r, 'l) t -> unit

  val reducei : (int -> int -> 'o -> 'o -> 'o) -> ('o, 'r, 'l) t -> 'o
  val foldi : (int -> int -> 'o -> 'a -> 'a) -> ('o, 'r, 'l) t -> 'a -> 'a
  val mapi : (int -> int -> 'o -> 'o2) -> ('o2, 'r2) kind -> ('o, 'r, 'l) t -> ('o2, 'r2, 'l) t
  val iteri : (int -> int -> 'o -> unit) -> ('o, 'r, 'l) t -> unit

  val reducei_array1 : (int -> (('o, 'r, 'l) Array1.t as 'a) -> 'a -> 'a) -> ('o, 'r, 'l) t -> 'a
  val foldi_array1 : (int -> ('o, 'r, 'l) Array1.t -> 'a -> 'a) -> ('o, 'r, 'l) t -> 'a -> 'a
  val mapi_array1 : (int -> ('o, 'r, 'l) Array1.t -> ('o2, 'r2, 'l) Array1.t) -> ('o2, 'r2) kind -> ('o, 'r, 'l) t -> ('o2, 'r2, 'l) t
  val iteri_array1 : (int -> ('o, 'r, 'l) Array1.t -> unit) -> ('o, 'r, 'l) t -> unit
end

module Array3 : sig
  include module type of Bigarray.Array3
  with type ('o, 'r, 'l) t = ('o, 'r, 'l) Bigarray.Array3.t

  include S with type ('o, 'r, 'l) t := ('o, 'r, 'l) Bigarray.Array3.t

  val dims : _ t -> int * int * int

  val make : ('o, 'r) kind -> 'l layout -> int -> int -> int -> 'o -> ('o, 'r, 'l) t

  val init : ('o, 'r) kind -> 'l layout -> int -> int -> int -> (int -> int -> int -> 'o) -> ('o, 'r, 'l) t

  val to_array : ('o, 'r, 'l) t -> 'o array array array

  val of_array1 : ('o, 'r, 'l) Array1.t -> int -> int -> int -> ('o, 'r, 'l) t
  val to_array1 : ('o, 'r, 'l) t -> ('o, 'r, 'l) Array1.t

  val sub : ('o, 'r, 'l) t -> int -> int -> ('o, 'r, 'l) t
  val slice1 : ('o, 'r, 'l) t -> int -> int -> ('o, 'r, 'l) Array1.t
  val slice2 : ('o, 'r, 'l) t -> int -> ('o, 'r, 'l) Array2.t

  val reduce : ('o -> 'o -> 'o) -> ('o, 'r, 'l) t -> 'o
  val fold : ('o -> 'a -> 'a) -> ('o, 'r, 'l) t -> 'a -> 'a
  val map : ('o -> 'o2) -> ('o2, 'r2) kind -> ('o, 'r, 'l) t -> ('o2, 'r2, 'l) t
  val iter : ('o -> unit) -> ('o, 'r, 'l) t -> unit

  val reduce_array1 : ((('o, 'r, 'l) Array1.t as 'a) -> 'a -> 'a) -> ('o, 'r, 'l) t -> 'a
  val fold_array1 : (('o, 'r, 'l) Array1.t -> 'a -> 'a) -> ('o, 'r, 'l) t -> 'a -> 'a
  val map_array1 : (('o, 'r, 'l) Array1.t -> ('o2, 'r2, 'l) Array1.t) -> ('o2, 'r2) kind -> ('o, 'r, 'l) t -> ('o2, 'r2, 'l) t
  val iter_array1 : (('o, 'r, 'l) Array1.t -> unit) -> ('o, 'r, 'l) t -> unit

  val reduce_array2 : ((('o, 'r, 'l) Array2.t as 'a) -> 'a -> 'a) -> ('o, 'r, 'l) t -> 'a
  val fold_array2 : (('o, 'r, 'l) Array2.t -> 'a -> 'a) -> ('o, 'r, 'l) t -> 'a -> 'a
  val map_array2 : (('o, 'r, 'l) Array2.t -> ('o2, 'r2, 'l) Array2.t) -> ('o2, 'r2) kind -> ('o, 'r, 'l) t -> ('o2, 'r2, 'l) t
  val iter_array2 : (('o, 'r, 'l) Array2.t -> unit) -> ('o, 'r, 'l) t -> unit

  val reducei : (int -> int -> int -> 'o -> 'o -> 'o) -> ('o, 'r, 'l) t -> 'o
  val foldi : (int -> int -> int -> 'o -> 'a -> 'a) -> ('o, 'r, 'l) t -> 'a -> 'a
  val mapi : (int -> int -> int -> 'o -> 'o2) -> ('o2, 'r2) kind -> ('o, 'r, 'l) t -> ('o2, 'r2, 'l) t
  val iteri : (int -> int -> int -> 'o -> unit) -> ('o, 'r, 'l) t -> unit

  val reducei_array1 : (int -> int -> (('o, 'r, 'l) Array1.t as 'a) -> 'a -> 'a) -> ('o, 'r, 'l) t -> 'a
  val foldi_array1 : (int -> int -> ('o, 'r, 'l) Array1.t -> 'a -> 'a) -> ('o, 'r, 'l) t -> 'a -> 'a
  val mapi_array1 : (int -> int -> ('o, 'r, 'l) Array1.t -> ('o2, 'r2, 'l) Array1.t) -> ('o2, 'r2) kind -> ('o, 'r, 'l) t -> ('o2, 'r2, 'l) t
  val iteri_array1 : (int -> int -> ('o, 'r, 'l) Array1.t -> unit) -> ('o, 'r, 'l) t -> unit

  val reducei_array2 : (int -> (('o, 'r, 'l) Array2.t as 'a) -> 'a -> 'a) -> ('o, 'r, 'l) t -> 'a
  val foldi_array2 : (int -> ('o, 'r, 'l) Array2.t -> 'a -> 'a) -> ('o, 'r, 'l) t -> 'a -> 'a
  val mapi_array2 : (int -> ('o, 'r, 'l) Array2.t -> ('o2, 'r2, 'l) Array2.t) -> ('o2, 'r2) kind -> ('o, 'r, 'l) t -> ('o2, 'r2, 'l) t
  val iteri_array2 : (int -> ('o, 'r, 'l) Array2.t -> unit) -> ('o, 'r, 'l) t -> unit
end

module Genarray : sig
  include module type of Bigarray.Genarray
  with type ('o, 'r, 'l) t = ('o, 'r, 'l) Bigarray.Genarray.t

  val make : ('o, 'r) kind -> 'l layout -> int array -> 'o -> ('o, 'r, 'l) t

(*
  val init : ('o, 'r) kind -> 'l layout -> int array -> (int array -> 'o) -> ('o, 'r, 'l) t
*)

  val reshape : ('o, 'r, 'l) t -> int array -> ('o, 'r, 'l) t

  val flatten : ('o, 'r, 'l) t -> ('o, 'r, 'l) t
  val flatten_to_array1 : ('o, 'r, 'l) t -> ('o, 'r, 'l) Array1.t

  val of_array1 : ('o, 'r, 'l) Array1.t -> ('o, 'r, 'l) t
  val of_array2 : ('o, 'r, 'l) Array2.t -> ('o, 'r, 'l) t
  val of_array3 : ('o, 'r, 'l) Array3.t -> ('o, 'r, 'l) t
  val to_array1 : ('o, 'r, 'l) t -> ('o, 'r, 'l) Array1.t
  val to_array2 : ('o, 'r, 'l) t -> ('o, 'r, 'l) Array2.t
  val to_array3 : ('o, 'r, 'l) t -> ('o, 'r, 'l) Array3.t

  val sub : ('o, 'r, 'l) t -> int -> int -> ('o, 'r, 'l) t
  val slice : ('o, 'r, 'l) t -> int array -> ('o, 'r, 'l) t

  val reduce : ('o -> 'o -> 'o) -> ('o, 'r, 'l) t -> 'o
  val fold : ('o -> 'a -> 'a) -> ('o, 'r, 'l) t -> 'a -> 'a
  val map : ('o -> 'o2) -> ('o2, 'r2) kind -> ('o, 'r, 'l) t -> ('o2, 'r2, 'l) t
  val iter : ('o -> unit) -> ('o, 'r, 'l) t -> unit

(*
  val reduce_array1 : ((('o, 'r, 'l) Array1.t as 'a) -> 'a -> 'a) -> ('o, 'r, 'l) t -> 'a
  val fold_array1 : (('o, 'r, 'l) Array1.t -> 'a -> 'a) -> ('o, 'r, 'l) t -> 'a -> 'a
  val map_array1 : (('o, 'r, 'l) Array1.t -> ('o2, 'r2, 'l) Array1.t) -> ('o2, 'r2) kind -> ('o, 'r, 'l) t -> ('o2, 'r2, 'l) t
  val iter_array1 : (('o, 'r, 'l) Array1.t -> unit) -> ('o, 'r, 'l) t -> unit

  val reduce_array2 : ((('o, 'r, 'l) Array2.t as 'a) -> 'a -> 'a) -> ('o, 'r, 'l) t -> 'a
  val fold_array2 : (('o, 'r, 'l) Array2.t -> 'a -> 'a) -> ('o, 'r, 'l) t -> 'a -> 'a
  val map_array2 : (('o, 'r, 'l) Array2.t -> ('o2, 'r2, 'l) Array2.t) -> ('o2, 'r2) kind -> ('o, 'r, 'l) t -> ('o2, 'r2, 'l) t
  val iter_array2 : (('o, 'r, 'l) Array2.t -> unit) -> ('o, 'r, 'l) t -> unit

  val reduce_array2 : ((('o, 'r, 'l) Array3.t as 'a) -> 'a -> 'a) -> ('o, 'r, 'l) t -> 'a
  val fold_array3 : (('o, 'r, 'l) Array3.t -> 'a -> 'a) -> ('o, 'r, 'l) t -> 'a -> 'a
  val map_array3 : (('o, 'r, 'l) Array3.t -> ('o2, 'r2, 'l) Array3.t) -> ('o2, 'r2) kind -> ('o, 'r, 'l) t -> ('o2, 'r2, 'l) t
  val iter_array3 : (('o, 'r, 'l) Array3.t -> unit) -> ('o, 'r, 'l) t -> unit

  val reducei : (int array -> 'o -> 'o -> 'o) -> ('o, 'r, 'l) t -> 'o
  val foldi : (int array -> 'o -> 'a -> 'a) -> ('o, 'r, 'l) t -> 'a -> 'a
  val mapi : (int array -> 'o -> 'o2) -> ('o2, 'r2) kind -> ('o, 'r, 'l) t -> ('o2, 'r2, 'l) t
  val iteri : (int array -> 'o -> unit) -> ('o, 'r, 'l) t -> unit

  val reducei_array1 : (int array -> (('o, 'r, 'l) Array1.t as 'a) -> 'a -> 'a) -> ('o, 'r, 'l) t -> 'a
  val foldi_array1 : (int array -> ('o, 'r, 'l) Array1.t -> 'a -> 'a) -> ('o, 'r, 'l) t -> 'a -> 'a
  val mapi_array1 : (int array -> ('o, 'r, 'l) Array1.t -> ('o2, 'r2, 'l) Array1.t) -> ('o2, 'r2) kind -> ('o, 'r, 'l) t -> ('o2, 'r2, 'l) t
  val iteri_array1 : (int array -> ('o, 'r, 'l) Array1.t -> unit) -> ('o, 'r, 'l) t -> unit

  val reducei_array2 : (int array -> (('o, 'r, 'l) Array2.t as 'a) -> 'a -> 'a) -> ('o, 'r, 'l) t -> 'a
  val foldi_array2 : (int array -> ('o, 'r, 'l) Array2.t -> 'a -> 'a) -> ('o, 'r, 'l) t -> 'a -> 'a
  val mapi_array2 : (int array -> ('o, 'r, 'l) Array2.t -> ('o2, 'r2, 'l) Array2.t) -> ('o2, 'r2) kind -> ('o, 'r, 'l) t -> ('o2, 'r2, 'l) t
  val iteri_array2 : (int array -> ('o, 'r, 'l) Array2.t -> unit) -> ('o, 'r, 'l) t -> unit

  val reducei_array3 : (int array -> (('o, 'r, 'l) Array3.t as 'a) -> 'a -> 'a) -> ('o, 'r, 'l) t -> 'a
  val foldi_array3 : (int array -> ('o, 'r, 'l) Array3.t -> 'a -> 'a) -> ('o, 'r, 'l) t -> 'a -> 'a
  val mapi_array3 : (int array -> ('o, 'r, 'l) Array3.t -> ('o2, 'r2, 'l) Array3.t) -> ('o2, 'r2) kind -> ('o, 'r, 'l) t -> ('o2, 'r2, 'l) t
  val iteri_array3 : (int array -> ('o, 'r, 'l) Array3.t -> unit) -> ('o, 'r, 'l) t -> unit
*)
end
