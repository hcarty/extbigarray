open Bigarray

module Layout = struct
  let min_index : type l. l layout -> int = function
    | C_layout -> 0
    | Fortran_layout -> 1
end

module Kind = struct
  let to_char : type o r. (o, r) kind -> o -> char = fun k ->
    match k with
    | Float32 -> fun x -> char_of_int (int_of_float x)
    | Float64 -> fun x -> char_of_int (int_of_float x)
    | Int8_signed -> char_of_int
    | Int8_unsigned -> char_of_int
    | Int16_signed -> char_of_int
    | Int16_unsigned -> char_of_int
    | Int32 -> fun x -> char_of_int (Int32.to_int x)
    | Int64 -> fun x -> char_of_int (Int64.to_int x)
    | Int -> char_of_int
    | Nativeint -> fun x -> char_of_int (Nativeint.to_int x)
    | Complex32 -> fun { Complex.re; _ } -> char_of_int (int_of_float re)
    | Complex64 -> fun { Complex.re; _ } -> char_of_int (int_of_float re)
    | Char -> fun x -> x

  let of_char : type o r. (o, r) kind -> char -> o = fun k ->
    match k with
    | Float32 -> fun x -> float_of_int (int_of_char x)
    | Float64 -> fun x -> float_of_int (int_of_char x)
    | Int8_signed -> int_of_char
    | Int8_unsigned -> int_of_char
    | Int16_signed -> int_of_char
    | Int16_unsigned -> int_of_char
    | Int32 -> fun x -> Int32.of_int (int_of_char x)
    | Int64 -> fun x -> Int64.of_int (int_of_char x)
    | Int -> int_of_char
    | Nativeint -> fun x -> Nativeint.of_int (int_of_char x)
    | Complex32 -> fun x ->
      { Complex.re = float_of_int (int_of_char x); im = 0.0 }
    | Complex64 -> fun x ->
      { Complex.re = float_of_int (int_of_char x); im = 0.0 }
    | Char -> fun x -> x

  let to_add : type o r. (o, r) kind -> (o -> o -> o) = function
    | Float32 -> ( +. )
    | Float64 -> ( +. )
    | Int8_signed -> ( + )
    | Int8_unsigned -> ( + )
    | Int16_signed -> ( + )
    | Int16_unsigned -> ( + )
    | Int -> ( + )
    | Int32 -> Int32.add
    | Int64 -> Int64.add
    | Nativeint -> Nativeint.add
    | Complex32 -> Complex.add
    | Complex64 -> Complex.add
    | Char -> invalid_arg "to_add"

  let to_sub : type o r. (o, r) kind -> (o -> o -> o) = function
    | Float32 -> ( -. )
    | Float64 -> ( -. )
    | Int8_signed -> ( - )
    | Int8_unsigned -> ( - )
    | Int16_signed -> ( - )
    | Int16_unsigned -> ( - )
    | Int -> ( - )
    | Int32 -> Int32.sub
    | Int64 -> Int64.sub
    | Nativeint -> Nativeint.sub
    | Complex32 -> Complex.sub
    | Complex64 -> Complex.sub
    | Char -> invalid_arg "to_sub"

  let to_mul : type o r. (o, r) kind -> (o -> o -> o) = function
    | Float32 -> ( *. )
    | Float64 -> ( *. )
    | Int8_signed -> ( * )
    | Int8_unsigned -> ( * )
    | Int16_signed -> ( * )
    | Int16_unsigned -> ( * )
    | Int -> ( * )
    | Int32 -> Int32.mul
    | Int64 -> Int64.mul
    | Nativeint -> Nativeint.mul
    | Complex32 -> Complex.mul
    | Complex64 -> Complex.mul
    | Char -> invalid_arg "to_mul"

  let to_div : type o r. (o, r) kind -> (o -> o -> o) = function
    | Float32 -> ( /. )
    | Float64 -> ( /. )
    | Int8_signed -> ( / )
    | Int8_unsigned -> ( / )
    | Int16_signed -> ( / )
    | Int16_unsigned -> ( / )
    | Int -> ( / )
    | Int32 -> Int32.div
    | Int64 -> Int64.div
    | Nativeint -> Nativeint.div
    | Complex32 -> Complex.div
    | Complex64 -> Complex.div
    | Char -> invalid_arg "to_div"

  let to_neg : type o r. (o, r) kind -> (o -> o) = function
    | Float32 -> ( ~-. )
    | Float64 -> ( ~-. )
    | Int8_signed -> ( ~- )
    | Int8_unsigned -> ( ~- )
    | Int16_signed -> ( ~- )
    | Int16_unsigned -> ( ~- )
    | Int -> ( ~- )
    | Int32 -> Int32.neg
    | Int64 -> Int64.neg
    | Nativeint -> Nativeint.neg
    | Complex32 -> Complex.neg
    | Complex64 -> Complex.neg
    | Char -> invalid_arg "to_neg"

  let zero : type o r. (o, r) kind -> o = function
    | Float32 -> 0.0
    | Float64 -> 0.0
    | Int8_signed -> 0
    | Int8_unsigned -> 0
    | Int16_signed -> 0
    | Int16_unsigned -> 0
    | Int32 -> 0l
    | Int64 -> 0L
    | Int -> 0
    | Nativeint -> 0n
    | Complex32 -> Complex.zero
    | Complex64 -> Complex.zero
    | Char -> '\000'

  let one : type o r. (o, r) kind -> o = function
    | Float32 -> 1.0
    | Float64 -> 1.0
    | Int8_signed -> 1
    | Int8_unsigned -> 1
    | Int16_signed -> 1
    | Int16_unsigned -> 1
    | Int32 -> 1l
    | Int64 -> 1L
    | Int -> 1
    | Nativeint -> 1n
    | Complex32 -> Complex.one
    | Complex64 -> Complex.one
    | Char -> '\001'

(*
let min_val : type o r. (o, r) kind -> o = function
  | Float32 -> neg_infinity
  | Float64 -> neg_infinity
  | Int8_signed -> ~-128
  | Int8_unsigned -> ~-256
  | Int16_signed -> ~-32768
  | Int16_unsigned -> ~-65536
  | Int32 -> Int32.min_int
  | Int64 -> Int64.min_int
  | Int -> min_int
  | Nativeint -> Nativeint.min_int
  | Complex32 -> invalid_arg "min_val"
  | Complex64 -> invalid_arg "min_val"
  | Char -> '\000'

let max_val : type o r. (o, r) kind -> o = function
  | Float32 -> infinity
  | Float64 -> infinity
  | Int8_signed -> 127
  | Int8_unsigned -> 255
  | Int16_signed -> 32767
  | Int16_unsigned -> 65535
  | Int32 -> Int32.max_int
  | Int64 -> Int64.max_int
  | Int -> max_int
  | Nativeint -> Nativeint.max_int
  | Complex32 -> invalid_arg "max_val"
  | Complex64 -> invalid_arg "max_val"
  | Char -> '\255'

let bytes_per_element : type o r. (o, r) kind -> int = function
  | Float32 -> 4
  | Float64 -> 8
  | Int8_signed -> 1
  | Int8_unsigned -> 1
  | Int16_signed -> 2
  | Int16_unsigned -> 2
  | Int32 -> 4
  | Int64 -> 8
  | Int -> Sys.word_size
  | Nativeint -> Sys.word_size
  | Complex32 -> 8
  | Complex64 -> 16
  | Char -> 1
*)

(*
  let to_pp : type o r. (o, r) kind -> (Format.formatter -> o -> unit) = function
    | Float32 -> Format.pp_print_float
    | Float64 -> Format.pp_print_float
    | Int8_signed -> Format.pp_print_int
    | Int8_unsigned -> Format.pp_print_int
    | Int16_signed -> Format.pp_print_int
    | Int16_unsigned -> Format.pp_print_int
    | Int32 -> fun fmt x -> Format.pp_print_string fmt (Int32.to_string x)
    | Int64 -> fun fmt x -> Format.pp_print_string fmt (Int64.to_string x)
    | Int -> Format.pp_print_int
    | Nativeint -> fun fmt x -> Format.pp_print_string fmt (Nativeint.to_string x)
    | Complex32 ->
      let pp_sep fmt () = Format.pp_print_char fmt ',' in
      fun fmt x ->
        Format.pp_print_list ~pp_sep Format.pp_print_float fmt
          Complex.[x.re; x.im]
    | Complex64 ->
      let pp_sep fmt () = Format.pp_print_char fmt ',' in
      fun fmt x ->
        Format.pp_print_list ~pp_sep Format.pp_print_float fmt
          Complex.[x.re; x.im]
    | Char -> Format.pp_print_char
*)
end

module type S = sig
  type ('o, 'r, 'l) t
  module Op : sig
    val ( + ) : (('o, 'r, 'l) t as 'a) -> 'a -> 'a
    val ( - ) : (('o, 'r, 'l) t as 'a) -> 'a -> 'a
    val ( * ) : (('o, 'r, 'l) t as 'a) -> 'a -> 'a
    val ( / ) : (('o, 'r, 'l) t as 'a) -> 'a -> 'a

    val ( +< ) : (('o, 'r, 'l) t as 'a) -> 'a -> unit
    val ( -< ) : (('o, 'r, 'l) t as 'a) -> 'a -> unit
    val ( *< ) : (('o, 'r, 'l) t as 'a) -> 'a -> unit
    val ( /< ) : (('o, 'r, 'l) t as 'a) -> 'a -> unit

    val ( ~- ) : (('o, 'r, 'l) t as 'a) -> 'a

    val ( +: ) : (('o, 'r, 'l) t as 'a) -> 'o -> 'a
    val ( -: ) : (('o, 'r, 'l) t as 'a) -> 'o -> 'a
    val ( *: ) : (('o, 'r, 'l) t as 'a) -> 'o -> 'a
    val ( /: ) : (('o, 'r, 'l) t as 'a) -> 'o -> 'a

    val ( ~-< ) : ('o, 'r, 'l) t -> unit

    val ( +:< ) : ('o, 'r, 'l) t -> 'o -> unit
    val ( -:< ) : ('o, 'r, 'l) t -> 'o -> unit
    val ( *:< ) : ('o, 'r, 'l) t -> 'o -> unit
    val ( /:< ) : ('o, 'r, 'l) t -> 'o -> unit
  end
end

module Array1 = struct
  include Array1

  let max_index a =
    let offset = Layout.min_index (layout a) in
    dim a + offset - 1

  let dims a = dim a

  let make kind layout n x =
    let a = create kind layout n in
    fill a x;
    a

  let copy a =
    let b = create (kind a) (layout a) (dim a) in
    blit a b;
    b

  let to_array a =
    let n = dim a in
    let offset = Layout.min_index (layout a) in
    Array.init n (fun i -> unsafe_get a (i + offset))

  let to_string_or_bytes init a =
    let offset = Layout.min_index (layout a) in
    let n = dim a in
    let convert x = Kind.to_char (kind a) x in
    init n (fun i -> unsafe_get a (i + offset) |> convert)

  let to_string a =
    to_string_or_bytes String.init a

  let to_bytes a =
    to_string_or_bytes Bytes.init a

  let of_string_or_bytes iteri length kind layout s =
    let a = create kind layout (length s) in
    let offset = Layout.min_index layout in
    let convert x = Kind.of_char kind x in
    iteri (fun i x -> unsafe_set a (i + offset) (convert x)) s;
    a

  let of_string kind layout s =
    of_string_or_bytes String.iteri String.length kind layout s

  let of_bytes kind layout s =
    of_string_or_bytes Bytes.iteri Bytes.length kind layout s

  let foldi f a accu_init =
    let offset = Layout.min_index (layout a) in
    (* Offset everything so we can treat Fortran and C arrays the same way *)
    let n = dim a + offset in
    let rec fold_inner i accu =
      if i < n then (
        fold_inner (succ i) (f i (unsafe_get a i) accu)
      )
      else (
        accu
      )
    in
    fold_inner offset accu_init

  let fold f a accu_init =
    foldi (fun _i x accu -> f x accu) a accu_init

  let modifyi f a =
    foldi (
      fun i x () ->
        unsafe_set a i (f i x)
    ) a ()

  let modify f a =
    modifyi (fun _i x -> f x) a

  let modify2 f a b =
    if dim a <> dim b then invalid_arg "Size mismatch";
    modifyi (fun i ax -> f ax (unsafe_get b i)) a

  let mapi f k a =
    let l = layout a in
    let n = dim a in
    let b = create k l n in
    let offset = Layout.min_index l in
    for i = offset to n - 1 + offset do
      unsafe_set b i (f i (unsafe_get a i))
    done;
    b

  let map f k a =
    mapi (fun _i x -> f x) k a

  let map2i f k a b =
    mapi (
      fun i ax ->
        f i ax (unsafe_get b i)
    ) k a

  let map2 f k a b =
    map2i (fun _i ax bx -> f ax bx) k a b

  let iteri f a =
    let l = layout a in
    let n = dim a in
    let offset = Layout.min_index l in
    for i = offset to n - 1 + offset do
      f i (unsafe_get a i)
    done

  let iter f a =
    iteri (fun _i x -> f x) a

  let reducei f a =
    let offset = Layout.min_index (layout a) in
    (* Offset everything so we can treat Fortran and C arrays the same way *)
    let n = dim a + offset in
    if n < 1 then invalid_arg "reduce";
    let rec fold_inner i accu =
      if i < n then (
        fold_inner (succ i) (f i (unsafe_get a i) accu)
      )
      else (
        accu
      )
    in
    fold_inner (offset + 1) (get a offset)

  let reduce f a =
    reducei (fun _i x accu -> f x accu) a

  let init k l n f =
    let a = create k l n in
    iteri (fun i _ -> unsafe_set a i (f i)) a;
    a

  module Op = struct
    let make_binop op a b =
      if dim a <> dim b then invalid_arg "Array1.Infix";
      let k = kind a in
      map2 (op k) k a b

    let ( + ) a b = make_binop Kind.to_add a b
    let ( - ) a b = make_binop Kind.to_sub a b
    let ( * ) a b = make_binop Kind.to_mul a b
    let ( / ) a b = make_binop Kind.to_div a b
    let ( ~- ) a = map (Kind.to_neg (kind a)) (kind a) a

    let make_binop op a v =
      let k = kind a in
      map (fun x -> op k x v) k a

    let ( +: ) a b = make_binop Kind.to_add a b
    let ( -: ) a b = make_binop Kind.to_sub a b
    let ( *: ) a b = make_binop Kind.to_mul a b
    let ( /: ) a b = make_binop Kind.to_div a b

    let make_binop op a b =
      if dim a <> dim b then invalid_arg "Array1.Infix";
      let op = op (kind a) in
      modifyi (fun i ax -> op ax (unsafe_get b i)) a

    let ( +< ) a b = make_binop Kind.to_add a b
    let ( -< ) a b = make_binop Kind.to_sub a b
    let ( *< ) a b = make_binop Kind.to_mul a b
    let ( /< ) a b = make_binop Kind.to_div a b
    let ( ~-< ) a = modify (Kind.to_neg (kind a)) a

    let make_binop op a v =
      let op = op (kind a) in
      modify (fun x -> op x v) a

    let ( +:< ) a b = make_binop Kind.to_add a b
    let ( -:< ) a b = make_binop Kind.to_sub a b
    let ( *:< ) a b = make_binop Kind.to_mul a b
    let ( /:< ) a b = make_binop Kind.to_div a b
  end
end

module Array2 = struct
  include Array2

  let dims a = dim1 a, dim2 a

  let max_index a =
    let offset = Layout.min_index (layout a) in
    dim1 a + offset - 1,
    dim2 a + offset - 1

  let max_major_index (type l) (a : (_, _, l) t) =
    let n1, n2 = max_index a in
    match layout a with
    | C_layout -> n1
    | Fortran_layout -> n2

  let make kind layout d1 d2 init =
    let a = create kind layout d1 d2 in
    fill a init;
    a

  let copy a =
    let b = create (kind a) (layout a) (dim1 a) (dim2 a) in
    blit a b;
    b

  let sub (type l) (a : (_, _, l) t) ofs len : (_, _, l) t =
    let layout = layout a in
    match layout with
    | C_layout -> sub_left a ofs len
    | Fortran_layout -> sub_right a ofs len

  let slice (type l) (a : (_, _, l) t) i : (_, _, l) Array1.t =
    let layout = layout a in
    match layout with
    | C_layout -> slice_left a i
    | Fortran_layout -> slice_right a i

  let to_array (type l) (a : (_, _, l) t) =
    let n1 = dim1 a in
    let n2 = dim2 a in
    let offset = Layout.min_index (layout a) in
    let n =
      match layout a with
      | C_layout -> n1
      | Fortran_layout -> n2
    in
    Array.init n (
      fun i ->
        let s = slice a (i + offset) in
        Array1.to_array s
    )

  let of_array1 s n1 n2 =
    let ga = genarray_of_array1 s in
    reshape_2 ga n1 n2

  let to_array1 a =
    let ga = genarray_of_array2 a in
    reshape_1 ga (dim1 a * dim2 a)

  let foldi f a accu_init =
    let offset = Layout.min_index (layout a) in
    let n1, n2 = max_index a in
    let result = ref accu_init in
    for i1 = offset to n1 do
      for i2 = offset to n2 do
        result := f i1 i2 (unsafe_get a i1 i2) !result
      done
    done;
    !result

  let fold f a accu_init =
    foldi (fun _i1 _i2 x accu -> f x accu) a accu_init

  let modifyi f a =
    foldi (
      fun i1 i2 x () ->
        unsafe_set a i1 i2 (f i1 i2 x)
    ) a ()

  let modify f a =
    modifyi (fun _i1 _i2 x -> f x) a

  let modify2 f a b =
    if dims a <> dims b then invalid_arg "Size mismatch";
    modifyi (fun i1 i2 ax -> f ax (unsafe_get b i1 i2)) a

  let mapi f k a =
    let l = layout a in
    let b =
      let d1, d2 = dims a in
      create k l d1 d2
    in
    let offset = Layout.min_index l in
    let n1, n2 = max_index a in
    for i1 = offset to n1 do
      for i2 = offset to n2 do
        unsafe_set b i1 i2 (f i1 i2 (unsafe_get a i1 i2))
      done
    done;
    b

  let map f k a =
    mapi (fun _i1 _i2 x -> f x) k a

  let map2i f k a b =
    mapi (
      fun i1 i2 ax ->
        f i1 i2 ax (unsafe_get b i1 i2)
    ) k a

  let map2 f k a b =
    map2i (fun _i1 _i2 ax bx -> f ax bx) k a b

  let iteri f a =
    let l = layout a in
    let offset = Layout.min_index l in
    let n1, n2 = max_index a in
    for i1 = offset to n1 do
      for i2 = offset to n2 do
        f i1 i2 (unsafe_get a i1 i2)
      done
    done

  let iter f a =
    iteri (fun _i1 _i2 x -> f x) a

  let reducei f a =
    let offset = Layout.min_index (layout a) in
    let n1, n2 = max_index a in
    if n1 < 1 || n2 < 1 then invalid_arg "reduce";
    let result = ref (unsafe_get a offset offset) in
    for i1 = offset to n1 do
      let offset = if i1 = offset then offset + 1 else offset in
      for i2 = offset to n2 do
        result := f i1 i2 (unsafe_get a i1 i2) !result
      done
    done;
    !result

  let reduce f a =
    reducei (fun _i1 _i2 x accu -> f x accu) a

  let mapi_to_array1 f k a =
    let n = max_major_index a in
    Array1.init k (layout a) n (
      fun i ->
        f i (slice a i)
    )

  let map_to_array1 f k a =
    mapi_to_array1 (fun _i x -> f x) k a

  let mapi_array1 f k a =
    let b = create k (layout a) (dim1 a) (dim2 a) in
    let offset = Layout.min_index (layout a) in
    let n = max_major_index a in
    for i = offset to n do
      let s = slice a i in
      let d = slice b i in
      Array1.blit (f i s) d
    done;
    b

  let map_array1 f k a =
    mapi_array1 (fun _i x -> f x) k a

  let iteri_array1 f a =
    let offset = Layout.min_index (layout a) in
    let n = max_major_index a in
    for i = offset to n do
      f i (slice a i)
    done

  let iter_array1 f a =
    iteri_array1 (fun _i x -> f x) a

  let reducei_array1 f a =
    let offset = Layout.min_index (layout a) in
    let n = max_major_index a in
    let result = ref (slice a offset) in
    for i = offset + 1 to n do
      result := f i (slice a i) !result
    done;
    !result

  let reduce_array1 f a =
    reducei_array1 (fun _i s accu -> f s accu) a

  let foldi_array1 f a accu_init =
    let offset = Layout.min_index (layout a) in
    let n = max_major_index a in
    let result = ref accu_init in
    for i = offset to n do
      result := f i (slice a i) !result
    done;
    !result

  let fold_array1 f a accu_init =
    foldi_array1 (fun _i s accu -> f s accu) a accu_init

  let init k l n1 n2 f =
    let a = create k l n1 n2 in
    iteri (fun i1 i2 _ -> unsafe_set a i1 i2 (f i1 i2)) a;
    a

  module Op = struct
    let make_binop op a b =
      if dims a <> dims b then invalid_arg "Array2.Infix";
      let k = kind a in
      map2 (op k) k a b

    let ( + ) a b = make_binop Kind.to_add a b
    let ( - ) a b = make_binop Kind.to_sub a b
    let ( * ) a b = make_binop Kind.to_mul a b
    let ( / ) a b = make_binop Kind.to_div a b
    let ( ~- ) a = map (Kind.to_neg (kind a)) (kind a) a

    let make_binop op a v =
      let k = kind a in
      map (fun x -> op k x v) k a

    let ( +: ) a b = make_binop Kind.to_add a b
    let ( -: ) a b = make_binop Kind.to_sub a b
    let ( *: ) a b = make_binop Kind.to_mul a b
    let ( /: ) a b = make_binop Kind.to_div a b

    let make_binop op a b =
      if dims a <> dims b then invalid_arg "Array2.Infix";
      let op = op (kind a) in
      modifyi (fun i1 i2 ax -> op ax (unsafe_get b i1 i2)) a

    let ( +< ) a b = make_binop Kind.to_add a b
    let ( -< ) a b = make_binop Kind.to_sub a b
    let ( *< ) a b = make_binop Kind.to_mul a b
    let ( /< ) a b = make_binop Kind.to_div a b
    let ( ~-< ) a = modify (Kind.to_neg (kind a)) a

    let make_binop op a v =
      let op = op (kind a) in
      modify (fun x -> op x v) a

    let ( +:< ) a b = make_binop Kind.to_add a b
    let ( -:< ) a b = make_binop Kind.to_sub a b
    let ( *:< ) a b = make_binop Kind.to_mul a b
    let ( /:< ) a b = make_binop Kind.to_div a b
  end
end

module Array3 = struct
  include Array3

  let dims a = dim1 a, dim2 a, dim3 a

  let max_index a =
    let offset = Layout.min_index (layout a) in
    dim1 a + offset - 1,
    dim2 a + offset - 1,
    dim3 a + offset - 1

  let max_major_index (type l) (a : (_, _, l) t) =
    let n1, _n2, n3 = max_index a in
    match layout a with
    | C_layout -> n1
    | Fortran_layout -> n3

  let max_major_indices (type l) (a : (_, _, l) t) =
    let n1, n2, n3 = max_index a in
    match layout a with
    | C_layout -> n1, n2
    | Fortran_layout -> n2, n3

  let make kind layout d1 d2 d3 init =
    let a = create kind layout d1 d2 d3 in
    fill a init;
    a

  let copy a =
    let b = create (kind a) (layout a) (dim1 a) (dim2 a) (dim3 a) in
    blit a b;
    b

  let sub (type l) (a : (_, _, l) t) ofs len : (_, _, l) t =
    let layout = layout a in
    match layout with
    | C_layout -> sub_left a ofs len
    | Fortran_layout -> sub_right a ofs len

  let slice1 (type l) (a : (_, _, l) t) i j : (_, _, l) Array1.t =
    let layout = layout a in
    match layout with
    | C_layout -> slice_left_1 a i j
    | Fortran_layout -> slice_right_1 a i j

  let slice2 (type l) (a : (_, _, l) t) i : (_, _, l) Array2.t =
    let layout = layout a in
    match layout with
    | C_layout -> slice_left_2 a i
    | Fortran_layout -> slice_right_2 a i

  let to_array (type l) (a : (_, _, l) t) =
    let n1, n2, n3 = dims a in
    let offset = Layout.min_index (layout a) in
    let n =
      match layout a with
      | C_layout -> n1
      | Fortran_layout -> n3
    in
    Array.init n (
      fun i ->
        let s = slice2 a (i + offset) in
        Array2.to_array s
    )

  let of_array1 s n1 n2 n3 =
    let ga = genarray_of_array1 s in
    reshape_3 ga n1 n2 n3

  let to_array1 a =
    let ga = genarray_of_array3 a in
    reshape_1 ga (dim1 a * dim2 a * dim3 a)

  let foldi f a accu_init =
    let offset = Layout.min_index (layout a) in
    let n1, n2, n3 = max_index a in
    let result = ref accu_init in
    for i1 = offset to n1 do
      for i2 = offset to n2 do
        for i3 = offset to n3 do
          result := f i1 i2 i3 (unsafe_get a i1 i2 i3) !result
        done
      done
    done;
    !result

  let fold f a accu_init =
    foldi (fun _i1 _i2 _i3 x accu -> f x accu) a accu_init

  let modifyi f a =
    foldi (
      fun i1 i2 i3 x () ->
        unsafe_set a i1 i2 i3 (f i1 i2 i3 x)
    ) a ()

  let modify f a =
    modifyi (fun _i1 _i2 _i3 x -> f x) a

  let modify2 f a b =
    if dims a <> dims b then invalid_arg "Size mismatch";
    modifyi (fun i1 i2 i3 ax -> f ax (unsafe_get b i1 i2 i3)) a

  let mapi f k a =
    let l = layout a in
    let b =
      let d1, d2, d3 = dims a in
      create k l d1 d2 d3
    in
    let offset = Layout.min_index l in
    let n1, n2, n3 = max_index a in
    for i1 = offset to n1 do
      for i2 = offset to n2 do
        for i3 = offset to n3 do
          unsafe_set b i1 i2 i3 (f i1 i2 i3 (unsafe_get a i1 i2 i3))
        done
      done
    done;
    b

  let map f k a =
    mapi (fun _i1 _i2 _i3 x -> f x) k a

  let map2i f k a b =
    mapi (
      fun i1 i2 i3 ax ->
        f i1 i2 i3 ax (unsafe_get b i1 i2 i3)
    ) k a

  let map2 f k a b =
    map2i (fun _i1 _i2 _i3 ax bx -> f ax bx) k a b

  let iteri f a =
    let l = layout a in
    let offset = Layout.min_index l in
    let n1, n2, n3 = max_index a in
    for i1 = offset to n1 do
      for i2 = offset to n2 do
        for i3 = offset to n3 do
          f i1 i2 i3 (unsafe_get a i1 i2 i3)
        done
      done
    done

  let iter f a =
    iteri (fun _i1 _i2 _i3 x -> f x) a

  let reducei f a =
    let offset = Layout.min_index (layout a) in
    let n1, n2, n3 = max_index a in
    if n1 < 1 || n2 < 1 || n3 < 1 then invalid_arg "reduce";
    let result = ref (unsafe_get a offset offset offset) in
    for i1 = offset to n1 do
      for i2 = offset to n2 do
        let offset =
          if i1 = offset && i2 = offset then offset + 1 else offset
        in
        for i3 = offset to n3 do
          result := f i1 i2 i3 (unsafe_get a i1 i2 i3) !result
        done
      done
    done;
    !result

  let reduce f a =
    reducei (fun _i1 _i2 _i3 x accu -> f x accu) a

  let mapi_to_array2 f k a =
    let n1, n2 = max_major_indices a in
    Array2.init k (layout a) n1 n2 (
      fun i1 i2 ->
        f i1 i2 (slice1 a i1 i2)
    )

  let map_to_array2 f k a =
    mapi_to_array2 (fun _i1 _i2 x -> f x) k a

  let mapi_to_array1 f k a =
    let n = max_major_index a in
    Array1.init k (layout a) n (
      fun i ->
        f i (slice2 a i)
    )

  let map_to_array1 f k a =
    mapi_to_array1 (fun _i x -> f x) k a

  let mapi_array2 f k a =
    let b = create k (layout a) (dim1 a) (dim2 a) (dim3 a) in
    let offset = Layout.min_index (layout a) in
    let n = max_major_index a in
    for i = offset to n do
      let s = slice2 a i in
      let d = slice2 b i in
      Array2.blit (f i s) d
    done;
    b

  let map_array2 f k a =
    mapi_array2 (fun _i x -> f x) k a

  let mapi_array1 f k a =
    let b = create k (layout a) (dim1 a) (dim2 a) (dim3 a) in
    let offset = Layout.min_index (layout a) in
    let n1 = max_major_index a in
    let _, n2, _ = max_index a in
    for i1 = offset to n1 do
      for i2 = offset to n2 do
        let s = slice1 a i1 i2 in
        let d = slice1 b i1 i2 in
        Array1.blit (f i1 i2 s) d
      done
    done;
    b

  let map_array1 f k a =
    mapi_array1 (fun _i1 _i2 x -> f x) k a

  let iteri_array2 f a =
    let offset = Layout.min_index (layout a) in
    let n = max_major_index a in
    for i = offset to n do
      f i (slice2 a i)
    done

  let iter_array2 f a =
    iteri_array2 (fun _i x -> f x) a

  let iteri_array1 f a =
    let offset = Layout.min_index (layout a) in
    let n1 = max_major_index a in
    let _, n2, _ = max_index a in
    for i1 = offset to n1 do
      for i2 = offset to n2 do
        f i1 i2 (slice1 a i1 i2)
      done
    done

  let iter_array1 f a =
    iteri_array1 (fun _i1 _i2 x -> f x) a

  let reducei_array2 f a =
    let offset = Layout.min_index (layout a) in
    let n = max_major_index a in
    let result = ref (slice2 a offset) in
    for i = offset + 1 to n do
      result := f i (slice2 a i) !result
    done;
    !result

  let reduce_array2 f a =
    reducei_array2 (fun _i s accu -> f s accu) a

  let reducei_array1 f a =
    let offset = Layout.min_index (layout a) in
    let n1 = max_major_index a in
    let _, n2, _ = max_index a in
    let result = ref (slice1 a offset offset) in
    for i1 = offset to n1 do
      let offset = if i1 = offset then offset + 1 else offset in
      for i2 = offset to n2 do
        result := f i1 i2 (slice1 a i1 i2) !result
      done
    done;
    !result

  let reduce_array1 f a =
    reducei_array1 (fun _i1 _i2 s accu -> f s accu) a

  let foldi_array2 f a accu_init =
    let offset = Layout.min_index (layout a) in
    let n = max_major_index a in
    let result = ref accu_init in
    for i = offset to n do
      result := f i (slice2 a i) !result
    done;
    !result

  let fold_array2 f a accu_init =
    foldi_array2 (fun _i s accu -> f s accu) a accu_init

  let foldi_array1 f a accu_init =
    let offset = Layout.min_index (layout a) in
    let n1 = max_major_index a in
    let _, n2, _ = max_index a in
    let result = ref accu_init in
    for i1 = offset to n1 do
      for i2 = offset to n2 do
        result := f i1 i2 (slice1 a i1 i2) !result
      done
    done;
    !result

  let fold_array1 f a accu_init =
    foldi_array1 (fun _i1 _i2 s accu -> f s accu) a accu_init

  let init k l n1 n2 n3 f =
    let a = create k l n1 n2 n3 in
    iteri (fun i1 i2 i3 _ -> unsafe_set a i1 i2 i3 (f i1 i2 i3)) a;
    a

  module Op = struct
    let make_binop op a b =
      if dims a <> dims b then invalid_arg "Array3.Infix";
      let k = kind a in
      map2 (op k) k a b

    let ( + ) a b = make_binop Kind.to_add a b
    let ( - ) a b = make_binop Kind.to_sub a b
    let ( * ) a b = make_binop Kind.to_mul a b
    let ( / ) a b = make_binop Kind.to_div a b
    let ( ~- ) a = map (Kind.to_neg (kind a)) (kind a) a

    let make_binop op a v =
      let k = kind a in
      map (fun x -> op k x v) k a

    let ( +: ) a b = make_binop Kind.to_add a b
    let ( -: ) a b = make_binop Kind.to_sub a b
    let ( *: ) a b = make_binop Kind.to_mul a b
    let ( /: ) a b = make_binop Kind.to_div a b

    let make_binop op a b =
      if dims a <> dims b then invalid_arg "Array3.Infix";
      let op = op (kind a) in
      modifyi (fun i1 i2 i3 ax -> op ax (unsafe_get b i1 i2 i3)) a

    let ( +< ) a b = make_binop Kind.to_add a b
    let ( -< ) a b = make_binop Kind.to_sub a b
    let ( *< ) a b = make_binop Kind.to_mul a b
    let ( /< ) a b = make_binop Kind.to_div a b
    let ( ~-< ) a = modify (Kind.to_neg (kind a)) a

    let make_binop op a v =
      let op = op (kind a) in
      modify (fun x -> op x v) a

    let ( +:< ) a b = make_binop Kind.to_add a b
    let ( -:< ) a b = make_binop Kind.to_sub a b
    let ( *:< ) a b = make_binop Kind.to_mul a b
    let ( /:< ) a b = make_binop Kind.to_div a b
  end
end

module Genarray = struct
  include Genarray

  let min_index a =
    let offset = Layout.min_index (layout a) in
    let n_dims = Array.length @@ dims a in
    Array.make n_dims offset

  let max_index a =
    let offset = Layout.min_index (layout a) in
    let dims = dims a in
    Array.map (fun x -> x - 1 + offset) dims

  let elements a =
    Array.fold_left (
      fun accu dim ->
        accu * dim
    ) 1 (dims a)

  let make k l size x =
    let a = create k l size in
    fill a x;
    a

  let copy a =
    let b = create (kind a) (layout a) (dims a) in
    blit a b;
    b

  let reshape = reshape

  let flatten a = reshape a [|elements a|]
  let flatten_to_array1 a = reshape_1 (flatten a) (elements a)

  let of_array1 = genarray_of_array1
  let to_array1 = array1_of_genarray
  let of_array2 = genarray_of_array2
  let to_array2 = array2_of_genarray
  let of_array3 = genarray_of_array3
  let to_array3 = array3_of_genarray

  let sub (type l) (a : (_, _, l) t) ofs len : (_, _, l) t =
    let layout = layout a in
    match layout with
    | C_layout -> sub_left a ofs len
    | Fortran_layout -> sub_right a ofs len

  let slice (type l) (a : (_, _, l) t) i : (_, _, l) t =
    let layout = layout a in
    match layout with
    | C_layout -> slice_left a i
    | Fortran_layout -> slice_right a i

  let reduce f a =
    let a = flatten_to_array1 a in
    Array1.reduce f a

  let fold f a accu_init =
    let a = flatten_to_array1 a in
    Array1.fold f a accu_init

  let map f k a =
    let dims = dims a in
    let a = flatten_to_array1 a in
    let a' = Array1.map f k a in
    reshape (of_array1 a') dims

  let iter f a =
    Array1.iter f (flatten_to_array1 a)

  let iter_dims_f dims f =
    let rec loop dims current_dimension pos f =
      for i = 1 to dims.(current_dimension) do
        pos.(current_dimension) <- i;
        if (current_dimension = 0) then f pos
        else loop dims (current_dimension - 1) pos f
      done
    in
    let n_dims = Array.length dims in
    let pos = Array.make n_dims 1 in
    loop dims (n_dims - 1) pos f

  let iter_dims_c dims f =
    let rec loop dims current_dimension pos f =
      for i = 0 to dims.(current_dimension) - 1 do
        pos.(current_dimension) <- i;
        if (current_dimension = Array.length pos - 1) then f pos
        else loop dims (current_dimension + 1) pos f
      done
    in
    let pos = Array.make (Array.length dims) 0 in
    loop dims 0 pos f

  let iter_dims (type l) (layout : l layout) dims f =
    match layout with
    | C_layout -> iter_dims_c dims f
    | Fortran_layout -> iter_dims_f dims f

  let iteri f a =
    iter_dims (layout a) (dims a) (fun i -> f i @@ get a i)

  let foldi f a accu_init =
    let result = ref accu_init in
    iter_dims (layout a) (dims a) (fun i -> result := f i (get a i) !result);
    !result

  let mapi f k a =
    let b = create k (layout a) (dims a) in
    iter_dims (layout a) (dims a) (fun i -> set b i (f i (get a i)));
    b

  let map2i f k a b =
    mapi (
      fun i ax ->
        f i ax (get b i)
    ) k a

  let map2 f k a b =
    map2i (fun _i ax bx -> f ax bx) k a b
(*

  let reducei (type l) f (a : (_, _, l) t) =
    FIX ME!  THIS NEEDS TO SKIP THE ELEMENT IT INITIALIZES WITH
    SO IT MAY BE WORTH INLINING THE iter_dims CODE AND SPECIALIZING
    IT FOR THIS CASE
    foldi f a (get a @@ min_index a)
*)

  let modifyi f a =
    foldi (
      fun i x () ->
        set a i (f i x)
    ) a ()

  let modify f a =
    modifyi (fun _i x -> f x) a

  let modify2 f a b =
    if dims a <> dims b then invalid_arg "Size mismatch";
    modifyi (fun i ax -> f ax (get b i)) a

  module Op = struct
    let make_binop op a b =
      if dims a <> dims b then invalid_arg "Genarray.Infix";
      let k = kind a in
      map2 (op k) k a b

    let ( + ) a b = make_binop Kind.to_add a b
    let ( - ) a b = make_binop Kind.to_sub a b
    let ( * ) a b = make_binop Kind.to_mul a b
    let ( / ) a b = make_binop Kind.to_div a b
    let ( ~- ) a = map (Kind.to_neg (kind a)) (kind a) a

    let make_binop op a v =
      let k = kind a in
      map (fun x -> op k x v) k a

    let ( +: ) a b = make_binop Kind.to_add a b
    let ( -: ) a b = make_binop Kind.to_sub a b
    let ( *: ) a b = make_binop Kind.to_mul a b
    let ( /: ) a b = make_binop Kind.to_div a b

    let make_binop op a b =
      if dims a <> dims b then invalid_arg "Genarray.Infix";
      let op = op (kind a) in
      modifyi (fun i ax -> op ax (get b i)) a

    let ( +< ) a b = make_binop Kind.to_add a b
    let ( -< ) a b = make_binop Kind.to_sub a b
    let ( *< ) a b = make_binop Kind.to_mul a b
    let ( /< ) a b = make_binop Kind.to_div a b
    let ( ~-< ) a = modify (Kind.to_neg (kind a)) a

    let make_binop op a v =
      let op = op (kind a) in
      modify (fun x -> op x v) a

    let ( +:< ) a b = make_binop Kind.to_add a b
    let ( -:< ) a b = make_binop Kind.to_sub a b
    let ( *:< ) a b = make_binop Kind.to_mul a b
    let ( /:< ) a b = make_binop Kind.to_div a b
  end
end
