open Extbigarray
open TestSimple

let k = Bigarray.Int
let c = Bigarray.C_layout
let f = Bigarray.Fortran_layout

let a1_int =
  Array1.init Bigarray.Int c 3 (fun i -> i)
let a1_float =
  Array1.init Bigarray.Float64 c 3 (fun i -> float_of_int i)
let a1_test =
  let a = Array1.create Bigarray.Char Bigarray.C_layout 4 in
  String.iteri (fun i c -> a.{i} <- c) "test";
  a

let a2_int =
  Array2.make Bigarray.Int c 3 2 1

let a3_int =
  Array3.make Bigarray.Int c 3 3 3 1

let ga_int =
  Genarray.of_array3 a3_int

let ga_float =
  Genarray.of_array3 (Array3.map float_of_int Bigarray.Float64 a3_int)

let a_3d = [|
  [|
    [|1; 1; 1|];
    [|1; 1; 1|];
    [|1; 1; 1|];
  |];
  [|
    [|1; 1; 1|];
    [|1; 1; 1|];
    [|1; 1; 1|];
  |];
  [|
    [|1; 1; 1|];
    [|1; 1; 1|];
    [|1; 1; 1|];
  |];
|]

let () =
  no_plan ();
  diag "Testing Array1";
  is (Array1.make k c 3 0) (Array1.init k c 3 (fun _ -> 0)) "Array1.init";
  is a1_float (Array1.map float_of_int Bigarray.Float64 a1_int) "Array1.map";
  is (Array1.fold ( + ) a1_int 0) 3 "Array1.fold";
  is (Array1.reduce ( + ) a1_int) 3 "Array1.reduce";
  is (Array1.to_array a1_int) [|0; 1; 2|] "Array1.to_array";
  is (Array1.of_array k c [|0; 1; 2|]) a1_int "Array1.of_array";
  is (Array1.of_string Bigarray.Char c "test") a1_test "Array1.of_string";
  is (Array1.to_string a1_test) "test" "Array1.to_string";

  diag "Testing Array2";
  is a2_int (Array2.init k c 3 2 (fun _ _ -> 1)) "Array2.init";
  is (Array2.map float_of_int Bigarray.Float64 a2_int)
    (Array2.make Bigarray.Float64 c 3 2 1.0)
    "Array2.map";
  is (Array2.reduce ( + ) a2_int) 6 "Array2.reduce";
  is (Array2.fold ( + ) a2_int 0) 6 "Array2.fold";
  is (Array2.of_array k c [|[|1; 1|]; [|1; 1|]; [|1; 1|]|]) a2_int "Array2.of_array";
  is (Array2.to_array a2_int) [|[|1; 1|]; [|1; 1|]; [|1; 1|]|] "Array2.to_array";
  is (Array2.of_array1 (Array1.make k c 6 1) 3 2) a2_int "Array2.of_array1";

  diag "Testing Array3";
  is a3_int (Array3.init k c 3 3 3 (fun _ _ _ -> 1)) "Array3.init";
  is (Array3.map float_of_int Bigarray.Float64 a3_int)
    (Array3.make Bigarray.Float64 c 3 3 3 1.0)
    "Array3.map";
  is (Array3.fold ( + ) a3_int 0) 27 "Array3.fold";
  is (Array3.reduce ( + ) a3_int) 27 "Array3.reduce";
  is (Array3.of_array k c a_3d) a3_int "Array3.of_array";
  is (Array3.to_array a3_int) a_3d "Array3.to_array";
  is (Array3.of_array1 (Array1.make k c 27 1) 3 3 3) a3_int "Array3.of_array1";

  diag "Testing Genarray";
  is (Genarray.map float_of_int Bigarray.Float64 ga_int)
    ga_float "Genarray.map";
  is (Genarray.reduce ( + ) ga_int) 27 "Genarray.reduce";
  is (Genarray.fold ( + ) ga_int 0) 27 "Genarray.fold";
