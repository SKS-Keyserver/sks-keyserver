open Big_int
open StdLabels
open MoreLabels
open Printf
open Number
open Number.Infix
open Common

(** Unit tests for number.ml *)

let rand_int = Random.State.int RMisc.det_rng
let rand_bits () = Random.State.bits RMisc.det_rng

let ctr = ref 0
let test cond = 
  printf ".%!";
  incr ctr;
  if not cond then raise (Unit_test_failure (sprintf "Number test %d failed" !ctr))


let conversion_test () = 
  let nbits = rand_int 400 + 1 in
  let nbytes = nbits / 8 + (if nbits mod 8 = 0 then 0 else 1) in
  let x = Prime.randbits rand_bits nbits in
  let xstr = to_bytes ~nbytes x in
  test (of_bytes xstr =! x)

let powmod_test () =
  let x = Prime.randbits rand_bits (rand_int 12 + 1) in
  let y = Prime.randbits rand_bits (rand_int 12 + 1) in
  let m = Prime.randbits rand_bits (rand_int 12 + 1) in
  test (powmod x y m =! dumb_powmod x y m)


let run () =
  for i = 1 to 100 do conversion_test () done;
  for i = 1 to 100 do powmod_test () done;
