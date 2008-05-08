open StdLabels
open Printf
open ZZp
open Number.Infix


let rec gcd_ex' a b = 
  if b =! zero then (one,zero,a)
  else
    let (q,r) = quomod_big_int a b in
    let (u',v',gcd) = gcd_ex' b r in
    (v',u' -! v' *! q, gcd)

let gcd_ex a b = 
  if b <=! a then gcd_ex' a b
  else 
    let (u,v,gcd) = gcd_ex' b a in
    (v,u,gcd)

let gcd_ex_test a b = 
     let (a,b) = (big_int_of_int a,big_int_of_int b) in
     let (u,v,gcd) = gcd_ex a b in
     if (u *! a +! v *! b <>! gcd) 
     then failwith (sprintf "gcd_ex failed on %s and %s" 
		      (string_of_big_int a) (string_of_big_int b))


let run_test () = 
  begin
    gcd_ex_test 95 25;
    gcd_ex_test 25 95;
    gcd_ex_test 1 95;
    gcd_ex_test 95 1;
    gcd_ex_test 22 21;
    gcd_ex_test 21 22;
    gcd_ex_test 12 6;
    gcd_ex_test 6 12;
    gcd_ex_test 6 12;
  end
