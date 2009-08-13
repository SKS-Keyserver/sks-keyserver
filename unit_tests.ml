open Printf
open Common

let run () = 
  printf "Running Decode unit tests:%!";
  begin
    try Decode_test.run ()
    with Unit_test_failure s ->
      printf "\nUnit test failure: %s\n%!" s
  end;
  printf "Done\n%!";

  printf "Running Number unit tests:%!";
  begin
    try Number_test.run ()
    with Unit_test_failure s ->
      printf "\nUnit test failure: %s\n%!" s
  end;
  printf "Done\n%!";

  printf "Running Poly unit tests:%!";
  begin
    try Poly_test.run ()
    with Unit_test_failure s ->
      printf "\nUnit test failure: %s\n%!" s
  end;
  printf "Done\n%!";

