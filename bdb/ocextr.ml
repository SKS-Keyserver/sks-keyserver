open StdLabels
open MoreLabels
open Printf

let fname = try Sys.argv.(1) with _ ->
  eprintf "No file specified\n";
  exit (-1)

let file = open_in fname
let () =
  try
    while true do
      let line = input_line file in
      let length = String.length line in
      if length >= 3 &&
        String.sub line ~pos:0 ~len:3 = "//+"
      then
        if length = 3 then print_string "\n"
        else
          if line.[3] = ' ' then
            printf "%s\n" (String.sub line ~pos:4 ~len:(length - 4))
          else
            printf "%s\n" (String.sub line ~pos:3 ~len:(length - 3))
    done
  with
    End_of_file -> ()
