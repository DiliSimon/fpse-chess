(* Base_quickcheck Examples
   Requires:
   (libraries core)
   (preprocess (pps ppx_jane))
   in dune file
   And #require "ppx_jane" in top-loop
*)

(* `Base_quickcheck` is highly integrated with Core libraries which is why we use it *)

open Core

(* Step 1 is you need a generator to make random data in a given type. *)
(* For all the built-in types, Core provides <Builtin_type>.quickcheck_generator, a default generator *)

let rand_int =  
  let int_gen = Int.quickcheck_generator in
  (Quickcheck.random_value ~seed:`Nondeterministic int_gen)

(* `Int.gen_incl` generates ints in a range *)  
let rand_int' =  
let int_gen = Int.gen_incl (-100) 100 in
(Quickcheck.random_value ~seed:`Nondeterministic int_gen)

let rand_string =
    let str_gen = String.quickcheck_generator in
    (Quickcheck.random_value ~seed:`Nondeterministic str_gen)

(* A little function to test out various generators: given a generator returns a random value *)
let rand_from (g : 'a Base_quickcheck.Generator.t) = 
  (Quickcheck.random_value ~seed:`Nondeterministic g)

(* Parameterized type generators need a generator for the parameter 
   Similar to how `List.equal` needs an `equal` on list contents *)
let int_list_gen = List.quickcheck_generator Int.quickcheck_generator

let str_list_gen = List.quickcheck_generator String.quickcheck_generator

(* One random list *)
let rand_list = rand_from int_list_gen

(* Shorthand ppx notation *)
let int_list_gen' = [%quickcheck.generator: int list]

(* Lists with a narrower range of integers *)
let int_list_gen'' = List.quickcheck_generator (Int.gen_incl (-100) 100)

(* Similarly can compose two generators to generate a pair via Quickcheck.Generator.both *)
let rand_list_pair = rand_from (Quickcheck.Generator.both int_list_gen int_list_gen)

(* **************************************** *)
(* Using generators to repeatedly test code *)
(* **************************************** *)

(* Simple failure example based on https://github.com/realworldocaml/book/tree/master/book/testing) *)
(* Replace `assert` with `OUnit2.assert_bool` and this code can be an OUnit test function. *)
let () =
  Quickcheck.test ~sexp_of:[%sexp_of: int] (* optional sexp_of needed to see failure case if any *)
    (Int.gen_incl Int.min_value Int.max_value)
    ~f:(fun x -> assert(Sign.equal (Int.sign (Int.neg x)) (Sign.flip (Int.sign x))))

(* Check to see if (reverse o reverse) is identity on all lists *)    
let check_a_list_rev revver = 
  Quickcheck.test ~sexp_of:[%sexp_of: int list]
    int_list_gen
    ~f:(fun l -> assert(List.equal Int.equal (revver (revver l)) l))

let check_str_sanitize sanitizer =
    Quickcheck.test ~sexp_of:[%sexp_of: string]
        String.quickcheck_generator
        ~f:(fun l -> assert(match sanitizer l with
                            | None -> true
                            | Some(s) -> not ((String.contains l '%')||(String.contains l '!')||(String.contains l '?'))))

(* The following contains no failures - silence means success *)
let () = check_a_list_rev List.rev

(* Making a bad version of reverse *)
let bad_rev l = match l with 1::_ -> [] | _ -> List.rev l

let () = check_a_list_rev bad_rev

(* Generators for your own types 
   Fortunately Quickcheck has an easy ppx to do this, analogous to `@@deriving equal`.  *)

type complex = CZero | Nonzero of float * float [@@deriving quickcheck]

let compl = rand_from quickcheck_generator_complex

(* The following parametric type gen should also work but is currently broken in the opam dist *)

(* type 'a bin_tree = Leaf | Node of 'a * 'a bin_tree * 'a bin_tree [@@deriving quickcheck] *)

type int_tree = Leaf | Node of int * int_tree * int_tree [@@deriving quickcheck]

type int_tree = 
  | Leaf
  | Node of int * int_tree * int_tree [@@deriving quickcheck]

let atree = rand_from quickcheck_generator_int_tree

(* Also Core has Map.quickcheck_generator, etc etc *)

(* In general you can write your own generators if you want a unique distribution
   But, the happy path is to use the built-in ones as above to make things worth the effort *)