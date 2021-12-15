(lang dune 2.9)
(generate_opam_files true)
  (authors "Tingwen Guo" "Tianyue Ou")
  (maintainers "tou3@jhu.edu")
  (package
   (name chess) 
   (synopsis "Ocaml chess")
   (description "Ocaml chess to play with human or Bot")
   (depends
    (ounit2 (> 2.0)) ; for each opam dependency list the version
    (core (> 0.14.1)) ; `opam list core` will display which version of core you have
 ))