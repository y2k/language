(lang dune 3.23)
(name language)
(generate_opam_files true)

(package
 (name language)
 (synopsis "A basic OCaml project")
 (depends angstrom (re (= 1.13.2)) (alcotest :with-test)))
