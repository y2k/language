(lang dune 3.11)

(name clj2js)

(maintainers y2k)

(generate_opam_files true)

(license LICENSE)

(package
 (name clj2js)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
  (alcotest :with-test)
  angstrom
  bencode
  dune
  ocaml
  ppx_deriving
  ppx_deriving_yojson
  re
  tiny_httpd
  yojson)
 (tags
  (topics "to describe" your project)))
