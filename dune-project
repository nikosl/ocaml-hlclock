(lang dune 2.9)

(name hlclock)

(documentation "https://nikosl.github.io/hlclock/")

(source
 (github nikosl/hlclock))

(license ISC)

(authors
  "Nikos Leivadaris")

(maintainers
  "Nikos Leivadaris")

(generate_opam_files true)

(package
 (name hlclock)
 (synopsis "Hybrid Logical Clocks")
 (description "Hybrid Logical Clocks")
 (depends
  (ocaml (>= 4.08.0))
  dune
  (bisect_ppx :with-test)
  (qcheck :with-test)
  (qcheck-alcotest :with-test)
  (alcotest :with-test)
  (odoc :with-doc)))
