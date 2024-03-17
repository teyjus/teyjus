(lang dune 3.1)
(name "teyjus")
(version "2.2.0")
(cram enable)

(generate_opam_files true)
(source (github teyjus/teyjus))
(homepage "http://teyjus.cs.umn.edu/")
(documentation "https://github.com/teyjus/teyjus/wiki/TeyjusManual")
(authors "Andrew Gacek, Nathan Guermond, Steven Holte, Gopalan Nadathur, Xiaochu Qi, Zach Snow")
(maintainers "guerm001@umn.edu")
(license "GPL-3.0-or-later")

(package
 (name "teyjus")
 (synopsis "An efficient implementation of the higher-order logic programming language Lambda Prolog")
 (description
"
The Teyjus system is an efficient implementation of the higher-order logic
programming language Lambda Prolog. The main strength of Lambda Prolog, a language
developed by Dale Miller and Gopalan Nadathur, is the support it
offers for what is known as lambda tree syntax, a new and effective
way to view the structure of complex syntactic objects such as types,
formulas, proofs and programs. Lambda Prolog provides this support by
using lambda terms as data representation devices and by including
primitives for probing such terms in logically meaningful ways. The
new features present in the language raise several implementation
challenges that have been addressed in collaborative work between
Nadathur and his students and colleagues. The name \"Teyjus\" actually
stands both for a project and for a class of compiler and
virtual machine based realizations of Lambda Prolog arising from
the research carried out within this project.
Underlying the most recent version of the Teyjus system is a
new and significantly improved virtual machine that has been designed by
Xiaochu Qi as part of her doctoral dissertation work
at the University of Minnesota. This virtual machine extensively
exploits a special form of higher-order unification known as pattern
unification.
")
 (depends
  dune
  (ocaml (and (>= 4.05.0) (or (< 5.0.0) (>= 5.1.0))))
))
