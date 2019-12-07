include module type of Proof_tactics_types

module Zipper = Proof_zipper

val intro_imp: _ application
val elim_and : _ application

val apply : Zipper.partial -> t -> _ result
