include module type of Proof_tactics_types

module Zipper = Proof_zipper

val apply : Zipper.partial -> t -> _ result
