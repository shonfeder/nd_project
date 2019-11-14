open Core_kernel

module Author = struct
  type  t =
    { first: string
    ; last: string
    }
  [@@deriving fields]

  let make = Fields.create
end

type t =
  { authors: Author.t list
  ; title: string
  ; page: [`Pg of int | `Roman of string] option
  ; excerpt: string
  }
[@@deriving fields]

let make ?(page=None) = Fields.create ~page

let gentzen_nd excerpt page = make
    ~authors:[Author.make ~first:"Gerhard" ~last:"Gentzen"]
    ~title:"Investigations Into Logical Deduction"
    ~page:(Some page)
    ~excerpt
