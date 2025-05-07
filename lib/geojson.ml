type t = { root : Yojson.Basic.t }

let of_file filename = { root = Yojson.Basic.from_file filename }

let features v =
  let open Yojson.Basic.Util in
  v.root |> member "features" |> to_list |> List.map Feature.v
