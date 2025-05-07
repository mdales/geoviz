type t = { root : Yojson.Basic.t }
type coord = { latitude : float; longitude : float }

type geometry =
  | Point of coord
  | MultiPoint of coord list
  | LineString of coord list
  | MultiLineString of coord list list
  | Polygon of coord list list
  | MultiPolygon of coord list list list
  | None

(* Not complete... *)
type property = String of string | Int of int | Float of float | Null

let v root = { root }

let to_float (x : Yojson.Basic.t) : float =
  match x with
  | `Float x -> x
  | `Int x -> float_of_int x
  | _ -> failwith "Expected number"

let convert_coords coordinates =
  match coordinates with
  | [ x; y ] -> Some { longitude = to_float x; latitude = to_float y }
  | _ -> None

let geometry v =
  let open Yojson.Basic.Util in
  let geometry = v.root |> member "geometry" in
  let geom_type = geometry |> member "type" |> to_string in
  match geom_type with
  | "Point" -> (
      let coordinates = geometry |> member "coordinates" |> to_list in
      match convert_coords coordinates with Some c -> Point c | None -> None)
  | "MultiPoint" ->
      let list_of_coords = geometry |> member "coordinates" |> to_list in
      MultiPoint
        (List.filter_map
           (fun point_list ->
             let coordinates = to_list point_list in
             convert_coords coordinates)
           list_of_coords)
  | "LineString" ->
      let list_of_coords = geometry |> member "coordinates" |> to_list in
      LineString
        (List.filter_map
           (fun point_list ->
             let coordinates = to_list point_list in
             convert_coords coordinates)
           list_of_coords)
  | "MultiLineString" ->
      (* coordinates is a list of lists of points *)
      let list_of_list_of_coords =
        geometry |> member "coordinates" |> to_list
      in
      MultiLineString
        (List.map
           (fun line_node ->
             let list_of_coords = to_list line_node in
             List.filter_map
               (fun point_list ->
                 let coordinates = to_list point_list in
                 convert_coords coordinates)
               list_of_coords)
           list_of_list_of_coords)
  | "Polygon" ->
      let list_of_list_of_coords =
        geometry |> member "coordinates" |> to_list
      in
      Polygon
        (List.map
           (fun line_node ->
             let list_of_coords = to_list line_node in
             List.filter_map
               (fun point_list ->
                 let coordinates = to_list point_list in
                 convert_coords coordinates)
               list_of_coords)
           list_of_list_of_coords)
  | "MultiPolygon" -> 
      let list_of_list_of_list_of_coords =
        geometry |> member "coordinates" |> to_list
      in
      MultiPolygon (
        List.map (fun list_of_list_of_coords ->
          List.map
             (fun line_node ->
               let list_of_coords = to_list line_node in
               List.filter_map
                 (fun point_list ->
                   let coordinates = to_list point_list in
                   convert_coords coordinates)
                 list_of_coords)
             (to_list list_of_list_of_coords)) list_of_list_of_list_of_coords)
  | _ -> None

let property_keys v =
  match Yojson.Basic.Util.member "properties" v.root with
  | `Assoc l -> List.map (fun (k, _v) -> k) l
  | _ -> []

let property v k =
  match Yojson.Basic.Util.member "properties" v.root with
  | `Assoc l -> (
      match List.assoc_opt k l with
      | Some p ->
          Some
            (match p with
            | `String s -> String s
            | `Float f -> Float f
            | `Int i -> Int i
            | _ -> Null)
      | None -> None)
  | _ -> None
