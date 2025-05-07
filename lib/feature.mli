type t
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

val v : Yojson.Basic.t -> t
val geometry : t -> geometry
val property_keys : t -> string list
val property : t -> string -> property option
