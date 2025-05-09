type vec = { x : float; y : float; z : float }
type value = Float of float | Int of int

type elem =
  | Point of vec * value
  | Line of vec * vec * value
  | Triangle of vec * vec * vec * value
  | Polygon of vec list * value

val rotate_x : float -> vec -> vec
val rotate_y : float -> vec -> vec
val rotate_z : float -> vec -> vec
val point_z_cmp : vec -> vec -> int
val get_represent_vec : elem -> vec
val element_z_cmp : elem -> elem -> int
