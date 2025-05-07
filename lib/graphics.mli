type vec = { x : float; y : float; z : float }

type elem =
  | Point of vec
  | Line of vec * vec
  | Triangle of vec * vec * vec
  | Polygon of vec list

val rotate_x : float -> vec -> vec
val rotate_y : float -> vec -> vec
val rotate_z : float -> vec -> vec
val point_z_cmp : vec -> vec -> int
val get_represent_vec : elem -> vec
val element_z_cmp : elem -> elem -> int
