type vec = { x : float; y : float; z : float }
type value = Float of float | Int of int

type elem =
  | Point of vec * value
  | Line of vec * vec * value
  | Triangle of vec * vec * vec * value
  | Polygon of vec list * value

let rotate_x (a : float) (p : vec) : vec =
  {
    p with
    y = (p.y *. cos a) -. (p.z *. sin a);
    z = (p.y *. sin a) +. (p.z *. cos a);
  }

let rotate_y (a : float) (p : vec) : vec =
  {
    p with
    x = (p.x *. cos a) -. (p.z *. sin a);
    z = (p.x *. sin a) +. (p.z *. cos a);
  }

let rotate_z (a : float) (p : vec) : vec =
  {
    p with
    x = (p.x *. cos a) -. (p.y *. sin a);
    y = (p.x *. sin a) +. (p.y *. cos a);
  }

let point_z_cmp (a : vec) (b : vec) : int =
  if a.z == b.z then 0 else if a.z < b.z then 1 else -1

let get_represent_vec elem =
  (* at some point this could be the mid point *)
  match elem with
  | Point (v, _) -> v
  | Line (a, _, _) -> a
  | Triangle (a, _, _, _) -> a
  | Polygon (al, _) -> List.nth al 0

let element_z_cmp (a : elem) (b : elem) : int =
  point_z_cmp (get_represent_vec a) (get_represent_vec b)
