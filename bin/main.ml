open Claudius
open Geoviz.Graphics

type state =
  | Waiting of string option
  | Loading of string
  | Showing of (elem * float) list

(* let state_to_string = function
	| Waiting -> "waiting"
	| Loading fn -> Printf.sprintf "loading %s" fn
	| Showing data -> Printf.sprintf "showing %d" (List.length data) *)

let state = ref (Waiting None)
let inputQ = Domainslib.Chan.make_unbounded ()
let outputQ = Domainslib.Chan.make_unbounded ()

let initial_screen optmsg s =
  let width, height = Screen.dimensions s in
  let fb = Framebuffer.init (width, height) (fun _ _ -> 0) in
  let msg = "Drop file here" in
  let msg_width = Framebuffer.draw_string 2000 2000 (Screen.font s) msg 15 fb in
  ignore
    (Framebuffer.draw_string
       ((width - msg_width) / 2)
       (height / 2) (Screen.font s) msg 15 fb);
  (match optmsg with
  | None -> ()
  | Some msg ->
      let msg_width =
        Framebuffer.draw_string 2000 2000 (Screen.font s) msg 15 fb
      in
      ignore
        (Framebuffer.draw_string
           ((width - msg_width) / 2)
           10 (Screen.font s) msg 15 fb));
  fb

let handle_file_drop filename current_state =
  match (current_state, filename) with
  | _, None -> current_state
  | Loading _, Some _ -> current_state
  | _, Some filename -> (
      match Filename.extension filename with
      | ".geojson" | ".csv" | ".tif" | ".tiff" ->
          Domainslib.Chan.send inputQ (Worker.Task filename);
          Loading filename
      | ext ->
          Waiting (Some (Printf.sprintf "File extension %s not recognised" ext))
      )

let loading_screen s filename =
  let width, height = Screen.dimensions s in
  let fb = Framebuffer.init (width, height) (fun _ _ -> 0) in
  let msg = Printf.sprintf "Loading %s" filename in
  let msg_width = Framebuffer.draw_string 2000 2000 (Screen.font s) msg 15 fb in
  ignore
    (Framebuffer.draw_string
       ((width - msg_width) / 2)
       10 (Screen.font s) msg 15 fb);
  fb

let update_loading_screen t s prev =
  let w, h = Screen.dimensions s in
  let radius = 50.0 in
  let ft = Float.of_int t /. -10. in
  Framebuffer.filled_circle (w / 2) (h / 2) radius 0 prev;
  let x0 = sin ft and y0 = cos ft in
  Framebuffer.draw_line
    (Int.of_float (x0 *. radius /. 2.) + (w / 2))
    (Int.of_float (y0 *. radius /. 2.) + (h / 2))
    (Int.of_float (x0 *. radius) + (w / 2))
    (Int.of_float (y0 *. radius) + (h / 2))
    15 prev;
  prev

let project s (v : vec) : Primitives.point =
  let width, height = Screen.dimensions s in
  let m = 2000. +. (cos (0. /. 30.) *. 600.) in
  {
    x = (width / 2) + (int_of_float (m *. v.x /. (v.z +. 400.)) / 1);
    y = (height / 2) + (int_of_float (m *. v.y /. (v.z +. 400.)) / 1);
  }

let render_to_primitives (_ft : float) (s : Screen.t)
    (elements : (elem * float) list) : Primitives.t list =
  let palette_size = Palette.size (Screen.palette s) - 1 in
  List.filter_map
    (fun (e, c) ->
      let col = Int.of_float (Float.of_int palette_size *. c) in
      match e with
      | Point e ->
          if e.z > 0. then None
          else Some (Primitives.Pixel (project s e, if col = 0 then 1 else 3))
      | Line (a, b) ->
          Some
            (Primitives.Line
               (project s a, project s b, col / if a.z < 0. then 1 else 3))
      | Triangle (a, b, c) ->
          Some
            (Primitives.FilledTriangle
               (project s a, project s b, project s c, col))
      | Polygon vl ->
          let rep = get_represent_vec e in
          Some
            (Primitives.FilledPolygon
               (List.map (project s) vl, col / if rep.z < 0. then 1 else 3)))
    elements

let rotate_element angle e =
  let rfunc x = rotate_x 0.1 (rotate_y angle x) in
  match e with
  | Point v -> Point (rfunc v)
  | Line (a, b) -> Line (rfunc a, rfunc b)
  | Triangle (a, b, c) -> Triangle (rfunc a, rfunc b, rfunc c)
  | Polygon vl -> Polygon (List.map rfunc vl)

let render_data t s elements =
  let w, h = Screen.dimensions s in
  let fb = Framebuffer.init (w, h) (fun _ _ -> 15) in

  let ft = Float.of_int t in

  List.map
    (fun (coord, col) -> (rotate_element (0.01 *. ft) coord, col))
    elements
  |> List.sort (fun (a, _) (b, _) -> element_z_cmp a b)
  (* |> List.filter_map (fun p ->
			 if p.z < 0. then Some p else None
		 )*)
  |> render_to_primitives ft s
  |> Framebuffer.render fb;
  fb

let tick t s prev (inputs : Base.input_state) =
  let event_state =
    match Domainslib.Chan.recv_poll outputQ with
    | None -> !state
    | Some msg -> (
        match msg with
        | Worker.Result data -> Showing data
        | Error msg -> Waiting (Some msg))
  in

  let filename =
    List.fold_left
      (fun acc ev -> match ev with Event.DropFile pth -> Some pth | _ -> acc)
      None inputs.events
  in

  let input_state = handle_file_drop filename !state in

  let fb, new_state =
    match (!state, event_state, input_state) with
    | Loading _, Loading _, Loading _ -> (update_loading_screen t s prev, !state)
    | Loading _, Showing data, _ -> (render_data t s data, event_state)
    | Loading _, Waiting msg, _ -> (initial_screen msg s, event_state)
    | Showing _, Showing data, Showing _ -> (render_data t s data, event_state)
    | _, _, Loading fn -> (loading_screen s fn, input_state)
    | Waiting a, _, Waiting b ->
        ((if a == b then prev else initial_screen b s), !state)
    | Showing _, _, Waiting msg -> (initial_screen msg s, input_state)
    | _ -> (prev, !state)
  in
  state := new_state;
  fb

let () =
  let worker = Domain.spawn (fun _ -> Worker.worker inputQ outputQ) in
  Palette.of_list
    (Palette.to_list (Palette.generate_mac_palette ())
    @ Palette.to_list (Palette.generate_plasma_palette (256 - 16)))
  |> Screen.create 1024 1024 1
  |> Base.run "Geoviz" (Some (initial_screen None)) tick;
  Domainslib.Chan.send inputQ Worker.Quit;
  Domain.join worker
