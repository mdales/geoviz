open Geoviz
open Geoviz.Graphics

type work_message = Task of string | Quit
type 'a result_message =  Result of 'a | Error of string


let radius = 60.

let pi = acos (-1.)
let deg_to_radians x = x /. 180. *. pi

let coord_to_vec (coord : Feature.coord) =
  let lat = deg_to_radians coord.latitude
  and lng = deg_to_radians coord.longitude in
  {
	x = radius *. cos lat *. cos lng;
	y = radius *. sin lng *. cos lat;
	z = radius *. sin lat;
  }
  |> rotate_x (pi *. 0.5)

let h3_lat_lng_to_vec (coord : H3.lat_lng) =
  let lat = coord.lat and lng = coord.lng in
  {
	x = radius *. cos lat *. cos lng;
	y = radius *. sin lng *. cos lat;
	z = radius *. sin lat;
  }
  |> rotate_x (pi *. 0.5)

let load_data_from_geojson filename =
  let features = Geojson.of_file filename |> Geojson.features in
  List.concat_map
	(fun feat ->
	  match Feature.geometry feat with
	  | Point coord -> [ (Point (coord_to_vec coord), 1.0) ]
	  | MultiLineString lines ->
		  List.concat_map
			(fun coordinate_list ->
			  match coordinate_list with
			  | [] | _ :: [] -> []
			  | hd1 :: hd2 :: tl ->
				  let rec loop last next rest acc =
					let n =
					  (Line (coord_to_vec last, coord_to_vec next), 1.0) :: acc
					in
					match rest with [] -> n | hd :: tl -> loop next hd tl n
				  in
				  loop hd1 hd2 tl [])
			lines
	  | Polygon coordinate_list_list -> (
		  (* GeoJSON polygons are lists of polygons, with the first being the outer and the rest being holes.
			 Claudius doesn't model those, so we just process the first one and ignore the others for now.
		  *)
		  match coordinate_list_list with
		  | coordinate_list :: _ ->
			  [ (Polygon (List.map coord_to_vec coordinate_list), 1.0) ]
		  | _ -> [])
	  | MultiPolygon coordinate_list_list_list -> (
		List.concat_map ( fun coordinate_list_list ->
		  match coordinate_list_list with
		  | coordinate_list :: _ ->
			  [
				( Polygon
					(List.map coord_to_vec ( coordinate_list)),
				  1.0 );
			  ]
		  | _ -> []
		) coordinate_list_list_list)
	  | _ -> [])
	features

let load_data_from_csv filename =
  In_channel.with_open_text filename (fun inc ->
	  let csv_inc = Csv.of_channel inc in
	  let max_val =
		Csv.fold_left
		  ~f:(fun acc row ->
			match row with
			| [ _cellid; value ] ->
				let fvalue = Float.of_string value in
				if fvalue > acc then fvalue else acc
			| _ -> acc)
		  ~init:0.0 csv_inc
	  in
	  Printf.printf "%f\n" max_val;
	  In_channel.seek inc 0L;
	  let csv_inc = Csv.of_channel inc in
	  Csv.fold_left
		~f:(fun acc row ->
		  match row with
		  | [ cellid; value ] -> (
			  let cell = H3.string_to_h3 cellid in
			  let boundary = Array.to_list (H3.cell_to_boundary cell) in
			  match boundary with
			  | [] -> acc
			  | _ ->
				  ( Polygon (List.map h3_lat_lng_to_vec boundary),
					Float.of_string value /. max_val )
				  :: acc)
		  | _ -> failwith "unable to parse CSV row")
		~init:[] csv_inc)

let load_data_from_geotiff filename =
  Tiff_unix.with_open_in filename @@ fun ro ->
	let tiff = Tiff.from_file ro in
	let ifd = Tiff.ifd tiff in
	let tiepoint = Tiff.Ifd.tiepoint ifd in
	Array.iter (fun  f -> Printf.printf "%f\n" f) tiepoint;
	let xorigin = tiepoint.(3)
	and yorigin = tiepoint.(4) in
	let pixel_scale = Tiff.Ifd.pixel_scale ifd in
	  Array.iter (fun  f -> Printf.printf "%f\n" f) pixel_scale;
	let xscale = pixel_scale.(0) in
	let yscale = pixel_scale.(1) in
	let res = ref [] in
	let data = Tiff.data ~plane:0 tiff ro Tiff.Data.Int16 in
	let data2d = Bigarray.array2_of_genarray data in
	let scale = 50 in
	for y = 0 to (((Bigarray.Array2.dim1 data2d) - 1) / scale) do
	  for x = 0 to (((Bigarray.Array2.dim2 data2d) - 1) / scale) do
		let value = data2d.{y * scale, x * scale} in
		let longitude = xorigin +. (xscale *. (float_of_int (x * scale))) in
		let latitude = yorigin +. (-1. *. yscale *. (float_of_int (y * scale))) in
		let coord: Feature.coord = {longitude; latitude} in
		let p = Point (coord_to_vec coord), if value == 0 then 0. else 0.5 +. (float_of_int value /. 12000.) in
		  res := p :: !res
	  done
	done;
	!res

let load_data_from_file filename =
  match Filename.extension filename with
  | ".geojson" -> load_data_from_geojson filename
  | ".csv" -> load_data_from_csv filename
  | ".tif" | ".tiff" -> load_data_from_geotiff filename
  | _ -> invalid_arg (Printf.sprintf "Unrecognised file extension on %s" filename)

let rec worker inputQ outputQ =
	match Domainslib.Chan.recv inputQ with
	| Task filename -> (
		(try
			let data = load_data_from_file filename in
			Domainslib.Chan.send outputQ (Result data);
		with
		| Invalid_argument _ -> Domainslib.Chan.send outputQ (Error "bpp"));
		worker inputQ outputQ
	)
	| Quit -> ()
