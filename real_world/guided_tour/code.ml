(*example of pattern matching on result of function call*)
open Base
let downcase_extension filename =
  match String.rsplit2 filename ~on:'.' with
  | None -> filename
  | Some (base,ext) ->
    base ^ "." ^ String.lowercase ext
(*call like this 
 * map downcase_extension ["ABC.TXT";"abc.Txt"];;*)
let map f l = 
  List.map ~f:f l

type point2d = {x:float;y:float}

let magnitude {x=x_pos; y=y_pos} =
  Float.sqrt(x_pos **. 2. +. y_pos **. 2.)

let distance v1 v2 =
  magnitude { x = v1.x -. v2.x; y = v1.y -. v2.y }
(*pattern match with records*)

type circle_desc  = { center: point2d; radius: float }

type rect_desc    = { lower_left: point2d; width: float; height: float }

type segment_desc = { endpoint1: point2d; endpoint2: point2d }

type scene_element =
  | Circle  of circle_desc
  | Rect    of rect_desc
  | Segment of segment_desc

(*Base.Float so i can do float comparisons like 1.1 < 2.2 *
  open with a let also*)
let is_inside_scene_element point scene_element =
  let open Base.Float in
  match scene_element with
  | Circle { center; radius } ->
    distance center point < radius
  | Rect { lower_left; width; height } ->
    point.x    > lower_left.x && point.x < lower_left.x + width
    && point.y > lower_left.y && point.y < lower_left.y + height
  | Segment { endpoint1; endpoint2 } -> false


let is_inside_scene point scene =
  List.exists scene
    ~f:(fun el -> is_inside_scene_element point el)

    (*
    is_inside_scene {x=3.;y=7.}
    [ Circle {center = {x=4.;y= 4.}; radius = 0.5 } ]
;;

is_inside_scene {x=3.;y=7.}
    [ Circle {center = {x=4.;y= 4.}; radius = 5.0 } ]
;;*)

open Stdio
(*can run this in utop, enter numbers then, on the empty line do CTRL-D*)
let rec read_and_accumulate accum =
  let line = In_channel.input_line In_channel.stdin in
  match line with
  | None -> accum
  | Some x -> read_and_accumulate (accum +. Float.of_string x)

let read_floats () =
  printf "Total: %F\n" (read_and_accumulate 0.)
