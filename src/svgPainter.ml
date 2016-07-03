
open Tyxml_svg
open Svg_types


type orientation = Orient_H | Orient_V
type coord = Coord of (int*int)
type size = Size of int
type justify = J_left | J_right | J_center | J_bottom | J_top
type style = Bold | Italic | BoldItalic | NoStyle
type kolor = NoColor | Black | Green | Red

type content = [ `Polyline | `Text | `Svg | `Rect | `Circle ]
type t =  content elt list

let style_attr_of_style = function
  | Italic -> [a_font_style "italic"]
  | Bold -> [a_font_weight "bold"]
  | BoldItalic -> [a_font_style "italic"; a_font_weight "bold"]
  | NoStyle -> []

let anchor_attr_of_justify justif =
      a_text_anchor (match justif with
      | J_left -> `Start
      | J_center -> `Middle
      | J_right -> `End
      | J_bottom -> `End
      | J_top -> `Start)

let color_of_kolor k =
  let cstring = match k with
  | NoColor -> "none"
  | Black -> "#000000"
  | Red -> "#FF0000"
  | Green -> "#00FF00"
  in `Color (cstring, None)

(** SVG coord type conversion from int **)
let coord_of_int x = float_of_int x, None

let svg_text ?(kolor=Black) t (o:orientation) (Coord (x,y)) (Size size) justif styl c =
  let size_in = Printf.sprintf "%f"  (float_of_int size) and
      j = anchor_attr_of_justify justif and
      s = style_attr_of_style styl and
      x_c = float_of_int x and
      y_c = float_of_int y and
      angle = match o with
      | Orient_H -> 0.
      | Orient_V -> (-90.) in
  let orient = (angle,None), Some(x_c,y_c)
  in
  let color = color_of_kolor kolor in
  (text ~a:([a_x_list [coord_of_int x] ; a_y_list [coord_of_int y] ; a_font_size size_in; j; a_transform[`Rotate orient]; a_fill color]@s) [pcdata t]) :: c

let svg_line (Coord (x1, y1)) (Coord (x2, y2)) c =
  let x1_in = float_of_int x1 in
  let y1_in = float_of_int y1 in
  let x2_in = float_of_int x2 in
  let y2_in = float_of_int y2 in
  (polyline ~a:([a_points [(x1_in, y1_in); (x2_in, y2_in) ]; a_stroke_width (1., Some `Px); a_stroke ( color_of_kolor Black) ]) []) :: c

let svg_rect ?(fill=NoColor) (Coord(x, y)) (Coord (dim_x, dim_y)) c =
  (rect ~a:[ a_x (coord_of_int x); a_y (coord_of_int y); a_width (coord_of_int dim_x); a_height (coord_of_int dim_y);a_fill (color_of_kolor fill); a_stroke_width (1., Some `Px); a_stroke (color_of_kolor Black)] []) :: c

let svg_circle ?(fill=NoColor) (Coord(x, y)) radius c =
  (circle ~a:[a_r (coord_of_int radius); a_cx (coord_of_int x); a_cy (coord_of_int y); a_fill (color_of_kolor fill); a_stroke_width (1., Some `Px); a_stroke (color_of_kolor Black) ] []) :: c

let svg_get_context () = []

let svg_write oc c =
  let svg_doc = svg  ~a:[a_width (29.7, Some `Cm); a_height (21., Some `Cm); a_viewBox (0.,0., 11693., 8268.)] c in
  let fmt = Format.formatter_of_out_channel oc in
  Tyxml.Svg.pp () fmt svg_doc
