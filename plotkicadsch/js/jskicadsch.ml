open Js_of_ocaml
open Js_of_ocaml_lwt
module Html = Dom_html

let ( >>= ) = Lwt.bind

let redraw_funct = ref (fun () -> ())

(*
let need_redraw = ref false


let perform_redraw () =
  need_redraw := false ;
  Printf.printf "perform_redraw: calling redraw_func\n" ;
  !redraw_funct ()

let schedule_redraw () =
  if not !need_redraw then (
    Printf.printf "schedule_redraw: redraw_needed\n" ;
    need_redraw := true ;
    Html._requestAnimationFrame
      (Js.wrap_callback (fun () -> if !need_redraw then perform_redraw ())) )
*)
open Kicadsch.Sigs

let lib_content =
  {|EESchema-LIBRARY Version 2.4
#encoding utf-8
#
# Connector:Screw_Terminal_01x02
#
DEF Connector:Screw_Terminal_01x02 J 0 40 Y N 1 F N
F0 "J" 0 100 50 H V C CNN
F1 "Connector:Screw_Terminal_01x02" 0 -200 50 H V C CNN
F2 "" 0 0 50 H I C CNN
F3 "" 0 0 50 H I C CNN
$FPLIST
 TerminalBlock*:*
$ENDFPLIST
DRAW
C 0 -100 25 1 1 6 N
C 0 0 25 1 1 6 N
S -50 50 50 -150 1 1 10 f
P 2 1 1 6 -21 -87 13 -120 N
P 2 1 1 6 -21 13 13 -20 N
P 2 1 1 6 -14 -80 20 -113 N
P 2 1 1 6 -14 20 20 -13 N
X Pin_1 1 -200 0 150 R 50 50 1 1 P
X Pin_2 2 -200 -100 150 R 50 50 1 1 P
ENDDRAW
ENDDEF
#
# Device:LED
#
DEF Device:LED D 0 40 N N 1 F N
F0 "D" 0 100 50 H V C CNN
F1 "Device:LED" 0 -100 50 H V C CNN
F2 "" 0 0 50 H I C CNN
F3 "" 0 0 50 H I C CNN
$FPLIST
 LED*
 LED_SMD:*
 LED_THT:*
$ENDFPLIST
DRAW
P 2 0 1 8 -50 -50 -50 50 N
P 2 0 1 0 -50 0 50 0 N
P 4 0 1 8 50 -50 50 50 -50 0 50 -50 N
P 5 0 1 0 -120 -30 -180 -90 -150 -90 -180 -90 -180 -60 N
P 5 0 1 0 -70 -30 -130 -90 -100 -90 -130 -90 -130 -60 N
X K 1 -150 0 100 R 50 50 1 1 P
X A 2 150 0 100 L 50 50 1 1 P
ENDDRAW
ENDDEF
#
# Device:R
#
DEF Device:R R 0 0 N Y 1 F N
F0 "R" 80 0 50 V V C CNN
F1 "Device:R" 0 0 50 V V C CNN
F2 "" -70 0 50 V I C CNN
F3 "" 0 0 50 H I C CNN
$FPLIST
 R_*
$ENDFPLIST
DRAW
S -40 -100 40 100 0 1 10 N
X ~ 1 0 150 50 D 50 50 1 1 P
X ~ 2 0 -150 50 U 50 50 1 1 P
ENDDRAW
ENDDEF
#
# Timer:LM555
#
DEF Timer:LM555 U 0 20 Y Y 1 F N
F0 "U" -400 350 50 H V L CNN
F1 "Timer:LM555" 100 350 50 H V L CNN
F2 "" 0 0 50 H I C CNN
F3 "" 0 0 50 H I C CNN
ALIAS ICM7555 LMC555xM LMC555xMM LMC555xN MC1455 TLC555CD TLC555CP TLC555CPS NA555 NE555 SE555 SA555
$FPLIST
 SOIC*3.9x4.9mm*P1.27mm*
 DIP*W7.62mm*
 TSSOP*3x3mm*P0.65mm*
$ENDFPLIST
DRAW
S -350 -300 350 300 0 1 10 f
S -350 -300 350 300 0 1 10 f
X GND 1 0 -400 100 U 50 50 0 0 W
X VCC 8 0 400 100 D 50 50 0 0 W
X TR 2 -500 200 150 R 50 50 1 1 I
X Q 3 500 200 150 L 50 50 1 1 O
X R 4 -500 -200 150 R 50 50 1 1 I I
X CV 5 -500 0 150 R 50 50 1 1 I
X THR 6 500 -200 150 L 50 50 1 1 I
X DIS 7 500 0 150 L 50 50 1 1 I
ENDDRAW
ENDDEF
#
# power:+5V
#
DEF power:+5V #PWR 0 0 Y Y 1 F P
F0 "#PWR" 0 -150 50 H I C CNN
F1 "power:+5V" 0 140 50 H V C CNN
F2 "" 0 0 50 H I C CNN
F3 "" 0 0 50 H I C CNN
DRAW
P 2 0 1 0 -30 50 0 100 N
P 2 0 1 0 0 0 0 100 N
P 2 0 1 0 0 100 30 50 N
X +5V 1 0 0 0 U 50 50 1 1 W N
ENDDRAW
ENDDEF
#
# power:GND
#
DEF power:GND #PWR 0 0 Y Y 1 F P
F0 "#PWR" 0 -250 50 H I C CNN
F1 "power:GND" 0 -150 50 H V C CNN
F2 "" 0 0 50 H I C CNN
F3 "" 0 0 50 H I C CNN
DRAW
P 6 0 1 0 0 0 0 -50 50 -50 0 -100 -50 -50 0 -50 N
X GND 1 0 0 0 D 50 50 1 1 W N
ENDDRAW
ENDDEF
#
#End Library
|}

(*
let sch_content =
  {|EESchema Schematic File Version 4
EELAYER 26 0
EELAYER END
$Descr A4 11693 8268
encoding utf-8
Sheet 1 2
Title ""
Date ""
Rev ""
Comp ""
Comment1 ""
Comment2 ""
Comment3 ""
Comment4 ""
$EndDescr
$EndSCHEMATC
|}
  *)

let sch_content =
  {|EESchema Schematic File Version 4
EELAYER 26 0
EELAYER END
$Descr A4 11693 8268
encoding utf-8
Sheet 1 2
Title ""
Date ""
Rev ""
Comp ""
Comment1 ""
Comment2 ""
Comment3 ""
Comment4 ""
$EndDescr
$Comp
L Timer:LM555 U?
U 1 1 5BC794C1
P 3050 2000
F 0 "U?" H 2750 2350 50  0000 C CNN
F 1 "LM555" H 3300 2350 50  0000 C CNN
F 2 "" H 3050 2000 50  0001 C CNN
F 3 "http://www.ti.com/lit/ds/symlink/lm555.pdf" H 3050 2000 50  0001 C CNN
	1    3050 2000
	1    0    0    -1
$EndComp
$Comp
L Device:R R?
U 1 1 5BC79558
P 4000 1250
F 0 "R?" H 4070 1296 50  0000 L CNN
F 1 "220K" H 4070 1205 50  0000 L CNN
F 2 "" V 3930 1250 50  0001 C CNN
F 3 "~" H 4000 1250 50  0001 C CNN
	1    4000 1250
	1    0    0    -1
$EndComp
Wire Wire Line
	2550 2200 2350 2200
Wire Wire Line
	2350 2200 2350 1000
Wire Wire Line
	2350 1000 3050 1000
Wire Wire Line
	3050 1000 3050 1600
$Comp
L Device:R R?
U 1 1 5BC795B1
P 4000 2350
F 0 "R?" H 4070 2396 50  0000 L CNN
F 1 "22K" H 4070 2305 50  0000 L CNN
F 2 "" V 3930 2350 50  0001 C CNN
F 3 "~" H 4000 2350 50  0001 C CNN
	1    4000 2350
	1    0    0    -1
$EndComp
Wire Wire Line
	3550 2200 3550 2550
Wire Wire Line
	3550 2550 4000 2550
Wire Wire Line
	4000 2550 4000 2500
Wire Wire Line
	3550 2000 4000 2000
Wire Wire Line
	4000 2200 4000 2000
Connection ~ 4000 2000
Wire Wire Line
	4000 2000 4000 1400
Wire Wire Line
	4000 1000 4000 1100
Connection ~ 3050 1000
Connection ~ 3550 2550
Wire Wire Line
	3550 2550 3050 2550
Wire Wire Line
	3050 2550 3050 2400
Wire Wire Line
	3050 2550 2450 2550
Wire Wire Line
	2450 2550 2450 2300
Wire Wire Line
	2450 1800 2550 1800
Connection ~ 3050 2550
Wire Wire Line
	4450 1800 3550 1800
NoConn ~ 2550 2000
$Comp
L power:+5V #PWR?
U 1 1 5BC7C18E
P 2350 950
F 0 "#PWR?" H 2350 800 50  0001 C CNN
F 1 "+5V" H 2365 1123 50  0000 C CNN
F 2 "" H 2350 950 50  0001 C CNN
F 3 "" H 2350 950 50  0001 C CNN
	1    2350 950
	1    0    0    -1
$EndComp
Wire Wire Line
	2350 950  2350 1000
Connection ~ 2350 1000
$Comp
L power:GND #PWR?
U 1 1 5BC7C401
P 2450 2650
F 0 "#PWR?" H 2450 2400 50  0001 C CNN
F 1 "GND" H 2455 2477 50  0000 C CNN
F 2 "" H 2450 2650 50  0001 C CNN
F 3 "" H 2450 2650 50  0001 C CNN
	1    2450 2650
	1    0    0    -1
$EndComp
Wire Wire Line
	2450 2650 2450 2550
Connection ~ 2450 2550
$Sheet
S 4450 1700 500  400
U 5BC7CB77
F0 "LEDs" 50
F1 "LEDs.sch" 50
F2 "DRV" I L 4450 1800 50
$EndSheet
Wire Wire Line
	3050 1000 4000 1000
$Comp
L Connector:Screw_Terminal_01x02 J?
U 1 1 5BC7DEAC
P 1500 2200
F 0 "J?" H 1420 2417 50  0000 C CNN
F 1 "Screw_Terminal_01x02" H 1420 2326 50  0000 C CNN
F 2 "" H 1500 2200 50  0001 C CNN
F 3 "~" H 1500 2200 50  0001 C CNN
	1    1500 2200
	-1   0    0    -1
$EndComp
Wire Wire Line
	1700 2200 2350 2200
Connection ~ 2350 2200
Wire Wire Line
	1700 2300 2450 2300
Connection ~ 2450 2300
Wire Wire Line
	2450 2300 2450 1800
$EndSCHEMATC
|}

let str_of_kolor = function
  | `NoColor ->
      "#FFFFFF"
  | `Black ->
      "#000000"
  | `Green ->
      "#00FF00"
  | `Red ->
      "#FF0000"
  | `Blue ->
      "#0000FF"
  | `Brown ->
      "#800000"

let fillStyle_of_kolor k = Js.string (str_of_kolor k)

type canvas_coord = CanvasCoord of float * float

type prims =
  | Text of kolor * string * orientation * coord * size * justify * style
  | Line of kolor * size * coord * coord
  | Rect of kolor * kolor * coord * coord
  | Circle of kolor * kolor * coord * float
  | Arc of kolor * kolor * coord * coord * coord * float
  | Image of coord * float * Buffer.t
  | Format of coord

type listcanvas = prims list

let max_x, max_y = (ref 0., ref 0.)

module ListPainter = Kicadsch.MakeSchPainter (struct
    type t = listcanvas

    let paint_text ?(kolor = `Black) text (o : orientation) c s j stl ctx =
      Text (kolor, text, o, c, s, j, stl) :: ctx

    let paint_line ?(kolor = `Black) ?(width = Size 10) pt_start pt_end ctx =
      Line (kolor, width, pt_start, pt_end) :: ctx

    let paint_rect ?(kolor = `Black) ?(fill = `NoColor) pt dims ctx =
      Rect (kolor, fill, pt, dims) :: ctx

    let paint_circle ?(kolor = `Black) ?(fill = `NoColor) center radius ctx =
      Circle (kolor, fill, center, float radius) :: ctx

    let paint_arc ?(kolor = `Black) ?(fill = `NoColor) pt_center pt_start pt_stop
        radius ctx =
      Arc (kolor, fill, pt_center, pt_start, pt_stop, float radius) :: ctx

    let paint_image corner scale b c = Image (corner, scale, b) :: c

    let get_context () = []

    let set_canevas_size x y c =
      max_x := float x ;
      max_y := float y ;
      Format (Coord (x, y)) :: c
  end)

(*
module ZoomAndClipPainter (P : Kicadsch.Sigs.Painter) : Kicadsch.Sigs.Painter =
struct
  type t = P.t

  let paint_text ?(kolor = `Black) text o (Coord (x, y)) (Size s) j stl
      (ctx : t) =
    ctx

  let paint_line ?(kolor = `Black) ?(width = Size 3) (Coord (x1, y1))
      (Coord (x2, y2)) (ctx : t) =
    ctx

  let paint_rect ?(kolor = `Black) ?(fill = `NoColor) (Coord (x, y))
      (Coord (dim_x, dim_y)) ctx =
    ctx

  let paint_circle ?(kolor = `Black) ?(fill = `NoColor) (Coord (x, y)) radius
      (ctx : t) =
    ctx

  let paint_arc ?(kolor = `Black) ?(fill = `NoColor) (Coord (_x, _y))
      (Coord (x1, y1)) (Coord (x2, y2)) radius (ctx : t) =
    ctx

  let paint_image (Coord (x, y)) _scale (b : Buffer.t) (ctx : t) = ctx

  let get_context () = P.get_context ()

  let set_canevas_size = P.set_canevas_size
end
*)

module CanvasPainter = struct
  type t = Html.canvasElement Js.t

  let style_attr_of_style = function
    | Italic ->
      "italic"
    | Bold ->
      "bold"
    | BoldItalic ->
      "italic bold"
    | NoStyle ->
      ""

  let textAlign_of_justify j =
    let alignStr =
      match j with
      | J_left | J_top ->
        "start"
      | J_right | J_bottom ->
        "end"
      | J_center ->
        "center"
    in
    Js.string alignStr

  let paint_text ?(kolor = `Black) text o (CanvasCoord (x, y)) (Size s) j stl
      (ctx : t) =
    let c = ctx##getContext Html._2d_ in
    c##save ;
    c##.font :=
      Js.string @@ string_of_int s ^ "px Arial " ^ style_attr_of_style stl ;
    c##.fillStyle := fillStyle_of_kolor kolor ;
    c##.textAlign := textAlign_of_justify j ;
    c##translate x y ;
    if o = Orient_V then c##rotate (-3.14159265359 /. 2.) ;
    c##fillText (Js.string text) 0. 0. ;
    c##restore

  let paint_line ?(kolor = `Black) ?(width = Size 3) (CanvasCoord (x1, y1))
      (CanvasCoord (x2, y2)) (ctx : t) =
    let c = ctx##getContext Html._2d_ in
    let (Size lineWidth) = width in
    c##beginPath ;
    c##.strokeStyle := fillStyle_of_kolor kolor ;
    c##.fillStyle := fillStyle_of_kolor kolor ;
    c##.lineWidth := float_of_int lineWidth ;
    c##moveTo x1 y1 ;
    c##lineTo x2 y2 ;
    c##stroke

  let paint_rect ?(kolor = `Black) ?(fill = `NoColor) (CanvasCoord (x, y))
      (CanvasCoord (dim_x, dim_y)) ctx =
    let c = ctx##getContext Html._2d_ in
    c##beginPath ;
    c##.fillStyle := fillStyle_of_kolor fill ;
    c##.strokeStyle := fillStyle_of_kolor kolor ;
    c##strokeRect x y dim_x dim_y

  let paint_circle ?(kolor = `Black) ?(fill = `NoColor) (CanvasCoord (x, y))
      radius (ctx : t) =
    let c = ctx##getContext Html._2d_ in
    c##beginPath ;
    c##.fillStyle := fillStyle_of_kolor fill ;
    c##.strokeStyle := fillStyle_of_kolor kolor ;
    c##.lineWidth := 2. ;
    c##arc x y radius 0. (2. *. 3.14159265359) Js._true ;
    c##stroke

  let paint_arc ?(kolor = `Black) ?(fill = `NoColor) (CanvasCoord (_x, _y))
      (CanvasCoord (x1, y1)) (CanvasCoord (x2, y2)) radius (ctx : t) =
    let c = ctx##getContext Html._2d_ in
    c##.fillStyle := fillStyle_of_kolor fill ;
    c##.strokeStyle := fillStyle_of_kolor kolor ;
    c##arcTo x1 y1 x2 y2 radius ;
    c##stroke

  let paint_image (CanvasCoord (x, y)) _scale (b : Buffer.t) (ctx : t) =
    let c = ctx##getContext Html._2d_ in
    let img = Html.createImg Html.document in
    let urlCreator = Html.window##._URL in
    let blob = File.blob_from_string @@ Buffer.contents b in
    let imageURL = urlCreator##createObjectURL blob in
    img##.src := imageURL ;
    c##drawImage img x y

  let get_context () =
    let doc = Html.window##.document in
    Html.createCanvas doc

  let set_canevas_size x y _c =
    max_x := x ;
    max_y := y ;
    ()
end

let origin_x, origin_y, scale_c = ref 0., ref 0., ref 1.

let canvascoord x y =
  CanvasCoord (((float x) *. !scale_c) -. !origin_x, ((float y) *. !scale_c -. !origin_y))

let to_canvascoord (Coord (x, y)) =
  canvascoord x y


let pt_in_window w h (CanvasCoord (x, y)) = x > 0. && x < w && y > 0. && y < h

let code_point w h x y =
  (* heavily inspired by Cohen-Sutherland Line Clipping
     using coding of the location of both points w.r.t the window:
     1001 | 1000 | 1010
     -----+------+-----
     0001 | 0000 | 0010
     -----+------+-----
     0101 | 0100 | 0110 *)
  (if x < 0. then 0x1 else if x > w then 0x2 else 0x0)
  lor if y < 0. then 0x4 else if y > h then 0x8 else 0x0


let rec segment_in_window w h (CanvasCoord (x1, y1)) (CanvasCoord (x2, y2)) =
  let code1 = code_point w h x1 y1 in
  let code2 = code_point w h x2 y2 in
  if code1 == 0 || code2 == 0 then (
    (* one point in rectangle *)
    true )
  else if code1 land code2 != 0 then false (* both points on one side *)
  else
    let code = code1 lor code2 in
    (* points opposite *)
    if code == 3 (*horizontal*) || code == 12 (*vertical*) then (
      true )
    else
    if code == 5 || code == 6 || code == 9 || code == 10 then
      let side = if code land 1 == 1 then 0. else w in
      (* by construction x1!=x2
         compute the intersection with the relevant vertical axis *)
      let y = ((side -. x1) /. (x2 -. x1) *. (y2 -. y1)) +. y1 in
      (* side to side *)
      y >= 0. && y <= h
    else if code==15 (* diagonal *) then
      (* we're sure to cross the y axis, relaunch with the sub-segment *)
      let y = y1 -. ((x1) /. (x2 -. x1) *. (y2 -. y1)) in
      if x1 < 0. then segment_in_window w h (CanvasCoord (0., y)) (CanvasCoord (x2, y2)) else
        segment_in_window w h (CanvasCoord (x1, y1)) (CanvasCoord (0., y))
    else      (* knight move configuration *)
    if (lnot code land 12 != 0) (* horizontal *) then
      let side = if (code1 == 1 || code2 == 1) then 0. else w in
      let y = ((side -. x1) /. (x2 -. x1) *. (y2 -. y1)) +. y1 in
      y >= 0. && y <= h
    else
      let side = if (code1 == 4 || code2 == 4) then 0. else h in
      let x = ((side -. y1) /. (y2 -. y1) *. (x2 -. x1)) +. x1 in
      x >= 0. && x <= w

let in_window l =
  let doc = Html.window##.document in
  let page = doc##.documentElement in
  let w = float page##.clientWidth in
  let h = float page##.clientHeight in
  let rec in_win = function
    | [] ->
      false
    | [p] ->
      pt_in_window w h p
    | p1 :: p2 :: tl ->
      segment_in_window w h p1 p2 || in_win (p2 :: tl)
  in
  in_win l

let plot_elt out_ctx (arg : prims) =
  let module O = CanvasPainter in
  match arg with
  | Text (kolor, text, o, c, s, j, style) ->
    let Size old_size = s in
    let new_size = Size ( int_of_float ( (float_of_int old_size) *. !scale_c)) in
    O.paint_text ~kolor text o (to_canvascoord c) new_size j style out_ctx
  | Line (kolor, s, from_, to_) ->
    let f = to_canvascoord from_ in
    let t = to_canvascoord to_ in
    if in_window [f; t] then O.paint_line ~kolor ~width:s f t out_ctx
  | Rect (kolor, fill, (Coord (x, y) as c1), Coord (w, h)) ->
    let c = to_canvascoord c1 in
    if
      in_window
        [ c
        ; canvascoord x (y + h)
        ; canvascoord (x + w) (y + h)
        ; canvascoord (x + w) y
        ; c ]
    then
      let w' = (float w) *. !scale_c in
      let h' = (float h) *. !scale_c in
      O.paint_rect ~kolor ~fill c (CanvasCoord (w', h')) out_ctx
  | Circle (kolor, fill, (Coord (x, y) as center), radius) ->
    let r = Float.to_int radius in
    if
      in_window
        [ canvascoord (x - r) (y - r)
        ; canvascoord (x - r) (y + r)
        ; canvascoord (x + r) (y + r)
        ; canvascoord (x + r) (y - r)
        ; canvascoord (x - r) (y - r) ]
    then O.paint_circle ~kolor ~fill (to_canvascoord center) (radius *. !scale_c) out_ctx
  | Arc (kolor, fill, (Coord (x, y) as center), c1, c2, radius) ->
    let r = Float.to_int radius in
    if
      in_window
        [ canvascoord (x - r) (y - r)
        ; canvascoord (x - r) (y + r)
        ; canvascoord (x + r) (y + r)
        ; canvascoord (x + r) (y - r)
        ; canvascoord (x - r) (y - r) ]
    then
      O.paint_arc ~kolor ~fill (to_canvascoord center) (to_canvascoord c1)
        (to_canvascoord c2) (radius *. !scale_c) out_ctx
  | Image (corner, scale, data) ->
    (*todo: extract image coordinates *)
    O.paint_image (to_canvascoord corner) scale data out_ctx
  | Format (Coord (x, y)) ->
    O.set_canevas_size (float x) (float y) out_ctx

(*
let http_get url =
  XmlHttpRequest.get url
  >>= fun {XmlHttpRequest.code= cod; content= msg; _} ->
  if cod = 0 || cod = 200 then Lwt.return msg else fst (Lwt.wait ())

let getfile f =
  try Lwt.return (Sys_js.read_file ~name:f) with Not_found -> http_get f
*)

let mk_canvas () =
  let doc = Html.window##.document in
  let canv = Html.createCanvas doc in
  let page = doc##.documentElement in
  let w = page##.clientWidth in
  let h = page##.clientHeight in
  canv##.width := w ;
  canv##.height := h ;
  origin_x := 0. ;
  origin_y := 0. ;
  canv

let load_sch () =
  let init = ListPainter.initial_context () in
  let after_lib =
    lib_content |> String.split_on_char '\n'
    |> List.fold_left (fun i l -> ListPainter.add_lib l i) init
  in
  sch_content |> String.split_on_char '\n'
  |> List.fold_left (fun i l -> ListPainter.parse_line l i) after_lib
  |> ListPainter.output_context

let start _ =
  let canvasPrim = load_sch () in
  let doc = Html.window##.document in
  let canvas = mk_canvas () in
  (redraw_funct :=
     fun () ->
       Printf.printf "redraw_funct\n" ;
       let page = doc##.documentElement in
       let w = page##.clientWidth in
       let h = page##.clientHeight in
       canvas##.width := w ;
       canvas##.height := h ;
       List.iter (plot_elt canvas) canvasPrim) ;
  (* Lwt.async (fun _ ->
      Lwt_js_events.limited_loop ~elapsed_time:0.1
        (fun ?use_capture ->
          let _ = use_capture in
          Lwt_js_events.onresize )
        ()
        (fun _ _ ->
          let page = doc##.documentElement in
          let w = page##.clientWidth in
          let h = page##.clientHeight in
          if w <> canvas##.width || h <> canvas##.height then !redraw_funct () ;
          Lwt.return () ) ) ;
  *)
  Lwt.async (fun _ ->
      Lwt_js_events.onresizes (fun _ _ ->
          let page = doc##.documentElement in
          let w = page##.clientWidth in
          let h = page##.clientHeight in
          if w <> canvas##.width || h <> canvas##.height then !redraw_funct () ;
          Lwt.return () ) ) ;
  Lwt.async (fun _ ->
      Lwt_js_events.keydowns Html.document (fun ev _ ->
          Printf.printf "key event %d: %f,%f\n" ev##.keyCode !origin_x
            !origin_y ;
          Lwt.return
            ( match ev##.keyCode with
              | 37 ->
                (* left *)
                if !origin_x > 0. then (
                  origin_x := !origin_x -. 10. ;
                  !redraw_funct () )
              | 38 ->
                (* up *)
                if !origin_y > 0. then (
                  origin_y := !origin_y -. 10. ;
                  !redraw_funct () )
              | 39 ->
                (* right *)
                if !origin_x < !max_x -. float canvas##.width then (
                  origin_x := !origin_x +. 10. ;
                  !redraw_funct () )
              | 40 ->
                (* down *)
                if !origin_y < !max_y -. float canvas##.height then (
                  origin_y := !origin_y +. 10. ;
                  !redraw_funct () )
              | 107 ->
                (* + *)
                (
                  scale_c := !scale_c +. 0.1;
                  !redraw_funct()
                )
              | 109 ->
                (* - *)
                if !scale_c > 0.2 then (
                  scale_c := !scale_c -. 0.1;
                  !redraw_funct() )
              | _ ->
                () ) ) ) ;
  Dom.appendChild Html.window##.document##.body canvas ;
  !redraw_funct () ;
  Js._false

let _ = Html.window##.onload := Html.handler start
