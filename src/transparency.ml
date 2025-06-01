open! Js_of_ocaml
open! Lwt.Syntax
open! StdLabels
open! ListLabels
open! MoreLabels

type t =
  {
    beta: float;
    low : float;
    high: float;
    max : float;
  } [@@deriving yojson]

let of_str str = Yojson.Safe.from_string str |> of_yojson |> Result.get_ok
let to_str entry = to_yojson entry |> Yojson.Safe.to_string

let default =
  {
    beta = 1.3;
    low = 3.0;
    high = 7.0;
    max = 0.9;
  }

let load () =
  let* value = Storage.load_key Storage.transparency_key in
  let value =
    match value with
    | Some value ->
      of_str value
    | None -> default
  in
  Lwt.return value

let save value =
  to_str value
  |> Storage.save_key Storage.transparency_key



let calculate t rating =
  let open Float in

  let clamp ~lower ~upper = function
    | v when v < lower -> lower
    | v when v > upper -> upper
    | v -> v
  in

  let ( / ) = div in
  let ( - ) = sub in

  (rating - t.low) / (t.high - t.low)
  |> clamp ~lower:0.0 ~upper:1.0
  |> fun v -> pow v t.beta
  |> mul t.max
  |> add (1.0 - t.max)
  |> sub 1.0


let update_transparency settings =
  Dom_html.document##querySelectorAll (Js.string ".movie.overlay")
  |> Dom.list_of_nodeList
  |> List.iter ~f:(fun el ->
    el##getAttribute (Js.string "imdb-rating")
    |> Js.Opt.to_option
    |> Option.map Js.to_string
    |> Option.map float_of_string
    |> function
    | Some rating ->
      let transparency = calculate settings rating in
      el##.style##setProperty
        (Js.string "--transparency")
        (Js.string (Printf.sprintf "%.1f" transparency)) Js.Optdef.empty
      |> ignore
    | None -> ()
  )

let listen () =
  let callback msg _sender _sendResponse =
    let action = (Js.Unsafe.coerce msg)##.action in
    Log.log `Info "Received callback.";
    try
      let settings = of_str (Js.to_string action) in
      Log.log `Info "New transparency: beta == %.5f" settings.beta;
      update_transparency settings
    with
    | _ -> Log.log `Info "Could not parse new settings"
  in
  Js.Unsafe.global##.chrome##.runtime##.onMessage##addListener callback |> ignore

let send_event t =
  let true_any = Js._true |> Js.Unsafe.coerce in
  let message = to_str t in
  let callback tabs =
    Log.log `Info "Found some tabs";
    match Js.Optdef.to_option (Js.array_get tabs 0) with
    | Some tab ->
        let tab_id = tab##.id in
        let message = Js.Unsafe.obj [| ("action", Js.string message |> Js.Unsafe.coerce) |] in
        Js.Unsafe.global##.chrome##.tabs##sendMessage tab_id message
    (* Maybe just use events on the chrome store *)
    | None -> Log.log `Info "No active tab found"
  in
  Js.Unsafe.global##.chrome##.tabs##query
    (Js.Unsafe.obj [| ("active", true_any); ("currentWindow", true_any) |])
    callback
  |> ignore
