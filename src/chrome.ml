open Js_of_ocaml
open! StdLabels
open! MoreLabels

module Storage = struct
  (** Chrome storage area interface *)
  class type chrome_storage_area = object
    (** Get data from storage *)
    method get :
      Js.Unsafe.any ->                          (* Keys to get *)
      (Js.Unsafe.any -> unit) Js.callback ->    (* Callback with result *)
      unit Js.meth

    (** Set data in storage *)
    method set :
      Js.Unsafe.any ->                          (* Object with key-value pairs *)
      (unit -> unit) Js.callback ->             (* Callback *)
      unit Js.meth

    (** Remove data from storage *)
    method remove :
      Js.Unsafe.any ->                          (* Keys to remove *)
      (unit -> unit) Js.callback ->             (* Callback *)
      unit Js.meth

    (** Clear all data from storage *)
    method clear :
      (unit -> unit) Js.callback ->             (* Callback *)
      unit Js.meth

    (** Get all keys *)
    method getBytesInUse :
      Js.Unsafe.any ->                          (* Keys to check *)
      (Js.number Js.t -> unit) Js.callback ->   (* Callback with bytes used *)
      unit Js.meth
  end

  (** Chrome storage interface *)
  class type chrome_storage = object
    (** Local storage (device-specific) *)
    method local : chrome_storage_area Js.t Js.readonly_prop

    (** Sync storage (synced across devices where user is logged in) *)
    method sync : chrome_storage_area Js.t Js.readonly_prop

    (** Session storage (memory only, cleared when browser closes) *)
    method session : chrome_storage_area Js.t Js.readonly_prop

    (** Storage change event *)
    method onChanged : Js.Unsafe.any Js.t Js.readonly_prop
  end

  (** Chrome interface *)
  class type chrome = object
    method storage : chrome_storage Js.t Js.readonly_prop
  end

  (** Global chrome object *)
  let chrome : chrome Js.t Js.optdef =
    Js.Unsafe.global##.chrome

  (** Check if we're running in a Chrome extension context *)
  let is_extension_context () =
    Js.Optdef.test chrome

  (** Access the chrome.storage.local API safely *)
  let local_storage () =
    if is_extension_context () then
      Some (Js.Optdef.get chrome (fun () -> failwith "Chrome API not available"))##.storage##.local
    else
      None

  (** Access the chrome.storage.sync API safely *)
  let sync_storage () =
    if is_extension_context () then
      Some (Js.Optdef.get chrome (fun () -> failwith "Chrome API not available"))##.storage##.sync
    else
      None

  (** Access the chrome.storage.session API safely *)
  let session_storage () =
    if is_extension_context () then
      Some (Js.Optdef.get chrome (fun () -> failwith "Chrome API not available"))##.storage##.session
    else
      None

  (** Set a string value in chrome.storage.local *)
  let set storage ~key ~value =
    let result = Lwt_mvar.create_empty () in
    let obj = Js.Unsafe.obj [| (key, Js.Unsafe.inject (Js.string value)) |] in
    storage##set
      obj
      (Js.wrap_callback (fun () ->
         Lwt.ignore_result (Lwt_mvar.put result ())
       ));
    Lwt_mvar.take result

  (** Get a string value from chrome.storage.local *)
  let get storage key =
    let result = Lwt_mvar.create_empty () in
    let return v = Lwt.ignore_result (Lwt_mvar.put result v) in
    storage##get
      (Js.Unsafe.inject (Js.string key))
      (Js.wrap_callback (fun result ->
         try
           let value = Js.Unsafe.get result key in
           match Js.Optdef.to_option value with
           | None -> return None
           | Some js_value ->
             let string_value = Js.to_string (Js.Unsafe.coerce js_value) in
             return (Some string_value)
         with _ ->
           (** Maybe log an error here *)
           return None
       ));
    Lwt_mvar.take result

  (** Remove a key from chrome.storage.local *)
  let remove storage key =
    let result = Lwt_mvar.create_empty () in
    storage##remove
      (Js.Unsafe.inject (Js.string key))
      (Js.wrap_callback (fun () ->
         Lwt.ignore_result (Lwt_mvar.put result ())
       ));
    Lwt_mvar.take result

  (** Clear all data from chrome.storage.local *)
  let clear storage =
    let result = Lwt_mvar.create_empty () in
    storage##clear
      (Js.wrap_callback (fun () ->
         Lwt.ignore_result (Lwt_mvar.put result ())
       ));
    Lwt_mvar.take result

  let set_batch storage items =
    let result = Lwt_mvar.create_empty () in
    let obj_items = Array.of_list
        (List.map ~f:(fun (k, v) -> (k, Js.Unsafe.inject (Js.string v))) items) in
    let obj = Js.Unsafe.obj obj_items in
    storage##set
      obj
      (Js.wrap_callback (fun () ->
         Lwt.ignore_result (Lwt_mvar.put result ())
       ));
    Lwt_mvar.take result

  (** Get all keys that match a prefix from chrome.storage.local *)
  let get_keys_by_prefix storage ~prefix =
    let res = Lwt_mvar.create_empty () in
    storage##get
      (Js.Unsafe.inject Js.null)  (* null gets all items *)
      (Js.wrap_callback (fun result ->
         try
           let keys = Js.object_keys result in
           let len = keys##.length in
           let prefix_len = String.length prefix in
           List.init ~len ~f:(fun i -> Js.Unsafe.get keys i)
           |> List.map ~f:(fun key -> Js.to_string (Js.Unsafe.coerce key))
           |> List.filter ~f:(function
             | key when String.length key >= prefix_len ->
               String.sub key ~pos:0 ~len:prefix_len = prefix
             | _ -> false)
           |> (fun keys -> Lwt.ignore_result (Lwt_mvar.put res keys))
         with _ ->
           Lwt.ignore_result (Lwt_mvar.put res [])
       ));
    Lwt_mvar.take res

end
