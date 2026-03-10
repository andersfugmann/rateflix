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

  (** Helper: wrap a Chrome callback-based API call into an Lwt promise *)
  let wrap_callback f =
    let promise, resolver = Lwt.task () in
    f (Lwt.wakeup_later resolver);
    promise

  (** Set a string value in chrome.storage.local *)
  let set storage ~key ~value =
    let obj = Js.Unsafe.obj [| (key, Js.Unsafe.inject (Js.string value)) |] in
    wrap_callback (fun resolve ->
      storage##set obj (Js.wrap_callback (fun () -> resolve ())))

  (** Get a string value from chrome.storage.local *)
  let get storage key =
    wrap_callback (fun resolve ->
      storage##get
        (Js.Unsafe.inject (Js.string key))
        (Js.wrap_callback (fun result ->
           try
             let value = Js.Unsafe.get result key in
             match Js.Optdef.to_option value with
             | None -> resolve None
             | Some js_value ->
               let string_value = Js.to_string (Js.Unsafe.coerce js_value) in
               resolve (Some string_value)
           with _ ->
             resolve None)))

  (** Remove a key from chrome.storage.local *)
  let remove storage key =
    wrap_callback (fun resolve ->
      storage##remove
        (Js.Unsafe.inject (Js.string key))
        (Js.wrap_callback (fun () -> resolve ())))

  (** Clear all data from chrome.storage.local *)
  let clear storage =
    wrap_callback (fun resolve ->
      storage##clear (Js.wrap_callback (fun () -> resolve ())))

  let set_batch storage items =
    let obj_items = Array.of_list
        (List.map ~f:(fun (k, v) -> (k, Js.Unsafe.inject (Js.string v))) items) in
    let obj = Js.Unsafe.obj obj_items in
    wrap_callback (fun resolve ->
      storage##set obj (Js.wrap_callback (fun () -> resolve ())))

  (** Get all keys that match a prefix from chrome.storage.local *)
  let get_keys_by_prefix storage ~prefix =
    wrap_callback (fun resolve ->
      storage##get
        (Js.Unsafe.inject Js.null)
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
             |> resolve
           with _ ->
             resolve [])))

end
