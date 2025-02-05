let ids = Atomic.make 0

module IntMap = Map.Make (Int)

type 'm t = { id : string; to_json : 'm -> Yojson.Safe.t; cache : 'm option IntMap.t Atomic.t }

let make id (to_json : 'v -> Yojson.Safe.t) : 'v t = { id; to_json; cache = Atomic.make IntMap.empty }

let rec update (signal : 'v t) (value : 'v) : unit =
  let cache = Atomic.get signal.cache in
  let new_cache = IntMap.map (fun _ -> Some value) cache in
  if Atomic.compare_and_set signal.cache cache new_cache then () else update signal value

let get (signal : 'v t) ~(timeout : float) : 'v option =
  let rec update (signal : 'v t) f =
    let cache = Atomic.get signal.cache in
    let new_cache = f cache in
    if Atomic.compare_and_set signal.cache cache new_cache then () else update signal f
  in
  let client_id = Atomic.fetch_and_add ids 1 in
  update signal (IntMap.add client_id None);
  let rec loop time_left =
    if time_left <= 0.0 then None
    else
      match IntMap.find client_id (Atomic.get signal.cache) with
      | None ->
          Unix.sleepf 0.1;
          loop (time_left -. 0.1)
      | Some value -> Some value
  in
  let result = loop timeout in
  update signal (IntMap.remove client_id);
  result
