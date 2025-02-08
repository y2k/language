let ids = Atomic.make 0

module IntMap = Map.Make (Int)

let global_cache : Yojson.Safe.t option IntMap.t Atomic.t = Atomic.make IntMap.empty

let rec dispatch (msg : Yojson.Safe.t) : unit =
  let old_cache = Atomic.get global_cache in
  let new_cache = IntMap.map (fun _ -> Some msg) old_cache in
  if Atomic.compare_and_set global_cache old_cache new_cache then () else dispatch msg

let get (decode : Yojson.Safe.t -> ('v, _) result) ~(timeout : float) : 'v option =
  let rec update f =
    let cache = Atomic.get global_cache in
    let new_cache = f cache in
    if Atomic.compare_and_set global_cache cache new_cache then () else update f
  in
  let client_id = Atomic.fetch_and_add ids 1 in
  update (IntMap.add client_id None);
  let rec loop time_left =
    if time_left <= 0.0 then None
    else
      let x1 = IntMap.find client_id (Atomic.get global_cache) in
      let x2 = Option.map decode x1 in
      match x2 with
      | Some (Ok value) -> Some value
      | _ ->
          Unix.sleepf 0.1;
          loop (time_left -. 0.1)
  in
  let result = loop timeout in
  update (IntMap.remove client_id);
  result
