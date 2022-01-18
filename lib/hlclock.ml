type ms = int
type wallclock_fn = unit -> ms

type hlc_error_type =
  [ `Untrustworthy_remote_walltime
  | `Counter_bound_exceeded
  | `Invalid_timestamp ]

type hlc_error = hlc_error_type * string

exception Hlc_error of hlc_error

let return v = Ok v
let error err fmt = Printf.kprintf (fun s -> Error (err, s)) fmt
let ( let* ) = Result.bind

let max_offset_error offset =
  error `Untrustworthy_remote_walltime
    "remote wall time is too far ahead (%d) to be trustworthy" offset

let invalid_timestamp_error s =
  error `Invalid_timestamp "invalid timestamp: %s" s

let ms_in_sec = 1_000.
let unix_sec_to_ms s : ms = s *. ms_in_sec |> Float.round |> Int.of_float
let ms_to_unix_sec (ms : ms) = Float.of_int ms /. ms_in_sec
let std_wallclock () : ms = Unix.gettimeofday () |> unix_sec_to_ms
let default_max_offset = 500
let disable_max_offset = 0

module type Timestamp = sig
  type t

  val walltime : t -> ms
  val walltime_unix : t -> float
  val to_pair : t -> int * int
  val of_pair : int * int -> (t, hlc_error) result
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val is_earlier : t -> than:t -> bool
  val is_later : t -> than:t -> bool
  val create : ms -> t
  val update : ms -> t -> t
  val merge : ms:ms -> rt:t -> t -> t

  module O : sig
    val ( >= ) : t -> t -> bool
    val ( <= ) : t -> t -> bool
    val ( = ) : t -> t -> bool
    val ( > ) : t -> t -> bool
    val ( < ) : t -> t -> bool
    val ( <> ) : t -> t -> bool
  end

  val min : t -> t -> t
  val max : t -> t -> t
  val to_string : t -> string
  val of_string : string -> (t, hlc_error) result
  val pp : Format.formatter -> t -> unit
end

module Timestamp : Timestamp = struct
  type counter = int
  type t = ms * counter

  let valid_tm ((ms, c) as t) =
    if ms >= 0 && c >= 0 then return t
    else invalid_timestamp_error (Printf.sprintf "%d/%d" ms c)

  let walltime (ms, _) = ms
  let walltime_unix (ms, _) = ms_to_unix_sec ms
  let to_pair (ms, c) = (ms, c)
  let of_pair (ms, c) = valid_tm (ms, c)

  let cmp_tm (x, _) (x', _) =
    if x = x' then `Eq else if x > x' then `Gt else `Lt

  let cmp_cnt (_, y) (_, y') =
    if y = y' then `Eq else if y > y' then `Gt else `Lt

  let compare t t' =
    match cmp_tm t t' with
    | `Eq -> ( match cmp_cnt t t' with `Eq -> 0 | `Lt -> -1 | `Gt -> 1)
    | `Lt -> -1
    | `Gt -> 1

  let equal t t' = compare t t' = 0
  let is_earlier t ~than:t' = compare t t' = -1
  let is_later t ~than:t' = compare t t' = 1
  let incr_cnt (ms, c) = (ms, Int.succ c)
  let create ms = (ms, 0)

  let update (ms : ms) (t : t) =
    let ms', _ = t in
    if ms <= ms' then incr_cnt t else create ms

  let merge_tm t rt =
    let tm =
      match cmp_tm t rt with
      | `Eq -> ( match cmp_cnt t rt with `Eq | `Gt -> t | `Lt -> rt)
      | `Lt -> rt
      | `Gt -> t
    in
    incr_cnt tm

  let merge ~ms ~rt t =
    let l = max (walltime t) (walltime rt) in
    if ms > l then create ms else merge_tm t rt

  module O = struct
    let ( >= ) t t' = equal t t' || is_later t ~than:t'
    let ( <= ) t t' = equal t t' || is_earlier t ~than:t'
    let ( = ) = equal
    let ( > ) t t' = is_later t ~than:t'
    let ( < ) t t' = is_earlier t ~than:t'
    let ( <> ) t t' = not (equal t t')
  end

  let min t t' = if t' < t then t' else t
  let max t t' = if t' > t then t' else t
  let to_string (ms, c) = Printf.sprintf "%d/%d" ms c

  let of_string s =
    let err () = invalid_timestamp_error s in
    let prd x y =
      match (x, y) with Some x, Some y -> Some (x, y) | _ -> None
    in
    let to_result e v = match v with None -> e () | Some x -> Ok x in
    let l = String.split_on_char '/' s in
    match l with
    | [ pt; c ] ->
        let pt' = int_of_string_opt pt and c' = int_of_string_opt c in
        let tm = prd pt' c' in
        let* tm' = to_result err tm in
        valid_tm tm'
    | _ :: _ | [] -> err ()

  let pp ppf t = Format.pp_print_string ppf (to_string t)
end

type t = {
  wallclock : wallclock_fn;
  max_offset : int;
  mutable last : ms;
  mutable timestamp : Timestamp.t;
}

let validate_rt_walltime pt rt max_offset =
  if max_offset > 0 then
    let offset = rt - pt in
    if offset < max_offset then return offset else max_offset_error offset
  else return 0

let init ?(wallclock = std_wallclock) ?(max_offset = disable_max_offset) () =
  let last = wallclock () in
  let timestamp = Timestamp.create last in
  { wallclock; max_offset; last; timestamp }

let now t =
  t.last <- t.wallclock ();
  t.timestamp <- Timestamp.update t.last t.timestamp;
  t.timestamp

let merge t ~rt =
  t.last <- t.wallclock ();
  let* _ = validate_rt_walltime t.last (Timestamp.walltime rt) t.max_offset in
  t.timestamp <- Timestamp.merge t.timestamp ~rt ~ms:t.last;
  return t.timestamp

let merge_exn t ~rt =
  match merge t ~rt with Error e -> raise (Hlc_error e) | Ok tm -> tm

let timestamp { timestamp; _ } = timestamp
let last { last; _ } = last
let max_offset { max_offset; _ } = max_offset

let to_string t =
  Printf.sprintf "{wallclock: unit -> ms; last: %d; timestamp: %s;}" t.last
    (Timestamp.to_string t.timestamp)

let pp ppf t = Format.pp_print_string ppf (to_string t)
