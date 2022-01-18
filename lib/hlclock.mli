(** Hybrid Logical Clocks *)

type ms = int
type wallclock_fn = unit -> ms

type hlc_error_type =
  [ `Untrustworthy_remote_walltime
  | `Counter_bound_exceeded
  | `Invalid_timestamp ]

type hlc_error = hlc_error_type * string

exception Hlc_error of hlc_error

val unix_sec_to_ms : float -> ms
val ms_to_unix_sec : ms -> float
val std_wallclock : unit -> ms
val default_max_offset : int
val disable_max_offset : int

module type Timestamp = sig
  type t

  (* val min_timestamp : t val max_timestamp : t *)
  val walltime : t -> ms
  val walltime_unix : t -> float
  val to_pair : t -> int * int
  val of_pair : int * int -> (t, hlc_error) result

  val compare : t -> t -> int
  (** [compare t t'] is a total order on timestamps that is compatible with
      timeline order.*)

  val equal : t -> t -> bool
  (** [equal t t'] is true iff t and t' are the same timestamps.*)

  val is_earlier : t -> than:t -> bool
  (** [is_earlier t ~than] is true iff compare t than = -1.*)

  val is_later : t -> than:t -> bool
  (** [is_later t] than is true iff compare t than = 1.*)

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
  val pp : Format.formatter -> t -> unit [@@ocaml.toplevel_printer]
end

module Timestamp : Timestamp

type t

val init : ?wallclock:wallclock_fn -> ?max_offset:int -> unit -> t
val now : t -> Timestamp.t
val merge : t -> rt:Timestamp.t -> (Timestamp.t, hlc_error) result
val merge_exn : t -> rt:Timestamp.t -> Timestamp.t
val timestamp : t -> Timestamp.t
val last : t -> ms
val max_offset : t -> int
val to_string : t -> string
val pp : Format.formatter -> t -> unit [@@ocaml.toplevel_printer]
