module C = Hlclock
module T = C.Timestamp

let max_offset = 250
let min_offset = max_offset / 2
let tm_bound = int_of_float @@ (2. ** 29.)
let offset_range = [ -max_offset; -min_offset; 0; min_offset; max_offset ]
let tm_range = List.init 9 (fun i -> tm_bound * i)

let calc_pt pt off =
  if off >= 0 then pt + off else if pt > 0 then pt - abs off else pt

let ptime_g =
  let open QCheck.Gen in
  let* pt = oneofl tm_range in
  let* off = oneofl offset_range in
  let pt' = calc_pt pt off in
  return (pt, pt', off)

let pp_ptime (pt, pt', off) =
  Printf.sprintf "(pt: %d, pt': %d, offset: %d)" pt pt' off

let ptime = QCheck.make ~print:pp_ptime ptime_g

let ptimel_g i =
  let open QCheck.Gen in
  list_size i ptime_g

let pp_ptimel = QCheck.Print.list pp_ptime
let ptimel i = QCheck.make ~print:pp_ptimel (ptimel_g i)

let ltime_g =
  let open QCheck.Gen in
  let create_ltm pt c =
    match T.of_pair (pt, c) with
    | Ok e' -> e'
    | Error _ -> raise (Invalid_argument (Printf.sprintf "(%d, %d)" pt c))
  in
  let* ((_pt, pt', _off) as t) = ptime_g in
  let* c = 0 -- 10 in
  return (create_ltm pt' c, t)

let pp_ltime = QCheck.Print.pair T.to_string pp_ptime
let ltime = QCheck.make ~print:pp_ltime ltime_g

let prop_e_hb_f e f =
  let lf, cf = T.to_pair f in
  let le, ce = T.to_pair e in
  (if lf = le then ce < cf else le < lf)
  && T.O.(e < f && f > e && T.max f e = f && T.min f e = e)

let prop_gt_pt pt e f =
  let lf = T.walltime f in
  let le = T.walltime e in
  QCheck.(lf > pt ==> (lf = le))

let prop_geq_pt pt f = T.walltime f >= pt

let receive_model =
  let open QCheck in
  let mk n fn =
    Test.make ~count:1000 ~name:n (pair ltime ltime)
    @@ fun ((f, (pt, _, _)), (e, _pte)) ->
    let f' = T.merge ~ms:pt ~rt:e f in
    let pt' = max pt (T.walltime f) in
    fn pt' e f'
  in
  let receive_e_hb_f =
    mk "receive event e hb f" @@ fun _ e f -> prop_e_hb_f e f
  in
  let receive_gt_pt = mk "receive event gt pt" prop_gt_pt in
  let receive_geq_pt =
    mk "receive event geq pt" @@ fun pt _e f -> prop_geq_pt pt f
  in
  [ receive_e_hb_f; receive_gt_pt; receive_geq_pt ]

let send_model =
  let open QCheck in
  let mk n fn =
    Test.make ~count:1000 ~name:n ltime @@ fun (e, (pt, _, _)) ->
    let f = T.update pt e in
    fn pt e f
  in
  let send_e_hb_f = mk "send event e hb f" @@ fun _pt e f -> prop_e_hb_f e f in
  let send_gt_pt = mk "send event gt pt" prop_gt_pt in
  let send_geq_pt = mk "send event geq pt" @@ fun pt _e f -> prop_geq_pt pt f in
  [ send_e_hb_f; send_gt_pt; send_geq_pt ]

let suite = List.map QCheck_alcotest.to_alcotest @@ receive_model @ send_model
let () = Alcotest.run "hlclock" [ ("Hlclock Timestamp spec", suite) ]
