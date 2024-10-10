open Base
open Stdio
open Stdlib

let base62_chars = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

let encode_base62 n =
  let rec aux n acc =
    if n = 0 then acc
    else
      let r = n % 62 in
      let n = n / 62 in
      aux n (String.make 1 base62_chars.[r] ^ acc)
  in
  aux n ""

let random_bytes length =
  let bytes = Bytes.create length in
  for i = 0 to length - 1 do
    Bytes.set bytes i (Char.chr (Random.int 256))
  done;
  bytes

let counter = ref 0
let last_timestamp = ref 0

let ksuid ?node_id () =
  let timestamp = Int32.to_int (Int32.of_float (Unix.time ())) in
  if !last_timestamp <> timestamp then (
    last_timestamp := timestamp;
    counter := 0
  );
  if !counter >= 4096 then None
  else (
    let counter_str = encode_base62 !counter in
    Base.Int.incr counter;
    let random_part_length = match node_id with
      | Some _ -> 12
      | None -> 14
    in
    let random_part = random_bytes random_part_length in
    let node_id_str = match node_id with
      | Some id when id >= 1 && id <= 10000 -> encode_base62 id
      | Some _ -> failwith "node_id must be between 1 and 10000"
      | None -> ""
    in
    let timestamp_str = encode_base62 timestamp in
    let random_str = 
      Base.Bytes.to_string random_part
      |> Base.String.to_list 
      |> List.map ~f:(fun c -> encode_base62 (Base.Char.to_int c)) 
      |> String.concat in
    Some (timestamp_str ^ counter_str ^ node_id_str ^ random_str)
  )

let () =
  match ksuid ~node_id:1234 () with
  | Some id -> printf "Generated KSUID with node_id: %s\n" id
  | None -> printf "Too many IDs generated in one millisecond\n";
  
  match ksuid () with
  | Some id -> printf "Generated KSUID without node_id: %s\n" id
  | None -> printf "Too many IDs generated in one millisecond\n"
