open Wand

(* Lines read in blocks (#158). `Evaluator.block_lines` is the reader under
   `FS.stream_lines` and `Shell.stream`; `In_channel.input_line` is what the
   file streams used before it, so it is the reference: every input gives
   the same lines from both, however the reads cut it. *)

let with_file text f =
  let path = Filename.temp_file "wand_block_lines_" ".txt" in
  Out_channel.with_open_bin path (fun oc -> output_string oc text);
  Fun.protect ~finally:(fun () -> try Sys.remove path with _ -> ())
    (fun () -> f path)

let by_input_line text =
  with_file text (fun path ->
    In_channel.with_open_bin path (fun ic ->
      let rec go acc =
        match In_channel.input_line ic with
        | Some l -> go (l :: acc)
        | None -> List.rev acc
      in
      go []))

(* A source that gives at most [step] bytes for each read, as a pipe does
   when a writer is slow. *)
let by_blocks ~step text =
  let pos = ref 0 in
  let read b off len =
    let n = min (min step len) (String.length text - !pos) in
    Bytes.blit_string text !pos b off n;
    pos := !pos + n;
    n
  in
  let next = Evaluator.block_lines read in
  let rec go acc =
    match next () with
    | Some l -> go (l :: acc)
    | None -> List.rev acc
  in
  let lines = go [] in
  (* After the end, a pull still answers with the end. *)
  Alcotest.(check (option string)) "the end stays the end" None (next ());
  lines

let inputs = [
  "empty", "";
  "one line, no newline", "a";
  "one line", "a\n";
  "a blank last line", "a\n\n";
  "only a newline", "\n";
  "no newline at the end", "x\ny";
  "carriage returns stay", "a\r\nb\r\n\r\nc\r";
  "a line as long as a block", String.make 65536 'a' ^ "\nb\n";
  "a newline at the block edge", String.make 65535 'a' ^ "\n" ^ String.make 10 'b';
  "a line longer than a block",
    String.make 200_000 'x' ^ "\nshort\n" ^ String.make 70_000 'y';
  "many short lines",
    String.concat "\n" (List.init 50_000 string_of_int) ^ "\n";
]

let test_same_as_input_line () =
  List.iter (fun (label, text) ->
    let want = by_input_line text in
    List.iter (fun step ->
      Alcotest.(check (list string))
        (Printf.sprintf "%s, reads of %d" label step)
        want (by_blocks ~step text))
      [1; 7; 4096; 65536; max_int])
    inputs

(* The whole path: `FS.stream_lines` over a real file. *)
let test_stream_lines_reads_them_all () =
  let text = String.make 100_000 'z' ^ "\r\nb\n\nlast" in
  with_file text (fun path ->
    let src = Printf.sprintf {|uses {FS.Read}
import FS
import List
import Stream
import String
let lines = Stream.to_list (FS.stream_lines %s)
List.map String.length lines|} path in
    Alcotest.(check (result string string)) "the lengths of the lines"
      (Ok "[100001, 1, 0, 4]") (Runner.run_string src))

let open_fds () = Array.length (Sys.readdir "/dev/fd")

(* A stream that stops early closes its file. *)
let test_an_early_stop_closes_the_file () =
  let text = String.concat "\n" (List.init 100_000 string_of_int) in
  with_file text (fun path ->
    let src = Printf.sprintf {|uses {FS.Read}
import FS
import List
import Stream
List.map (fn _ -> Stream.to_list (Stream.take 2 (FS.stream_lines %s))) (List.range 0 50)
  |> List.head!|} path in
    let before = open_fds () in
    Alcotest.(check (result string string)) "the first two lines"
      (Ok {|["0", "1"]|}) (Runner.run_string src);
    Alcotest.(check int) "no file is left open" before (open_fds ()))

(* A subprocess's line comes out when it arrives: the command sleeps after
   its first line, and the stream stops on that line. A reader that waited
   for a full block would wait for the sleep. *)
let test_a_command_line_comes_when_it_arrives () =
  let src = {|uses {Shell(sh)}
import Shell
import Stream
Stream.to_list (Stream.take 1 (Shell.stream $*(sh -c "echo first; sleep 20; echo second")))|} in
  let t0 = Unix.gettimeofday () in
  Alcotest.(check (result string string)) "the first line"
    (Ok {|["first"]|}) (Runner.run_string src);
  let took = Unix.gettimeofday () -. t0 in
  if took > 10. then
    Alcotest.failf "the stream waited %.1fs for its first line" took

let () =
  Alcotest.run "block lines" [
    "the reader", [
      Alcotest.test_case "the same lines as input_line" `Quick
        test_same_as_input_line;
    ];
    "a stream", [
      Alcotest.test_case "stream_lines reads them all" `Quick
        test_stream_lines_reads_them_all;
      Alcotest.test_case "an early stop closes the file" `Quick
        test_an_early_stop_closes_the_file;
      Alcotest.test_case "a command's line comes when it arrives" `Slow
        test_a_command_line_comes_when_it_arrives;
    ];
  ]
