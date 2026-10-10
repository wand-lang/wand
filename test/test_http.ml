(* What a manifest admits is a property of a file, not of a call, so these
   run whole files through the real binary. The double tests -- which need
   no manifest, because a sealed file reaches nothing -- are in
   `test/wand/test_http.wand`. *)

let wand_binary =
  let dir = Filename.dirname (Filename.dirname Sys.executable_name) in
  Filename.concat (Filename.concat dir "bin") "wand.exe"

let contains haystack needle =
  let hn = String.length haystack and nn = String.length needle in
  let rec go i = i + nn <= hn && (String.sub haystack i nn = needle || go (i + 1)) in
  go 0

let run_source src =
  let path = Filename.temp_file "wand_http_" ".wand" in
  let oc = open_out path in
  output_string oc src; close_out oc;
  let cmd =
    String.concat " " (List.map Filename.quote [wand_binary; path]) ^ " 2>&1"
  in
  let ic = Unix.open_process_in cmd in
  let out = In_channel.input_all ic in
  ignore (Unix.close_process_in ic);
  (try Sys.remove path with Sys_error _ -> ());
  out

(* Nothing here reaches a host: a request that the manifest refuses is
   refused before anything is sent, and one it admits is never sent because
   these files stop at building it. *)
let building url manifest =
  Printf.sprintf
    {|uses {IO, %s}
import IO
let r = HTTPRequest(url = %s)
let () = IO.println r.url|} manifest url

let refuses label ~manifest ~url ~host =
  let out = run_source (building url manifest) in
  if not (contains out "does not allow") then
    Alcotest.failf "%s: expected a refusal, got: %s" label out;
  if not (contains out host) then
    Alcotest.failf "%s: the message does not name the host: %s" label out

let admits label ~manifest ~url =
  let out = run_source (building url manifest) in
  if contains out "does not allow" then
    Alcotest.failf "%s: expected it to be admitted, got: %s" label out

(* The host as written is what the manifest allows. *)
let test_a_named_host_is_admitted () =
  admits "the named host" ~manifest:"Net(api.example.com)"
    ~url:"https://api.example.com/x"

let test_an_unnamed_host_is_refused () =
  refuses "an unnamed host" ~manifest:"Net(api.example.com)"
    ~url:"https://evil.test/x" ~host:"evil.test"

(* A pattern covers one label below the domain and not the domain itself,
   which is what a TLS certificate does with the same spelling. *)
let test_a_pattern_covers_one_level () =
  admits "one level below" ~manifest:"Net(*.example.com)"
    ~url:"https://api.example.com/x";
  refuses "two levels below" ~manifest:"Net(*.example.com)"
    ~url:"https://a.b.example.com/x" ~host:"a.b.example.com";
  refuses "the bare domain" ~manifest:"Net(*.example.com)"
    ~url:"https://example.com/x" ~host:"example.com"

(* Bare `Net` admits any host, which is the same reading a bare `Shell`
   gives. *)
let test_bare_net_admits_anything () =
  admits "bare Net" ~manifest:"Net" ~url:"https://anywhere.test/x"

(* A port is not part of the host, and neither are credentials. *)
let test_a_port_is_not_part_of_the_host () =
  admits "a port" ~manifest:"Net(api.example.com)"
    ~url:"https://api.example.com:8443/x"

(* An update may name a different host, so it is checked like a
   construction. *)
let test_an_update_is_checked () =
  let out = run_source
    {|uses {IO, Net(api.example.com)}
import IO
let base = HTTPRequest(url = https://api.example.com/x)
let r = HTTPRequest(base, url = https://evil.test/y)
let () = IO.println r.url|}
  in
  if not (contains out "does not allow") then
    Alcotest.failf "an update to an unnamed host was admitted: %s" out

(* `HTTP.get` builds its request inside the standard library, so the bound
   cannot come from the construction. It comes from the URL, which is the
   part the caller wrote -- without that, the manifest bounded nothing on
   the module's commonest path. *)
let test_the_convenience_functions_are_bounded () =
  let out = run_source
    {|uses {IO, Net(api.example.com)}
import HTTP
import IO
let r = HTTP.get! https://evil.test/x
let () = IO.println r.body|}
  in
  if not (contains out "does not allow") then
    Alcotest.failf "HTTP.get reached an unnamed host: %s" out;
  if not (contains out "evil.test") then
    Alcotest.failf "the message does not name the host: %s" out

(* A rehearsal follows the filesystem rule, and the protocol already draws
   the line: GET and HEAD are defined not to change anything. *)
let rehearse src =
  let path = Filename.temp_file "wand_http_dry_" ".wand" in
  let oc = open_out path in
  output_string oc src; close_out oc;
  let cmd =
    String.concat " " (List.map Filename.quote [wand_binary; "--dry-run"; path])
    ^ " 2>&1"
  in
  let ic = Unix.open_process_in cmd in
  let out = In_channel.input_all ic in
  ignore (Unix.close_process_in ic);
  (try Sys.remove path with Sys_error _ -> ());
  out

let test_a_rehearsal_withholds_an_unsafe_method () =
  let out = rehearse
    {|uses {IO, Net(api.example.com)}
import HTTP
import IO
let r = HTTP.post! https://api.example.com/deploy "{}"
let () = IO.println r.status|}
  in
  if not (contains out "would post") then
    Alcotest.failf "the rehearsal did not withhold the post:\n%s" out;
  (* Withheld, so it answers rather than reaching anything: 202 is what a
     server that took it and said nothing would say. *)
  if not (contains out "202") then
    Alcotest.failf "the rehearsal did not answer the withheld request:\n%s" out

(* A URL the run computed carries no literal to take a bound from, and the
   request `HTTP.get` builds is built inside the standard library, where the
   manifest is the standard library's. So the bound of the file that is
   running answers for it, checked at the send -- or a narrowed `Net` bounds
   only the hosts a file happened to write down. *)
let computed_url_source manifest text =
  Printf.sprintf
    {|uses {IO, Raise, %s}
import HTTP
import IO
import String
import URL
let () =
  match URL.of_string "%s" with
  | Ok u -> IO.println "%%{try HTTP.get u}"
  | Error e -> IO.println e|} manifest text

let test_a_computed_host_is_refused () =
  let out = run_source
    (computed_url_source "Net(api.example.com)" "https://evil.test/x") in
  if not (contains out "does not allow") then
    Alcotest.failf "a computed host reached past the manifest:\n%s" out;
  if not (contains out "evil.test") then
    Alcotest.failf "the message does not name the host:\n%s" out

(* Bare `Net` bounds nothing, so the same file with no narrowing is admitted
   and fails at the transport instead. *)
let test_a_computed_host_under_bare_net_is_admitted () =
  let out = run_source (computed_url_source "Net" "https://api.example.com/x") in
  if contains out "does not allow" then
    Alcotest.failf "bare Net refused a host:\n%s" out

(* A URL the run built from another URL keeps that URL's bound. *)
let test_a_rebuilt_url_keeps_its_bound () =
  let out = run_source
    {|uses {IO, Net(api.example.com)}
import HTTP
import IO
import URL
let () =
  match URL.with_hostname "evil.test" https://api.example.com/x with
  | Ok u -> IO.println "%{try HTTP.get u}"
  | Error e -> IO.println e|}
  in
  if not (contains out "does not allow") then
    Alcotest.failf "a rebuilt URL reached past the manifest:\n%s" out

(* The origin a header belongs to: scheme, host and port, without user
   info. *)
let test_origins () =
  let same a b = Alcotest.(check bool) (a ^ " ~ " ^ b) true
      (Wand.Runner.origin_of a = Wand.Runner.origin_of b) in
  let differ a b = Alcotest.(check bool) (a ^ " !~ " ^ b) false
      (Wand.Runner.origin_of a = Wand.Runner.origin_of b) in
  same "https://api.example.com/a" "https://api.example.com/b?c#d";
  same "https://API.example.com/a" "https://api.example.com";
  same "https://u:p@api.example.com/a" "https://api.example.com/b";
  differ "https://api.example.com/a" "https://other.example.com/a";
  differ "https://api.example.com/a" "http://api.example.com/a";
  differ "https://api.example.com/a" "https://api.example.com:8443/a"

let test_credentials_stay_with_their_origin () =
  let headers = [("Authorization", "Bearer t"); ("cookie", "s=1");
                 ("Proxy-Authorization", "p"); ("Accept", "a")] in
  let names hs = List.map fst hs in
  Alcotest.(check (list string)) "the same origin keeps them" (names headers)
    (names (Wand.Runner.headers_for_hop ~from:"https://a.test/x"
              ~next:"https://a.test/y" headers));
  Alcotest.(check (list string)) "another origin gets the rest" ["Accept"]
    (names (Wand.Runner.headers_for_hop ~from:"https://a.test/x"
              ~next:"https://b.test/y" headers))

(* End to end: a server that redirects to a second one, on another port and
   so another origin, and the second writes down the headers it was sent. *)
let serve_once ~reply =
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt sock Unix.SO_REUSEADDR true;
  Unix.bind sock (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
  Unix.listen sock 1;
  let port = match Unix.getsockname sock with
    | Unix.ADDR_INET (_, p) -> p | _ -> assert false in
  let request_file = Filename.temp_file "wand_http_req_" ".txt" in
  match Unix.fork () with
  | 0 ->
    let (c, _) = Unix.accept sock in
    let buf = Buffer.create 512 and chunk = Bytes.create 512 in
    let rec read () =
      let n = Unix.read c chunk 0 512 in
      Buffer.add_subbytes buf chunk 0 n;
      let text = Buffer.contents buf in
      let ends = String.length text >= 4
                 && (let rec find i = i + 4 <= String.length text
                       && (String.sub text i 4 = "\r\n\r\n" || find (i + 1)) in find 0) in
      if n > 0 && not ends then read ()
    in
    read ();
    Out_channel.with_open_bin request_file (fun oc ->
      Out_channel.output_string oc (Buffer.contents buf));
    let r = reply () in
    ignore (Unix.write_substring c r 0 (String.length r));
    Unix.close c;
    Unix._exit 0
  | pid -> Unix.close sock; (port, pid, request_file)

let test_a_redirect_to_another_origin_drops_credentials () =
  let (b_port, b_pid, b_req) =
    serve_once ~reply:(fun () -> "HTTP/1.1 200 OK\r\nContent-Length: 2\r\n\r\nok") in
  let (a_port, a_pid, _) =
    serve_once ~reply:(fun () -> Printf.sprintf
      "HTTP/1.1 302 Found\r\nLocation: http://127.0.0.1:%d/x\r\nContent-Length: 0\r\n\r\n"
      b_port) in
  let out = run_source (Printf.sprintf
    {|uses {IO, Net}
import IO
import HTTP
let r = HTTP.request! HTTP.Request(url = http://127.0.0.1:%d/start, headers = {"Authorization" = "Bearer secret", "Accept" = "text/plain"})
let () = IO.println "%%{r.status}"|} a_port) in
  (* A server that was never reached is still waiting. *)
  List.iter (fun pid ->
    (try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ());
    ignore (Unix.waitpid [] pid)) [a_pid; b_pid];
  let seen = In_channel.with_open_bin b_req In_channel.input_all in
  if not (contains out "200") then Alcotest.failf "the redirect was not followed: %s" out;
  Alcotest.(check bool) "the second host saw the other headers" true
    (contains seen "Accept: text/plain");
  Alcotest.(check bool) "and not the credentials" false (contains seen "secret")

let () =
  Alcotest.run "HTTP" [
    "redirects", [
      Alcotest.test_case "origins" `Quick test_origins;
      Alcotest.test_case "credentials stay with their origin" `Quick
        test_credentials_stay_with_their_origin;
      Alcotest.test_case "a redirect to another origin drops them" `Slow
        test_a_redirect_to_another_origin_drops_credentials;
    ];
    "what a manifest admits", [
      Alcotest.test_case "a named host" `Slow test_a_named_host_is_admitted;
      Alcotest.test_case "an unnamed host is refused" `Slow
        test_an_unnamed_host_is_refused;
      Alcotest.test_case "a pattern covers one level" `Slow
        test_a_pattern_covers_one_level;
      Alcotest.test_case "bare Net admits anything" `Slow
        test_bare_net_admits_anything;
      Alcotest.test_case "a port is not part of the host" `Slow
        test_a_port_is_not_part_of_the_host;
      Alcotest.test_case "an update is checked" `Slow test_an_update_is_checked;
      Alcotest.test_case "the convenience functions are bounded" `Slow
        test_the_convenience_functions_are_bounded;
      Alcotest.test_case "a computed host is refused" `Slow
        test_a_computed_host_is_refused;
      Alcotest.test_case "bare Net admits a computed host" `Slow
        test_a_computed_host_under_bare_net_is_admitted;
      Alcotest.test_case "a rebuilt URL keeps its bound" `Slow
        test_a_rebuilt_url_keeps_its_bound;
    ];
    "a rehearsal", [
      Alcotest.test_case "withholds an unsafe method" `Slow
        test_a_rehearsal_withholds_an_unsafe_method;
    ];
  ]
