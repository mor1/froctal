(* Copyright (c) 2013 Richard Mortier <mort@cantab.net>
 * 
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
 * SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE. 
 *)

open Printf
module F = Froc

module R = struct

  let r () = let r = Random.int 5 in printf "[r %d]\n%!" r; r

  let c = ref 0
  let d = ref 0

  let f () = 
    printf "[c %d]\n%!" (!c); incr c;
    let x, sx = F.make_cell (r ()) in
    let y, sy = F.make_cell (r ()) in
    sx, sy, F.bind2 x y (fun x y -> 
      printf "[d %d] x=%d y=%d\n%!" (!d) x y;
      incr d;
      F.return (compare x y))

  let main () = 
    let sx, sy, z = f () in
    printf "z %d\n%!" (F.sample z);
    sx (r ()); printf "1z %d\n%!" (F.sample z);
    sx (r ()); printf "2z %d\n%!" (F.sample z);
    sx (r ()); printf "3z %d\n%!" (F.sample z);
    sx (r ()); printf "4z %d\n%!" (F.sample z)

end

module L = struct

  let watch delay fn cb = 
    (* racy kludge. *)

    printf "+ watch %f %s\n%!" delay fn;
    Lwt.(Lwt_unix.(
      lwt o = stat fn in
      let ot = ref o.st_mtime in

      let rec aux delay fn = 
        printf "  + aux %s\n%!" fn;
        sleep delay 
        >> lwt f = stat fn in
           let t = f.st_mtime in
           if !ot -. t <> 0. then (
             ot := t;
             cb fn
           );
           return ()
        >> aux delay fn
      in aux delay fn
    ))

  let timeout tm ts = 
    Lwt.(
      let tm = Lwt_unix.sleep tm >> return None in
      let ts = List.map (fun t -> t >>= fun v -> return (Some v)) ts in
      pick (tm :: ts)
    )

  let main () = 
    let ts = [ watch 0.3 "watched" ignore; watch 0.6 "also-watched" ignore ] in
    match Lwt_main.run (timeout 10. ts) with
      | None -> printf "timedout!\n%!"
      | Some () -> printf "changed\n%!"

end

let () = 
  Random.self_init ();
  F.init ();
  (* appears to be a no-op 
  F.set_debug (fun s -> printf "+ %s\n%!" s);
  *)

(*
  R.main ();
  L.main ();
*)

  let cb s = printf "#%s\n%!" s in
  let x, sx = F.make_cell (Lwt_main.run (L.watch 0.3 "x" cb)) in
  let y, sy = F.make_cell (Lwt_main.run (L.watch 0.6 "y" cb)) in

  let count = ref 0 in
  let m = F.bind2 x y (fun x y ->
    printf "@%d \n%!" !count;
    incr count;
    F.return ()
  )
  in
  while !count < 100 do
    F.sample m
  done
