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

module Watcher = struct
    
  type notify = 
    | Changed of string

  let watch delay fn = 
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
             return (Changed fn)
           )
           else return ()
        >> aux delay fn
      in aux delay fn
    ))

(*
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
*)

end

module Lwt_in_froc = struct
  (*  x --> x' --> m
      y --> y' /
      where x, y: the file watcher, 
                  sends e[filelength] when file mtime changes via xs', ys' to
            x',y': the filelength option, 
                   bound to
            m: simply unpacks option tuple (None -> -1 | Some l -> l)
  *)

  let main () =
    (* doesn't work -- only one Lwt_main.run (ie., thread scheduler) is active
       at a time, so we watch first "x" then when that changes we get it's
       length and start watching "y". *)
    
    (* create x',y' the changeable holding the current filelength option, and a
       setter *)
    let x', xs' = F.make_cell None in
    let y', ys' = F.make_cell None in

    (* wrap the file watcher for now *)
    let w f s =     
      let rec aux d fn = 
        (* watch the file; thread terminates on change *)
        match Lwt_main.run (Watcher.watch d fn) with
          | Watcher.Changed fn ->
              let open Lwt in
                  (* read file length; may block *)
                  lwt l = Lwt_io.file_length fn in
              
              (* now we have the length, print it and send event with its
                 value option to x' *)
              printf "  %Ld\n%!" l;
              return (s (Some l))
      in aux 0.3 f
    in

    (* create x, the changeable holding the watcher itself, and a setter *)
    let x, xs = F.make_cell (w "x" xs') in
    let y, ys = F.make_cell (w "y" ys') in
    
    (* invocation counter, tracking when root node (m) changes *)
    let count = ref 0 in

    (* create root changeable m, and bind to the value of (x',y') currently.
       extend this to compute over/select from multiple inputs. *)
    let m = F.bind2 x' y' (fun x y ->
      printf "@%d\n%!" !count;
      incr count;
      F.return ( (* unpack option for now; more complex in future *)
        let lx = match x with None -> -1L | Some l -> l in
        let ly = match y with None -> -1L | Some l -> l in
        (lx, ly)
      ))
    in
    (* iterate watching for 100 changes to watched file *)
    while !count < 100 do
      (* get filelength from root, m *)
      let lx, ly = F.sample m in
      printf "* %Ld, %Ld\n%!" lx ly;
      (* the watcher thread completed when it returned the change notification,
         so reinitialise it with a new copy of the same watcher *)
      xs (w "x" xs');
      ys (w "y" ys')
    done
end 

let () = 
  F.init ();

  (*
    Random.self_init ();
    R.main ();
    L.main ();
  *)

  Lwt_in_froc.main ()
