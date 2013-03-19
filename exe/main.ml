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
open Froc

let (/) = lift2 (/)
and ( * ) = lift2 ( * )
and (+) = lift2 (+)

let () = 

  let v = return 4 in
  let w = return 2 in
  let x = return 2 in
  let y = return 3 in
  let z = return 1 in

  let u = v / w + x * y  + z in
      
  printf "Result u = %d\n%!" (sample u)

(*


  let (e1 : unit Froc.event), s1 = Froc.make_event ()
  let (e2 : unit Froc.event), s2 = Froc.make_event ()

  let b1 = Froc.count e1
  let b2 = Froc.count e2

  let b3 = ~|(fun () -> ~.b1 + ~.b2)
(*
  let b3 =
  Froc_direct.direct begin fun () ->
  Froc_direct.read b1 + Froc_direct.read b2
  end
*)

;;

prerr_endline (string_of_int (Froc.sample b3));
Froc.send s1 ();
prerr_endline (string_of_int (Froc.sample b3));
Froc.send s2 ();
prerr_endline (string_of_int (Froc.sample b3));
*)
