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
module DG = Froc_ddg
module SA = Froc_sa

let (/) = F.lift2 (/)
and ( * ) = F.lift2 ( * )
and (+) = F.lift2 (+)

let ex_1 () = 
  (* simple first example from
     <http://ambassadortothecomputers.blogspot.co.uk/2010/05/how-froc-works.html> *)

  let v = F.return 4 in
  let w = F.return 2 in
  let x = F.return 2 in
  let y = F.return 3 in
  let z = F.return 1 in

  let u = v / w + x * y + z in
  
  printf "+ ex_1 u = %d\n%!" (F.sample u)

let ex_1a () = 
  (* extending ex_1 *)

  let v = F.return 4 in
  let w = F.return 2 in
  let x = F.return 2 in
  let y = F.return 3 in
  let z, zs = F.make_cell 1 in

  let u = v / w + x * y + z in
  
  printf "+ ex_1a u = %d\n%!" (F.sample u); 
  zs 2;
  DG.propagate (); (* wonder why `propagate` not exposed in Froc? *)
  printf "+ ex_1a u = %d\n%!" (F.sample u)

let () = 
  F.init ();
  F.set_debug (fun s -> printf "+ %s\n%!" s);
  
  ex_1 ();
  ex_1a ()