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
    sx (r ()); DG.propagate(); printf "1z %d\n%!" (F.sample z);
    sx (r ()); DG.propagate(); printf "2z %d\n%!" (F.sample z);
    sx (r ()); DG.propagate(); printf "3z %d\n%!" (F.sample z);
    sx (r ()); DG.propagate(); printf "4z %d\n%!" (F.sample z)

end

let () = 
  Random.self_init ();
  F.init ();
  (* appears to be a no-op *)
  F.set_debug (fun s -> printf "+ %s\n%!" s);

  R.main ()
