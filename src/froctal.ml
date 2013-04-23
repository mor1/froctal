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

module Bridge : sig

  (** This module attempts to bridge between the data-space and the
      configuration-space. Verbs are in relation to the configuration-space. *)
    
  type 'a event = 'a Froc.event
  type 'a thread = 'a Lwt.t

  val absorb : (unit -> 'a thread) -> 'a event
    (** [absorb f] creates an event occurring each time [f ()] yields a value. *)

  val emit : 'a event -> 'a thread
    (** [emit e] creates a thread that yields its value before terminating. *)

end = struct

  let absorb f = 
    let v, vs = Froc.make_cell () in
    let rec loop () = 
      f () >>= fun x -> if Froc.sample v != x then vs x; loop ()
    in
    Lwt.pause () >>= loop  

  let emit e = 
    let ev = Froc.sample e in
    Lwt.return ev

end
