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

let (|>) x f = f x

module Ex_1 = struct 
  (* simple first example from
     <http://ambassadortothecomputers.blogspot.co.uk/2010/05/how-froc-works.html>
  *)

  let (/) = F.lift2 (/)
  and ( * ) = F.lift2 ( * )
  and (+) = F.lift2 (+)
  
  let main () = 
    let v = F.return 4 in
    let w = F.return 2 in
    let x = F.return 2 in
    let y = F.return 3 in
    let z = F.return 1 in

    let u = v / w + x * y + z in
    
    printf "+ ex_1 u = %d\n%!" (F.sample u)
end

module Ex_1a = struct 
  (* extending ex_1 *)
    
  let (/) = F.lift2 (/)
  and ( * ) = F.lift2 ( * )
  and (+) = F.lift2 (+)

  let main () = 
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
end

module Ex_2 = struct
  open Froc
  
  (* second example from blog post (above) *)
  let main () = 
    let f x = 
      let b = x = 0 in
      let y = if b then 0 else 100 / x in
      y
    in    
    printf "+ ex_2 (f 10 = %d) (f 0 = %d)\n%!" (f 10) (f 0);

    let f' x = 
      let x' = return x in (* begin by boxing the input `x` into a Froc behavior *)
      let b = x' >>= fun x -> return (x = 0) in
      let n0 = x' >>= fun x -> return (100 / x) in
      (* example claims following:

          let y = bind2 b n0 (fun b n0 -> if b then return 0 else n0) in

         ...but looks to me like bind2_gen causes value to be read out of the
         Froc.behavior. hence we try (return n0) below
      *)
      let y = bind2 b n0 (fun b n0 -> if b then return 0 else return n0) in
      sample y
    in
    printf "+ ex_2 (f' 10 = %d) (f' 0 = BARF)\n%!" (f' 10);
    (* following barfs with Division_by_zero because the graph does not take
       account of the control dependency between `b` and `n0` (that is, 100/x)
       -- the change in x changes x' which changes *both* b and n0

       printf "+ ex_2 (f' 10 = %d)\n%!" (f' 0)
    *)
    ();

    let f'' x = 
      let x' = return x in
      let b = x' >>= fun x -> return (x = 0) in
      let y = b >>= (fun b -> 
        if b then return 0 
        else x' >>= fun x -> return (100 / x)
      )
    in
    sample y
  
  in
  printf "+ ex_2 (f'' 10 = %d) (f'' 0 = %d)\n%!" (f'' 10) (f'' 0)

end

module Ex_3 = struct
  open Froc

  let fmt lst = 
    "[ " ^ (List.map (fun e -> sprintf "%d" e) lst |> String.concat "; ") ^ " ]"

  type 'a lst = Nil | Cons of 'a * 'a lst Froc.behavior

  let rec make_lst = function
    | [] -> return Nil
    | h :: t -> let t = make_lst t in return (Cons (h, t))
    
  let rec map f lst = 
    lst >>= function
      | Nil -> return Nil
      | Cons (h, t) -> let t = map f t in return (Cons (f h, t))

  let mmap f lst = 
    let memo = memo () in 
    let rec map lst = 
      lst >>= function 
        | Nil -> return Nil 
        | Cons (h, t) -> let t = memo map t in return (Cons (f h, t)) 
    in 
    memo map lst 

  let fmt' lst = 
    let rec aux lst = match sample lst with
      | Nil -> []
      | Cons (h, t) -> let h = sprintf "%d" h in h :: aux t
    in
    "[ " ^ (String.concat "; " (aux lst)) ^ " ]"
      
  let main () = 
    let lst = make_lst [1; 2; 3] in
    let lst' = map (fun x -> x + 1) lst in
    let lst'' = mmap (fun x -> x + 1) lst in
    printf "+ ex_3 (lst = %s) (lst' = %s) (lst'' = %s)\n%!" (fmt' lst) (fmt' lst') (fmt' lst'')
    
end

let () = 
  F.init ();
  (* appears to be a no-op *)
  F.set_debug (fun s -> printf "+ %s\n%!" s);
  
  Ex_1.main ()
  ; Ex_1a.main ()
  ; Ex_2.main ()
  ; Ex_3.main ()
