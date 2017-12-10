(** By Łukasz Zarębski  *)

(** Typ drzewa BST
    Node(lewe poddrzewo, przedział, prawe poddrzewo, wysokość drzewa, ilość elementów przedziałów drzewa)
    Wszystkie przedziały w drzewie są rozłączne
    Drzewo jest wybalansowane, tzn różnica wysokości dzieci każdego ojca to
    maksymalnie 2 *)
type t =
  | Empty
  | Node of t * (int * int) * t * int * int

(** Założenia: a <= b, c <= d
    Zwraca:
    -2 jeśli (a, b) < (c, d)
    -1 jeśli (a, b) i (c, d) nachodzą na siebie i a < c i b < d
     0 jeśli (a, b) i (c, d) nachodzą na siebie i a < c i b > d
     1 jeśli (a, b) i (c, d) nachodzą na siebie i a > c i b > d
     2 jeśli (a, b) > (c, d)
     42 jeśli (a, b) zawiera się w (c, d) *)
let cmp (a, b) (c, d) =
  if b < c - 1 && c <> min_int then -2
  else if a < c && b <= d then -1
  else if a < c then 0
  else if a > d + 1 && d <> max_int then 2
  else if b > d then 1
  else 42

(** Założenia: a <= b
    Zwraca:
    -1 jeśli x < a
     0 jeśli a <= x <= b
     1 jeśli x > b  *)
let nCmp x (a, b) =
  if x < a then -1
  else if x > b then 1
  else 0

(** Zwraca pusty set  *)
let empty = Empty

(** Sprawdza czy drzewo s jest puste  *)
let is_empty s =
  s = Empty

(** Założenia: a <= b, c <= d
    Zwraca sumę dwóch przediałów  *)
let sum (a, b) (c, d) = (min a c, max b d)

(** Zwraca wysokość danego drzewa  *)
let height = function
  | Node (_, _, _, h, _) -> h
  | Empty -> 0

(** Założenia a <= b
    Zwraca liczbę elementów w przedziale
    Wynikiem jest liczba >= 0  *)
let iSize (a, b) =
  if b - a + 1 < 0 then max_int
  else b - a + 1

let sSize = function
  | Node (_, _, _, _, el) -> el
  | Empty -> 0

(** Złącza poddrzewa l i r podłączając je do nowego korzenia z przedziałem k
    Jeśli l i r są wybalansowane, to wynik funkcji również  *)
let make l k r =
  let sizek = iSize k
  and sizel = sSize l
  and sizer = sSize r
  in
    let summed = 
      if sizek + sizel + sizer < 0 || sizek + sizel < 0 || sizek + sizer < 0 || sizer + sizel < 0 then max_int 
      else sizek + sizel + sizer
    in 
      Node (l, k, r, max (height l) (height r) + 1, summed)

(** Założenia: Różnica wysokości l i r jest <= 3, oba drzewa są wybalansowane
    Wykonuje rotacje aby drzewo pozostało wybalansowane
    Wynikiem jest wybalansowane drzewo  *)
let bal l k r =
  let hl = height l in
  let hr = height r in
  if hl > hr + 2 then
    match l with
    | Empty -> assert false
    | Node (ll, lk, lr, _, _) ->
        if height ll >= height lr then make ll lk (make lr k r)
        else
          match lr with
          | Node (lrl, lrk, lrr, _, _) ->
              make (make ll lk lrl) lrk (make lrr k r)
          | Empty -> assert false
  else if hr > hl + 2 then
    match r with
    | Empty -> assert false
    | Node (rl, rk, rr, _, _) ->
        if height rr >= height rl then make (make l k rl) rk rr
        else
          match rl with
          | Node (rll, rlk, rlr, _, _) ->
              make (make l k rll) rlk (make rlr rk rr)
          | Empty -> assert false
  else make l k r

(** Zwraca najmniejszy przedział w drzewie  *)
let rec minElt = function
  | Node (Empty, k, _, _, _) -> k
  | Node (l, _, _, _, _) -> minElt l
  | Empty -> raise Not_found

(** Zwraca drzewo bez jego najmniejszego elementu  *)
let rec removeMinElt = function
  | Node (Empty, _, r, _, _) -> r
  | Node (l, k, r, _, _) -> bal (removeMinElt l) k r
  | Empty -> invalid_arg "ISet.removeMinElt"

(** Złącza ze sobą drzewa t1 i t2  *)
let merge t1 t2 =
  match t1, t2 with
  | Empty, _ -> t2
  | _, Empty -> t1
  | _ ->
      let k = minElt t2 in
      bal t1 k (removeMinElt t2)

(** Założenia: przedział x nie tworzy ciągłego przedziału z żadnym z przedziałów
    danego drzewa
    Zwraca drzewo będące wynikiem dodania do danego drzewa elementów z x  *)
let rec addOne x = function
  | Node (l, k, r, h, el) ->
      let c = cmp x k in
        if c = 42 then Node (l, k, r, h, el)
        else if c < 0 then
          let nl = addOne x l
          in bal nl k r
        else
          let nr = addOne x r
          in bal l k nr
  | Empty -> Node (Empty, x, Empty, 1, iSize x)

(** Założenia: x <= y, a <= b
    Zwraca przedział z danego drzewa z którego suma z (x, y) tworzy jak
    największy przedział przeszukując lewe poddrzewo t  *)
let rec leftMost (x, y) = function
    | Empty -> (x, y)
    | Node(l, (a, b), r, _, _) ->
        let c = cmp (a, b) (x, y)
        in
          if c = -1 || c = 0 then (a, b)
          else if c = -2 then leftMost (x, y) r
          else
            let left = leftMost (x, y) l
            in
              if left <> (x, y)
                then left
              else if (left = (x, y) && y >= a)
                then (a, b)
              else (x, y)

(** Założenia: x <= y, a <= b
    Zwraca przedział z danego drzewa z którego suma z (x, y) tworzy jak
    największy przedział przeszukując prawe poddrzewo t  *)
let rec rightMost (x, y) = function
    | Empty -> (x, y)
    | Node(l, (a, b), r, _, _) ->
        let c = cmp (a, b) (x, y)
        in
          if c = 1 || c = 0 then (a, b)
          else if c = 2 then rightMost (x, y) l
          else
            let right = rightMost (x,y) r
            in
              if(right <> (x, y))
                then right
              else if (right = (x, y) && x <= b)
                then (a, b)
              else (x, y)

(** Założenia: drzewa l i r są wybalansowane
    Złącza sety l i r dodając do nich przedział v
    Wynikiem jest wybalansowane drzewo  *)
let rec join l v r =
  match (l, r) with
  | (Empty, _) -> addOne v r
  | (_, Empty) -> addOne v l
  | (Node(ll, lv, lr, lh, _), Node(rl, rv, rr, rh, _)) ->
      if lh > rh + 2 then bal ll lv (join lr v r) else
      if rh > lh + 2 then bal (join l v rl) rv rr else
      make l v r

(** Zwraca trójkę (l, p, r) w której l jest drzewem elementów drzewa s
    mniejszych od x, r jest drzewem elementów drzewa s większych od x, p jest
    równe false jeśli s nie zawiera elementu równego x, true jeśli zawiera *)
let rec split x s =
  let rec loop x = function
    | Empty ->
        (Empty, false, Empty)
    | Node (l, (a, b), r, _, _) ->
        let c = nCmp x (a, b) in
        if c = 0 then
          if x = a && x = b then
           (l, true, r)
          else if x = a then
            (l, true, addOne (x + 1, b) r)
          else if x = b then
            (addOne (a, x - 1) l, true, r)
          else
            (addOne (a, x - 1) l, true, addOne (x + 1, b) r)
        else if c < 0 then
          let (ll, pres, rl) = loop x l
          in (ll, pres, join rl (a, b) r)
        else
          let (lr, pres, rr) = loop x r
          in (join l (a, b) lr, pres, rr)
  in
    let (setl, pres, setr) = loop x s
    in setl, pres, setr

(** Założenia: x <= y
    Zwraca drzewo będące wynikiem dodania do danego drzewa elementów z (x, y) *)
and add (x, y) s =
  let (l1, l2) = leftMost (x, y) s
  and (r1, r2) = rightMost (x, y) s
  in
    if (l1, l2) = (r1, r2)  && (l1, l2) = (x, y)
    then addOne (x, y) s
    else
      let (l,_,_) = split l1 s
      and (_,_,r) = split r2 s
      in
        join l (min l1 x, max r2 y) r

(** Założenia: x <= y
    Zwraca drzewo będące wynikiem usunięcia z drzewa s elementów z (x, y)  *)
let remove (x, y) s =
  let (left, _, _) = split x s
  and (_, _, right) = split y s
  in
    if right = Empty then left
    else if left = Empty then right
    else join left (minElt right) (removeMinElt right)


(** Sprawdza czy drzewo s zawiera element x  *)
let mem x s =
  let rec loop = function
    | Node (l, k, r, _, _) ->
        let c = nCmp x k in
        c = 0 || loop (if c < 0 then l else r)
    | Empty -> false
  in loop s

(** Założenia: f: (int * int) -> unit()
    Wywołuje funkcję f na każdym przedziale danego setu w rosnącej kolejności *)
let iter f s =
  let rec loop = function
    | Empty -> ()
    | Node (l, k, r, _, _) -> loop l; f k; loop r
  in loop s

(** Założenia: f: (int * int) -> 'a -> 'a
    Wynikiem jest wartość (f xN ... (f x2 (f x1 a))...) gdzie x1, ..., xN to
    kolejne przedziały drzewa s ułożone rosnąco  *)
let fold f s acc =
  let rec loop acc = function
    | Empty -> acc
    | Node (l, k, r, _, _) ->
        loop (f k (loop acc l)) r
  in loop acc s

(** Zwraca listę wszystkich przedziałów setu s w kolejności rosnącej  *)
let elements s =
  let rec loop acc = function
      Empty -> acc
    | Node(l, k, r, _, _) -> loop (k :: loop acc r) l
  in loop [] s

(** Zwraca liczbę elementów drzewa s które są mniejsze lub równe x
    Dla liczby większej od max_int wynikiem jest max_int
    Wynikiem jest liczba >= 0  *)
let below x s =
  let (lower, ifIncludes, _) = split x s
  in
    let size = sSize lower
    in
      if ifIncludes && size <> max_int then size + 1
      else size
;;

(*
/////////////////////TESTY////////////////////////////////////////

let zle = ref 0
let test (id:int) (result:bool) (expected:bool) : unit =
    if result <> expected then begin
        Printf.printf "Zly wynik testu %d!\n" id;
        incr zle
    end;;


Printf.printf "Testowanie is_empty\n";;

let s = empty;;
test 11 (is_empty s) true;;
test 12 (is_empty (add (1, 1) s)) false;;


Printf.printf "Testowanie add...\n";;
(* niestety musimy zalozyc poprawnosc mem... *)

let s = add (10, 12) empty;;
test 21 (mem 9 s) false;;
test 22 (mem 10 s) true;;
test 23 (mem 12 s) true;;
test 24 (mem 13 s) false;;

let s = add (4, 7) s;;
test 25 (mem 8 s) false;;
test 26 (mem 11 s) true;;
test 27 (mem 5 s) true;;
test 28 (mem 3 s) false;;


Printf.printf "Testowanie remove...\n";;

let s = add (1, 1) (add (15, 16) (add (10, 14) (add (6, 9) empty)));;
test 31 (mem 10 (remove (10, 10) s)) false;;
test 32 (is_empty (remove (1, 20) s)) true;;
test 33 (mem 7 (remove (8, 15) s)) true;;

let s = add (-1, 1) (add (3, 7) (add (10, 12) (add (15, 18)
        (add (-15, -13) empty))));;
let s = remove (-10, 12) s;;
test 34 (is_empty s) false;;
test 35 (mem 5 s) false;;
test 36 (mem (-10) s) false;;
test 37 (mem (-15) s) true;;
test 38 (mem 17 s) true;;


Printf. printf "Testowanie elements...\n";;

test 41 (elements (add (4, 5) (add (7, 8) empty)) = [(4, 5); (7, 8)]) true;;
test 42 (elements (add (1, 1) (add (11, 14) (add (6, 9) (add (4, 5) empty))))
        = [(1, 1); (4, 9); (11, 14)]) true;;


Printf. printf "Testowanie below...\n";;

let s = add (3, 4) (add (8, 10) (add (15, 20) empty));;
test 51 (below 2 s = 0) true;;
test 52 (below 3 s = 1) true;;
test 53 (below 10 s = 5) true;;
test 54 (below 15 s = 6) true;;
test 55 (below 100 s = 11) true;;
let s = add (1, max_int) (add (-1, 0) empty);;
test 56 (below max_int s = max_int) true;;
let s = add (-min_int, max_int) empty;;
test 57 (below max_int s = max_int) true;;
test 58 (below min_int s = 1) true;;


Printf. printf "Testowanie split...\n";;

let s = add (3, 4) (add (8, 10) (add (15, 20) empty));;
let l, pres, r = split 9 s;;
test 61 (mem 9 l) false;;
test 62 (mem 9 r) false;;
test 63 (mem 8 l) true;;
test 64 (mem 10 r) true;;
test 65 pres true;;
test 66 (mem 7 l) false;;
test 67 (mem 4 l) true;;
test 68 (mem 11 r) false;;
test 69 (mem 16 r) true;;


Printf. printf "Testowanie iter...\n";;

let s = add (1, 1) (add (11, 14) (add (6, 9) (add (4, 5) empty)));;
let a = ref [];;
let foo x = a := x::!a; ();;
test 71 (iter foo s; !a = [(11, 14); (4, 9); (1, 1)]) true;;


Printf. printf "Testowanie fold...\n";;

let s = add (1, 1) (add (11, 14) (add (6, 9) (add (4, 5) empty)));;
let foo x a = x::a;;
test 81 (fold foo s [] = [(11, 14); (4, 9); (1, 1)]) true;;


let _ =
    if !zle = 0 then
        Printf.printf "\nTesty ok!\n"
    else
        Printf.printf "\nZlych odpowiedzi: %d.\n" !zle
;;
  *)
