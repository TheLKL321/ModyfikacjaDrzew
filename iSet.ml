(** By Łukasz Zarębski  *)

(** Typ drzewa BST
    Node(lewe poddrzewo, przedział, prawe poddrzewo, wysokość drzewa)
    Wszystkie przedziały w drzewie są rozłączne
    Drzewo jest wybalansowane, tzn różnica wysokości dzieci każdego ojca to
    maksymalnie 2 *)
type t =
  | Empty
  | Node of t * (int * int) * t * int

(** Założenia: a <= b, c <= d
    Zwraca:
    -2 jeśli (a, b) < (c, d)
    -1 jeśli (a, b) i (c, d) nachodzą na siebie i a < c i b < d
     0 jeśli (a, b) i (c, d) nachodzą na siebie i a < c i b > d
     1 jeśli (a, b) i (c, d) nachodzą na siebie i a > c i b > d
     2 jeśli (a, b) > (c, d)
     42 jeśli (a, b) zawiera się w (c, d) *)
let cmp (a, b) (c, d) =
  if b < c - 1 then -2
  else if a < c && b <= d then -1
  else if a < c then 0
  else if a > d + 1 then 2
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

(** Założenia: a <= b
    Sprawdza czy dany przedział zawiera liczbę x  *)
let zawiera (a, b) x =
  x >= a && x <= b

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
  | Node (_, _, _, h) -> h
  | Empty -> 0

(** Złącza poddrzewa l i r podłączając je do nowego korzenia z przedziałem k
    Jeśli l i r są wybalansowane, to wynik funkcji również  *)
let make l k r =
  Node (l, k, r, max (height l) (height r) + 1)

(** Założenia: Różnica wysokości l i r jest <= 1, oba drzewa są wybalansowane
    Wykonuje rotacje aby drzewo pozostało wybalansowane
    Wynikiem jest wybalansowane drzewo  *)
let bal l k r =
  let hl = height l in
  let hr = height r in
  if hl > hr + 2 then
    match l with
    | Empty -> assert false
    | Node (ll, lk, lr, _) ->
        if height ll >= height lr then make ll lk (make lr k r)
        else
          match lr with
          | Node (lrl, lrk, lrr, _) ->
              make (make ll lk lrl) lrk (make lrr k r)
          | Empty -> assert false
  else if hr > hl + 2 then
    match r with
    | Empty -> assert false
    | Node (rl, rk, rr, _) ->
        if height rr >= height rl then make (make l k rl) rk rr
        else
          match rl with
          | Node (rll, rlk, rlr, _) ->
              make (make l k rll) rlk (make rlr rk rr)
          | Empty -> assert false
  else Node (l, k, r, max hl hr + 1)

(** Zwraca najmniejszy przedział w drzewie  *)
let rec minElt = function
  | Node (Empty, k, _, _) -> k
  | Node (l, _, _, _) -> minElt l
  | Empty -> raise Not_found

(** Zwraca drzewo bez jego najmniejszego elementu  *)
let rec removeMinElt = function
  | Node (Empty, _, r, _) -> r
  | Node (l, k, r, _) -> bal (removeMinElt l) k r
  | Empty -> invalid_arg "ISet.removeMinElt"

(** Złącza ze sobą drzewa t1 i t2  *)
let merge t1 t2 =
  match t1, t2 with
  | Empty, _ -> t2
  | _, Empty -> t1
  | _ ->
      let k = minElt t2 in
      bal t1 k (removeMinElt t2)

(** Założenia: drzewa l i r są wybalansowane
    Złącza sety l i r dodając do nich przedział v
    Wynikiem jest wybalansowane drzewo  *)
let rec join l v r =
  match (l, r) with
  | (Empty, _) -> add v r
  | (_, Empty) -> add v l
  | (Node(ll, lv, lr, lh), Node(rl, rv, rr, rh)) ->
      if lh > rh + 2 then bal ll lv (join lr v r) else
      if rh > lh + 2 then bal (join l v rl) rv rr else
      make l v r

(** Założenia: x <= y, s to wybalansowane drzewo
    Zwraca drzewo będące wynikiem usunięcia z drzewa s wszystkich elementów
    przedziału (x, y)
    Wynikiem jest wybalansowane drzewo *)
and remove (x, y) s =
  let rec loop = function
    | Node (l, (a, b), r, _) ->
        let c = cmp (x, y) (a, b) in
        if c = 42 then
          if x = a then
            if y = b then merge l r
            else make l (y + 1, b) r
          else
            if y = b then make l (a, x - 1) r
            else join (add (a, x - 1) l) (y + 1, b) r
        else if c = -2 then join (loop l) (a, b) r
        else if c = -1 then
          if y + 1 > b then merge (loop l) r
          else join (loop l) (y + 1, b) r
        else if c = 1 then
          if a > x - 1 then merge l (loop r)
          else join l (a, x - 1) (loop r)
        else if c = 2 then join l (a, b) (loop r)
        else merge (loop l) (loop r)
    | Empty -> Empty
  in loop s

(** Założenia: x <= y
    Dodaje elementy przedziału (x, y) do danego drzewa
    Wynikiem jest wybalansowane drzewo  *)
and add (x, y) = function
  | Node (l, k, r, h) ->
      let c = cmp (x, y) k
      in
        if c = 42 then
          Node (l, k, r, h)
        else if c = -2 then
          let nl = add (x, y) l
          in
            bal nl k r
        else if c = -1 then
          let nl = remove (x, y) l
          in
            join nl (sum (x, y) k) r
        else if c = 0 then
          let nl = remove (x, y) l
          and nr = remove (x, y) r
          in
            join nl (x, y) nr
        else if c = 1 then
          let nr = remove (x, y) r
          in
            join l (sum (x, y) k) nr
        else
          let nr = add (x, y) r
          in
            bal l k nr
  | Empty -> Node (Empty, (x, y), Empty, 1)

(** Sprawdza czy drzewo s zawiera element x  *)
let mem x s =
  let rec loop = function
    | Node (l, k, r, _) ->
        let c = nCmp x k in
        c = 0 || loop (if c < 0 then l else r)
    | Empty -> false
  in loop s

(** Założenia: f: (int * int) -> unit()
    Wywołuje funkcję f na każdym przedziale danego setu w rosnącej kolejności *)
let iter f s =
  let rec loop = function
    | Empty -> ()
    | Node (l, k, r, _) -> loop l; f k; loop r
  in loop s

(** Założenia: f: (int * int) -> 'a -> 'a
    Wynikiem jest wartość (f xN ... (f x2 (f x1 a))...) gdzie x1, ..., xN to
    kolejne przedziały drzewa s ułożone rosnąco  *)
let fold f s acc =
  let rec loop acc = function
    | Empty -> acc
    | Node (l, k, r, _) ->
        loop (f k (loop acc l)) r
  in loop acc s


(** Zwraca listę wszystkich przedziałów setu s w kolejności rosnącej  *)
let elements s =
  let rec loop acc = function
      Empty -> acc
    | Node(l, k, r, _) -> loop (k :: loop acc r) l
  in loop [] s

(** Założenia: a <= b
    Zwraca liczbę elementów w przedziale
    Wynikiem jest liczba >= 0  *)
let iSize (a, b) =
  if b = max_int && a <= 0 || a = min_int && b >= 0 then max_int
  else b - a + 1

(** Zwraca trójkę (l, p, r) w której l jest drzewem elementów drzewa s
    mniejszych od x, r jest drzewem elementów drzewa s większych od x, p jest
    równe false jeśli s nie zawiera elementu równego x, true jeśli zawiera *)
let split x s =
  let rec loop x = function
    | Empty ->
        (Empty, false, Empty)
    | Node (l, (a, b), r, _) ->
        let c = nCmp x (a, b) in
        if c = 0 then
          if x = a then
            (l, true, add (x + 1, b) r)
          else if x = b then
            (add (a, x - 1) l, true, r)
          else
            (add (a, x - 1) l, true, add (x + 1, b) r)
        else if c < 0 then
          let (ll, pres, rl) = loop x l in (ll, pres, join rl (a, b) r)
        else
          let (lr, pres, rr) = loop x r in (join l (a, b) lr, pres, rr)
  in
    let (setl, pres, setr) = loop x s
    in setl, pres, setr

(** Zwraca liczbę elementów drzewa s które są mniejsze lub równe x
    Dla liczby większej od max_int wynikiem jest max_int
    Wynikiem jest liczba >= 0  *)
let below x s =
  let (lower, ifIncludes, _) = split x s
  in
    let rec pom = function
      | Empty -> 0
      | Node(l, k, r, _) ->
          let sizek = iSize k
          and sizel = pom l
          and sizer = pom r
          in
            let summed = sizel + sizer + sizek
            in
              if summed < sizek || summed < sizel || summed < sizer then max_int
              else summed
    in
      if ifIncludes then
        pom (add (x, x) lower)
      else
        pom lower

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
