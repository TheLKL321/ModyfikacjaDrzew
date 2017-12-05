(** TODO: komentarze takie jakie chcą w moodle'u  *)
(** TODO: jeśli upublicznisz repo to musisz dodać licencję LGPL, taką jak pSet  *)

(** typ setu
    Node(lewe poddrzewo, przedział, prawe poddrzewo, wysokość drzewa) *)
type set =
  | Empty
  | Node of set * (int * int) * set * int

(** typ zawierający funkcję porównującą dwa przedziały oraz jeden set *)
type t =
  {
    cmp : (int * int) -> (int * int) -> int;
    set : set;
  }

(** Zwraca:
    -2 jeśli (a, b) < (c, d)
    -1 jeśli (a, b) i (c, d) nachodzą na siebie i a < c i b < d
     0 jeśli (a, b) i (c, d) nachodzą na siebie i a < c i b > d
     1 jeśli (a, b) i (c, d) nachodzą na siebie i a > c i b > d
     2 jeśli (a, b) > (c, d)
     42 jeśli (a, b) zawiera się w (c, d) *)
     (** TODO: optimize  *)
let iCompare (a, b) (c, d) =
  if b < c - 1 then -2
  else if a < c && b <= d then -1
  else if a < c && b > d then 0
  else if a > d + 1 then 2
  else if a >= c && b > d then 1
  else 42

(** Zwraca:
    -1 jeśli x < a
     0 jeśli a <= x <= b
     1 jeśli x > b  *)
let nCompare x (a, b) =
  if x < a then -1
  else if x > b then 1
  else 0

(** Sprawdza czy dany przedział zawiera liczbę x  *)
let zawiera (a, b) x =
  x >= a && x <= b

(** Zwraca pusty set  *)
let empty = { cmp = iCompare; set = Empty }

(** Sprawdza czy set s jest pusty  *)
let is_empty s =
  s.set = Empty

(** Zwraca sumę dwóch przediałów  *)
let sum (a, b) (c, d) = (min a c, max b d)

(** Zwraca wysokość danego drzewa  *)
let height = function
  | Node (_, _, _, h) -> h
  | Empty -> 0

(** Złącza poddrzewa l i r podłączając je do nowego korzenia o wartości k  *)
let make l k r =
  Node (l, k, r, max (height l) (height r) + 1)

(** Wykonuje rotacje aby drzewo pozostało wybalansowane  *)
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

(** Złącza sety l i r dodając do nich przedział v  *)
let rec join cmp l v r =
  match (l, r) with
  | (Empty, _) -> addOne cmp v r
  | (_, Empty) -> addOne cmp v l
  | (Node(ll, lv, lr, lh), Node(rl, rv, rr, rh)) ->
      if lh > rh + 2 then bal ll lv (join cmp lr v r) else
      if rh > lh + 2 then bal (join cmp l v rl) rv rr else
      make l v r

(** Usuwa z drzewa odpowiednie przedziały tak, aby pozostałe przedziały
    pozostały rozłączne *)
and solver str x t =
  let rec pom = function
    | Empty -> Empty
    | Node (l, (a, b), r, h) ->
        if str = "left" then
          let c = nCompare x (a, b + 1)
          in
            if c < 0 then
              solver str x l
            else if c > 0 then
              join iCompare l (a, b) (pom r)
            else
              r
        else
          let c = nCompare x (a - 1, b)
          in
            if c < 0 then
              join iCompare (pom l) (a, b) r
            else if c > 0 then
              solver str x r
            else r
  in pom t

(** Używana przez add do dodawania (x, y) do danego setu  *)
and addOne cmp (x, y) = function
  | Node (l, k, r, h) ->
      let c = cmp (x, y) k
      in
        if c = 42 then
          Node (l, k, r, h)
        else if c = -2 then
          let nl = addOne cmp (x, y) l
          in
            bal nl k r
        else if c = -1 then
          let nl = solver "left" x l
          in
            join iCompare nl (sum (x, y) k) r
        else if c = 0 then
          let nl = solver "left" x l
          and nr = solver "right" y r
          in
            join iCompare nl (x, y) nr
        else if c = 1 then
          let nr = solver "right" x r
          in
            join iCompare l (sum (x, y) k) nr
        else
          let nr = addOne cmp (x, y) r
          in
            bal l k nr
  | Empty -> Node (Empty, (x, y), Empty, 1)

(** Zwraca set będący wynikiem dodania elementów przedziału (x, y) do setu s
    x <= y    *)
let add (x, y) { cmp = cmp; set = set } =
  { cmp = cmp; set = addOne cmp (x, y) set }

(** Zwraca najmniejszy element w drzewie  *)
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

(** Zwraca set będący wynikiem usunięcia z setu s elementów przedziału (x, y)
    x <= y    *)
let remove (x, y) { cmp = cmp; set = set } =
  let rec loop = function
    | Node (l, (a, b), r, _) ->
        let c = cmp (x, y) (a, b) in
        if c = 42 then join cmp (addOne cmp (a, x - 1) l) (y + 1, b) r
        else if c = -2 then join cmp (loop l) (a, b) r
        else if c = -1 then
          if y + 1 > b then merge (loop l) r
          else join cmp (loop l) (y + 1, b) r
        else if c = 1 then
          if a > x - 1 then merge l (loop r)
          else join cmp l (a, x - 1) (loop r)
        else if c = 2 then join cmp l (a, b) (loop r)
        else merge (loop l) (loop r)
    | Empty -> Empty in
  { cmp = cmp; set = loop set }

(** Sprawdza czy set s zawiera element x  *)
let mem x { set = set } =
  let rec loop = function
    | Node (l, k, r, _) ->
        let c = nCompare x k in
        c = 0 || loop (if c < 0 then l else r)
    | Empty -> false in
  loop set

(** Iteruje po secie w rosnącej kolejności  *)
let iter f { set = set } =
  let rec loop = function
    | Empty -> ()
    | Node (l, k, r, _) -> loop l; f k; loop r in
  loop set

(** Oblicza wartość (f xN ... (f x2 (f x1 a))...) gdzie x1, ..., xN to kolejne
    przedziały setu s ułożone rosnąco   *)

let fold f { set = set } acc =
  let rec loop acc = function
    | Empty -> acc
    | Node (l, k, r, _) ->
          loop (f k (loop acc l)) r in
  loop acc set


(** Zwraca listę wszystkich przedziałów setu s w kolejności rosnącej  *)
let elements { set = set } =
  let rec loop acc = function
      Empty -> acc
    | Node(l, k, r, _) -> loop (k :: loop acc r) l in
  loop [] set

(** Zwraca liczbę elementów w przedziale  *)
let iSize (a, b) =
  if b = max_int && a <= 0 || a = min_int && b >= 0 then max_int
  else b - a + 1

(** Zwraca trójkę (l, p, r) w której l jest setem elementów setu s mniejszych
    od x, r jest setem elementów setu s większych od x, p jest równe false jeśli
    s nie zawiera elementu równego x, true jeśli zawiera *)
let split x { cmp = cmp ; set = set } =
  let rec loop x = function
    | Empty ->
        (Empty, false, Empty)
    | Node (l, (a, b), r, _) ->
        let c = nCompare x (a, b) in
        if c = 0 then
          if x = a then
            (Empty, true, addOne cmp (x + 1, b) r)
          else if x = b then
            (addOne cmp (a, x - 1) l, true, Empty)
          else
            (addOne cmp (a, x - 1) l, true, addOne cmp (x + 1, b) r)
        else if c < 0 then
          let (ll, pres, rl) = loop x l in (ll, pres, join cmp rl (a, b) r)
        else
          let (lr, pres, rr) = loop x r in (join cmp l (a, b) lr, pres, rr)
  in
  let (setl, pres, setr) = loop x set in
  { cmp = cmp; set = setl }, pres, { cmp = cmp; set = setr }

(** Zwraca liczbę elementów setu s które są mniejsze lub równe x
    Dla liczby większej od max_int wynikiem jest max_int    *)
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
        pom (addOne iCompare (x, x) lower.set)
      else
        pom lower.set

;;

(**
#use "iSet.ml";;
#use "iSet_test.ml";;
#use "iSet_random_test.ml";;
  *)
