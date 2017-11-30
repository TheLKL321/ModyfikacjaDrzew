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

(** Szuka przedziału w drzewie t, będącym poddrzewem ze strony str
    (lewej/prawer) który nachodzi na przedział k. Gdy taki znajdzie, dodaje go
    do korzenia, a jego rozłączne poddrzewa dołącza do jego ojca *)
let solver str (c, d) t =
  let rec pom = function
    | Empty ->
    | Node (l, (a, b), r, h) ->
        let c = iCompare (a, b) (c, d)
        in
          if str = "left"
            if c = -2 then
              let sr = solver str (c, d) r
              in
                make l (a, b) sr
            else if c = -1 then
              Node (l, (c, a - 1), Empty, height l)
            else l
          else
            if c = 2 then
              let sl = solver str (c, d) l
              in
                make sl (a, b) r
            else if c = 1 then
              Node (Empty, (c, a - 1), r, height r)
            else r

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
    | Node (ll, lk, lr, _) ->
        if height ll >= height lr then make ll lk (make lr k r)
        else
          match lr with
          | Node (lrl, lrk, lrr, _) ->
              make (make ll lk lrl) lrk (make lrr k r)
          | Empty -> assert false
    | Empty -> assert false
  else if hr > hl + 2 then
    match r with
    | Node (rl, rk, rr, _) ->
        if height rr >= height rl then make (make l k rl) rk rr
        else
          match rl with
          | Node (rll, rlk, rlr, _) ->
              make (make l k rll) rlk (make rlr rk rr)
          | Empty -> assert false
    | Empty -> assert false
  else Node (l, k, r, max hl hr + 1)

(** Używana przez add do dodawania (x, y) do danego setu  *)
let rec add_one cmp (x, y) = function
  | Node (l, k, r, h) ->
      let c = cmp (x, y) k
      in
        if c = 42 then
          Node (l, k, r, h)
        else if c = -2 then
          let nl = add_one cmp (x, y) l
          in
            bal nl k r
        else if c = -1 then
          let nl = solver "left" k l
          in
            bal nl (sum (x, y) k) r
        else if c = 0 then
          let nl = solver "left" k l
          and nr = solver "right" k r
          in
            bal nl (x, y) nr
        else if c = 1 then
          let nr = solver "right" k r
          in
            bal l (sum (x, y) k) nr
        else
          let nr = add_one cmp (x, y) r
          in
            bal l k nr
  | Empty -> Node (Empty, (x, y), Empty, 1)

(** Zwraca set będący wynikiem dodania elementów przedziału (x, y) do setu s
    x <= y    *)
let add (x, y) { cmp = cmp; set = set } =
  { cmp = cmp; set = add_one cmp (x, y) set }

(** Zwraca set będący wynikiem usunięcia z setu s elementów przedziału (x, y)
    x <= y    *)
let remove (x, y) s = 42

(** Sprawdza czy set s zawiera element x  *)
let mem x s = 42

(** Iteruje po secie w rosnącej kolejności  *)
let iter f s = 42

(** Oblicza wartość (f xN ... (f x2 (f x1 a))...) gdzie x1, ..., xN to kolejne
    przedziały setu s ułożone rosnąco   *)
let fold f s a = 42

(** Zwraca listę wszystkich przedziałów setu s w kolejności rosnącej  *)
let elements s = 42

(** Zwraca liczbę elementów setu s które są mniejsze lub równe n
    Dla liczby większej od max_int wynikiem jest max_int    *)
let below n s = 42

(** Złącza sety l i r dodając do nich przedział v  *)
let rec join cmp l v r =
  match (l, r) with
    (Empty, _) -> add_one cmp v r
  | (_, Empty) -> add_one cmp v l
  | (Node(ll, lv, lr, lh), Node(rl, rv, rr, rh)) ->
      if lh > rh + 2 then bal ll lv (join cmp lr v r) else
      if rh > lh + 2 then bal (join cmp l v rl) rv rr else
      make l v r

(** Zwraca trójkę (l, p, r) w której l jest setem elementów setu s mniejszych
    od x, r jest setem elementów setu s większych od x, p jest równe false jeśli
    s nie zawiera elementu równego x, true jeśli zawiera *)
let split x { _ ; set = set } =
  let rec loop x = function
      Empty ->
        (Empty, false, Empty)
    | Node (l, v, r, _) ->
        let c = nCompare x v in
        if c = 0 then (l, true, r)
        else if c < 0 then
          let (ll, pres, rl) = loop x l in (ll, pres, join cmp rl v r)
        else
          let (lr, pres, rr) = loop x r in (join cmp l v lr, pres, rr)
  in
  let setl, pres, setr = loop x set in
  { cmp = cmp; set = setl }, pres, { cmp = cmp; set = setr }

;;
