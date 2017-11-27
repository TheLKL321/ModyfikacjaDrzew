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
    -2 jeśli b < c
    -1 jeśli a < c lecz c <= b <= d
     0 jeśli a < c oraz b > d
     1 jeśli b > d lecz c <= a <= d
     2 jeśli a > d
     42 jeśli c <= a <= b <= d *)
let iCompare (a, b) (c, d) =
  if b < c then -2
  else if a < c && b <= d then -1
  else if a < c && b > d then 0
  else if a > d then 2
  else if a >= c && b > d then 1
  else 42

(** Zwraca pusty set  *)
let empty = { cmp = iCompare; set = Empty }

(** Szuka przedziału w poddrzewie drzewa t ze strony str (lewej/prawer) który
    nachodzi na przedział w korzeniu. Gdy taki znajdzie, dodaje go do korzenia,
    a jego rozłączne poddrzewa dołącza do jego ojca *)
let solver t str = 42

(** Sprawdza czy set s jest pusty  *)
let is_empty s =
  s.set = Empty

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
(** TODO: modify to include omnomnomnom cases  *)
let rec add_one cmp (x, y) = function
  | Node (l, k, r, h) ->
      let c = cmp (x, y) k
      in
        if c = 0 then
          Node (l, x, r, h)
        else if c < 0 then
          let nl = add_one cmp (x, y) l
          in
            bal nl k r
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

(** TODO: dafuq does it do?   *)
let iter f s = 42

(** Oblicza wartość (f xN ... (f x2 (f x1 a))...) gdzie x1, ..., xN to kolejne
    przedziały setu s ułożone rosnąco   *)
let fold f s a = 42

(** Zwraca listę wszystkich przedziałów setu s w kolejności rosnącej  *)
let elements s = 42

(** Zwraca liczbę elementów setu s które są mniejsze lub równe n
    Dla liczby większej od max_int wynikiem jest max_int    *)
let below n s = 42

(** Zwraca trójkę (l, p, r) w której l jest setem elementów setu s mniejszych
    od x, r jest setem elementów setu s większych od x, p jest równe false jeśli
    s nie zawiera elementu równego x, true jeśli zawiera *)
let split x s = 42

;;
