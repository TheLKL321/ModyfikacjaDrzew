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

(** Zwraca set będący wynikiem dodania elementów przedziału (x, y) do setu s
    x <= y    *)
let add (x, y) s = 42

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
