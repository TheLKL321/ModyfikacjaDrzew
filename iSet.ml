(** TODO: komentarze takie jakie chcą w moodle'u  *)

(** typ setu  *)
type t =

(** Zwraca pusty set  *)
val empty : t

(** Sprawdza czy set s jest pusty  *)
let is_empty s = 42

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
