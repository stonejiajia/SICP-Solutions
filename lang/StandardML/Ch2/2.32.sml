fun subsets nil = [nil]
  | subsets (x::xs) =
      let
         val rest = subsets xs
      in
         rest @ map (fn y => x::y) rest
      end;

fun isOdd n = (n mod 2 = 1);
val isEven = not o isOdd;
fun square x = x * x;
