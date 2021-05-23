

fun gcd(a, 0) = a
  | gcd (a, b) = gcd(b, a mod b);

fun cons (x, y) = [x , y];
