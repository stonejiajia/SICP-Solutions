datatype 'a nestedlist = Leaf of 'a
                       | Node of 'a nestedlist list;


fun scale_tree (factor, Leaf x)         = Leaf(x*factor)
  | scale_tree (factor, Node nil)       = Node nil
  | scale_tree (factor, Node (x::xs))   =
    let
        val a = scale_tree(factor, Node xs)
        val b = case a of
                    Node c => c
                  | Leaf c => [Leaf c]
    in
        Node (scale_tree(factor, x) ::b)
    end;

scale_tree(10, Node[Leaf 1, Node[Leaf 2, Node[Leaf 3, Leaf 4], Leaf 5], Node[Leaf 6, Leaf 7]]);

fun square_tree (Leaf x)  = Leaf (x*x)
  | square_tree (Node xs) = Node(map square_tree xs);
square_tree(
   Node[Leaf 1,
     Node[Leaf 2, Node[Leaf 3, Leaf 4], Leaf 5],
     Node[Leaf 6, Leaf 7]]);
