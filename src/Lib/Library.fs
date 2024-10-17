namespace Matrix

type QTree<'value> =
    | Node of QTree<'value> * QTree<'value> * QTree<'value> * QTree<'value>
    | Leaf of 'value

module QTree =
    let rec map f tree =
        match tree with
        | Node (v1, v2, v3, v4 ) ->
            Node (map f v1 , map f v2 , map f v3 ,map f v4 )
        | Leaf v -> Leaf (f v)

    let rec map2 f tree1 tree2 = // Классический мэп для штук одинакового размера
        match (tree1, tree2) with
        | Leaf (v1), Leaf (v2) -> Leaf(f v1 v2)
        | Node (v1, v2, v3, v4),
          Node (u1, u2, u3, u4 ) ->
            Node (map2 f v1 u1, map2 f v2 u2, map2 f v3 u3, map2 f v4 u4)
        |_ ->failwith "!!!!"
    
(*[<Struct>]
{
type Matrix =
        val Collumn
}
*)
    let rec private q_tree_map2 f tree1 tree2 =
        match (tree1, tree2) with
        | Leaf (v1), Leaf (v2) -> Leaf(f v1 v2)
        | Node (v1, v2, v3, v4),
          Node (u1, u2, u3, u4 ) ->
            Node (q_tree_map2 f v1 u1, q_tree_map2 f v2 u2, q_tree_map2 f v3 u3, q_tree_map2 f v4 u4)
        | Leaf v1, Node(u1, u2, u3, u4) ->
            Node (q_tree_map2 f tree1 u1, q_tree_map2 f tree1 u2, q_tree_map2 f tree1 u3, q_tree_map2 f tree1 u4)
        | Node(u1, u2, u3, u4), Leaf (v1) ->
            Node (q_tree_map2 f u1 tree2, q_tree_map2 f u2 tree2, q_tree_map2 f u3 tree2, q_tree_map2 f u4 tree2)
    let map2 f (mtx1:Matrix<_>) (mtx2:Matrix<_>) =
        if mtx1.CollumnCount = mtx2.CollumnCount
            && mtx1.RowCount = mtx2.RowCount
        then Matrix(mtx1.CollumnCount, mtx1.RowCount, q_tree_map2 f mtx1.Co.....)
        else failwith "!!!!"

    let mXm (mtx1:Matrix<_>) (mtx2:Matrix<_>) op_add op_mut zero =
        let rec _mXm mtxTree1 mtxTree2 =
            match (mtxTree1, mtxTree2) with
            | 
        if true (*size_check*)
        then 

        else failwith "!!!!"