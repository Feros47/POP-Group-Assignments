module BTree

/// <summary>
/// Represents a binary tree data structure.
/// </summary>
type BTree<'a> =
    | Leaf
    | Branch of BTree<'a> * 'a * BTree<'a>

/// <summary>
/// Calculates the size of the binary tree.
/// </summary>
/// <param name="tree">The input binary tree.</param>
/// <returns>The size of the binary tree, i.e., the number of internal nodes and leaf nodes.</returns>
let rec size tree = 
    match tree with
    | Leaf -> 0    
    | Branch (left, _, right) -> 1 + size left + size right  

/// <summary>
/// Performs a fold operation on the binary tree.
/// </summary>
/// <param name="f">The folding function that combines the results.</param>
/// <param name="acc">The initial accumulator value.</param>
/// <param name="tree">The input binary tree.</param>
/// <returns>The result of folding the tree using the provided function and accumulator.</returns>
let rec fold (folder: 'b  *'a * 'b -> 'b) (acc: 'b) (tree: BTree<'a>) : 'b =
    match tree with
        | Leaf -> acc
        | Branch (l, v, r) ->
            let leftResult = fold folder acc l
            let rightResult = fold folder acc r
            folder (leftResult, v, rightResult)

/// <summary>
/// A folding function to calculate the size of a binary tree.
/// </summary>
/// <param name="acc">The initial accumulator value.</param>
/// <param name="tree">The input binary tree.</param>
/// <returns>The size of the binary tree as the result of folding the tree.
let folder acc tree =
    fold (fun (sizeLeft, x, sizeRight) -> sizeLeft + 1 + sizeRight) acc tree
