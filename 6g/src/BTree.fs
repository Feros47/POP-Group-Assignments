module BTree
open DiffList

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
/// Performs an in-order traversal of a binary tree and returns a difference list.
/// </summary>
/// <param name="tree">The input binary tree.</param>
/// <returns>A difference list representing the in-order traversal result.</returns>
let rec inorderD (tree: BTree<'a>) : 'a dlist =
    match tree with
    | Leaf -> nil
    | Branch (left, value, right) ->
        let leftD = inorderD left
        let rightD = inorderD right
        leftD << (fun ys -> value :: rightD ys)

/// <summary>
/// Performs an in-order traversal of a binary tree and returns the result as a list.
/// </summary>
/// <param name="tree">The input binary tree.</param>
/// <returns>A list containing the in-order traversal result.</returns>
let inorder (tree: BTree<'a>) : 'a list = (toList << inorderD) tree