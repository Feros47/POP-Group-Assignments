module DiffList
open BTree

type 'a dlist = 'a list -> 'a list
let fromList : 'a list -> 'a dlist = (@)
let toList (dl : 'a dlist) : 'a list = dl []
let nil : 'a dlist = fun ys -> ys   // = fromList []
let cons (x : 'a, dl : 'a dlist) : 'a dlist =
    fun ys -> x :: dl ys
let append : 'a dlist -> 'a dlist -> 'a dlist = (<<)


/// <summary>
/// Performs an in-order traversal of a binary tree and returns a difference list.
/// </summary>
/// <param name="tree">The input binary tree.</param>
/// <returns>A difference list representing the in-order traversal result.</returns>
let rec inorderD (tree: 'a BTree) : 'a dlist =
    match tree with
    | Leaf -> fun xs -> xs
    | Branch (left, value, right) ->
        let rec leftTraversal = inorderD left
        let rightTraversal = inorderD right
        fun xs -> leftTraversal (value :: rightTraversal xs)

/// <summary>
/// Performs an in-order traversal of a binary tree and returns the result as a list.
/// </summary>
/// <param name="tree">The input binary tree.</param>
/// <returns>A list containing the in-order traversal result.</returns>
let inorder = inorderD >> toList