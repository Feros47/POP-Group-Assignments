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