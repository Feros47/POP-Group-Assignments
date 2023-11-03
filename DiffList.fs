module DiffList

type 'a dlist = 'a list -> 'a list
type 'a BTree =
    | Leaf
    | Branch of 'a BTree * 'a * 'a BTree

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
let inorder (tree: 'a BTree) : 'a list =
    let dlist = inorderD tree
    dlist []

/// <summary>
/// Test the DiffList module
/// </summary>
(*
let testDiffList () = 
    // Tree
    let emptyTree = Leaf
    let t1 = Branch (Leaf, 1, Leaf)
    let t2 = Branch (t1, 2, t1)
    let t3 = Branch (Leaf, 3, Leaf)
    let t4 = Branch (t3, 4, t3)
    let t5 = Branch (t2, 5, t4)

    let result = inorder t5
    printfn "%A" result
    
    if result = [1; 2; 1; 5; 3; 4; 3] then
        printfn "DiffList test Passed"
    else 
        printfn "DiffList test failed"
           
testDiffList()
*)
