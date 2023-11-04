module DiffList

type 'a dlist = 'a list -> 'a list
let fromList : 'a list -> 'a dlist = (@)
let toList (dl : 'a dlist) : 'a list = dl []
let nil : 'a dlist = fun ys -> ys   // = fromList []
let cons (x : 'a, dl : 'a dlist) : 'a dlist =
    fun ys -> x :: dl ys
let append : 'a dlist -> 'a dlist -> 'a dlist = (<<)