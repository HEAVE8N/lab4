open System

type Tree =
    | Empty
    | Node of int * Tree * Tree

// Функция для вставки элемента в бинарное дерево поиска
let rec insertIntoTree (value: int) (tree: Tree) =
    match tree with
    | Empty -> Node(value, Empty, Empty)
    | Node(nodeValue, left, right) ->
        if value <= nodeValue then
            Node(nodeValue, insertIntoTree value left, right)
        else
            Node(nodeValue, left, insertIntoTree value right)

// Функция для построения дерева из списка чисел
let buildTreeFromList (values: int list) =
    let rec build values acc =
        match values with
        | [] -> acc
        | head :: tail -> build tail (insertIntoTree head acc)
    build values Empty

let rec fold f acc tree =
    match tree with
    | Empty -> acc
    | Node(v, left, right) ->
        let acc1 = fold f acc left
        let acc2 = f acc1 v left right
        fold f acc2 right

let folder acc value left right =
        if left = Empty && right = Empty && value % 2 = 0 then
            acc + value
        else
            acc

let rec printTree indent tree =
    match tree with
    | Empty -> printfn "%sEmpty" indent
    | Node(value, left, right) ->
        printfn "%sNode(%d)" indent value
        printTree (indent + "  ") left
        printTree (indent + "  ") right

let readInt prompt =
    let rec loop () =
        printf "%s" prompt
        match Int32.TryParse(Console.ReadLine()) with
        | true, value when value > 0 -> value
        | _ -> 
            printfn "Ошибка! Введите положительное число."
            loop ()
    loop ()

// Функция для генерации случайных чисел
let generateRandomNumbers count =
    let rnd = Random()
    printfn "\nГенерируем %d случайных чисел от 1 до 100" count
    
    [ for i in 1..count -> 
        let number = rnd.Next(1, 101)
        printf "%d " number
        number ]

[<EntryPoint>]
let main argv =    
    printfn "=== ПОИСК СУММЫ ЧЕТНЫХ ЧИСЕЛ В ЛИСТЬЯХ ДЕРЕВА ==="
    let count = readInt "Введите количество элементов в дереве: "
    let numbers = generateRandomNumbers count
    let tree = buildTreeFromList numbers
    printfn "\nПостроенное дерево:"
    printTree "" tree
    let sum = fold folder 0 tree
    printfn "Сумма четных значений в листьях: %d" sum    
    0
