open System

type StringTree =
    | Empty
    | Node of string * StringTree * StringTree

// Функция для вставки элемента в бинарное дерево (для простоты вставляем как в BST по длине строки)
let rec insertIntoTree (value: string) (tree: StringTree) =
    match tree with
    | Empty -> Node(value, Empty, Empty)
    | Node(nodeValue, left, right) ->
        // Сравниваем по длине строки, чтобы дерево было упорядоченным
        if value.Length <= nodeValue.Length then
            Node(nodeValue, insertIntoTree value left, right)
        else
            Node(nodeValue, left, insertIntoTree value right)

// Функция для построения дерева из списка строк
let buildTreeFromList (values: string list) =
    let rec build values acc =
        match values with
        | [] -> acc
        | head :: tail -> build tail (insertIntoTree head acc)
    build values Empty

// Функция для замены последнего символа в строке
let replaceLastChar (newChar: char) (str: string) =
    if String.IsNullOrEmpty(str) then
        str
    else
        let chars = str.ToCharArray()
        chars.[chars.Length - 1] <- newChar
        String(chars)

// Основная функция map для дерева
let mapStringTree (newChar: char) (tree: StringTree) : StringTree =
    let rec mapTree node =
        match node with
        | Empty -> Empty
        | Node(value, left, right) ->
            let newValue = replaceLastChar newChar value
            Node(newValue, mapTree left, mapTree right)
    mapTree tree

// Функция для вывода дерева
let rec printTree indent tree =
    match tree with
    | Empty -> printfn "%sEmpty" indent
    | Node(value, left, right) ->
        printfn "%sNode(\"%s\")" indent value
        printTree (indent + "  ") left
        printTree (indent + "  ") right

// Функция для чтения целого числа
let readInt prompt =
    let rec loop () =
        printf "%s" prompt
        match Int32.TryParse(Console.ReadLine()) with
        | true, value when value > 0 -> value
        | _ -> 
            printfn "Ошибка! Введите положительное число."
            loop ()
    loop ()

// Функция для чтения символа
let readChar prompt =
    let rec loop () =
        printf "%s" prompt
        let input = Console.ReadLine()
        if String.IsNullOrEmpty(input) then
            printfn "Ошибка! Введите один символ."
            loop ()
        else
            input.[0]
    loop ()

// Функция для генерации случайных чисел и преобразования в строки
let generateRandomStrings count =
    let rnd = Random()
    printfn "Генерируем %d случайных чисел от 1 до 10000..." count
    
    [ for i in 1..count -> 
        let number = rnd.Next(1, 1000001)  // генерация от 1 до 1000000
        string number ]  // преобразуем число в строку

[<EntryPoint>]
let main argv =
    printfn "=== СОЗДАНИЕ БИНАРНОГО ДЕРЕВА ==="
    
    // Запрашиваем только количество элементов
    let count = readInt "Введите количество элементов в дереве: "
    
    // Генерируем случайные числа и преобразуем в строки
    let strings = generateRandomStrings count
    
    // Построение дерева
    printfn "Оригинальное дерево"
    let originalTree = buildTreeFromList strings
    
    // Вывод исходного дерева
    printfn "Исходное дерево:"
    printTree "" originalTree
    
    // Ввод символа для замены
    let symbol = readChar "Введите символ на который необходимо заменить: "
    
    // Применяем map с заменой последнего символа
    let newTree = mapStringTree symbol originalTree
    
    // Вывод результата
    printfn "Дерево после замены последнего символа на '%c':" symbol
    printTree "" newTree
    
    0
