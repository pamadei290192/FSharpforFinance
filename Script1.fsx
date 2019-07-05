///////////////////////////
//CHAPTER 1 : Introduction
///////////////////////////

// Variable & Immuability
let sum = 4 + 5
let newsum = sum + 3
let mutable a = 1
a <- 2

//Explaining functions:
let sumfunction (x:float) (y:float) = x+y

//Anonymous functions & High order functions:
let square = fun x->x*x
let cube = fun x->x*x*x
let squarebyfour f = 
    f 4
squarebyfour cube
squarebyfour square

//Concept of Currying:
let sum1 x y = x+y
let sumby2 y = sum1 2 y
sumby2 4

//Investigating lists:
let prices = [45.0;45.1;44.9;46.0]
let range = [0..100]
let finerange = [0.0..0.5..100.0]

//Concatenating lists:
let myNewList = [1;2;3] @ [4;5;6]
myNewList.Length
myNewList.Head
myNewList.Tail //Tail is everything except first element
List.map (fun x-> x*x) myNewList
List.filter (fun x-> x<4) myNewList

// Tuples:
(1.0, 2.0)
(1, 2.0, 3, '4', "four")  //can be of different types
let (b,c) = (1.0,2.0)   // extract values from tuples
let (_,d) = (1.0,2.0)   // if not interested by first value > Wildcard char

// Pipe Operators:
[0..100]
|> List.filter (fun x -> x % 2 = 0)
|> List.map (fun x -> x * 2)
|> List.sum

// Comment Types:
(*
This is a comment on multiple lines
New line
*)
/// Single line comment, supporting XML-tags
// This is also a single line comment

// Basic data work
let stockData = [
    "2013-06-06,51.15,51.66,50.83,51.52,9848400,51.52";
    "2013-06-05,52.57,52.68,50.91,51.36,14462900,51.36";
    "2013-06-04,53.74,53.75,52.22,52.59,10614700,52.59";
    "2013-06-03,53.86,53.89,52.40,53.41,13127900,53.41";
    "2013-05-31,54.70,54.91,53.99,54.10,12809700,54.10";
    "2013-05-30,55.01,55.69,54.96,55.10,8751200,55.10";
    "2013-05-29,55.15,55.40,54.53,55.05,8693700,55.05"
    ]

let splitCommas (l:string) = l.Split(',')
let lowestvolume = 
    stockData
    |> List.map (fun x-> splitCommas x)
    |> List.minBy(fun x-> x.[5]) // conditionnal min on specific value

// Parsing dates:
let date = "01/01/2018"
let dateTime = System.DateTime.ParseExact(date, "dd/mm/yyyy",null)


////////////////////////////////////
//CHAPTER 2: Learning more about f#
////////////////////////////////////

// Modules -> they look like classes but are a group of functionnalities
// They can be nested, to organise functions to structure the code.
// Functions in nested module can acccess parent values
module MainModule =
    let x = 2
    let y = 3
    module NestedModule =
        let f =
        x + y
printfn "%A" MainModule.NestedModule.f 

// Namespaces: Hierarchical categorization of modules
// Need to be the first declaration in the code file
// Main goal: minimize name collision

//namespace Namespace1.Library1
//    module Module2 =
//        let internal Version() =
//            "Version 1.0"

//namespace Namespace1.Library2
//    module Module1 =
//        let x = Namespace1.Library1.Module2.Version()


// ### LOOKING DEEPER inside data structure ###

//Record types:
type OpenHighLowClose = 
    {
    o:float
    h:float
    l:float
    c:float}

let ohclBar:OpenHighLowClose = {o = 1.0; h = 2.0; l = 3.0; c = 4.0}

type Quote =
    {
    mutable bid : float
    mutable ask : float
    }
    member this.midpoint() = (this.bid + this.ask) / 2.0 // Member function

let q:Quote = {bid = 100.0; ask = 200.0}
q.bid <- 150.0
q.midpoint()

// Record types and pattern matching (biggest reason for types to exist)
let matchQuote (quote : Quote) =
    match quote with
        | { bid = 0.0; ask = 0.0 } -> printfn "Both bid and ask is zero"
        | { bid = b; ask = a } -> printfn "bid: %f, ask: %f" b a

let q1:Quote = {bid = 100.0; ask = 200.0}
let q2:Quote = {bid = 0.0; ask = 0.0}

matchQuote q2

//Discrimination unions:

type OrderSide =       // represent a finite, weel defined set of choices
    | Buy
    | Sell
let buy = Buy
let sell = Sell

let toggle1 x =
    match x with
        | Buy -> Sell
        | Sell -> Buy

let toggle2 = function
    | Buy -> Sell
    | Sell -> Buy

buy |> toggle2
toggle2 buy

type OptionT =
    | Put of float
    | Call of float
    | Combine of OptionT * OptionT

type Tree =
    | Leaf of int
    | Node of Tree * Tree

let SimpleTree =
    Node (
        Leaf 1,
        Leaf 2
    )

let countLeaves tree =    // Iterate through tree
    let rec loop sum = function
        | Leaf(_) -> sum + 1
        | Node(tree1, tree2) ->
        sum + (loop 0 tree1) + (loop 0 tree2)
    loop 0 tree
countLeaves SimpleTree

//Enumerations:  > used to map labels to constant values.

type RGB =
    | Red = 0
    | Green = 1
    | Blue = 2


//Arrays:  > mutable collections of a fixed size and must contain values of
//           the same type. Large arrays of constant types can be compiled to efficient
//           binary representations.

let array1 = [| 1; 2; 3 |]
array1.[0] <- 10
let array3 = [| for i in 1 .. 10 -> i * i |] // comprehensive array
let arrayOfTenZeroes : int array = Array.zeroCreate 10 // initialisation example
let myEmptyArray = Array.empty // Second initilisation example
Array.append [| 1; 2; 3|] [| 4; 5; 6|]
[| 1 .. 10|] |> Array.filter (fun elem -> elem % 2 = 0) 

//Lists:  Lists in F# are implemented as linked lists and are immutable
//        you cannot mix different types in the same list

let list1 = [1 .. 10]
let list2 = [11 .. 20]
0::list1  // :: O(1) operator prepends elements to the start of a list
list1@[11] //concat operator (@) has a performance penalty because lists are immutable and the first list has to be copied

List.zip list1 list2
List.fold (+) 0 list1 // 0=inital value to fold
List.fold (*) 1 list1 // 1=inital value to fold

//Pattern matching and lists:




