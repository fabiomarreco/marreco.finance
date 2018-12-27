[1..10] |> List.fold (+) 0


let rec myFold (fn:'a -> 'a) (acc:'a) (lst:'a list) = 
    match lst with
    | [] -> acc
    | h:_ -> fn acc h

