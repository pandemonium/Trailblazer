namespace Trailblazer.Infrastructure


type 'a Stream = End | Emit of 'a

type ('i, 'o) Iteratee =
  | Return   of 'o
  | Continue of ('i Stream -> Iteratee<'i, 'o>)

module Iteratee =
  let tryHead<'i> : Iteratee<'i, 'i option> =
    function End    -> None   |> Return
           | Emit a -> Some a |> Return
    |> Continue

  let list<'i> : Iteratee<'i, 'i list> =
    let rec step acc = function End    -> Return   <| List.rev acc
                              | Emit a -> Continue <| step (a :: acc)
    in Continue <| step []

  let filter<'i> p : Iteratee<'i, 'i list> =
    let rec step acc = function End      -> Return <| List.rev acc
                              | Emit a 
                                when p a -> Continue <| step (a :: acc)
                              | Emit a   -> Continue <| step acc
    in Continue <| step []
  
  let tryTake<'i> n : Iteratee<'i, 'i list option> =
    let rec step i acc = function
                       | Emit a
                         when i > 0 -> Continue <| step (i - 1) (a :: acc)
                       | Emit _     
                       | End        -> if i = 0
                                        then Return <| Some (List.rev acc)
                                        else Return None 
    in Continue <| step n []
