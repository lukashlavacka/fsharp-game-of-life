module Operators
    let inline swap a b =
        b a
    let inline (><) a b =
        b a

    let inline (-&-) a b = (a, b)