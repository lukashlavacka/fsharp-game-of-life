module Seq

let takeFirstNthLast (nth: int) (s: seq<'T>): seq<'T> =
    seq {
        use en = s.GetEnumerator()
        // Call MoveNext at most 'n' times (or return false earlier)
        let rec nextN n =
            let cur = en.Current
            if n = 1 then cur // the outside loops skips once so we have to skip n-1 times
            else if not (en.MoveNext()) then cur else (nextN (n - 1))

        if en.MoveNext() then
            // yield the first
            yield en.Current
            // skips nth - 1 and yield
            if nth > 1 then yield nextN nth
            // move once to next
            while en.MoveNext() do
                // skips nth - 1 and yield
                yield nextN nth
    }

let takeFirstNthLastIndex (nth: int) (s: seq<'T>): seq<'T * int> =
    seq {
        let mutable i = 0
        use en = s.GetEnumerator()
        // Call MoveNext at most 'n' times (or return false earlier)
        let rec nextN n =
            i <- i + 1
            let cur = en.Current
            if n = 1 then cur // the outside loops skips once so we have to skip n-1 times
            else if not (en.MoveNext()) then cur else (nextN (n - 1))
        if en.MoveNext() then
            // yield the first
            yield (en.Current, i + 1)
            // skips nth - 1 and yield
            if nth > 1 then yield (nextN nth, i)
            // move once to next
            while en.MoveNext() do
                // skips nth - 1 and yield
                yield (nextN nth, i)
    }

let takeWhileUnique (hash: 'T -> 'U) (s: seq<'T>): seq<'T> =
    seq {
        let mutable hashStore = List.empty<'U>
        use en = s.GetEnumerator()

        while en.MoveNext() && not (List.contains (hash en.Current) hashStore) do
            hashStore <- (hash en.Current) :: hashStore
            yield en.Current
    }