app "d08"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.1/Icc3xJoIixF3hCcfXrDwLCu4wQHtNdPyoJkEbkgIElA.tar.br" }
    imports [pf.File, pf.Path, pf.Stdout, pf.Task.{ await, onErr }]
    provides [main] to pf

main =
    inputStr <- File.readUtf8 (Path.fromStr "input.txt")
        |> onErr exit
        |> await

    when parseInput inputStr is
        Ok input ->
            result = input |> List.map extrapolate |> List.walk 0 Num.add

            Stdout.line "Result: \(Num.toStr result)"

        _ -> Stdout.line "Error"

exit = \_ -> Task.err 1

parseInput = \s ->
    s
    |> Str.split "\n"
    |> List.dropIf Str.isEmpty
    |> List.mapTry parseLine

parseLine = \s ->
    when s |> Str.split " " |> List.mapTry Str.toI64 is
        # ensure that lines are non-empty
        Ok ([first, .. as rest]) -> Ok (first, rest)
        _ -> Err InvalidLine

expect 
    actual = parseLine "1"

    actual == Ok (1, [])

expect 
    actual = parseLine "1 2"

    actual == Ok (1, [2])

expect 
    actual = parseLine "" 

    actual == Err InvalidLine

extrapolate = \firstLayer ->
    # the first result is the last element of the first layer    
    last = nonEmptyLast firstLayer
    extrapolate1 last firstLayer

expect
    actual = extrapolate (0, [3, 6, 9, 12, 15])

    actual == 18

expect
    actual = extrapolate (1, [3, 6, 10, 15, 21])

    actual == 28

expect
    actual = extrapolate (10, [13, 16, 21, 30, 45])

    actual == 68

extrapolate1 = \result, layer ->
    when nextDiffLayer layer is
        [first, .. as rest] ->
            if Num.isZero first && List.all rest Num.isZero then
                result
            else
                nextLayer = (first, rest)
                last = nonEmptyLast nextLayer
                extrapolate1 (result + last) nextLayer
        _ -> result

nextDiffLayer = \(first, rest) ->
    List.map2 rest (List.prepend rest first) Num.sub

nonEmptyLast = \(first, rest) ->
    rest |> List.last |> Result.withDefault first