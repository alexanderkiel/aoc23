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
            result = input |> List.map extrapolate |> List.sum

            Stdout.line "Result: \(Num.toStr result)"

        _ -> Task.err 1

exit = \_ -> Task.err 1

parseInput = \s ->
    s
    |> Str.split "\n"
    |> List.dropIf Str.isEmpty
    |> List.mapTry \line -> line |> Str.split " " |> List.mapTry Str.toI64

extrapolate = \firstLayer ->
    when firstLayer is
        [.., last] -> extrapolate1 last firstLayer
        _ -> 0

expect
    actual = extrapolate []

    actual == 0

expect
    actual = extrapolate [0]

    actual == 0

expect
    actual = extrapolate [0, 3, 6, 9, 12, 15]

    actual == 18

expect
    actual = extrapolate [1, 3, 6, 10, 15, 21]

    actual == 28

expect
    actual = extrapolate [10, 13, 16, 21, 30, 45]

    actual == 68

extrapolate1 = \result, layer ->
    nextLayer = List.map2 (List.dropFirst layer 1) layer Num.sub
    when nextLayer is
        [.., last] if List.any nextLayer \n -> n != 0 -> extrapolate1 (result + last) nextLayer
        _ -> result
