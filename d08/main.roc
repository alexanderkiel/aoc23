app "d08"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.1/Icc3xJoIixF3hCcfXrDwLCu4wQHtNdPyoJkEbkgIElA.tar.br" }
    imports [pf.File, pf.Path, pf.Stdout, pf.Task.{ await, onErr }]
    provides [main] to pf

Dir : [L, R]

Path : List Dir

Node : U16

Input : {
    instructions : Path,
    starts : List Node,
    lTable : List Node,
    rTable : List Node,
}

maxNode : Node
maxNode = 0b11001_11001_11001

emptyTable = parseNode "XXX" |> List.repeat (Num.toNat maxNode + 1)

main =
    inputStr <- File.readUtf8 (Path.fromStr "input.txt")
        |> onErr error
        |> await

    input = parse inputStr

    (ends, pathLen) = input |> multiWalkGraph 100000

    Stdout.line "End Nodes: \(ends |> List.map nodeToStr |> Str.joinWith ", ")\nPath length: \(Num.toStr pathLen)"

error = \_ -> Task.err 1

parse = \s ->
    when Str.split s "\n" is
        [instructions, .. as tableLines] ->
            {
                instructions: parseInstructions instructions,
                starts: parseStarts tableLines,
                lTable: parseTable tableLines L,
                rTable: parseTable tableLines R,
            }

        _ -> crash "missing instructions"

parseInstructions = \s ->
    s
    |> Str.graphemes
    |> List.map parseInstruction

parseInstruction = \s ->
    when s is
        "L" -> L
        "R" -> R
        _ -> crash "unknown instruction"

parseStarts = \lines ->
    lines
    |> List.dropIf Str.isEmpty
    |> List.map \line ->
        (start, _) = parseTableLine line L
        start
    |> List.keepIf isStart

parseTable = \lines, dir ->
    lines
    |> List.dropIf Str.isEmpty
    |> List.walk emptyTable \table, line ->
        (start, end) = parseTableLine line dir
        List.set table (Num.toNat start) end

parseTableLine = \line, dir ->
    when Str.split line "=" is
        [start, ends] ->
            when Str.split ends "," is
                [left, right] ->
                    when dir is
                        L ->
                            (
                                start |> Str.trim |> parseNode,
                                left |> Str.replaceFirst "(" "" |> Str.trim |> parseNode,
                            )

                        R ->
                            (
                                start |> Str.trim |> parseNode,
                                right |> Str.replaceFirst ")" "" |> Str.trim |> parseNode,
                            )

                _ -> crash "invalid tableLines line: \(line)"

        _ -> crash "invalid tableLines line: \(line)"

parseNode : Str -> Node
parseNode = \s ->
    utf8ToU16 = \u -> (Num.toU16 u) - 65
    when Str.toUtf8 s is
        [a, b, c] ->
            Num.shiftLeftBy (utf8ToU16 a) 10
            + (Num.shiftLeftBy (utf8ToU16 b) 5)
            + (utf8ToU16 c)

        _ -> crash "invalid node"

expect Str.toUtf8 "A" == [65]
expect Str.toUtf8 "Z" == [90]

expect parseNode "AAA" == 0b00000_00000_00000
expect parseNode "AAB" == 0b00000_00000_00001
expect parseNode "ABA" == 0b00000_00001_00000
expect parseNode "BAA" == 0b00001_00000_00000
expect parseNode "ZZZ" == maxNode

walkGraph : Input -> (Node, Nat)
walkGraph = \input ->
    (.instructions input)
    |> List.repeat 100
    |> List.join
    |> List.walkUntil (parseNode "AAA", 0) \(node, pathLen), instruction ->
        targetNode = step input node instruction
        if isTerminal targetNode then
            Break (targetNode, pathLen + 1)
        else
            Continue (targetNode, pathLen + 1)

expect
    walkGraph {
        instructions: [R, L],
        starts: [parseNode "AAA"],
        lTable: tableFromList [(parseNode "ZZZ", parseNode "BBB")],
        rTable: tableFromList [(parseNode "AAA", parseNode "ZZZ")],
    }
    == (parseNode "ZZZ", 1)

multiWalkGraph : Input, Nat -> (List Node, Nat)
multiWalkGraph = \input, maxRounds ->
    multiWalkGraph1 input (maxRounds - 1) (.starts input, 0)

multiWalkGraph1 = \input, maxRounds, state ->
    (targetNodes, pathLen) = multiWalkGraph2 input state
    if List.all targetNodes isTerminal || maxRounds == 0 then
        (targetNodes, pathLen)
    else
        multiWalkGraph1 input (maxRounds - 1) (targetNodes, pathLen)

multiWalkGraph2 = \input, state ->
    (.instructions input)
    |> List.walkUntil state \(nodes, pathLen), instruction ->
        targetNodes = List.map nodes \node -> step input node instruction
        if List.all targetNodes isTerminal then
            Break (targetNodes, pathLen + 1)
        else
            Continue (targetNodes, pathLen + 1)

expect
    multiWalkGraph
        {
            instructions: [L, R],
            starts: [parseNode "AAA", parseNode "BBA"],
            lTable: tableFromList [
                (parseNode "AAA", parseNode "AAB"),
                (parseNode "AAB", parseNode "XXX"),
                (parseNode "AAZ", parseNode "AAB"),
                (parseNode "BBA", parseNode "BBB"),
                (parseNode "BBB", parseNode "BBC"),
                (parseNode "BBC", parseNode "BBZ"),
                (parseNode "BBZ", parseNode "BBB"),
                (parseNode "XXX", parseNode "XXX"),
            ],
            rTable: tableFromList [
                (parseNode "AAA", parseNode "XXX"),
                (parseNode "AAB", parseNode "AAZ"),
                (parseNode "AAZ", parseNode "XXX"),
                (parseNode "BBA", parseNode "XXX"),
                (parseNode "BBB", parseNode "BBC"),
                (parseNode "BBC", parseNode "BBZ"),
                (parseNode "BBZ", parseNode "BBB"),
                (parseNode "XXX", parseNode "XXX"),
            ],
        }
        3
    == ([parseNode "AAZ", parseNode "BBZ"], 6)

step = \input, node, instruction ->
    when List.get (tableFrom input instruction) (Num.toNat node) is
        Ok targetNode ->
            targetNode

        Err _ -> crash "can't find node: \(node |> nodeToStr)"

isStart : Node -> Bool
isStart = \node -> Num.bitwiseAnd 0b00000_00000_11111 node == 0

expect isStart (parseNode "XXA")
expect !(isStart (parseNode "XXB"))

isTerminal : Node -> Bool
isTerminal = \node -> Num.bitwiseAnd 0b00000_00000_11111 node == 25

tableFrom = \{ lTable, rTable }, instruction ->
    when instruction is
        L -> lTable
        R -> rTable

nodeToStr : Node -> Str
nodeToStr = \node ->
    when
        Str.fromUtf8 [
            node |> Num.shiftRightBy 10 |> Num.add 65 |> Num.toU8,
            node |> Num.shiftRightBy 5 |> Num.bitwiseAnd 0b00000_11111 |> Num.add 65 |> Num.toU8,
            node |> Num.bitwiseAnd 0b00000_00000_11111 |> Num.add 65 |> Num.toU8,
        ]
    is
        Ok s -> s
        Err _ -> crash "invalid node"

expect parseNode "ABC" |> nodeToStr == "ABC"

tableFromList = \list ->
    List.walk list emptyTable \result, (start, end) ->
        List.set result (Num.toNat start) end

expect
    tableFromList [(parseNode "ZZZ", parseNode "BBB")]
    |> List.get (Num.toNat (parseNode "ZZZ"))
    == Ok (parseNode "BBB")
