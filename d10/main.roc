app "d08"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.1/Icc3xJoIixF3hCcfXrDwLCu4wQHtNdPyoJkEbkgIElA.tar.br" }
    imports [pf.File, pf.Path, pf.Stdout, pf.Task.{ await, onErr }]
    provides [main] to pf

main =
    inputStr <- File.readUtf8 (Path.fromStr "input.txt")
        |> onErr exit
        |> await

    graph = parseInput inputStr

    result = farthestPoint graph

    Stdout.line "Result: \(Num.toStr result)"

exit = \_ -> Task.err 1

Graph : {
    start : Coord,
    nodes : Dict Coord Node,
}

Node : [NS, EW, NE, NW, SW, SE]

Coord : (Nat, Nat)

Dir : [N, E, S, W]

emptyGraph = { start: (0, 0), nodes: Dict.empty {} }

insertNode : Graph, Coord, Node -> Graph
insertNode = \graph, pos, node ->
    { graph & nodes: Dict.insert graph.nodes pos node }

parseInput : Str -> Graph
parseInput = \s ->
    s
    |> Str.split "\n"
    |> List.dropIf Str.isEmpty
    |> List.walkWithIndex emptyGraph parseLine

parseLine = \graph, line, y ->
    line
    |> Str.graphemes
    |> List.walkWithIndex graph \g, node, x -> parseNode g node x y

parseNode = \graph, node, x, y ->
    when node is
        "S" -> { graph & start: (x, y) }
        "|" -> insertNode graph (x, y) NS
        "-" -> insertNode graph (x, y) EW
        "L" -> insertNode graph (x, y) NE
        "J" -> insertNode graph (x, y) NW
        "7" -> insertNode graph (x, y) SW
        "F" -> insertNode graph (x, y) SE
        _ -> graph

expect
    actual = parseInput ""

    actual == emptyGraph

expect
    actual = parseInput ".S"

    actual == { emptyGraph & start: (1, 0) }

expect
    actual = parseInput "|"

    actual == emptyGraph |> insertNode (0, 0) NS

expect
    actual = parseInput "-"

    actual == emptyGraph |> insertNode (0, 0) EW

expect
    actual = parseInput "L"

    actual == emptyGraph |> insertNode (0, 0) NE

dirOf : Coord, Coord -> Dir
dirOf = \(px, py), (x, y) ->
    if px == x && py == y + 1 then
        N
    else if px + 1 == x && py == y then
        E
    else if px == x && py + 1 == y then
        S
    else
        W

nextPos : Graph, Coord, Coord -> Result Coord [NotPossible]
nextPos = \graph, prev, (x, y) ->
    when dirOf prev (x, y) is
        N ->
            when Dict.get graph.nodes (x, y) is
                Ok NS if y > 0 -> Ok (x, y - 1)
                Ok SW if x > 0 -> Ok (x - 1, y)
                Ok SE -> Ok (x + 1, y)
                _ -> Err NotPossible

        E ->
            when Dict.get graph.nodes (x, y) is
                Ok EW -> Ok (x + 1, y)
                Ok NW if y > 0 -> Ok (x, y - 1)
                Ok SW -> Ok (x, y + 1)
                _ -> Err NotPossible

        S ->
            when Dict.get graph.nodes (x, y) is
                Ok NS -> Ok (x, y + 1)
                Ok NE -> Ok (x + 1, y)
                Ok NW if x > 0 -> Ok (x - 1, y)
                _ -> Err NotPossible

        W ->
            when Dict.get graph.nodes (x, y) is
                Ok EW if x > 0 -> Ok (x - 1, y)
                Ok NE if y > 0 -> Ok (x, y - 1)
                Ok SE -> Ok (x, y + 1)
                _ -> Err NotPossible

        _ -> Err NotPossible

expect
    graph = parseInput "S-"
    actual = nextPos graph (0, 0) (1, 0)

    actual == Ok (2, 0)

expect
    graph = parseInput "S7"
    actual = nextPos graph (0, 0) (1, 0)

    actual == Ok (1, 1)

expect
    graph = parseInput
        """
        ..
        SJ
        """
    actual = nextPos graph (0, 1) (1, 1)

    actual == Ok (1, 0)

expect
    graph = parseInput "SJ"
    actual = nextPos graph (0, 0) (1, 0)

    actual == Err NotPossible

expect
    graph = parseInput
        """
        S
        |
        """
    actual = nextPos graph (0, 0) (0, 1)

    actual == Ok (0, 2)

expect
    graph = parseInput
        """
        S
        L
        """
    actual = nextPos graph (0, 0) (0, 1)

    actual == Ok (1, 1)

expect
    graph = parseInput
        """
        .S
        .J
        """
    actual = nextPos graph (1, 0) (1, 1)

    actual == Ok (0, 1)

expect
    graph = parseInput
        """
        S
        J
        """
    actual = nextPos graph (0, 0) (0, 1)

    actual == Err NotPossible

expect
    graph = parseInput ".-S"
    actual = nextPos graph (2, 0) (1, 0)

    actual == Ok (0, 0)

expect
    graph = parseInput "-S"
    actual = nextPos graph (1, 0) (0, 0)

    actual == Err NotPossible

expect
    graph = parseInput "FS"
    actual = nextPos graph (1, 0) (0, 0)

    actual == Ok (0, 1)

expect
    graph = parseInput "LS"
    actual = nextPos graph (1, 0) (0, 0)

    actual == Err NotPossible

expect
    graph = parseInput
        """
        ..
        LS
        """
    actual = nextPos graph (1, 1) (0, 1)

    actual == Ok (0, 0)

expect
    graph = parseInput
        """
        .
        |
        S
        """
    actual = nextPos graph (0, 2) (0, 1)

    actual == Ok (0, 0)

expect
    graph = parseInput
        """
        |
        S
        """
    actual = nextPos graph (0, 1) (0, 0)

    actual == Err NotPossible

expect
    graph = parseInput
        """
        7
        S
        """
    actual = nextPos graph (0, 1) (0, 0)

    actual == Err NotPossible

expect
    graph = parseInput
        """
        .7
        .S
        """
    actual = nextPos graph (1, 1) (1, 0)

    actual == Ok (0, 0)

expect
    graph = parseInput
        """
        F
        S
        """
    actual = nextPos graph (0, 1) (0, 0)

    actual == Ok (1, 0)

calcLoop : Graph, List Coord -> Result (List Coord) _
calcLoop = \graph, poses ->
    when poses is
        [.., a, b] ->
            when nextPos graph a b is
                Ok pos if pos == graph.start -> Ok poses
                Ok pos -> calcLoop graph (List.append poses pos)
                Err e -> Err e

        _ -> Err InsufficientInput

startLoop : Graph, Dir -> Result (List Coord) _
startLoop = \graph, dir ->
    when goDir graph.start dir is
        Ok secondPos -> calcLoop graph [graph.start, secondPos]
        Err e -> Err e

goDir = \(x, y), dir ->
    when dir is
        N if y > 0 -> Ok (x, y - 1)
        E -> Ok (x + 1, y)
        S -> Ok (x, y + 1)
        W if x > 1 -> Ok (x - 1, y)
        _ -> Err NotPossible

expect
    graph = parseInput
        """
        .....
        .S-7.
        .|.|.
        .L-J.
        .....
        """
    actual = startLoop graph N

    actual == Err NotPossible

expect
    graph = parseInput
        """
        .....
        .S-7.
        .|.|.
        .L-J.
        .....
        """
    actual = startLoop graph E

    actual == Ok [(1, 1), (2, 1), (3, 1), (3, 2), (3, 3), (2, 3), (1, 3), (1, 2)]

expect
    graph = parseInput
        """
        .....
        .S-7.
        .|.|.
        .L-J.
        .....
        """
    actual = startLoop graph S

    actual == Ok [(1, 1), (1, 2), (1, 3), (2, 3), (3, 3), (3, 2), (3, 1), (2, 1)]

expect
    graph = parseInput
        """
        .....
        .S-7.
        .|.|.
        .L-J.
        .....
        """
    actual = startLoop graph W

    actual == Err NotPossible

expect
    graph = parseInput
        """
        7-F7-
        .FJ|7
        SJLL7
        |F--J
        LJ.LJ
        """
    actual = startLoop graph E

    actual == Ok [(0, 2), (1, 2), (1, 1), (2, 1), (2, 0), (3, 0), (3, 1), (3, 2), (4, 2), (4, 3), (3, 3), (2, 3), (1, 3), (1, 4), (0, 4), (0, 3)]

farthestPoint = \graph ->
    [N, E, S, W]
    |> List.walkUntil 0 \_, dir ->
        when startLoop graph dir is
            Ok loop -> Break (List.len loop // 2)
            _ -> Continue 0

expect
    graph = parseInput
        """
        7-F7-
        .FJ|7
        SJLL7
        |F--J
        LJ.LJ
        """
    actual = farthestPoint graph

    actual == 8

# Part 2

tilesInside = \graph, loop ->
    List.map2 loop (List.dropFirst loop 1) \prev, (x, y) ->
        when dirOf prev (x, y) is
            N -> (x + 1, y)
            E -> (x, y + 1)
            S if x > 0 -> (x - 1, y)
            W if y > 0 -> (x, y - 1)
            _ -> graph.start
    |> Set.fromList
    |> Set.difference (Set.fromList loop)
    |> Set.len

expect
    graph = parseInput
        """
        ...........
        .S-------7.
        .|F-----7|.
        .||.....||.
        .||.....||.
        .|L-7.F-J|.
        .|..|.|..|.
        .L--J.L--J.
        ...........
        """
    actual =
        startLoop graph E
        |> Result.map \loop -> tilesInside graph loop

    actual == Ok 4

#expect
#    graph = parseInput
#        """
#        .F----7F7F7F7F-7....
#        .|F--7||||||||FJ....
#        .||.FJ||||||||L7....
#        FJL7L7LJLJ||LJ.L-7..
#        L--J.L7...LJS7F-7L7.
#        ....F-J..F7FJ|L7L7L7
#        ....L7.F7||L7|.L7L7|
#        .....|FJLJ|FJ|F7|.LJ
#        ....FJL-7.||.||||...
#        ....L---J.LJ.LJLJ...
#        """
#    actual =
#        startLoop graph E
#        |> Result.map \loop -> tilesInside graph loop
#
#    actual == Ok 8
