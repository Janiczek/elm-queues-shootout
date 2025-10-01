module QueueInvariants exposing (suite)

import CommonApi exposing (Queue, empty, isEmpty, singleton, fromList, toList, enqueue, dequeue, lengthViaToList, lengthViaDequeue, lengthOriginal)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list)
import Test exposing (Test)


suite : Test
suite =
    Test.concat
        [ Test.test "empty / isEmpty: isEmpty empty == True" <|
            \_ ->
                isEmpty empty
                    |> Expect.equal True
        , Test.fuzz int "empty / singleton / enqueue: ∀x: enqueue x empty == singleton x" <|
            \x ->
                enqueue x empty
                    |> expectEqualQueue (singleton x)
        , Test.fuzz int "empty / singleton / dequeue: ∀x: dequeue (singleton x) == Just (x, empty)" <|
            \x ->
                dequeue (singleton x)
                    |> Maybe.map (Tuple.mapSecond toList)
                    |> Expect.equal (Just ( x, toList empty ))
        , Test.test "empty / fromList: empty == fromList []" <|
            \_ ->
                empty
                    |> expectEqualQueue (fromList [])
        , Test.test "empty / toList: toList empty == []" <|
            \_ ->
                toList empty
                    |> Expect.equal []
        , Test.test "empty / dequeue: dequeue empty == Nothing" <|
            \_ ->
                dequeue empty
                    |> Expect.equal Nothing
        , Test.test "empty / length: length empty == 0" <|
            \_ ->
                lengthViaToList empty
                    |> Expect.equal 0
        , Test.fuzz int "isEmpty / singleton: ∀x: isEmpty (singleton x) == False" <|
            \x ->
                isEmpty (singleton x)
                    |> Expect.equal False
        , Test.fuzz (list int) "isEmpty / fromList: ∀xs: isEmpty (fromList xs) == List.isEmpty xs" <|
            \xs ->
                isEmpty (fromList xs)
                    |> Expect.equal (List.isEmpty xs)
        , Test.fuzz queueFuzzer "isEmpty / toList: ∀q: isEmpty q == List.isEmpty (toList q)" <|
            \q ->
                isEmpty q
                    |> Expect.equal (List.isEmpty (toList q))
        , Test.fuzz queueFuzzer "isEmpty / length: ∀q: isEmpty q == (length q == 0)" <|
            \q ->
                isEmpty q
                    |> Expect.equal (lengthViaToList q == 0)
        , Test.fuzz int "singleton / fromList: ∀x: singleton x == fromList [x]" <|
            \x ->
                singleton x
                    |> expectEqualQueue (fromList [ x ])
        , Test.fuzz int "singleton / toList: ∀x: toList (singleton x) == [x]" <|
            \x ->
                toList (singleton x)
                    |> Expect.equal [ x ]
        , Test.fuzz int "singleton / length: ∀x: length (singleton x) == 1" <|
            \x ->
                lengthViaToList (singleton x)
                    |> Expect.equal 1
        , Test.fuzz (list int) "fromList / toList: ∀xs: toList (fromList xs) == xs" <|
            \xs ->
                toList (fromList xs)
                    |> Expect.equal xs
        , Test.fuzz2 int (list int) "fromList / enqueue: ∀x,xs: enqueue x (fromList xs) == fromList (xs ++ [x])" <|
            \x xs ->
                enqueue x (fromList xs)
                    |> expectEqualQueue (fromList (xs ++ [ x ]))
        , Test.fuzz (list int) "fromList / length: ∀xs: length (fromList xs) == List.length xs" <|
            \xs ->
                lengthViaToList (fromList xs)
                    |> Expect.equal (List.length xs)
        , Test.fuzz2 int queueFuzzer "toList / enqueue: ∀x,q: toList (enqueue x q) == toList q ++ [x]" <|
            \x q ->
                toList (enqueue x q)
                    |> Expect.equal (toList q ++ [ x ])
        , Test.fuzz queueFuzzer "toList / length: ∀q: length q == List.length (toList q)" <|
            \q ->
                lengthViaToList q
                    |> Expect.equal (List.length (toList q))
        , Test.fuzz2 int queueFuzzer "enqueue / length: ∀x,q: length (enqueue x q) == 1 + length q" <|
            \x q ->
                lengthViaToList (enqueue x q)
                    |> Expect.equal (1 + lengthViaToList q)
        , Test.fuzz queueFuzzer "length: ∀q: lengthOriginal q == lengthViaDequeue q" <|
            \q ->
                lengthOriginal q
                    |> Expect.equal (lengthViaDequeue q)
        , Test.fuzz queueFuzzer "length: ∀q: lengthOriginal q == lengthViaToList q" <|
            \q ->
                lengthOriginal q
                    |> Expect.equal (lengthViaToList q)
        , Test.fuzz queueFuzzer "length: ∀q: lengthViaToList q == lengthViaDequeue q" <|
            \q ->
                lengthViaToList q
                    |> Expect.equal (lengthViaDequeue q)
        ]


queueFuzzer : Fuzzer (Queue Int)
queueFuzzer =
    list int
        |> Fuzz.map fromList


expectEqualQueue : Queue comparable -> Queue comparable -> Expectation
expectEqualQueue q1 q2 =
    Expect.equal (toList q1) (toList q2)
