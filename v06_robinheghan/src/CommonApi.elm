module CommonApi exposing (Queue, empty, isEmpty, singleton, fromList, toList, enqueue, dequeue, lengthViaToList, lengthViaDequeue, lengthOriginal)

import Deque


-- Type alias for the common queue interface
type alias Queue a =
    Deque.Deque a


-- Create an empty queue
empty : Queue a
empty =
    Deque.empty


-- Check if the queue is empty
isEmpty : Queue a -> Bool
isEmpty =
    Deque.isEmpty


-- Create a queue with a single element
singleton : a -> Queue a
singleton =
    Deque.singleton


-- Create a queue from a list
fromList : List a -> Queue a
fromList =
    Deque.fromList


-- Convert a queue to a list
toList : Queue a -> List a
toList =
    Deque.toList


-- Add an element to the queue (using pushBack for FIFO behavior)
enqueue : a -> Queue a -> Queue a
enqueue =
    Deque.pushBack


-- Remove and return the first element from the queue (using popFront for FIFO behavior)
dequeue : Queue a -> Maybe ( a, Queue a )
dequeue queue =
    let
        ( maybeValue, newQueue ) =
            Deque.popFront queue
    in
    case maybeValue of
        Nothing ->
            Nothing
        
        Just value ->
            Just ( value, newQueue )


-- Get the length of the queue via toList
lengthViaToList : Queue a -> Int
lengthViaToList queue =
    List.length (Deque.toList queue)


-- Get the length of the queue via dequeue
lengthViaDequeue : Queue a -> Int
lengthViaDequeue queue =
    let
        countElements : Queue a -> Int -> Int
        countElements currentQueue acc =
            case Deque.popFront currentQueue of
                ( Nothing, _ ) ->
                    acc
                
                ( Just _, newQueue ) ->
                    countElements newQueue (acc + 1)
    in
    countElements queue 0


lengthOriginal : Queue a -> Int
lengthOriginal =
    Deque.length