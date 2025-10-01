module CommonApi exposing (Queue, empty, isEmpty, singleton, fromList, toList, enqueue, dequeue, lengthViaToList, lengthViaDequeue)

import Fifo


-- Type alias for the common queue interface
type alias Queue a =
    Fifo.Fifo a


-- Create an empty queue
empty : Queue a
empty =
    Fifo.empty


-- Check if the queue is empty
isEmpty : Queue a -> Bool
isEmpty queue =
    case Fifo.remove queue of
        ( Nothing, _ ) ->
            True
        
        ( Just _, _ ) ->
            False


-- Create a queue with a single element
singleton : a -> Queue a
singleton value =
    Fifo.fromList [ value ]


-- Create a queue from a list
fromList : List a -> Queue a
fromList =
    Fifo.fromList


-- Convert a queue to a list
toList : Queue a -> List a
toList =
    Fifo.toList


-- Add an element to the queue
enqueue : a -> Queue a -> Queue a
enqueue =
    Fifo.insert


-- Remove and return the first element from the queue
dequeue : Queue a -> Maybe ( a, Queue a )
dequeue queue =
    let
        ( maybeValue, newQueue ) =
            Fifo.remove queue
    in
    case maybeValue of
        Nothing ->
            Nothing
        
        Just value ->
            Just ( value, newQueue )


-- Get the length of the queue via toList
lengthViaToList : Queue a -> Int
lengthViaToList queue =
    List.length (Fifo.toList queue)


-- Get the length of the queue via dequeue
lengthViaDequeue : Queue a -> Int
lengthViaDequeue queue =
    let
        countElements : Queue a -> Int -> Int
        countElements currentQueue acc =
            case Fifo.remove currentQueue of
                ( Nothing, _ ) ->
                    acc
                
                ( Just _, newQueue ) ->
                    countElements newQueue (acc + 1)
    in
    countElements queue 0

