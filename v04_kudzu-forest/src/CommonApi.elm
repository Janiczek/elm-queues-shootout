module CommonApi exposing (Queue, empty, isEmpty, singleton, fromList, toList, enqueue, dequeue, lengthViaToList, lengthViaDequeue, lengthOriginal)

import Queue


-- Type alias for the common queue interface
type alias Queue a =
    Queue.Queue a


-- Create an empty queue
empty : Queue a
empty =
    Queue.empty


-- Check if the queue is empty
isEmpty : Queue a -> Bool
isEmpty =
    Queue.isEmpty


-- Create a queue with a single element
singleton : a -> Queue a
singleton value =
    Queue.enqueue value Queue.empty


-- Create a queue from a list
fromList : List a -> Queue a
fromList list =
    List.foldl Queue.enqueue Queue.empty list


-- Convert a queue to a list
toList : Queue a -> List a
toList =
    Queue.toListFIFO


-- Add an element to the queue
enqueue : a -> Queue a -> Queue a
enqueue =
    Queue.enqueue


-- Remove and return the first element from the queue
dequeue : Queue a -> Maybe ( a, Queue a )
dequeue queue =
    case Queue.head queue of
        Nothing ->
            Nothing
        
        Just value ->
            let
                newQueue =
                    Queue.dequeue queue
            in
            Just ( value, newQueue )


-- Get the length of the queue via toList
lengthViaToList : Queue a -> Int
lengthViaToList queue =
    List.length (Queue.toListFIFO queue)


-- Get the length of the queue via dequeue
lengthViaDequeue : Queue a -> Int
lengthViaDequeue queue =
    let
        countElements : Queue a -> Int -> Int
        countElements currentQueue acc =
            case Queue.head currentQueue of
                Nothing ->
                    acc
                
                Just _ ->
                    let
                        newQueue =
                            Queue.dequeue currentQueue
                    in
                    countElements newQueue (acc + 1)
    in
    countElements queue 0

lengthOriginal : Queue a -> Int
lengthOriginal =
    Queue.length