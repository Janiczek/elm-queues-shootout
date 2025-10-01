module CommonApi exposing (Queue, empty, isEmpty, singleton, fromList, toList, enqueue, dequeue, lengthViaToList, lengthViaDequeue)

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
fromList =
    Queue.fromList


-- Convert a queue to a list
toList : Queue a -> List a
toList =
    Queue.toList


-- Add an element to the queue
enqueue : a -> Queue a -> Queue a
enqueue =
    Queue.enqueue


-- Remove and return the first element from the queue
dequeue : Queue a -> Maybe ( a, Queue a )
dequeue =
    Queue.dequeue


-- Get the length of the queue via toList
lengthViaToList : Queue a -> Int
lengthViaToList queue =
    List.length (Queue.toList queue)


-- Get the length of the queue via dequeue
lengthViaDequeue : Queue a -> Int
lengthViaDequeue queue =
    let
        countElements : Queue a -> Int -> Int
        countElements currentQueue acc =
            case Queue.dequeue currentQueue of
                Nothing ->
                    acc
                
                Just ( _, newQueue ) ->
                    countElements newQueue (acc + 1)
    in
    countElements queue 0

