# make CMD CHECK shut up!
car <- cdr <- NULL


#' Create an empty queue
#' @export
empty_queue <- function() list(front = NIL, back = NIL)

#' Tests if a queue is empty
#'
#' @param queue The queue
#' @export
is_queue_empty <- function(queue) {
    ll_is_nil(queue$front) && ll_is_nil(queue$back)
}

#' Add an element to the back of a queue
#'
#' @param queue The queue
#' @param elm   An element to add to the queue
#' @return The modified queue
#' @export
enqueue <- function(queue, elm) {
    queue$back <- CONS(elm, queue$back)
    queue
}

#' Move elements from the back list to the front list
#' @param queue A queue
#' @return modified queue
move_lists <- function(queue) {
    # only move if the front list is empty
    if (ll_is_nil(queue$front)) {
        queue$front <- llrev(queue$back)
        queue$back <- NIL
    }
    queue
}

#' Get the element at the front of a queue
#'
#' @param queue The queue
#' @return a list containing two values, the element at the front of the queue
#'         as well as the modified queue.
#' @export
front <- function(queue) {
    queue <- move_lists(queue)
    ## FIXME: not that elegant
    match <- pmatch::case_func(
        NIL -> stop("You cannot get the top element from an empty queue"),
        CONS(car, cdr) -> car
    )
    front <- match(queue$front)
    list(front = front, queue = queue)
}



#' Remove an element from the front of a queue
#'
#' @param queue The queue
#' @return The modified queue
#' @export
dequeue <- function(queue) {
    queue <- move_lists(queue)
    match <- pmatch::case_func(
        NIL -> stop("You cannot get the top element from an empty queue"),
        CONS(car, cdr) -> cdr
    )
    queue$front <- match(queue$front)
    queue
}
