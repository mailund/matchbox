
#' Create an empty stack
#'
#' @return An empty stack
#' @export
empty_stack <- function() NIL

#' Tests if a stack is empty
#'
#' @param stack A stack
#' @return `TRUE` if `stack` is empty, `FALSE` otherwise
#' @export
is_stack_empty <- function(stack) {
    pmatch::cases(
        stack,
        NIL -> TRUE,
        otherwise -> FALSE
    )
}
is_stack_empty <- pmatch::transform_cases_function(is_stack_empty)

#' Push an element onto a stack.
#'
#' @param stack A stack object
#' @param elm   An element to push to the top of the stack
#' @return Updated stack
#' @export
push <- function(stack, elm) CONS(elm, stack)

#' Get the element at the top of the stack.
#'
#' @param stack A non-empty stack
#' @return The element at the top of `stack`
#' @export
top <- function(stack) {
    pmatch::cases(
        stack,
        # the do.call is a trick to make the function pass the
        # byte-compile function
        NIL -> stop("You cannot get the top of an empty stack."),
        CONS(car, cdr) -> car
    )
}
top <- pmatch::transform_cases_function(top)

#' Remove the top element from a stack
#'
#' @param stack A non-empty stack
#' @return A copy of `stack` where the first element has been removed
#' @export
pop <- function(stack) {
    pmatch::cases(
        stack,
        CONS(car, cdr) -> cdr,
        NIL -> stop("You cannot pop an empty stack.")
    )
}
pop <- pmatch::transform_cases_function(pop)
