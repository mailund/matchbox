
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
is_stack_empty <- case_func(
    NIL -> TRUE,
    .   -> FALSE

)

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
top <- pmatch::case_func(
    NIL -> stop("You cannot get the top of an empty stack."),
    CONS(car, cdr) -> car
)

#' Remove the top element from a stack
#'
#' @param stack A non-empty stack
#' @return A copy of `stack` where the first element has been removed
#' @export
pop <- pmatch::case_func(
    CONS(car, cdr) -> cdr,
    NIL -> stop("You cannot pop an empty stack.")
)
