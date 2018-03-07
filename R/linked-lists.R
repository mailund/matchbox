
# dummy variables for documentation

#' An empty list
#' @export
NIL <- NULL
#' Constructs a new linked list from an element and another list
#' @param car  The head of the new list
#' @param cdr  The remainder of the list
#' @return A new list
#' @export
CONS <- function(car, cdr) {}
#' Print linked list objects
#' @param x   The linked list
#' @param ... Additional parameters (not used in this version)
#' @export
print.llist <- function(x, ...) {}
#' Make a string-representation of a linked list.
#' @param x The linked list
#' @param ... Additional parameters (not used in this version)
#' @return A string representation of the list
#' @export
toString.llist <- function(x, ...) {}

pmatch::`:=`(llist, NIL | CONS(car, cdr : llist))

#' Compute the length of a linked list.
#'
#' This function computes the length of a linked list by scanning
#' through it. This is thus a linear time function.
#'
#' @param llist The linked list
#' @param acc   An accumulator (for making the function tail-recursive)
#' @return The length of `llist`
#'
#' @export
llength <- function(llist, acc = 0) {
    pmatch::cases(
        llist,
        NIL -> acc,
        CONS(car, cdr) -> llength(cdr, acc + 1)
    )
}
llength <- tailr::loop_transform(llength)

#' Reverse a linked list.
#'
#' This function scans through `llist`, building the reverse list in `acc`.
#'
#' @param llist A linked list
#' @param acc   An accumulator (for making the function tail-recursive)
#' @return A list containing the elements in `llist` in reverse order.
#'
#' @export
llrev <- function(llist, acc = NIL) {
    pmatch::cases(
        llist,
        NIL -> acc,
        CONS(car, cdr) -> llrev(cdr, CONS(car, acc))
    )
}
llrev <- tailr::loop_transform(llrev)


#' Translate a list object into a linked list.
#'
#' @param x A `list` object
#' @return A linked list containing the elements in `x`
#'
#' @export
llist_from_list <- function(x) {
    llist <- NIL
    n <- length(x)
    while (n > 0) {
        llist <- CONS(x[[n]], llist)
        n <- n - 1
    }
    llist
}
