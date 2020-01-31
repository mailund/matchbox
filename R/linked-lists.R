
# dummy variables for documentation

#' An empty list
#' @export
NIL <- NULL
#' Constructs a new linked list from an element and another list
#' @param car  The head of the new list
#' @param cdr  The remainder of the list
#' @return A new list
#' @export
CONS <- function(car, cdr) {} # nolint
#' Print linked list objects
#' @param x   The linked list
#' @param ... Additional parameters (not used in this version)
#' @export
print.llist <- function(x, ...) {} # nolint
#' Make a string-representation of a linked list.
#' @param x The linked list
#' @param ... Additional parameters (not used in this version)
#' @return A string representation of the list
#' @export
toString.llist <- function(x, ...) {} # nolint

pmatch::`:=`(llist, NIL | CONS(car, cdr:llist))

#' Tests if a list is empty.
#'
#' @param llist The linked list
#' @return `TRUE` if `llist` is empty (i.e. equal to `NIL`) and `FALSE`
#'     otherwise.
#' @export
ll_is_nil <- pmatch::case_func(
    NIL -> TRUE,
    . -> FALSE
)

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
llength <- pmatch::case_trfunc(acc = 0,
    NIL -> acc,
    CONS(car, cdr) -> llength(cdr, acc + 1)
)

#' Reverse a linked list.
#'
#' This function scans through `llist`, building the reverse list in `acc`.
#'
#' @param llist A linked list
#' @param acc   An accumulator (for making the function tail-recursive)
#' @return A list containing the elements in `llist` in reverse order.
#'
#' @export
llrev <- pmatch::case_trfunc(acc = NIL,
    NIL -> acc,
    CONS(car, cdr) -> llrev(cdr, CONS(car, acc))
)

#' Tests if an element is contained in a list
#'
#' @param llist A linked list
#' @param elm   An element
#' @return `TRUE`` if `elm` is in `llist` and `FALSE` otherwise
#'
#' @export
llcontains <- pmatch::case_trfunc(elm,
    NIL -> FALSE,
    CONS(car, cdr) -> if (elm == car) TRUE else llcontains(cdr, elm)
)


#' Extract the first `k` elements from a linked list.
#'
#' @param llist A linked list
#' @param k     The number of elements to extract
#' @return A new linked list containing the first `k` elements of `llist`.
#'
#' @export
lltake <- pmatch::case_trfunc(k, acc = NIL,
    NIL -> stop("There are not k elements in the list"),
    CONS(car, cdr) ->
        if (k == 0) llrev(acc) else lltake(cdr, k - 1, CONS(car, acc))
)


#' Map a function over a linked list.
#'
#' @param llist A linked list
#' @param f     A function
#' @return A new linked list constructed by applying `f` to all elements of `llist`
#'
#' @export
llmap <- function(llist, f, acc = NIL) {
    pmatch::cases(
        llist,
        NIL -> llrev(acc),
        CONS(car, cdr) -> llmap(cdr, f, CONS(f(car), acc))
    )
}
llmap <- tailr::loop_transform(llmap)

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

#' Translate a linked list into a list.
#'
#' @param x The linked list
#' @return A `list` object containing the same elements as the linked list.
#' @export
as.list.llist <- function(x, ...) {
    n <- llength(x)
    l <- vector("list", length = n)
    for (i in seq_along(l)) {
        l[[i]] <- x[[1]]
        x <- x$cdr
    }
    l
}

#' Translate a linked list into a vector
#'
#' @param x The linked list
#' @return A vector object containing the same elements as the linked list.
#' @export
as.vector.llist <- function(x, ...) {
    unlist(as.list(x))
}
