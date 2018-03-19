
# dummy variables for documentation

otherwise <- NULL

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

pmatch::`:=`(llist, NIL | CONS(car, cdr:llist))

#' Tests if a linked list is empty
#'
#' @param llist The list
#' @export
is_llist_empty <- function(llist) {
    t <- TRUE
    f <- FALSE # to satisfy lintr
    pmatch::cases(
        llist,
        NIL -> t,
        otherwise -> f
    )
}

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

# FIXME: Figure out how to avoid putting this ouside llconcat
mk_closure <- function(car, cnt) {
    force(car)
    force(cnt)
    function(cdr) cnt(CONS(car, cdr))
}
#' Concatenates two linked lists.
#'
#' @param l1  First list.
#' @param l2  Second list.
#' @param cnt Continuation to make the function tail-recursive
#' @return The concatenation of `l1` and `l2`.
#' @export
# FIXME: use a trampoline for cnt.
llconcat <- function(l1, l2, cnt = identity) {
    pmatch::cases(l1,
                  NIL -> cnt(l2),
                  CONS(car, cdr) -> llconcat(cdr, l2, mk_closure(car, cnt)))
}
llconcat <- tailr::loop_transform(llconcat)


#' Checks if a linked list contains an element
#'
#' @param llist The linked list
#' @param elm   The element
#' @return `TRUE` if `elm` is in `llist` and `FALSE` otherwise
#' @export
llcontains <- function(llist, elm) {
    pmatch::cases(
        llist,
        NIL -> FALSE,
        CONS(car, cdr) ->
            if (car == elm) TRUE else llcontains(cdr, elm)
    )
}
llcontains <- tailr::loop_transform(llcontains)

#' Extract the first `k` elements from a list
#'
#' @param llist The linked list
#' @param k     The number of elements to take
#' @param acc   Accumulator to make the function tail-recursive
#' @return The first `k` elements of `llist`
#' @export
lltake <- function(llist, k, acc = NIL) {
    if (k == 0) return(llrev(acc))
    pmatch::cases(
        llist,
        NIL -> stop("There were less than k elements in the list"),
        CONS(car, cdr) -> lltake(cdr, k - 1, CONS(car, acc))
    )
}
lltake <- tailr::loop_transform(lltake)

#' Remove the first `k` elements from a list
#'
#' @param llist The linked list
#' @param k     The number of elements to remove
#' @param acc   Accumulator needed to make the function tail-recursive.
#' @return The list without the first k elements
#' @export
lldrop <- function(llist, k, acc = NIL) {
    if (k == 0) return(llist)
    pmatch::cases(
        llist,
        NIL -> stop("There were less than k elements in the list"),
        CONS(car, cdr) -> lldrop(cdr, k - 1)
    )
}
lldrop <- tailr::loop_transform(lldrop)


#' Map a function over a linked list.
#'
#' @param llist The linked list
#' @param f     The function to apply to all elements in `llist`
#' @param acc   Accumulator to make the function tail-recursive
#' @export
llmap <- function(llist, f, acc = NIL) {
    pmatch::cases(
        llist,
        NIL -> llrev(acc),
        CONS(car, cdr) -> llmap(cdr, f, CONS(f(car), acc))
    )
}
llmap <- tailr::loop_transform(llmap)

#' Remove elements that do not satisfy a predicate
#'
#' @param llist The list
#' @param p     A predicate function
#' @param acc   Accumulator to make the function tail-recursive
#' @export
llfilter <- function(llist, p, acc = NIL) {
    pmatch::cases(
        llist,
        NIL -> llrev(acc),
        CONS(car, cdr) ->
        if (p(car)) {
            llfilter(cdr, p, CONS(car, acc))
        } else {
            llfilter(cdr, p, acc)
        }
    )
}
llfilter <- tailr::loop_transform(llfilter)

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

#' @export
as.list.llist <- function(x, all.names = FALSE, sorted = FALSE, ...) {
    n <- llength(x)
    v <- vector("list", length = n)
    i <- 1
    while (i <= n) {
        v[i] <- x$car
        i <- i + 1
        x <- x$cdr
    }
    v
}

#' @export
as.vector.llist <- function(x, mode = "any") {
    unlist(as.list(x))
}
