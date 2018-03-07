pmatch::`:=`(llist, NIL | CONS(car, cdr : llist))

#' @export
llength <- function(llist, acc = 0) {
    pmatch::cases(
        llist,
        NIL -> acc,
        CONS(car, cdr) -> llength(cdr, acc + 1)
    )
}
llength <- tailr::loop_transform(llength)

#' @export
llrev <- function(llist, acc = NIL) {
    pmatch::cases(
        llist,
        NIL -> acc,
        CONS(car, cdr) -> llrev(cdr, CONS(car, acc))
    )
}
llrev <- tailr::loop_transform(llrev)


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
