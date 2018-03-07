context("test-linked-lists.R")

test_that("we can construct linked lists", {
    lst <- NIL
    expect_equal(
        pmatch::cases(lst, NIL -> 1),
        1
    )

    lst <- CONS(2, NIL)
    expect_equal(
        pmatch::cases(lst,
                       NIL -> 1,
                       CONS(elm, NIL) -> elm),
        2
    )
})
