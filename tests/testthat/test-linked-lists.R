context("test-linked-lists.R")

test_that("we can construct linked lists", {
    lst <- NIL
    expect_equal(
        pmatch::cases(lst, NIL -> 1),
        1
    )
    expect_true(is_llist_empty(lst))

    lst <- CONS(2, NIL)
    expect_equal(
        pmatch::cases(
            lst,
            NIL -> 1,
            CONS(elm, NIL) -> elm
        ),
        2
    )
    expect_false(is_llist_empty(lst))
})

test_that("we can compute the length of a list", {
    lst <- CONS(1, CONS(2, CONS(3, NIL)))
    expect_equal(llength(lst), 3)
})

test_that("we can reverse a list", {
    lst <- CONS(1, CONS(2, CONS(3, NIL)))
    rlst <- llrev(lst)
    expect_equal(as.vector(rlst), c(3, 2, 1))
})

test_that("we can concatenate two lists", {
    l1 <- CONS(1, CONS(2, CONS(3, NIL)))
    l2 <- CONS(4, CONS(5, CONS(6, NIL)))
    expect_equal(as.vector(llconcat(l1, l2)), as.numeric(1:6))
})

test_that("we can check for containment", {
    lst <- llist_from_list(1:3)
    expect_false(llcontains(lst, 0))
    expect_true(llcontains(lst, 1))
    expect_true(llcontains(lst, 2))
    expect_true(llcontains(lst, 3))
    expect_false(llcontains(lst, 4))
})

test_that("we can take and drop", {
    lst <- llist_from_list(1:4)
    first <- lltake(lst, 2)
    last <- lldrop(lst, 2)
    expect_equal(as.vector(first), 1:2)
    expect_equal(as.vector(last), 3:4)
})

test_that("we can map over a list", {
    lst <- llist_from_list(1:4)
    lst2 <- llmap(lst, function(x) 2 * x)
    expect_equal(as.vector(lst2), 2 * 1:4)
})

test_that("we can filter a list", {
    lst <- llist_from_list(1:4)
    first <- llfilter(lst, function(n) n <= 2)
    last <- llfilter(lst, function(n) n > 2)
    expect_equal(as.vector(first), 1:2)
    expect_equal(as.vector(last), 3:4)
})

test_that("we can translate a list into a linked list", {
    ll <- llist_from_list(1:3)
    expect_true(pmatch::cases(ll, CONS(1, cdr) -> TRUE))
    expect_true(pmatch::cases(ll, CONS(1, CONS(2, cdr)) -> TRUE))
    expect_true(pmatch::cases(ll, CONS(1, CONS(2, CONS(3, NIL))) -> TRUE))
})

test_that("we can translate a linked list into a list and a vector", {
    l <- as.list(1:3)
    ll <- llist_from_list(l)
    expect_equal(l, as.list(ll))
    expect_equal(1:3, as.vector(ll))
})

test_that("we can compute the length of a linked lists", {
    ll <- llist_from_list(1:10)
    expect_equal(llength(ll), 10)
})

test_that("we can reverse a list", {
    ll <- llist_from_list(1:3)
    rev_ll <- llrev(ll)
    rev_v <- as.vector(rev_ll)
    expect_equal(rev_v, c(3, 2, 1))
})

test_that("we can test for containment", {
    ll <- llist_from_list(1:3)
    expect_true(llcontains(ll, 1))
    expect_true(llcontains(ll, 2))
    expect_true(llcontains(ll, 3))
    expect_false(llcontains(ll, 4))
})

test_that("we can take the first k elements from a list", {
    ll <- llist_from_list(1:5)
    expect_equal(1:3, as.vector(lltake(ll, 3)))
    expect_error(lltake(ll, 6))
})

