context("test-stacks.R")

test_that("we can create an empty stack", {
    stack <- empty_stack()
    expect_true(is_stack_empty(stack))
})

test_that("we can push and pop stack", {
    stack <- empty_stack()
    expect_true(is_stack_empty(stack))

    stack <- push(stack, 3)
    stack <- push(stack, 2)
    stack <- push(stack, 1)
    expect_false(is_stack_empty(stack))

    expect_equal(top(stack), 1)
    stack <- pop(stack)
    expect_equal(top(stack), 2)
    stack <- pop(stack)
    expect_equal(top(stack), 3)
    stack <- pop(stack)

    expect_true(is_stack_empty(stack))
})


test_that("we get errors if we try to access the top of an empty stack", {
    stack <- empty_stack()
    expect_true(is_stack_empty(stack))

    expect_error(top(stack))
    expect_error(pop(stack))
})
