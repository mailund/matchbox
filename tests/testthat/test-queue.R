context("test-queue.R")

test_that("we can construct an empty queue", {
    queue <- empty_queue()
    expect_true(is_queue_empty(queue))
})

test_that("we enqueue and dequeue", {
    queue <- empty_queue()
    expect_true(is_queue_empty(queue))

    queue <- enqueue(queue, 1)
    queue <- enqueue(queue, 2)
    queue <- enqueue(queue, 3)

    library(pmatch)
    bind[front, queue] <- front(queue)
    expect_equal(front, 1)
    expect_equal(as.vector(queue$front), 1:3)
    expect_true(is_llist_empty(queue$back))

    queue <- dequeue(queue)
    expect_equal(as.vector(queue$front), 2:3)
    expect_true(is_llist_empty(queue$back))
})
