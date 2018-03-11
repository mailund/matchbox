context("test-red-black-trees.R")

test_that("we can create a red-black tree and insert into it", {
    tree <- empty_red_black_set()
    expect_true(is_red_black_set_empty(tree))

    tree <- empty_red_black_map()
    expect_true(is_red_black_map_empty(tree))
})

test_that("we can insert into a red-black tree set and check membership", {
    tree <- empty_red_black_set()
    expect_true(is_red_black_set_empty(tree))

    for (v in sample(1:10)) {
        tree <- rbt_set_insert(tree, v)
    }
    expect_false(is_red_black_set_empty(tree))

    expect_false(rbt_set_member(tree, 0))
    for (v in sample(1:10)) {
        expect_true(rbt_set_member(tree, v))
    }
    expect_false(rbt_set_member(tree, 11))
})

test_that("we can insert into a red-black tree map and check membership", {
    tree <- empty_red_black_map()
    expect_true(is_red_black_map_empty(tree))

    for (v in sample(1:10)) {
        tree <- rbt_map_insert(tree, v, v)
    }
    expect_false(is_red_black_map_empty(tree))

    expect_false(rbt_map_member(tree, 0))
    for (v in sample(1:10)) {
        expect_true(rbt_map_member(tree, v))
    }
    expect_false(rbt_map_member(tree, 11))
})
