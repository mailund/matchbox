
#' Colour used in red-black trees.
#' @export
RBT_BLACK <- NULL
#' Colour used in red-black trees.
#' @export
RBT_RED <- NULL
#' Colour used in red-black trees.
#' @export
RBT_DOUBLE_BLACK <- NULL

#' An empty set
#' @export
RBT_SET_EMPTY <- NULL

#' An empty map
#' @export
RBT_MAP_EMPTY <- NULL

#' Constructs a new node in a red-black tree set.
#' @param col   The colour for the node.
#' @param val   The value kept in the node
#' @param left  The left subtree
#' @param right The right subtree
#' @return      A new tree node
#' @export
RBT_SET <- function(col, val, left, right) {}

#' Constructs a new node in a red-black tree map.
#' @param col   The colour for the node.
#' @param key   The key for the map stored in the node
#' @param val   The value kept in the node
#' @param left  The left subtree
#' @param right The right subtree
#' @return      A new tree node
#' @export
RBT_MAP <- function(col, key, val, left, right) {}

#' Print red-black colour objects
#' @param x   The colour
#' @param ... Additional parameters (not used in this version)
#' @export
print.rbt_colour <- function(x, ...) {}
#' Make a string-representation of a red-black colour
#' @param x The set
#' @param ... Additional parameters (not used in this version)
#' @return A string representation of the colour
#' @export
toString.rbt_colour <- function(x, ...) {}

#' Print red-black sets objects
#' @param x   The set
#' @param ... Additional parameters (not used in this version)
#' @export
print.rbt_set <- function(x, ...) {}
#' Make a string-representation of a red-black set.
#' @param x The set
#' @param ... Additional parameters (not used in this version)
#' @return A string representation of the set
#' @export
toString.rbt_set <- function(x, ...) {}

#' Print red-black map objects
#' @param x   The set
#' @param ... Additional parameters (not used in this version)
#' @export
print.rbt_map <- function(x, ...) {}
#' Make a string-representation of a red-black map.
#' @param x The map
#' @param ... Additional parameters (not used in this version)
#' @return A string representation of the map
#' @export
toString.rbt_map <- function(x, ...) {}

pmatch::`:=`(rbt_colours, RBT_BLACK | RBT_RED | RBT_DOUBLE_BLACK)
pmatch::`:=`(rbt_set, RBT_SET_EMPTY | RBT_SET(
    col:rbt_colours,
    val,
    left:rbt_set,
    right:rbt_set
))
pmatch::`:=`(rbt_map, RBT_MAP_EMPTY | RBT_MAP(
    col:rbt_colours,
    key, val,
    left:rbt_map,
    right:rbt_map
))


#' Create an empty red-black tree representation for a set.
#'
#' @export
empty_red_black_set <- function() RBT_SET_EMPTY

#' Check if a tree is empty
#'
#' @param tree Tree we check whether is empty.
#'
#' @export
is_red_black_set_empty <- function(tree) {
    t <- TRUE
    f <- FALSE
    pmatch::cases(
        tree,
        RBT_SET_EMPTY -> t,
        otherwise -> f
    )
}

rbt_set_balance <- function(tree) { # fixme: add deletion transformations
    pmatch::cases(
        tree,
        RBT_SET(RBT_BLACK, z, RBT_SET(RBT_RED, x, a, RBT_SET(RBT_RED, y, b, c)), d) ->
        RBT_SET(RBT_RED, y, RBT_SET(RBT_BLACK, x, a, b), RBT_SET(RBT_BLACK, z, c, d)),
        RBT_SET(RBT_BLACK, z, RBT_SET(RBT_RED, y, RBT_SET(RBT_RED, x, a, b), c), d) ->
        RBT_SET(RBT_RED, y, RBT_SET(RBT_BLACK, x, a, b), RBT_SET(RBT_BLACK, z, c, d)),
        RBT_SET(RBT_BLACK, x, a, RBT_SET(RBT_RED, y, b, RBT_SET(RBT_RED, z, c, d))) ->
        RBT_SET(RBT_RED, y, RBT_SET(RBT_BLACK, x, a, b), RBT_SET(RBT_BLACK, z, c, d)),
        RBT_SET(RBT_BLACK, x, a, RBT_SET(RBT_RED, z, RBT_SET(RBT_RED, y, b, c), d)) ->
        RBT_SET(RBT_RED, y, RBT_SET(RBT_BLACK, x, a, b), RBT_SET(RBT_BLACK, z, c, d)),
        otherwise -> tree
    )
}

make_thunk <- function(f, ...) {
    force(f)
    params <- list(...)
    function() do.call(f, params)
}
trampoline <- function(thunk) {
    while (is.function(thunk)) thunk <- thunk()
    thunk
}

set_make_left_cont <- function(tree, cont) {
    force(tree)
    force(cont)
    function(new_tree) {
        make_thunk(
            cont,
            rbt_set_balance(RBT_SET(tree$col, tree$val, new_tree, tree$right))
        )
    }
}
set_make_right_cont <- function(tree, cont) {
    force(tree)
    force(cont)
    function(new_tree) {
        make_thunk(
            cont,
            rbt_set_balance(RBT_SET(tree$col, tree$val, tree$left, new_tree))
        )
    }
}

rbt_set_insert_ <- function(tree, elm, cont) {
    if (is_red_black_set_empty(tree)) {
        return(
            trampoline(cont(RBT_SET(RBT_RED, elm, RBT_SET_EMPTY, RBT_SET_EMPTY)))
        )
    }

    if (elm < tree$val) {
        rbt_set_insert_(tree$left, elm, set_make_left_cont(tree, cont))
    } else if (elm > tree$val) {
        rbt_set_insert_(tree$right, elm, set_make_right_cont(tree, cont))
    } else {
        trampoline(cont(tree))
    }
}
rbt_set_insert_ <- tailr::loop_transform(rbt_set_insert_)

#' Insert an element into a red-black tree set.
#'
#' @param tree The set
#' @param elm  The element to insert
#' @export
rbt_set_insert <- function(tree, elm) {
    tree <- rbt_set_insert_(tree, elm, identity)
    tree$col <- RBT_BLACK
    tree
}

#' Determines if a red-black tree contains the value `v`
#'
#' @param tree The red-black tree
#' @param v    The value to search for
#' @export
rbt_set_member <- function(tree, v) {
    t <- TRUE
    f <- FALSE
    pmatch::cases(
        tree,
        RBT_SET_EMPTY -> f,
        RBT_SET(col, val, left, right) -> {
            if (val == v) {
                  t
              } else if (val > v) {
                  rbt_set_member(left, v)
              } else {
                  rbt_set_member(right, v)
              }
        }
    )
}
rbt_set_member <- tailr::loop_transform(rbt_set_member)

#' Create an empty red-black tree representation for a map
#'
#' @export
empty_red_black_map <- function() RBT_MAP_EMPTY

#' Check if a tree is empty
#'
#' @param tree Tree we check whether is empty.
#'
#' @export
is_red_black_map_empty <- function(tree) {
    t <- TRUE
    f <- FALSE
    pmatch::cases(
        tree,
        RBT_MAP_EMPTY -> t,
        otherwise -> f
    )
}

rbt_map_balance <- function(tree) { # fixme: add deletion transformations
    pmatch::cases(
        tree,
        RBT_MAP(
            RBT_BLACK,
            zkey, zval,
            RBT_MAP(
                RBT_RED, xkey, xval, a,
                RBT_MAP(RBT_RED, ykey, yval, b, c)
            ),
            d
        ) -> RBT_MAP(
            RBT_RED,
            ykey, yval,
            RBT_MAP(RBT_BLACK, xkey, xval, a, b),
            RBT_MAP(RBT_BLACK, zkey, zval, c, d)
        ),

        RBT_MAP(
            RBT_BLACK,
            zkey, zval,
            RBT_MAP(RBT_RED, ykey, yval, RBT_MAP(RBT_RED, xkey, xval, a, b), c),
            d
        ) -> RBT_MAP(
            RBT_RED,
            ykey, yval,
            RBT_MAP(RBT_BLACK, xkey, xval, a, b),
            RBT_MAP(RBT_BLACK, zkey, zval, c, d)
        ),

        RBT_MAP(
            RBT_BLACK,
            xkey, xval,
            a,
            RBT_MAP(RBT_RED, ykey, yval, b, RBT_MAP(RBT_RED, zkey, zval, c, d))
        ) -> RBT_MAP(
            RBT_RED,
            ykey, yval,
            RBT_MAP(RBT_BLACK, xkey, xval, a, b),
            RBT_MAP(RBT_BLACK, zkey, zval, c, d)
        ),

        RBT_MAP(
            RBT_BLACK,
            xkey, xval,
            a,
            RBT_MAP(RBT_RED, zkey, zval, RBT_MAP(RBT_RED, ykey, yval, b, c), d)
        ) -> RBT_MAP(
            RBT_RED,
            ykey, yval,
            RBT_MAP(RBT_BLACK, xkey, xval, a, b),
            RBT_MAP(RBT_BLACK, zkey, zval, c, d)
        ),

        otherwise -> tree
    )
}


map_make_left_cont <- function(tree, cont) {
    force(tree)
    force(cont)
    function(new_tree) {
        make_thunk(
            cont,
            rbt_map_balance(RBT_MAP(tree$col, tree$key, tree$val, new_tree, tree$right))
        )
    }
}
map_make_right_cont <- function(tree, cont) {
    force(tree)
    force(cont)
    function(new_tree) {
        make_thunk(
            cont,
            rbt_map_balance(RBT_MAP(tree$col, tree$key, tree$val, tree$left, new_tree))
        )
    }
}

rbt_map_insert_ <- function(tree, key, val, cont) {
    if (is_red_black_map_empty(tree)) {
        return(
            trampoline(cont(RBT_MAP(RBT_RED, key, val, RBT_MAP_EMPTY, RBT_MAP_EMPTY)))
        )
    }

    if (key < tree$key) {
        rbt_map_insert_(tree$left, key, val, map_make_left_cont(tree, cont))
    } else if (key > tree$key) {
        rbt_map_insert_(tree$right, key, val, map_make_right_cont(tree, cont))
    } else {
        trampoline(cont(tree))
    }
}
rbt_map_insert_ <- tailr::loop_transform(rbt_map_insert_)

#' Insert an element into a red-black tree map
#'
#' @param tree The set
#' @param key  The key of the element to insert
#' @param val  The value of the element to insert
#' @export
rbt_map_insert <- function(tree, key, val) {
    tree <- rbt_map_insert_(tree, key, val, identity)
    tree$col <- RBT_BLACK
    tree
}


#' Determines if a red-black map contains the key `k`
#'
#' @param tree The red-black tree
#' @param k    The key to search for
#' @export
rbt_map_member <- function(tree, k) {
    t <- TRUE
    f <- FALSE
    pmatch::cases(
        tree,
        RBT_MAP_EMPTY -> f,
        RBT_MAP(col, key, val, left, right) -> {
            if (key == k) {
                  t
              } else if (key > k) {
                  rbt_map_member(left, k)
              } else {
                  rbt_map_member(right, k)
              }
        }
    )
}
rbt_map_member <- tailr::loop_transform(rbt_map_member)
