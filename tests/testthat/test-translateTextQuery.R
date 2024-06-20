# This tests the translation machinery.
# library(testthat); library(gypsum); source("test-translateTextQuery.R")

test_that("translateTextQuery works for simple requests", {
    out <- translateTextQuery("foobar")
    expect_identical(out$type, "text")
    expect_identical(out$text, "foobar")

    # Works with spaces.
    out <- translateTextQuery("foo\nbar   whee")
    expect_identical(out$type, "and")
    expect_identical(out$children[[1]]$text, "foo")
    expect_identical(out$children[[2]]$text, "bar")
    expect_identical(out$children[[3]]$text, "whee")

    # Works with fields.
    out <- translateTextQuery("stuff:blah")
    expect_identical(out$type, "text")
    expect_identical(out$field, "stuff")
    expect_identical(out$text, "blah")

    # Works with a space after the colon.
    out <- translateTextQuery("stuff: yay blah")
    expect_identical(out$type, "and")
    expect_identical(out$children[[1]]$text, "yay")
    expect_identical(out$children[[1]]$field, "stuff")
    expect_identical(out$children[[2]]$text, "blah")
    expect_identical(out$children[[2]]$field, "stuff")

    # Recognizes partial hits.
    out <- translateTextQuery("foo%")
    expect_identical(out$type, "text")
    expect_identical(out$text, "foo%")
    expect_true(out$partial)

    out <- translateTextQuery("foo% bar")
    expect_identical(out$type, "and")
    expect_identical(out$children[[1]]$text, "foo%")
    expect_true(out$children[[1]]$partial)
    expect_identical(out$children[[2]]$text, "bar")
    expect_false(out$children[[2]]$partial)

    # Fails correctly.
    expect_error(translateTextQuery("\n"), "no search terms")
})

test_that("translateTextQuery works for NOT requests", {
    out <- translateTextQuery("NOT foobar")
    expect_identical(out$type, "not")
    expect_identical(out$child$type, "text")
    expect_identical(out$child$text, "foobar")

    # Works with multiple words.
    out <- translateTextQuery("NOT foo bar")
    expect_identical(out$type, "not")
    expect_identical(out$child$type, "and")
    expect_identical(out$child$children[[1]]$text, "foo")
    expect_identical(out$child$children[[2]]$text, "bar")

    # Adding parentheses and spaces.
    out <- translateTextQuery("NOT ( foobar )")
    expect_identical(out$type, "not")
    expect_identical(out$child$type, "text")
    expect_identical(out$child$text, "foobar")

    out <- translateTextQuery("NOT(foobar)")
    expect_identical(out$type, "not")
    expect_identical(out$child$type, "text")
    expect_identical(out$child$text, "foobar")

    # Fails correctly.
    expect_error(translateTextQuery("NOT "), "no search terms")
    expect_error(translateTextQuery("foo NOT bar"), "illegal placement") 
    expect_error(translateTextQuery("foo NOT(bar)"), "illegal placement") 
    expect_error(translateTextQuery("(foo) NOT (bar)"), "illegal placement") 
    expect_error(translateTextQuery("(foo)NOT(bar)"), "illegal placement") 
})

test_that("translateTextQuery works for AND requests", {
    out <- translateTextQuery("foo AND bar")
    expect_identical(out$type, "and")
    expect_identical(out$children[[1]]$text, "foo")
    expect_identical(out$children[[2]]$text, "bar")

    # Works with multiple words and conditions.
    out <- translateTextQuery("foo bar AND whee stuff AND other")
    expect_identical(out$type, "and")
    expect_identical(out$children[[1]]$type, "and")
    expect_identical(out$children[[1]]$children[[1]]$text, "foo")
    expect_identical(out$children[[1]]$children[[2]]$text, "bar")
    expect_identical(out$children[[2]]$type, "and")
    expect_identical(out$children[[2]]$children[[1]]$text, "whee")
    expect_identical(out$children[[2]]$children[[2]]$text, "stuff")
    expect_identical(out$children[[3]]$text, "other")

    # Works with parentheses.
    out <- translateTextQuery("(foo) AND (bar)")
    expect_identical(out$type, "and")
    expect_identical(out$children[[1]]$text, "foo")
    expect_identical(out$children[[2]]$text, "bar")

    out <- translateTextQuery("(foo)AND(bar)")
    expect_identical(out$type, "and")
    expect_identical(out$children[[1]]$text, "foo")
    expect_identical(out$children[[2]]$text, "bar")

    # Fails correctly.
    expect_error(translateTextQuery("AND"), "trailing AND")
    expect_error(translateTextQuery("asdasd AND"), "trailing AND")
    expect_error(translateTextQuery("AND asdasd"), "no search terms")
})

test_that("translateTextQuery works for OR requests", {
    out <- translateTextQuery("foo OR bar")
    expect_identical(out$type, "or")
    expect_identical(out$children[[1]]$text, "foo")
    expect_identical(out$children[[2]]$text, "bar")

    # Works with multiple words and conditions.
    out <- translateTextQuery("foo bar OR whee stuff OR other")
    expect_identical(out$type, "or")
    expect_identical(out$children[[1]]$type, "and")
    expect_identical(out$children[[1]]$children[[1]]$text, "foo")
    expect_identical(out$children[[1]]$children[[2]]$text, "bar")
    expect_identical(out$children[[2]]$type, "and")
    expect_identical(out$children[[2]]$children[[1]]$text, "whee")
    expect_identical(out$children[[2]]$children[[2]]$text, "stuff")
    expect_identical(out$children[[3]]$text, "other")

    # Works with parentheses.
    out <- translateTextQuery("(foo) OR (bar)")
    expect_identical(out$type, "or")
    expect_identical(out$children[[1]]$text, "foo")
    expect_identical(out$children[[2]]$text, "bar")

    out <- translateTextQuery("(foo)OR(bar)")
    expect_identical(out$type, "or")
    expect_identical(out$children[[1]]$text, "foo")
    expect_identical(out$children[[2]]$text, "bar")

    # Fails correctly.
    expect_error(translateTextQuery("OR"), "trailing AND/OR")
    expect_error(translateTextQuery("asdasd OR"), "trailing AND/OR")
    expect_error(translateTextQuery("OR asdasd"), "no search terms")
})

test_that("translateTextQuery works for complex requests", {
    out <- translateTextQuery("foo AND bar OR NOT whee")
    expect_identical(out$type, "or")
    expect_identical(out$children[[1]]$type, "and")
    expect_identical(out$children[[1]]$children[[1]]$text, "foo")
    expect_identical(out$children[[1]]$children[[2]]$text, "bar")
    expect_identical(out$children[[2]]$type, "not")
    expect_identical(out$children[[2]]$child$text, "whee")

    out <- translateTextQuery("foo AND (bar OR NOT (whee))")
    expect_identical(out$type, "and")
    expect_identical(out$children[[1]]$text, "foo")
    expect_identical(out$children[[2]]$type, "or")
    expect_identical(out$children[[2]]$children[[1]]$text, "bar")
    expect_identical(out$children[[2]]$children[[2]]$type, "not")
    expect_identical(out$children[[2]]$children[[2]]$child$text, "whee")
})
