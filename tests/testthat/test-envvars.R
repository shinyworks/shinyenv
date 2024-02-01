test_that("shiny_setenv sets values", {
  session <- rlang::env(userData = rlang::env())
  expect_identical(
    shiny_setenv(foo = "bar", baz = "foo", session = session),
    c(TRUE, TRUE)
  )
  expect_contains(names(session$userData), c("foo", "baz"))
  expect_equal(session$userData$foo, "bar")
  expect_equal(session$userData$baz, "foo")
})

test_that("shiny_getenv gets values", {
  session <- rlang::env(userData = rlang::env(foo = "bar", baz = "foo"))
  expect_equal(
    shiny_getenv(c("foo", "baz"), session = session),
    c("bar", "foo")
  )
})

test_that("shiny_getenv returns proper unset values", {
  session <- rlang::env(userData = rlang::env(foo = "bar", baz = "foo"))
  expect_equal(
    shiny_getenv(c("foo", "baz", "qux"), unset = "unset", session = session),
    c("bar", "foo", "unset")
  )
})

test_that("shiny_unsetenv unsets values", {
  session <- rlang::env(
    userData = rlang::env(foo = "bar", baz = "foo", bar = "baz")
  )
  expect_silent({
    test_result <- shiny_unsetenv(c("foo", "baz"), session = session)
  })
  expect_type(test_result, "environment")
  expect_false("foo" %in% names(test_result))
  expect_false("baz" %in% names(test_result))
  expect_contains(names(test_result), "bar")
  expect_false("foo" %in% names(session$userData))
  expect_false("baz" %in% names(session$userData))
  expect_contains(names(session$userData), "bar")
})
