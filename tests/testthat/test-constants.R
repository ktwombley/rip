context("enums and constants")

test_that("enum works", {
  enum(n="foo", list(foobar=1,
                     foobaz=2,
                     fooquux=3))
  expect_that(exists("foo"), is_true())
  expect_that(exists("foobar"), is_true())
  expect_that(exists("foobaz"), is_true())
  expect_that(exists("fooquux"), is_true())
  expect_that(foo, is_a("list"))
  expect_that(foobar, equals(1))
  expect_that(foobaz, equals(2))
  expect_that(fooquux, equals(3))
  expect_that(foo_ok(foobar), is_true())
  expect_that(foo_ok(99), is_false())
  
})

test_that("_ok works", {
  enum(n="foo", list(foobar=1,
                     foobaz=2,
                     fooquux=3))
  expect_that(foo_ok(foobar), is_true())
  expect_that(foo_ok(99), is_false())
  expect_that(foo_ok(NULL), is_false())
  expect_that(foo_ok(NA), is_false())
  expect_that(foo_ok(TRUE), is_false())
  expect_that(foo_ok(FALSE), is_false())
  
})
