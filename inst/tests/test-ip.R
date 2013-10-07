context("OO basics")

test_that("ip() returns an ip object", {
  ob <- ip(1L)

  expect_that(ob, is_a("ip"))  
  
})

test_that("is.ip() works", {
  ob <- ip(0L)  #That's a zero

  expect_that(is.ip(ob), is_true())
  expect_that(is.ip("foo"), is_false())
})

test_that("type conversion", {
  ob <- ip(2130706433L)
  expect_that(as.integer(ob), equals(2130706433L))
  expect_that(as.numeric(ob), equals(2130706433L))
  expect_that(as.double(ob), equals(2130706433L))
  expect_that(as.character(ob), equals("127.0.0.1"))
  
  
})