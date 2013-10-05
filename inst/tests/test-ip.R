context("OO basics")

test_that("ip() returns an ip object", {
  ob <- ip(0L)  #That's a zero

  expect_that(ob, is_a("ip"))
  expect_that(as.integer(ob), equals(0L))
})
