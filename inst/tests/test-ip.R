context("ip OO basics")

test_that("ip() returns an ip object", {
  ipo <- ip(1L)

  expect_that(ipo, is_a("ip"))  
  
})

test_that("is.ip() works", {
  ipo <- ip(0L)  #That's a zero

  expect_that(is.ip(ipo), is_true())
  expect_that(is.ip("foo"), is_false())
})

test_that("ip() understands numeric input", {
  ipo <- ip(2130706433L)
  expect_that(as.integer(ipo), equals(2130706433L))
  expect_that(as.numeric(ipo), equals(2130706433L))
  expect_that(as.double(ipo), equals(2130706433L))
  expect_that(as.character(ipo), equals("127.0.0.1"))
  
  
})

test_that("ip() understands character strings", {

  tmp <- throws_error()(ip("127.0.0.1"))
  expect_that(tmp$passed, is_false())

  tmp <- gives_warning()(ip("127.0.0.1"))
  expect_that(tmp$passed, is_false(), "ip(\"127.0.0.1\") emitted a warning.")

  ipo <- ip("1:2:3:4:5:6:7:8")
  expect_that(ipo$af, equals(AF_INET6), "ipv6 sets correct AF") 
  expect_that(ipo$address, equals(as.raw(c(0,1,0,2,0,3,0,4,0,5,0,6,0,7,0,8))), "ipv6 sets correct address")

  ipo <- ip("127.0.0.1")
  expect_that(ipo$af, equals(AF_INET), "ipv4 sets correct AF") 
  expect_that(ipo$address, equals(as.raw(c(127,0,0,1))), "ipv4 sets correct address")
 
})
