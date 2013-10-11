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
  threw_it <- throws_error("raw vector length does not agree with AF.")(as.character(ipo))
  expect_that(threw_it$passed, is_false(), "Threw vector did not agree with AF error.")
  expect_that(as.integer(ipo), equals(2130706433L))
  expect_that(as.numeric(ipo), equals(2130706433L))
  expect_that(as.double(ipo), equals(2130706433L))
  expect_that(as.character(ipo), equals("127.0.0.1"))
    
})

test_that("ip(numeric x) internal structure is correct", {
  ipo <- ip(185273099)
  expect_that(ipo$address, is_a("raw"), "address vector is not raw")
})

test_that("ip() understands character strings", {

  tmp <- throws_error()(ip("127.0.0.1"))
  expect_that(tmp$passed, is_false())

  tmp <- gives_warning()(ip("127.0.0.1"))
  expect_that(tmp$passed, is_false(), "ip(\"127.0.0.1\") did not emit a warning.")

  ipo <- ip("1:2:3:4:5:6:7:8")
  expect_that(ipo$af, equals(AF_INET6), "ipv6 sets correct AF") 
  expect_that(ipo$address, equals(as.raw(c(0,1,0,2,0,3,0,4,0,5,0,6,0,7,0,8))), "ipv6 set incorrect address")

  ipo <- ip("127.0.0.1")
  expect_that(ipo$af, equals(AF_INET), "ipv4 sets correct AF") 
  expect_that(ipo$address, equals(as.raw(c(127,0,0,1))), "ipv4 set incorrect address")
 
})

test_that("ip understands raw byte input", {
  ipo <- ip(as.raw(c(10,10,10,10)))
  expect_that(ipo$af, equals(AF_INET))
  expect_that(ipo$address, equals(as.raw(c(10,10,10,10))))
  expect_that(ipo, equals(ip("10.10.10.10")))
  expect_that(ip(as.raw(c(1,2,3))), throws_error(), "Creating IP with wrong-sized rawlist succeeded.")
})


test_that("Ops work", {
  a <- ip("10.10.10.10")
  b <- ip("1.2.3.4")

  c <- a + b
  d <- a - b
  e <- a * b
  f <- a / b
  g <- a ^ b
  h <- a %% b
  i <- a %/% b
  j <- a & b
  k <- a && b
  l <- a | b
  m <- a || b
  n <- !a

  expect_that(c, is_a("ip"), "addition result is not an ip")
  expect_that(c, equals(ip("11.12.13.14")), "addition result is incorrect")
  expect_that(d, is_a("ip"), "subtraction result is not an ip")
  expect_that(d, equals(ip("11.12.13.14")), "subtraction result is incorrect")
  expect_that(e, is_a("ip"), "multiplcation result is not an ip")
  expect_that(e, equals(ip("11.12.13.14")), "multiplication result is incorrect")
  expect_that(f, is_a("ip"), "division result is not an ip")
  expect_that(f, equals(ip("11.12.13.14")), "division result is incorrect")
  expect_that(g, is_a("ip"), "exponentiation result is not an ip")
  expect_that(g, equals(ip("11.12.13.14")), "exponentiation result is incorrect")
  expect_that(h, is_a("ip"), "modulo result is not an ip")
  expect_that(h, equals(ip("11.12.13.14")), "modulo result is incorrect")
  expect_that(i, is_a("ip"), "modulo remainder result is not an ip")
  expect_that(i, equals(ip("11.12.13.14")), "modulo remainder result is incorrect")
  expect_that(j, is_a("ip"), "and result is not an ip")
  expect_that(j, equals(ip("11.12.13.14")), "and result is incorrect")
  expect_that(k, is_a("ip"), "and2 result is not an ip")
  expect_that(k, equals(ip("11.12.13.14")), "and2 result is incorrect")
  expect_that(l, is_a("ip"), "or result is not an ip")
  expect_that(l, equals(ip("11.12.13.14")), "or result is incorrect")
  expect_that(m, is_a("ip"), "or2 result is not an ip")
  expect_that(m, equals(ip("11.12.13.14")), "or2 result is incorrect")
  expect_that(n, is_a("ip"), "not result is not an ip")
  expect_that(n, equals(ip("11.12.13.14")), "not result is incorrect")

})

test_that("ip(numeric) does not give warning about condition length", {
  warned_me <- gives_warning("the condition has length > 1 and only the first element will be used")(ip(2130706433L))
  expect_that(warned_me$passed, is_false(), "ip(2130706433L) threw warning about condition length")
})
