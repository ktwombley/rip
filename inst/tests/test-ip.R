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

test_that("ip with numeric input doesnt die when ip is too big for IPv4", {
  threw <- throws_error()(ip(168430090 * 16909060))
  expect_that(threw$passed, is_false(), "ip constructor threw an error.")
})

test_that("rip.slash option", {
  #ensure default behavior
  options("rip.slash"=NULL)
  a<-ip("6.6.6.6")
  b<-ip("2.2.2.2")

  #Fake up interactive() 
  real.interactive <- interactive
  unlockBinding('interactive', as.environment('package:base'))
  assign('interactive', function() TRUE, envir=as.environment('package:base')) 

  msg.match <- "Hey! By default I treat `/` as CIDR notation. See \\?`/\\.ip` for details\\."

  expect_that(interactive(), is_true(), "Faking interactive mode failed in test-ip.R")
  expect_that(a/b, shows_message(msg.match), "First invocation, no message")
  message_again <- shows_message(msg.match)(a/b)
  expect_that(message_again$passed, is_false(), "Displayed message a second time")
  
  assign('interactive',real.interactive, envir=as.environment('package:base'))
})

test_that("Ops work", {
  options("rip.slash"="division")

  a <- ip("10.10.10.10")
  b <- ip("1.2.3.4")
  little <- ip("0.0.0.11")

  expect_that(a+b, is_a("ip"), "addition result is not an ip")
  expect_that(a+b, equals(ip("11.12.13.14")), "addition result is incorrect")
  expect_that(a-b, is_a("ip"), "subtraction result is not an ip")
  expect_that(a-b, equals(ip("9.8.7.6")), "subtraction result is incorrect")
  expect_that(a*b, is_a("ip"), "multiplcation result is not an ip")
  expect_that(a*b, equals(ip("::a:1e3c:645a:4628")), "multiplication result is incorrect")
  expect_that(a/b, is_a("ip"), "division result is not an ip")
  expect_that(a/b, equals(ip("0.0.0.9")), "division result is incorrect")
  expect_that(little^little, is_a("ip"), "exponentiation result is not an ip")
  expect_that(little^little, equals(ip("109.230.153.83")), "exponentiation result is incorrect")
  expect_that(a%%b, is_a("ip"), "modulo result is not an ip")
  expect_that(a%%b, equals(ip("0.247.238.230")), "modulo result is incorrect")
  expect_that(a%/%b, is_a("ip"), "integer division result is not an ip")
  expect_that(a%/%b, equals(ip("0.0.0.9")), "integer division result is incorrect")
  expect_that(a&b, is_a("ip"), "and result is not an ip")
  expect_that(a&b, equals(ip("0.2.2.0")), "and result is incorrect")
  expect_that(a|b, is_a("ip"), "or result is not an ip")
  expect_that(a|b, equals(ip("11.10.11.14")), "or result is incorrect")
  expect_that(!a, equals(ip("245.245.245.245")), "not result is incorrect")
  expect_that(!a, is_a("ip"), "not result is not an ip")
})

test_that("ip(numeric) does not give warning about condition length", {
  warned_me <- gives_warning("the condition has length > 1 and only the first element will be used")(ip(2130706433L))
  expect_that(warned_me$passed, is_false(), "ip(2130706433L) threw warning about condition length")
})
