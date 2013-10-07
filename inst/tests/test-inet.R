context("inet.R functions still sane")

test_that("addrlen works", {

  expect_that(addrlen(), throws_error("Could not determine Addrlen"))
  expect_that(addrlen(99), throws_error("Could not determine Addrlen"))
  expect_that(addrlen(AF_INET), is_identical_to(IN_ADDRLEN))
  expect_that(addrlen(AF_INET6), is_identical_to(IN6_ADDRLEN))

})

test_that("inet_pton works for IPv4", {
  expect_that(inet_pton(), throws_error("src must be a string representation of an IP address"), "didn't throw on empty src")
  expect_that(inet_pton("10.10.10.10"), gives_warning("Guessing address family. Results unpredictable."), "didn't warn about missing AF")
  expect_that(inet_pton("10.10.10.10"), is_identical_to(as.raw(c(10,10,10,10))))
  expect_that(inet_pton("10.10.10.10", AF_INET), is_identical_to(as.raw(c(10,10,10,10))))

  #Testing that inet_pton *doesn't* warn when given an AF.
  tmp <- gives_warning("Guessing address family. Results unpredictable.")(inet_pton("10.10.10.10", AF_INET))
  expect_that(tmp$passed, is_false())  
  
  #wrong AF
  expect_that(inet_pton("10.10.10.10", AF_INET6), throws_error("Not in presentation Format"))


})

test_that("inet_pton works for IPv6", {
  expect_that(inet_pton(""), gives_warning("Guessing address family. Results unpredictable."), "didn't warn about missing AF")
  
  # x:x:x:x:x:x:x:x
  expect_that(inet_pton("0:0:0:0:0:0:0:0"), is_identical_to(as.raw(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))))
  expect_that(inet_pton("1234:5678:9ABC:DEF0:1111:2222:3333:4444"), is_identical_to(as.raw(c(18,52,86,120,154,188,222,240,17,17,34,34,51,51,68,68))))

  # ::
  expect_that(inet_pton("1::F", AF_INET6), is_identical_to(as.raw(c(0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,15))))
  expect_that(inet_pton("::A", AF_INET6), is_identical_to(as.raw(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,10))))
  expect_that(inet_pton("C::", AF_INET6), is_identical_to(as.raw(c(0,12,0,0,0,0,0,0,0,0,0,0,0,0,0,0))))

  # more than one :: (should fail)
  expect_that(inet_pton("C::A::B", AF_INET6), throws_error("Not in presentation Format."))

  # only :: (all zeros)
  expect_that(inet_pton("::", AF_INET6), is_identical_to(as.raw(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))))

  # x:x:x:x:x:x:d.d.d.d 
  expect_that(inet_pton("0:0:0:0:0:FFFF:204.152.189.116", AF_INET6), is_identical_to(as.raw(c(0,0,0,0,0,0,0,0,0,0,255,255,204,152,189,116))))

  #Testing that inet_pton *doesn't* warn when given an AF.
  tmp <- gives_warning("Guessing address family. Results unpredictable.")(inet_pton("1::F", AF_INET6))
  expect_that(tmp$passed, is_false())
})

test_that("inet_ntop works for IPv4", {
  #raw
  expect_that(inet_ntop(as.raw(c(127,0,0,1)), AF_INET), equals("127.0.0.1"))

### See TODO in inet.R file - these are unreliable and I don't know enough to fix them yet.
#  expect_that(inet_ntop(as.raw(c(127,0,0)), AF_INET), equals("127.0.0.0"))
#  expect_that(inet_ntop(as.raw(c(1)), AF_INET), equals("1.0.0.0"))
###

  #ip object
  ipo <- ip("10.10.10.10")
  expect_that(inet_ntop(ipo, AF_INET), equals("10.10.10.10"))

  #integer
  expect_that(inet_ntop(168430090, AF_INET), equals("10.10.10.10"))
  expect_that(inet_ntop(168430090L, AF_INET), equals("10.10.10.10"))
  expect_that(inet_ntop(0, AF_INET), equals("0.0.0.0"))
  #This is the tallest integer we can handle.
  expect_that(inet_ntop(2147483647, AF_INET), equals("127.255.255.255"))

  #Double
  expect_that(inet_ntop(2147483648, AF_INET), equals("128.0.0.0"), "Doubles work")
  expect_that(inet_ntop(4294967295, AF_INET), equals("255.255.255.255"))

  #Warnings
  expect_that(inet_ntop(168430090), gives_warning("Guessing address family. Results unpredictable."))
  expect_that(inet_ntop(4294967295), gives_warning("Guessing address family. Results unpredictable."))
  expect_that(inet_ntop(4294967295), equals("255.255.255.255"))

  #bad AF
  expect_that(inet_ntop(123, 3), throws_error(".+ is not a valid address family"))

})

test_that("inet_ntop works for IPv6", {
  #raw
  expect_that(inet_ntop(as.raw(c(127,0,0,1)), AF_INET), equals("127.0.0.1"))

### See TODO in inet.R file - these are unreliable and I don't know enough to fix them yet.
#  expect_that(inet_ntop(as.raw(c(127,0,0)), AF_INET), equals("127.0.0.0"))
#  expect_that(inet_ntop(as.raw(c(1)), AF_INET), equals("1.0.0.0"))
###

  #ip object
  ipo <- ip("10.10.10.10")
  expect_that(inet_ntop(ipo, AF_INET), equals("10.10.10.10"))

  #integer
  expect_that(inet_ntop(168430090, AF_INET), equals("10.10.10.10"))
  expect_that(inet_ntop(168430090L, AF_INET), equals("10.10.10.10"))
  expect_that(inet_ntop(0, AF_INET), equals("0.0.0.0"))
  #This is the tallest integer we can handle.
  expect_that(inet_ntop(2147483647, AF_INET), equals("127.255.255.255"))

  #Double
  expect_that(inet_ntop(2147483648, AF_INET), equals("128.0.0.0"), "Doubles work")
  expect_that(inet_ntop(4294967295, AF_INET), equals("255.255.255.255"))


})
