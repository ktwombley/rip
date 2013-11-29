context("inet.R functions still sane")

test_that("C functions are loaded", {
  pton_err <- throws_error("C symbol name \"_inet_pton\" not in load table")(.Call('_inet_pton', "1.1.1.1", AF_INET))
  expect_that(pton_err$passed, is_false(), "_inet_pton is not loaded.")

  ntop_err <- throws_error("C symbol name \"_inet_ntop\" not in load table")(.Call('_inet_ntop', as.raw(c(1,1,1,1)), AF_INET))
  expect_that(pton_err$passed, is_false(), "_inet_pton is not loaded.")
})

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

  #wrong AF
  expect_that(inet_pton("1:2:3:4:5:6:7:8", AF_INET), throws_error("Not in presentation Format"))
})

test_that("inet_ntop rejects weird inputs", {
  expect_that(inet_ntop(), throws_error(), "Throws error on no input")
  expect_that(inet_ntop(as.raw(c(1,2,3)), AF_INET), throws_error(), "(ipv4) Throws error on funky raw list length")
  expect_that(inet_ntop(as.raw(c(1,2,3)), AF_INET6), throws_error(), "(ipv6) Throws error on funky raw list length")
  expect_that(inet_ntop(as.raw(c(1,2,3))), throws_error(), "(no AF) Throws error on funky raw list length")
  expect_that(inet_ntop(as.raw(c(1,2,3,4)), AF_INET6), throws_error(), "Throws error on AF mismatch")
  expect_that(inet_ntop(as.raw(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)), AF_INET), throws_error(), "Throws error on AF mismatch")
  expect_that(inet_ntop(123, 3), throws_error(".+ is not a valid address family"), "Throws error on bad AF")

})

test_that("inet_ntop works for IPv4", {
  #raw
  expect_that(inet_ntop(as.raw(c(127,0,0,1)), AF_INET), equals("127.0.0.1"))

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


})

test_that("inet_ntop works for IPv6", {
  #raw
  expect_that(inet_ntop(as.raw(c(127,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0)), AF_INET6), equals("7f00:1::"))
  expect_that(inet_ntop(as.raw(c(222,173,190,239,222,173,190,239,222,173,190,239,222,173,190,239)), AF_INET6), 
              equals("dead:beef:dead:beef:dead:beef:dead:beef"))
  expect_that(inet_ntop(as.raw(c(127,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0))), equals("7f00:1::"))
  expect_that(inet_ntop(as.raw(c(222,173,190,239,222,173,190,239,222,173,190,239,222,173,190,239))), 
              equals("dead:beef:dead:beef:dead:beef:dead:beef"))

  #ip object
  ipo <- ip("7f00:1::")
  expect_that(inet_ntop(ipo, AF_INET6), equals("7f00:1::"), "with AF")
  expect_that(inet_ntop(ipo), equals("7f00:1::"), "without AF")

  expect_that(inet_ntop(ipo, AF_INET), equals("7f00:1::"), "inet_ntop correctly ignores bad AF argument")
  expect_that(inet_ntop(ipo, AF_INET), gives_warning("af argument,.+, disagrees with src argument,.+"), "inet_ntop gives warning on AF mismatch with ip object")


  #integer
  expect_that(inet_ntop(168430090, AF_INET6), equals("::10.10.10.10"))
  expect_that(inet_ntop(4568430090, AF_INET6), equals("::1:104c:b60a"))
  expect_that(inet_ntop(0, AF_INET6), equals("::"))
  expect_that(inet_ntop(4294967295, AF_INET6), equals("::255.255.255.255"))
  expect_that(inet_ntop(4294967296, AF_INET6), equals("::1:0:0"))


})

test_that("inet_ntop grabs the correct AF from an ip object", {
  a <- ip("127.0.0.1")
  expect_that(a$af, equals(AF_INET), "Testing error in ip object - AF incorrect")

  threw_it <- throws_error("raw vector length does not agree with AF.")(inet_ntop(a))
  expect_that(threw_it$passed, is_false(), "inet_ntop threw an error about raw vector length.")
})

test_that("as.numeric.rawlist works", {
  rl <- as.raw(c(1,2,3,4))
  ii <- as.numeric.rawlist(rl)
  expect_that(ii, equals(16909060L), "Converts to numeric properly with 4 byte word")
  expect_that(mode(ii), equals("numeric"))  

  rl <- as.raw(c(255,255,255,255))
  ii <- as.numeric.rawlist(rl)
  expect_that(ii, equals(4294967295))
  expect_that(mode(ii), equals("numeric"))  

})

test_that("as.rawlist.numeric works", {
  ii <- 16909060L
  rl <- as.rawlist.numeric(ii, 4)

  expect_that(rl, equals(as.raw(c(1,2,3,4))))
  expect_that(rl, is_a("raw"))

  rl <- as.rawlist.numeric(ii)
  expect_that(rl, equals(as.raw(c(1,2,3,4))), "Works with guessed length")
  expect_that(rl, is_a("raw"))
  

})
