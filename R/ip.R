#source('constants.R')
#source('inet.R')

#' A representation of an IP address
#'
#' \code{ip} objects behave like numbers if you perform math on them, but
#' display in the familiar IPv4 or IPv6 formats if you use them as a
#' character string.
#'
#' This function creates an IP address out of its argument. It understands
#' string input (e.g. \code{ip("127.0.0.1")}), raw byte vectors (e.g.
#' \code{ip(as.raw(c(127,0,0,1)))}), and even numerical input (e.g. 
#' \code{ip(2130706433)}). \code{ip} speaks both IPv4 and IPv6.
#' 
#' \code{ip} objects can be used as operands in math operations, but
#' beware of doing math with IPv6 addresses. Due to loss of precision, 
#'
#' @param x something to turn into an IP address.
#' @return ip
#' @references http://tools.ietf.org/html/rfc791 http://tools.ietf.org/html/rfc2460
#' @export
#' @examples
#' a <- ip("10.10.10.10")
#' print(a + 5)
#' b <- ip("255.255.255.0")
#' print(a & b)
ip <- function(x) {
  #coerce an IP to an IP
  if (is.ip(x))
    return(x)
  o <- structure(list(address = 0, af = NA), class = "ip")
  if(is.raw(x)) {
    if(length(x)==IN_ADDRLEN) {
      o$address <- x
      o$af <- AF_INET
    } else if (length(x)==IN6_ADDRLEN) {
      o$address <- x
      o$af <- AF_INET6
    } else {
      stop(gettextf("Not converting raw list of length %f to ip.", length(x)))
    }
  } else if(is.numeric(x)) {
    o$af <- AF_INET
    o$address <- tryCatch(as.rawlist.numeric(x, IN_ADDRLEN), error = function(e) NA)
    if(any(is.na(o$address))) {
      o$af <- AF_INET6
      o$address <- tryCatch(as.rawlist.numeric(x, IN6_ADDRLEN), error = function(e) stop(gettextf("%f is way too big.", x)))
    }
  } else if (is.character(x)) {
    o$af <- AF_INET
    o$address <- tryCatch(inet_pton(x, o$af), 
                          error = function(e) {
			    return(NA)
                          })
    if(any(is.na(o$address))) {
      o$af <- AF_INET6
      o$address <-  tryCatch(inet_pton(x, o$af),
                             error = function(e) {
                               stop(paste("Did not recognize an IP address", x))
                             })
    }
  }
  o
}

as.ip <- ip

is.ip <- function(x) inherits(x, "ip")

as.double.ip <- function(o) {
  as.numeric.rawlist(o$address)
}

as.integer.ip <- as.double.ip

as.character.ip <- inet_ntop

print.ip <- function(x) {
  print(inet_ntop(x))
}

Ops.ip <- function(e1, e2=NA) {
  bitwise <- switch(.Generic, '&' = , '|' = , '!' = TRUE, FALSE)
  comparison <- switch(.Generic, '==' = , '!=' = , '<' = , '<=' = , '>=' = , '>' = TRUE, FALSE)
  arith <- switch(.Generic, '+' = , '-' = , '*' = , '/' = , '^' = , '%%' = , '%/%' = TRUE, FALSE)

  if(bitwise) {
    op <- function(x) as.raw(x$address)
  } else if (comparison) {
    op <- function(x) as.raw(x$address)
  } else if (arith) {
    op <- as.numeric
  } else {
    stop(gettextf("Unexpected operator: %s", .Generic))
  }

  if(comparison) { #a comparison expects a logical value
  # == != < <= >= >
    e1 <- op(ip(e1))
    e2 <- op(ip(e2))
    if(identical(all.equal(e1, e2), TRUE)) {
      if(.Generic %in% c('==', '>=', '<=')) {
        return(TRUE)
      } else {
        return(FALSE)
      } 
    } else if (.Generic == '!=') {
      return(TRUE)
    } else {
      if(length(e1) > length(e2)) {
        e2 <- c(rep(0, length(e2)-length(e1)), e2)
      } else if (length(e1) < length(e2)) {
        e1 <- c(rep(0, length(e1)-length(e2)), e1)
      }
      for(n in seq_along(e1)) {
        if(e1[n] > e2[n]) {
          return(.Generic %in% c('>=', '>'))
        } else if(e1[n] < e2[n]) {
          return(.Generic %in% c('<=', '<'))
        }
      }
      return(.Generic %in% c('>=', '<='))
    }

  } else { #everything else we wrap into an ip object
    if(.Generic == '!') {
      ip(do.call(.Generic, list(op(ip(e1)))))
    } else {
      ip(do.call(.Generic, list(op(ip(e1)), op(ip(e2)))))
    }
  }
}

.rip.warn.about.slash <<- TRUE

#' CIDR notation and the / operator
#'
#' It is common to use a \code{ / } to denote a network address in CIDR format. 
#' To make usage more (or less!) intuitive, this package can treat the \code{ / } 
#' operator either way. It's up to you! By default, this package treats the 
#' \code{ / } operator as denoting CIDR notation. If you'd prefer division, 
#' set the option rip.slash to be \code{"division"} like this:
#'
#' \code{option("rip.slash"="division")}
#' 
#' Regardless of how you set this option, a slash embedded within a string
#' will still be parsed as denoting CIDR notation, and the usual \code{ipnet}
#' functions will work as expected.
#'
#' The first time you use \code{ / } in an interactive session, you will be
#' warned. After that, you're on your own.
#'
#' @param e1 an IP address.
#' @param e2 Either an IP address or a numeric.
#' @return ip or numeric, depending on contents of option rip.slash.
#' @references http://tools.ietf.org/html/rfc1519 http://tools.ietf.org/html/rfc4632
#' @export
#' @examples
#' a <- ip("10.10.10.10")
#' print(a / 24)
#'
#' option("rip.slash"="division")
#' print(a / 2)
`/.ip` <- function(e1, e2) {
  op <- getOption("rip.slash")
  if(interactive() && .rip.warn.about.slash && is.null(op)) {
    .rip.warn.about.slash <<- FALSE
    packageStartupMessage("Hey! By default I treat `/` as CIDR notation. See ?`/.ip` for details.")
  }
  if(is.null(op) || op=="cidr") {
    ipnet(e1, cidr=e2)
  } else if(op=="division") {
    ip(as.numeric(ip(e1))/as.numeric(ip(e2)))
  } else {
    stop("Did not understand option. Set rip.slash to either 'cidr' or 'division'")
  }
}

as.logical.ip <- function(x) {
  any(x > 0)
}

`||.ip` <- function(e1, e2) {
  e1 | e2
}
