source('constants.R')

#' @export
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
      stop(gettextf("Not converting raw list of length %d to ip.", length(x)))
    }
  } else if(is.numeric(x)) {
    o$af <- AF_INET
    o$address <- tryCatch(as.rawlist.numeric(x, IN_ADDRLEN), error = NA)
    if(any(is.na(o$address))) {
      o$af <- AF_INET6
      o$address <- tryCatch(as.rawlist.numeric(x, IN6_ADDRLEN), error = function(x) stop(gettextf("%d is way too big.", x)))
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
  return(o)
}


is.ip <- function(x) inherits(x, "ip")

as.double.ip <- function(o) {
  as.numeric.rawlist(o$address)
}

as.integer.ip <- as.double.ip

as.character.ip <- inet_ntop

#Ops.ip <- function(e1, e2=NA) {
#  bitwise <- switch(.Generic, `&` = , `|` = , `!` = TRUE, FALSE)
#  comparison <- switch(.Generic, `==` = , `!=` = , `<` = , `<=` = , `>=` = , `>` = TRUE, FALSE)
#  arith <- switch(.Generic, `+` = , `-` = , `*` = , `/` = , `^` = , `%%` = , `%/%` = TRUE, FALSE)
#
#  if(bitwise) {
#    return(bitwise(.Generic, e1, e2))
#  } else if (comparison) {
#    return(compare(.Generic, e1, e2))
#  } else if (arith) {
#    return(arith(.Generic, e1, e2))
#  } else {
#    stop(gettextf("Unexpected operator: %s", .Generic))
#  }
#}

`&.ip` <- function(e1, e2) {
  bitwise.ip('&', e1, e2)
}

`|.ip` <- function(e1, e2) {
  bitwise.ip('&', e1, e2)
}

bitwise.ip <- function(op, e1, e2) {
  realop <- switch(op, `&` = , `&&` = bitwAnd, 
                       `|` = , "||" = bitwOr, `!` = bitwNot, 
                              `xor` = bitwXor,
                               `<<` = bitwShiftL,
                               `>>` = bitwShiftR,
                                      NA)

  if(!is.function(realop)) {
    stop(gettextf("Unexpected operator: %s", op))
  }
  ip(as.raw(realop(as.numeric(ip(e1)$address), as.numeric(ip(e2)$address))))
}

`+.ip` <- function(e1, e2) {
  arith.ip(`+`, e1, e2)
}

arith.ip <- function(op, e1, e2) {
  ip(op(as.numeric(ip(e1)),as.numeric(ip(e2))))
}
