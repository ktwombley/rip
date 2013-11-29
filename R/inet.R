require(Rcpp)
require(inline)

#dyn.load('../src/inetstuff.so')
#source('constants.R')

#' Determine the correct address length for a given address family.
#'
#' Given a valid address family (currently either \code{AF_INET} or 
#' \code{AF_INET6}), returns the number of bytes required to store the ip
#' address. Either \code{IN_ADDRLEN} (4) or \code{IN6_ADDRLEN} (16).
#' @param af an address family, either \code{AF_INET} or \code{AF_INET6}
#' @return numeric the number of bytes required to store an ip address in the 
#' given address family.
#' @export
addrlen <- function(af) ifelse(missing(af) || !AF_ok(af),stop("Could not determine Addrlen"),ifelse(af==AF_INET, IN_ADDRLEN, IN6_ADDRLEN))


#' Convert a character string IP address into its numeric form.
#'
#' \code{inet_pton} works like the C function from inet.h, with some R 
#' niceties on top. If you do not supply an address family, \code{inet_pton}
#' tries first to create an IPv4 address, and then an IPv6 address if that
#' fails. 
#'
#' @param src a string representation of an IP address.
#' @param af (optional) the address family of the IP in \code{src}.
#' @return list of raw bytes
#' @references \url{http://man7.org/linux/man-pages/man3/inet_pton.3.html}
#' @seealso \link{inet_ntop}
#' @export
inet_pton <- function(src, af) {
  if(missing(src)) {
    stop("src must be a string representation of an IP address")
  }
  #no af? no problem! we try both.
  if(missing(af)) {
#    print("missing af")
    warning("Guessing address family. Results unpredictable.")
    return(tryCatch(.Call('_inet_pton', src, AF_INET), error = function(e) .Call('_inet_pton', src, AF_INET6)))
  }
  
  if(!AF_ok(af)) {
    error("Bad address family.")
  }
#  print(paste("addy: ", src, " af:", af))
  return(.Call('_inet_pton', src, af))
}


#' Convert a numeric IP address into a character string representation.
#'
#' \code{inet_ntop} works like the C function from inet.h, with some R 
#' niceties on top. If you do not supply an address family, \code{inet_pton}
#' tries first to create an IPv4 address, and then an IPv6 address if that
#' fails. \code{inet_ntop} understands as input numberics, a list of raw
#' bytes, or an \code{\link{ip}} object.
#'
#' @param src a numeric representation of an IP address.
#' @param af (optional) the address family of the IP in \code{src}.
#' @return character string representation of the IP address.
#' @references \url{http://man7.org/linux/man-pages/man3/inet_ntop.3.html}
#' @export
#' @seealso \link{inet_pton}
inet_ntop <- function(src, af) {
  if(missing(src)) {
    stop("src must be a numeric representation of an IP address")
  }
  if(is.raw(src)) {
    if(!(length(src) %in% ADDRLEN)) {
      stop("raw vector is a funky length.")
    }
    if(missing(af)) {
      if(length(src)==IN_ADDRLEN) {
        af <- AF_INET
      } else if(length(src)==IN6_ADDRLEN) {
        af <- AF_INET6
      } else {
        stop("No address family given, and raw vector is a funky length, too.")
      }
    } else {
      if((length(src)==IN_ADDRLEN && af != AF_INET) ||
         (length(src)==IN6_ADDRLEN && af != AF_INET6)) {
        stop("raw vector length does not agree with AF.")
      }
    }
  } else if(is.ip(src)) {
    if(!missing(af) && src$af != af) warning(paste("af argument,",substitute(af),", disagrees with src argument,",substitute(src)))
    af <- src$af
    src <- src$address
  }
# TODO(kwt): make ipnetwork object
#  else if(is.ipnetwork(src)) {
#    af <- src$af
#    src <- src$ip$address
#  }
  else {
  #let's do some coersion.
  src <- as.rawlist.numeric(src, addrlen(ifelse(missing(af) || !AF_ok(af),
                                        ifelse(src > 4294967295,     #255.255.255.255 
                                               AF_INET6, 
                                               AF_INET),
                                        af)
                                 )
                    )
  }
  
  #no af? no problem! we try both.
  if(missing(af) || is.na(af)) {
    warning("Guessing address family. Results unpredictable.")
    return(tryCatch(.Call('_inet_ntop', src, AF_INET), error = function(e) .Call('_inet_ntop', src, AF_INET6)))
  }
  
  if(!AF_ok(af)) stop(sprintf("%s is not a valid address family", af))

  return(.Call('_inet_ntop', src, af))
}

#' Convert a list of raws into a single numeric value
#'
#' This is a convenience function to convert a list of raw bytes into a
#' numeric value. \code{as.numeric.rawlist} interprets the list in
#' network byte order. It works relatively well for length 4 or 16 lists,
#' but longer lists will easily overflow.
#'
#' @param rawin a list of raw bytess.
#' @return numeric value obtained by treating \code{rawin} as an single integer
#' stored in network byte order.
#' @references \url{http://man7.org/linux/man-pages/man3/inet_ntop.3.html}
#' @export
#' @seealso \link{inet_pton}
as.numeric.rawlist <- function(rawin) {
  x <- 0
  for(i in seq(1,length(rawin))) {
    x <- x + as.integer(rawin[i])*(256L^(length(rawin)-i))
  }
  return(x)
}

as.rawlist.numeric <- function(numin, len) {
  if(missing(len)) {
    i <- 100          #I hope that's enough
    r <- raw()
  } else {
    i <- as.integer(len)
    r <- raw(length=len)
  }
  while((numin > 0) & (i >=0) ) {
    r[i] <- as.raw( numin %% 256 )
    i <- i-1L
    numin <- numin %/% 256
  }
  if (missing(len) && i > 0) {
    r <- r[(i+1):100]
  }
  if (numin > 0) {
    stop(gettextf("number too large to fit into list of length %s", len))
  }
  return(r)
}


