require(Rcpp)
require(inline)

dyn.load('../src/inetstuff.so')
source('constants.R')


addrlen <- function(af) ifelse(missing(af) || !AF_ok(af),stop("Could not determine Addrlen"),ifelse(af==AF_INET, IN_ADDRLEN, IN6_ADDRLEN))



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
  src <- as.rawlist.integer(src, addrlen(ifelse(missing(af) || !AF_ok(af),
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

as.integer.rawlist <- function(rawin) {
  x <- 0
  for(i in seq(1,length(rawin))) {
    x <- x + as.integer(rawin[i])*(256L^(length(rawin)-i))
  }
  return(x)
}

as.rawlist.integer <- function(intin, len) {
  if(missing(len)) len <- 4L
  r <- raw(length=len)
  i <- as.integer(len)
  while((intin != 0L) & (i >=0L) ) {
    r[i] <- as.raw( intin %% 256L )
    i <- i-1L
    intin <- intin %/% 256L
  }
  if (intin > 0L) {
    stop("number too large")
  }
  return(r)
}


