source('constants.R')

ip <- function(x) {
  o <- structure(list(address = 0, af = NA), class = "ip")
  if(is.numeric(x)) {
    o$address <- as.integer(x)
    xaf <- attr(x, "af")
    if (!is.null(xaf) && AF_ok(xaf)) {
      o$af <- xaf
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
  x <- 0
  for(i in seq(1,length(o$address))) {
    x <- x + as.integer(o$address[i])*(256L^(length(o$address)-i))
  }
  return(x)
}

as.integer.ip <- as.double.ip

as.character.ip <- function(o) {
  inet_ntop(o$address, o$af)
}
