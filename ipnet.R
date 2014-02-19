#source('constants.R')
#source('inet.R')

#' A representation of a range of IP addresses (a network)
#'
#' \code{ipnet} objects are to \code{ip} objects as vectors are to numbers.
#'
#' The goal is for an \code{ipnet} to behave like a vector when you treat it
#' like one, but with syntactic sugar for working with \code{ip} addresses
#' and networking.
#'
#' A starting IP address is required. You probably want to supply either
#' an ending IP address or a CIDR. If not, \code{ipnet} will happily
#' construct a boring network consisting of only one IP address. If you
#' supply both an ending address and a CIDR, and they don't agree, then
#' \code{ipnet} will bore a hole through spacetime to kill your grandfather.
#'
#' If a CIDR is supplied, \code{ipnet} will construct a valid CIDR-style
#' network. This means that the resulting object's starting IP may not
#' necessarily be the same as you supplied. \code{ipnet} will warn you when
#' it does this.
#'
#' If you insist on defining networks classfully, \code{ipnet} will oblige.
#' The same warning about changing the start of your network to match
#' applies here.
#'
#' @param start The starting ip address of the network.
#' @param end The ending ip address of the network. Optional.
#' @param cidr The CIDR of the network. Optional.
#' @param class The class of the network. Very optional.
#' @return ipnet
#' @seealso \link{\code{ip}}
#' @export
#' @examples
#' a <- ipnet("10.10.10.0/24")
#' print(broadcast(a)) #It's an IP network!
#' print(range(a))     #It's a vector!
ipnet <- function(start, end, cidr, ipnetclass) {
  #coerce an ipnet to an ipnet
  o <- structure(list(start = 0, end = 0, cidr = 0), class = "ipnet")

  if (is.ipnet(start)) {
    if(missing(end)) {
      end <- max(start)
    }
    if(missing(cidr)) {
      cidr <- cidr(start)
    }
    if(missing(ipnetclass)) {
      ipnetclass <- ipnetclass(start)
    }
    start <- as.ip(start)
  }

  o$start <- as.ip(start)

  if(!missing(end)) {
    o$end <- as.ip(end)
  }
  
  if(!missing(cidr)) {
    o$cidr <- as.cidr(cidr)

    #fixup the start and end
  }
}
