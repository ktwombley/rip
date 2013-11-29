#' Create C-like enums for fun and profit
#'
#' This function is a convenient shorthand for defining an enum in the user's
#' workspace. This function will define these convenience variables and
#' functions:
#' \itemize{
#' \item A variable with the name and value of each item in \code{l}.
#' \item A list named \code{n}, containing the name/value pairs from \code{l}
#' \item A function named \code{n_ok} with one argument. It returns true if its
#' argument is a member of the enum, false otherwise.
#' }
#'
#' @param l list of values. With names.
#' @param n the name of the enum
#' @param where the environment into which variables should be defined.
#' Defaults to the parent frame.
#' @export
#' @examples
#' enum(n="AF", list(AF_INET=2, 
#'                   AF_INET6=10))
#' AF_INET         # 2
#' AF_ok(AF_INET6) # TRUE
#' AF_ok(3)        # FALSE
enum <- function(l="list", n, where=parent.frame()) {
  if(!missing(n)) {
    assign(n, l, where)
    assign(paste(n, "_ok",sep=""), function(x) {ifelse(is.null(x), FALSE, x %in% get(n, where))}, where)
  }
  lapply(names(l), FUN=function(x) assign(x, l[[x]], where))  
}

#' Address Family Enum
#' 
#' An enum containing the valid address families for internet communication.
#' 
#' \itemize{
#'   \item AF_INET 2
#'   \item AF_INET6 10 
#' }
#' 
#' @docType data
#' @keywords datasets
#' @format An enum with 2 elements.
#' @name AF
#' @alias address family
enum(n="AF", list(AF_INET=2, 
                  AF_INET6=10))

#' Address Length Enum
#' 
#' An enum containing the valid lengths (in bytes) of ip addresses.
#' 
#' \itemize{
#'   \item IN_ADDRLEN 4
#'   \item IN6_ADDRLEN 16
#' }
#' 
#' @docType data
#' @keywords datasets
#' @format An enum with 2 elements.
#' @name ADDRLEN
enum(n="ADDRLEN", list(IN_ADDRLEN=4, 
                       IN6_ADDRLEN=16))

