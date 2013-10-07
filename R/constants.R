

enum <- function(l="list", n, where=parent.frame()) {
  if(!missing(n)) {
    assign(n, l, where)
    assign(paste(n, "_ok",sep=""), function(x) {ifelse(is.null(x), FALSE, x %in% get(n, where))}, where)
  }
  lapply(names(l), FUN=function(x) assign(x, l[[x]], where))  
}

enum(n="AF", list(AF_INET=2, 
                  AF_INET6=10))
enum(n="ADDRLEN", list(IN_ADDRLEN=4, 
                       IN6_ADDRLEN=16))

