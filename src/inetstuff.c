#include <netdb.h>
#include <arpa/inet.h>
#include <netinet/in.h>
#include <string.h>
#include <errno.h>
#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>



SEXP _inet_ntop(SEXP src, SEXP af) {
  //input
  const char* srcbuf;
  srcbuf = RAW(src);
  int afint = asInteger(af);

  //output
  SEXP ret = R_NilValue;
  
  //results check
  const char* s;
  char dst[INET6_ADDRSTRLEN+1];

  s=inet_ntop( afint, srcbuf, dst, INET6_ADDRSTRLEN);

  if(s==NULL) {
    error(strerror(errno));
    return R_NilValue;    
  }
  else {
    ret = PROTECT(allocVector(STRSXP, 1));
    SET_STRING_ELT(ret, 0, mkChar(dst));
    UNPROTECT(1);
  }
  return(ret);
}

SEXP _inet_pton(SEXP src, SEXP af) {
  //input
  const char* srcstr;
  srcstr = translateChar(STRING_ELT(src, 0));
  int afint = asInteger(af);

  //results check
  int s;
  char dst[sizeof(struct in6_addr)+1];
  
//output
  SEXP ret = R_NilValue;


  s=inet_pton( afint, srcstr, dst);

  if(s==0) {
    error("Not in presentation Format.");
  }
  else if(s<0) {
    error(strerror(errno));
    return R_NilValue;
  }
  else {
    ret = PROTECT(allocVector(RAWSXP, afint == AF_INET ? sizeof(struct in_addr) : sizeof(struct in6_addr)));
    memcpy(RAW(ret), dst, afint == AF_INET ? sizeof(struct in_addr) : sizeof(struct in6_addr));
    UNPROTECT(1);
  }
  return(ret);
}
