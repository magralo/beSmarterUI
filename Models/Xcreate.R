Xcreate<- function (p, na, nd, Xa, Xd, INT = TRUE, DIFF = FALSE, base = p) 
{
  pandterm = function(message) {
    stop(message, call. = FALSE)
  }
  if (missing(p)) 
    pandterm("requires p (# choice alternatives)")
  if (missing(na)) 
    pandterm("requires na arg (use na=0 if none)")
  if (missing(nd)) 
    pandterm("requires nd arg (use nd=0 if none)")
  if (missing(Xa)) 
    pandterm("requires Xa arg (use Xa=NULL if none)")
  if (missing(Xd)) 
    pandterm("requires Xd arg (use Xd=NULL if none)")
  if (is.null(Xa) && is.null(Xd)) 
    pandterm("both Xa and Xd NULL -- requires one non-null")
  if (!is.null(na) && !is.null(Xa)) {
    if (ncol(Xa) != p * na) 
      pandterm(paste("bad Xa dim, dim=", dim(Xa)))
  }
  if (!is.null(nd) && !is.null(Xd)) {
    if (ncol(Xd) != nd) 
      pandterm(paste("ncol(Xd) ne nd, ncol(Xd)=", ncol(Xd)))
  }
  if (!is.null(Xa) && !is.null(Xd)) {
    if (nrow(Xa) != nrow(Xd)) {
      pandterm(paste("nrow(Xa) ne nrow(Xd),nrow(Xa)= ", 
                     nrow(Xa), " nrow(Xd)= ", nrow(Xd)))
    }
  }
  if (is.null(Xa)) {
    n = nrow(Xd)
  }
  else {
    n = nrow(Xa)
  }
  if (INT) {
    Xd = cbind(c(rep(1, n)), Xd)
  }
  if (DIFF) {
    Imod = diag(p - 1)
  }
  else {
    Imod = matrix(0, p, p - 1)
    Imod[-base, ] = diag(p - 1)
  }
  if (!is.null(Xd)) 
    Xone = Xd %x% Imod
  else Xone = NULL
  Xtwo = NULL
  if (!is.null(Xa)) {
    if (DIFF) {
      tXa = matrix(t(Xa), nrow = p)
      Idiff = diag(p)
      Idiff[, base] = c(rep(-1, p))
      Idiff = Idiff[-base, ]
      tXa = Idiff %*% tXa
      Xa = matrix(as.vector(tXa), ncol = (p - 1) * na, 
                  byrow = TRUE)
      for (i in 1:na) {
        Xext = Xa[, ((i - 1) * (p - 1) + 1):((i - 1) * 
                                               (p - 1) + p - 1)]
        Xtwo = cbind(Xtwo, as.vector(t(Xext)))
      }
    }
    else {
      for (i in 1:na) {
        Xext = Xa[, ((i - 1) * p + 1):((i - 1) * p + 
                                         p)]
        Xtwo = cbind(Xtwo, as.vector(t(Xext)))
      }
    }
  }
  return(cbind(Xone, Xtwo))
}
