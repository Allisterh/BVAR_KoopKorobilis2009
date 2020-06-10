eye <- function (m, n) 
{
  if (is.size_t(m)) {
    m <- as.integer(m)
  }
  if (missing(n)) {
    len.m <- length(m)
    if (len.m == 1) {
      n <- m
    }
    else if (len.m > 1) {
      n <- m[-1]
      m <- m[1]
    }
  }
  if (!is.numeric(n)) {
    stop(sprintf("argument %s must be numeric", sQuote("n")))
  }
  else if (!(length(n) == 1)) {
    stop(sprintf("argument %s must be of length 1", sQuote("n")))
  }
  else if (!(n > 0)) {
    stop(sprintf("argument %s must be a positive quantity", 
                 sQuote("n")))
  }
  if (!is.numeric(m)) {
    stop(sprintf("argument %s must be numeric", sQuote("m")))
  }
  else if (!(length(m) == 1)) {
    stop(sprintf("argument %s must be of length 1", sQuote("m")))
  }
  else if (!(m > 0)) {
    stop(sprintf("argument %s must be a positive quantity", 
                 sQuote("m")))
  }
  return(diag(1, m, n))
}


ones <- function (...) 
{
  nargs <- length(dots <- list(...))
  dims <- as.integer(if (nargs == 1 && is.size_t(dots[[1]])) {
    dots[[1]]
  } else {
    unlist(dots)
  })
  if (length(dims) == 1) {
    dims[2] <- dims[1]
  }
  if (!(length(dims) > 1)) {
    stop("dimensions must be of length greater than 1")
  }
  else if (!(all(dims > 0))) {
    stop("dimensions must be a positive quantity")
  }
  return(array(1, dims))
}

zeros <- function (...) 
{
  nargs <- length(dots <- list(...))
  dims <- as.integer(if (nargs == 1 && is.size_t(dots[[1]])) {
    dots[[1]]
  } else {
    unlist(dots)
  })
  if (length(dims) == 1) {
    dims[2] <- dims[1]
  }
  if (!(length(dims) > 1)) {
    stop("dimensions must be of length greater than 1")
  }
  else if (!(all(dims > 0))) {
    stop("dimensions must be a positive quantity")
  }
  return(array(0, dims))
}