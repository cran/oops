#' Extract or Replace Parts of a Class or Instance
#'
#' Operators acting on \code{\link{oClass}} generators and their instances.
#'
#' @param x object of class \code{"Instance"} or \code{"ClassGenerator"}
#' @param i,name character or symbol for \code{`$`} describing field name to return or set
#' @param value new field value
#' @param exact logical controlling whether a partial match is acceptable. Defaults to \code{TRUE} for no partial matching
#' @param inherits logical describing whether parent environments should be searched
#'
#' @details
#' For \code{\link{oClass}} instances, \code{`$`} and \code{`[`} first search the instance environment for the object.
#' If no object is found, then all inherited objects are searched in order. Any object assigned to the instance
#' will be inserted into the instance's environment. These operators act on the underlying Class template environment
#' when applied to a Class generator.
#'
#' @return Environment of class \code{"Instance"} or function of class \code{"ClassGenerator"}
#' @rdname Extract
#' @name Extract
#'
NULL


#' @export
#' @rdname Extract
`$.ClassGenerator` <- function(x, name){
  get(as.character(name), envir=attr(x, "fields"), inherits = TRUE)
}

#' @export
#' @rdname Extract
`[[.ClassGenerator` <- function(x, i, exact = TRUE, inherits = TRUE){
  if (!exact){
    obj <- get_partial(as.character(i)[[1]], attr(x, "fields"), inherits)
  } else {
    obj <- get(as.character(i)[[1]], envir=attr(x, "fields"), inherits = inherits)
  }
  obj
}

#' @export
#' @rdname Extract
`$<-.ClassGenerator` <- function(x, name, value){
  assign(as.character(name), value, envir = attr(x, "fields"))
  invisible(x)
}

#' @export
#' @rdname Extract
`[[<-.ClassGenerator` <- function(x, name, value){
  assign(as.character(name), value, envir = attr(x, "fields"))
  invisible(x)
}

#' @export
#' @rdname Extract
`$.Instance` <- function(x, name){
  get0(as.character(name), envir=x, inherits = TRUE, ifnotfound=NULL)
}

#' @export
#' @rdname Extract
`[[.Instance` <- function(x, i, exact = TRUE, inherits = TRUE){
  if (!exact){
    obj <- get_partial(as.character(i)[[1]], x, inherits)
  } else {
    obj <- get0(as.character(i)[[1]], envir=x, inherits = inherits, ifnotfound=NULL)
  }
  obj
}


get_partial <- function(x, e, inherits = TRUE){
  nm <- ls(e)
  p  <- pmatch(x, nm, nomatch = NA_integer_, duplicates.ok = FALSE)
  if (!is.na(p)){ return(get0(nm[p], envir=e)) }
  if (inherits && !identical(parent.env(e), baseenv())){ return(get_partial(x, parent.env(e), inherits)) }
  NULL
}

