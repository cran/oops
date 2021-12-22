#' Add Fields to oClasses and Other Objects
#'
#' For environments, oClass instances and generator, \code{add_fields} is a wrapper for
#' \code{\link[base]{list2env}}; it adds the objects in \code{...} the environment if they are named.
#' For list and other objects, it behaves similar to appending \code{...} as a list.
#'
#' @param x oClass instance, generator, environment, list, or other object
#' @param ... named objects to be added to \code{x}
#'
#' @return object of same type as \code{x} or list
#' @export
#'
#' @examples
#' clown <- oClass("clown")
#' add_fields(clown, laugh = "haha", is_funny=TRUE)
#'
#' clown$laugh
#'
add_fields <- function(x, ...) UseMethod("add_fields")
#' @export
#' @method add_fields default
add_fields.default <- function(x, ...){
  z <- append(list(x), list(...))
  return(z)
}
#' @export
#' @method add_fields list
add_fields.list <- function(x, ...){
  x <- append(x, list(...))
  return(x)
}
#' @export
#' @method add_fields environment
add_fields.environment <- function(x, ...){
  list2env(list(...), envir=x)
  return(x)
}
#' @export
#' @method add_fields Instance
add_fields.Instance <- function(x, ...){
  dots <- list(...)
  if (length(dots) == 0) return(x)
  list2env(dots, envir=x)
  x
}
#' @export
#' @method add_fields ClassGenerator
add_fields.ClassGenerator <- function(x, ...){
  list2env(list(...), envir=attr(x, "fields"))
  return(x)
}

#' Change the Formal Arguments of a oClass Generator
#'
#' This accepts an \code{\link{oClass}} generator and updates its formal arguments based either on those
#' passed in \code{...} or the function passed to \code{from_init}. The results will be
#' passed to the appropriate \code{\link{init}} function each time an instance is generated.
#'
#' @param x oClass generator function
#' @param ... named or unnamed objects used as the formal arguments of the generator function
#' @param envir environment from which to evaluate arguments
#' @param from_init function containing the formal arguments to use; typically an \code{\link{init}} function. \code{...} and \code{envir} are ignored if not \code{NULL}.
#'
#' @return oClass generator function
#' @export
#'
#' @examples
#' clown <- oClass("clown")
#' clown
#'
#' # 'init' requires a laugh
#' init.clown <- function(x, laugh, ...){
#'   x$laugh <- laugh
#'   add_fields(x, ...)
#'   return(x)
#' }
#'
#' # change formals of clown
#' clown <- change_formals(clown, from_init = init.clown)
#'
#' # alternatively,
#' clown <- change_formals(clown, laugh, ..dots)
#'
#' # creation
#' happy_clown <- clown("HAHA")
#' sad_clown   <- clown("ha")
#'
change_formals <- function(x, ..., envir = parent.frame(), from_init = NULL){
  att <- attributes(x)
  if (!is.null(from_init)){
    if (is.function(from_init)){
      form_list <- formals(from_init)[-1]
      form_int  <- from_formal_internal(formals(from_init))
    } else { stop("'from_init' must be a function with formals.") }
  } else {
    form_list <- eval(substitute(alist(...)))
    form_int  <- to_formal_internal(form_list, envir)
    form_list <- to_formal_list(form_list, envir)
  }
  if (attr(x, "portable")){
    formals(x)   <- form_list
    body(x)[[5]] <- form_int # Not right! Needs init(); also oClass
  } else {
    formals(x)   <- form_list
    body(x)[[3]] <- form_int # Not right! Needs init()
  }
  attributes(x) <- att
  x
}

#' Change the Inheritance of an oClass
#'
#' This function takes twos \code{\link{oClass}} generator function and alters the first
#' so that it inherits the template and classes of the second. Existing instances will
#' inherit the objects contained in the new parent, but will not gain access to the S3
#' methods.
#'
#' @param x oClass generator function
#' @param parent oClass generator function from which \code{x} inherits
#'
#' @return oClass generator function
#' @export
#'
#' @examples
#' typist <- oClass("typist")
#' job    <- oClass("job", hours = 40, pay=15)
#'
#' typist <- change_inherit(typist, job)
#' typist$hours
change_inherit <- function(x, parent){
  if (missing(parent) || is.null(parent)){
    attr(x, "inherit") <- NULL
    parent.env(x) <- baseenv()
    dclass <- "Instance"
  } else if (is.oClass(parent)){
    attr(x, "inherit") <- parent
    dclass   <- class(attr(parent, "fields"))
    portable <- attr(x, "portable")
    if (portable){
      attr(x, "fields") <- set_parent_fields(attr(x, "fields"), attr(parent, "fields"))
    } else {
      parent.env(attr(x, "fields"))  <- attr(parent, "fields")
    }
  } else { stop("oClasses can only inherit from other oClasses.") }
  dclass <- c(attr(x, "name"), dclass)
  attr(x, "class.names") <- dclass
  class(attr(x, "fields")) <- dclass
  x
}

