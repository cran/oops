#' Initialize Class Instance
#'
#' Function called on \code{\link{oClass}} instance when it is created. Users create \code{init} methods to
#' customize creation behavior for their Classes. All \code{init} methods should return the Instance.
#' \code{init_next} calls the objects next \code{init} methods. \code{init_next} should only be
#' used inside if \code{init}.
#'
#' @param x environment of class \code{"Instance"}
#' @param ... named fields inherited by the class instance or passed to methods
#'
#' @return environment of class \code{"Instance"}
#' @export
#'
#' @examples
#'
#' Animal <- oClass("Animal")
#'
#' init.Animal <- function(self, x, y){
#'   self$x <- x
#'   self$y <- y
#'   self
#' }
#'
#' turtle <- Animal(5, 10)
#' turtle$x == 5    # TRUE
#' turtle$y == 10   # TRUE
#'
#'
init <- function(x, ...) UseMethod("init")
#' @export
#' @method init default
init.default <- function(x, ...){
  return(x)
}

#' Init Method for Instance
#'
#' See \code{\link{init}} for details.
#'
#' @param x environment of class \code{"Instance"}
#' @param ... named fields inherited by the class instance or passed to methods
#'
#' @return environment of class \code{"Instance"}
#' @export
#' @export init.Instance
#' @method init Instance
init.Instance <- function(x, ...){
  list2env(list(...), envir=x)
  return(x)
}

#' @export
#' @describeIn init Initialize the inherited Class.
init_next <- function(x, ...){
  tryCatch(
    {
      .Class <- get(".Class", envir=sys.frame(sys.nframe()-1))[-1]
      NextMethod("init", x, ...)
    },
    error=function(e) init.Instance(x, ...)
  )
}


