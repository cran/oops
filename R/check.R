#' Is Object an "oClass" Generator?
#'
#' Check whether object inherits the \code{"ClassGenerator"} class. This is used to check \code{\link{oClass}}
#' generators, not the instance. See \code{\link{is.Instance}} to check whether object is an oClass instance.
#'
#' @param x object to be tested
#'
#' @return \code{TRUE} if object inherits \code{"ClassGenerator"}, \code{FALSE} otherwise
#'
#' @export
is.oClass <- function(x) inherits(x, "ClassGenerator")

#' @export
#' @describeIn is.oClass check whether object is an oClass generator
is.ClassGenerator <- function(x) inherits(x, "ClassGenerator")

#' Is Object a Class Instance?
#'
#' Check whether object inherits the \code{"Instance"} class. See \code{\link{is.oClass}} to check whether object
#' is a \code{\link{oClass}} generator.
#'
#' @param x object to be tested
#'
#' @return \code{TRUE} if object inherits \code{"Instance"}, \code{FALSE} otherwise
#' @export
is.Instance <- function(x) inherits(x, "Instance")
