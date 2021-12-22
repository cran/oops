#' Create a Copy of an oClass Instance
#'
#' A copy of all objects and attributes within an environment. If \code{deep=TRUE},
#' all objects inside of \code{x}, including other environments, will also be deeply "cloned".
#' The global and base environments will not be cloned.
#'
#' @param x environment of class \code{"Instance"}
#' @param deep logical for whether clone should be applied to all objects.
#' @param cloned environment containing references to environments that have already been cloned. This is passed to internal methods when \code{deep=TRUE} and should not be set directly.
#' @param ... arguments passed to methods
#'
#' @return environment of class \code{"Instance"}
#' @export
#'
clone <- function(x, deep=FALSE, ...) UseMethod("clone")
#' @export
#' @method clone default
clone.default <- function(x, deep=FALSE, ...){
  if (deep) attributes(x) <- clone_attributes(x, deep=TRUE)
  x
}
#' @export
#' @method clone list
clone.list <- function(x, deep=FALSE, cloned=NULL, ...){
  if (deep) return(.clone_list_deep(x, cloned, ...))
  l <- lapply(x, function(i) i)
  attributes(l) <- clone_attributes(x, deep=FALSE, cloned=cloned)
  return(l)
}
#' @export
#' @method clone environment
clone.environment <- function(x, deep=FALSE, cloned=NULL, ...){
  if (deep) return(.clone_env_deep(x, cloned=cloned, ...))
  .clone_env(x, ...)
}
#' @export
#' @method clone Instance
clone.Instance <- function(x, deep=FALSE, cloned=NULL, ...){
  if (deep) return(.clone_env_deep(x, cloned=cloned, ...))
  .clone_env(x, ...)
}
#' @export
#' @describeIn clone Clone the attributes of an object.
clone_attributes <- function(x, deep=FALSE, cloned=NULL){
  att <- attributes(x)
  if (is.null(att)) return(NULL)
  if (deep) return(.clone_att_deep(att, cloned))
  lapply(att, function(i) clone(i, deep=FALSE, cloned=cloned))
}


.new_cloned_list <- function() {
  e <- new.env()
  e[["envs"]] <- list()
  e
}

.clone_att_deep <- function(att, cloned=NULL){
  if (is.null(cloned)) cloned <- .new_cloned_list()
  lapply(att, function(i) clone(i, deep=TRUE, cloned=cloned))
}

.clone_list_deep <- function(x, cloned=NULL, ...){
  if (is.null(cloned)) cloned <- .new_cloned_list()
  l <- lapply(x, function(i) clone(i, deep=TRUE, cloned=cloned, ...))
  att <- attributes(x)
  if (is.null(att)) return(l)
  attributes(l) <- .clone_att_deep(att, cloned)
  return(l)
}

.clone_env <- function(x, ...){
  if (identical(x, .GlobalEnv) || identical(x, baseenv())) return(x)
  is.hash <- !is.null(env.profile(x))
  e <- new.env(hash=is.hash, parent=parent.env(x))
  list2env(eapply(x, function(i) i), envir=e)
  attributes(e) <- clone_attributes(x, deep=FALSE, ...)
  return(e)
}

.clone_env_deep <- function(x, cloned=NULL, ...){
  if (identical(x, .GlobalEnv) || identical(x, baseenv())) return(x)
  if (is.null(cloned)){
    cloned <- .new_cloned_list()
  } else {
    for (env in cloned[["envs"]]){
      if (identical(x, env[[1]])) return(env[[2]])
    }
  }
  is.hash <- !is.null(env.profile(x))
  e <- new.env(hash=is.hash)
  cloned[["envs"]][[length(cloned[["envs"]]) + 1]] <- list(x, e)
  list2env(eapply(x, function(i) clone(i, deep=TRUE, cloned=cloned, ...)), envir=e)
  parent.env(e) <- .clone_env_deep(parent.env(x), cloned=cloned, ...)
  attributes(e) <- clone_attributes(x, deep=TRUE, cloned=cloned, ...)
  return(e)
}

