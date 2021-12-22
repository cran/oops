#' Create an Object Class
#'
#' Create a function used to generate instances (environments) with the specified class and fields.
#'
#' @param name character string describing the name of the class
#' @param inherit oClass used as the \code{\link[base]{parent.env}} for the generated instances
#' @param hash logical indicating whether instances should use hashing, see \code{\link[base]{new.env}}
#' @param portable logical indicating whether all inherited values should be copied into each instance
#' @param formals list containing the formal arguments for the resulting generator function. These are passed to the \code{\link{init}} function when a new instance is created.
#' @param ... named fields inherited by the class instance
#'
#' @details
#' \code{oClass} is used to create classes with reference semantics that modify in place similar to \code{R5}
#' and \code{R6} classes. Unlike those, functions on \code{oClass} instances dispatch using the standard S3
#' dispatch system. Furthermore, \code{oClass} objects and instances are created similar to other R objects
#' to ensure that they are easy and painless to use.
#'
#' To create a new object class, provide its name and a named list of its fields and their default values.
#' This generates a function that creates a new "instance" of the class each time that it is called.
#' For example, \code{poly <- oClass("polygon", sides = NA)} creates a new class called \code{"polygon"} with a
#' field called \code{"sides"} that can be created using \code{poly()}. Object methods that act on the
#' instance are created in the same manner as S3 methods. Therefore, class methods should be created
#' separately.
#'
#' Each instance of the object class is an environment. The parent environment of the instance is attached
#' to the attributes of the function created by the \code{oClass} function. This environment in the function
#' attributes serves as a instance template. Any variables that are specified during the creation of the
#' object instance are placed within the environment of said instance. When searching for an object within
#' an instance, the instance environment is first searched, then the template. This ensures that each object
#' instance remains as small as necessary and minimizes copying. A hashmap is not used by default so that
#' the instance size is smaller, but this can be changed by the \code{oClass} function.
#'
#' \code{oClass} objects can also inherit other class objects. If another class object is inherited,
#' the template environment in the inherited object's attributes is added to each instances search path.
#' Furthermore, the name of the inherited class **(and all classes it inherits)** is added to each instance's
#' S3 class. If an environment is inherited, then it is added to the search path.
#'
#' Since \code{oClass} relies on pointers to other environments, oClass instances are generally not portable.
#' If \code{portable=TRUE} is added, then each instance will include the default values of each inherited oClass.
#' This generally increases creation time and memory usage, but may result in marginally faster field access.
#' If the fields are relatively few and small, though, memory usage may decline when each Instance is portable.
#'
#' \code{oClass} instances automatically call \code{\link{init}} when created. Write custom S3 methods for \code{init}
#' to control this behavior. This requires the Class to be named so that instances inherit the named S3
#' class. The \code{\link{formals}} defines the Class generator's formal function arguments. If used, then
#' an \code{init} method for the Class should be created with identical formal arguments; otherwise,
#' instance creation may fail. If no formals are defined, then all objects passed to the generator function
#' are passed to \code{init} at creation.
#'
#' @return a function of class \code{"ClassGenerator"} with attributes describing each generated class instance
#' @export
#'
#' @examples
#'
#' ## Creating a Stack
#' stack <- oClass(
#'   "stack",
#'   data = list()
#' )
#'
#' # Methods
#' print.stack <- function(x, ...) print(x$data, ...)
#' push <- function(x, item){
#'   x$data[[length(x$data)+1]] <- item
#'   x
#' }
#' pop <- function(x){
#'   n <- length(x$data)
#'   last <- x$data[[n]]
#'   x$data[[n]] <- NULL
#'   last
#' }
#'
#' # Create a new instance
#' x <- stack()
#' push(x, 6)
#' push(x, 7)
#'
#' identical(x$data, list(6, 7)) # TRUE
#'
#' last <- pop(x)
#' identical(last, 7)            # TRUE
#' identical(x$data, list(6))    # TRUE
#'
#'
#' ## Person/Student
#' ##   Example of Inheritance and using Formals
#'
#' # Declare formal arguments of Person Generator
#' Person <- oClass(
#'   "Person",
#'   formals = list(first, last)
#' )
#'
#' # Formal arguments of init should match Person
#' init.Person <- function(x, first, last){
#'   x$first <- first
#'   x$last  <- last
#'   return(x)
#' }
#'
#' # Create init for Student class
#' init.Student <- function(x, first, last, year = 1, major = "Econ", ...){
#'   x$year  <- year
#'   x$major <- major
#'   add_fields(x, ...)
#'   init_next(x, first = first, last = last)
#'   return(x)
#' }
#'
#' # Create Student class, inherits Person
#' Student <- oClass(
#'   "Student",
#'   inherit = Person,
#'   formals = init.Student
#' )
#'
#' # Creating a student
#' Student("Chris", "Mann", 4, gpa = 4.0)
#'
oClass <- function(name = NULL, inherit = NULL, portable = FALSE, hash = FALSE, formals=NULL, ...){
  dots <- list2env(list(...))
  form <- substitute(formals)
  create_oclass(dots, name=name, inherit=inherit, portable=portable, hash=hash, formals=form)
}

#' Convert Object to an oClass Generator
#'
#' This function takes any named object such as an environment, fully-named list, or an Instance
#' and converts it to an \code{\link{oClass}} generator function so that instances have
#' access to the fields in the named object. See \code{\link{oClass}} for details about the
#' arguments and functionality of the oClass generator.
#'
#' @param x object to be cloned and converted
#' @param name character string describing the name of the class
#' @param inherit oClass used as the \code{\link[base]{parent.env}} for the generated instances
#' @param hash logical indicating whether instances should use hashing, see \code{\link[base]{new.env}}
#' @param portable logical indicating whether all inherited values should be copied into each instance
#' @param formals list containing the formal arguments for the resulting generator function. These are passed to the \code{\link{init}} function when a new instance is created.
#' @param deep logical. Should the object be deep-cloned?
#' @param ... named fields inherited by the class instance
#'
#' @return a function of class \code{"ClassGenerator"} with attributes describing each generated class instance
#' @export
#'
as.oClass <- function(x, name = NULL, inherit = NULL, portable = FALSE, hash = FALSE, formals=NULL, deep=TRUE, ...) UseMethod("as.oClass")

#' @method as.oClass ClassGenerator
#' @export
as.oClass.ClassGenerator <- function(x, name = NULL, inherit = NULL, portable = FALSE, hash = FALSE, formals=NULL, deep=TRUE, ...) x

#' @method as.oClass environment
#' @export
as.oClass.environment <- function(x, name = NULL, inherit = NULL, portable = FALSE, hash = FALSE, formals=NULL, deep=TRUE, ...){
  dots <- clone(x, deep=deep)
  list2env(list(...), envir=dots)
  form <- substitute(formals)
  create_oclass(dots, name=name, inherit=inherit, portable=portable, hash=hash, formals=form)
}

#' @method as.oClass list
#' @export
as.oClass.list <- function(x, name = NULL, inherit = NULL, portable = FALSE, hash = FALSE, formals=NULL, deep=TRUE, ...){
  dots <- list2env(clone(x, deep=deep))
  list2env(list(...), envir=dots)
  form <- substitute(formals)
  create_oclass(dots, name=name, inherit=inherit, portable=portable, hash=hash, formals=form)
}

#' @method as.oClass Instance
#' @export
as.oClass.Instance <- function(x, name = NULL, inherit = NULL, portable = FALSE, hash = FALSE, formals=NULL, deep=TRUE, ...){
  att  <- list(
    fields   = clone(x, deep=deep),
    hash     = hash,
    portable = portable,
    class    = "ClassGenerator"
  )
  if (!is.null(name)){
    att$class.names   <- c(name, class(att$fields))
    class(att$fields) <- att$class.names
  } else {
    att$class.names <- class(att$fields)
  }
  formals <- substitute(formals)
  if (is.null(formals)){ has_formals <- FALSE
  } else if (is.call(formals)){
    if (formals[[1]] != as.symbol("list")) stop("Object could not be parsed as arguments. Formals must be a list.")
    form_list <- to_formal_list(as.list(formals)[-1], parent.frame())
    form_int  <- to_formal_internal(as.list(formals)[-1], parent.frame())
    has_formals <- TRUE
  } else if (is.symbol(formals)){
    form <- eval(formals)
    if (is.function(form)){
      form_list <- formals(form)[-1]
      form_int  <- formals(form)
    } else if (!is.list(formals) || some_unnamed(formals)){
      stop("Provided formals are not a fully-named list.")
    } else {
      form_list <- formals
      form_int  <- from_formal_internal(formals)
    }
    has_formals <- TRUE
  } else { stop("Object could not be parsed as arguments. Formals must be a list.") }
  if (portable){
    fun <- generator_port_
    if (has_formals){
      formals(fun)   <- form_list
      body(fun)[[5]] <- form_int
    }
  } else {
    fun <- generator_noport_
    att$fields[[".__Class__"]] <- fun
    if (has_formals){
      formals(fun)   <- form_list
      body(fun)[[3]] <- form_int
    }
  }
  attributes(fun) <- att
  if (!is.null(inherit)){
    fun <- change_inherit(fun, inherit)
  }
  fun
}


create_oclass <- function(dots, name = NULL, inherit = NULL, portable = FALSE, hash = FALSE, formals=NULL){
  att  <- list(
    fields   = dots,
    hash     = hash,
    portable = portable,
    class    = "ClassGenerator"
  )
  if (is.null(inherit)){
    parent.env(dots) <- baseenv()
    dclass <- "Instance"
  } else if (is.oClass(inherit)){
    att$inherit   <- inherit
    dclass        <- class(attr(inherit, "fields"))
    if (portable){
      att$fields <- set_parent_fields(att$fields, attr(inherit, "fields"))
    } else {
      parent.env(dots)  <- attr(inherit, "fields")
    }
  } else { stop("oClasses can only inherit from other oClasses.") }
  if (is.null(name)){ warning("Classes should be named to ensure that methods are properly deployed.")
  } else if (is.character(name)) {
    att$name <- name
    dclass   <- c(name, dclass)
  } else { stop("Class names must be a character string.") }

  if (is.null(formals)){ has_formals <- FALSE
  } else if (is.call(formals)){
    if (formals[[1]] != as.symbol("list")) stop("Object could not be parsed as arguments. Formals must be a list.")
    form_list <- to_formal_list(as.list(formals)[-1], parent.frame())
    form_int  <- to_formal_internal(as.list(formals)[-1], parent.frame())
    has_formals <- TRUE
  } else if (is.symbol(formals)){
    form <- eval(formals)
    if (is.function(form)){
      form_list <- formals(form)[-1]
      form_int  <- from_formal_internal(formals(form))
    } else if (!is.list(formals) || some_unnamed(formals)){
      stop("Provided formals are not a fully-named list.")
    } else {
      form_list <- formals
      form_int  <- from_formal_internal(formals)
    }
    has_formals <- TRUE
  } else { stop("Object could not be parsed as arguments. Formals must be a list.") }

  if (portable){
    fun <- generator_port_
    if (has_formals){
      formals(fun)   <- form_list
      body(fun)[[5]] <- form_int
    }
  } else {
    fun <- generator_noport_
    att$fields[[".__Class__"]] <- fun
    if (has_formals){
      formals(fun)   <- form_list
      body(fun)[[3]] <- form_int
    }
  }
  att$class.names   <- dclass
  class(att$fields) <- dclass
  attributes(fun) <- att
  fun
}

set_parent_fields <- function(fields, parent){
  if (identical(parent, baseenv())) return(fields)
  for (i in ls(parent)){
    if (!exists(i, where = fields, inherits=FALSE)){
      fields[[i]] = parent[[i]]
    }
  }
  return(set_parent_fields(fields, parent.env(parent)))
}

generator_noport_ <- function(...){
  e <- structure(
    new.env(
      parent = attr(sys.function(), "fields"),
      hash   = attr(sys.function(), "hash")
    ),
    class   = attr(sys.function(), "class.names")
  )
  init(e, ...)
  e
}

generator_port_ <- function(...){
  e <- structure(
    new.env(hash = attr(sys.function(), "hash"), parent=baseenv()),
    class   = attr(sys.function(), "class.names")
  )
  fields <- attr(sys.function(), "fields")
  for (i in ls(fields, all.names=TRUE, sorted=FALSE)){
    e[[i]] <- fields[[i]]
  }
  init(e, ...)
  e
}


to_formal_list <- function(form, env){
  n <- length(form)
  if (n == 0) return(form)
  fname <- names(form)
  if (is.null(fname)){fname <- rep("",n)}
  for (i in 1:n){
    if (fname[i] == ""){
      if (is.symbol(form[[i]])){
        char <- as.character(form[[i]])
        if (char == "..dots") char <- "..."
        names(form)[i] <- char
      } else if (is.character(form[[i]])){ names(form)[i] <- form[[i]][1]
      } else { stop("All formal arguments must be named with a symbol or character string.") }
      form[[i]] <- substitute()
    } else {
      form[[i]] <- eval(form[[i]], envir=env)
    }
  }
  form
}

to_formal_internal <- function(form, env){
  form <- lapply(form, function(i){
    if (is.symbol(i) && identical(i, as.symbol("..dots"))) return(as.symbol("..."))
    return(i)
  })
  as.call(append(list(as.symbol("init"), as.symbol("e")), form))
}

#from_formal_internal <- function(form){
#  l <- list(as.symbol("init"), as.symbol("e"))
#  form <- as.list(form)[-1]
#  n <- length(form)
#  if (n == 0) return(as.call(l))
#  w <- sapply(1:n, function(i) identical(i, substitute()))
#  if (any(w)){
#    form[w] <- lapply(names(form)[w], as.symbol)
#    names(form)[w] <- ""
#  }
#  return(as.call(append(l, form)))
#}

from_formal_internal <- function(form){
  l    <- list(as.symbol("init"), as.symbol("e"))
  form <- as.list(form)[-1]
  n    <- length(form)
  if (n == 0) return(as.call(l))
  nm   <- names(form)
  form <- lapply(nm, as.symbol)
  nm[which(nm == "...")] <- ""
  names(form) <- nm
  as.call(append(l, form))
}

some_unnamed <- function(form){
  fname <- names(form)
  if (is.null(fname)) return(TRUE)
  any(fname == "")
}
