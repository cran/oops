#' @export
#' @method print Instance
#' @useDynLib oops get_address
print.Instance <- function(x, ...){
  cat(paste0(
    "<", class(x)[1], ": ", .Call("get_address", x), ">\n  ", instance2text(x)
  ))
}
#' @export
#' @method print ClassGenerator
#' @importFrom utils capture.output
print.ClassGenerator <- function(x, ...){
  ar <- capture.output(args(x))
  ar <- ar[1:(length(ar)-1)]
  ar[1] <- substring(ar[1], 10)
  cat(paste0("oClass::", attr(x, "class.names")[1], paste(ar, collapse="\n"), "\n  "))
  cat(inherit2text(x))
  cat(fields2text(x))
}

inherit2text <- function(x){
  cx <- class(x)[-1]
  nc <- length(cx)
  if (nc > 2){ return(paste0("inherit: ", paste(cx[1:(nc-1)], collapse=", "), "\n")) }
  return("")
}

instance2text <- function(x){
  paste(sapply(ls(x), function(name){
    paste0(name, ": ", print_short(x[[name]]))
  }), collapse = "\n  ")
}
fields2text <- function(x){
  e <- attr(x, "fields")
  paste(sapply(ls(e), function(name){
    paste0(name, ": ", print_short(e[[name]]))
  }), collapse = "\n  ")
}

form2text <- function(x){
  f <- formals(x)
  fname <- names(f)
  n <- length(fname)
  if (n == 0) return("")
  txt <- sapply(1:n, function(i){
    if (identical(f[[i]], substitute())){
      return(fname[i])
    }
    paste0(fname[i], " = ", print_short(f[[i]]))
  })
  paste(txt, collapse = ", ")
}

print_short <- function(x){
  if (is.null(x)) return("NULL")
  n <- length(x)
  cl <- paste0("<", class(x)[1], "> ")
  if (is.atomic(x)){
    if (n == 0) return(cl)
    if (is.character(x)){
      z <- paste0(cl, "\"", x[1], "\"")
      if (n > 1) return(paste0(z, "..."))
      return(z)
    }
    z <- as.character(x[1])
    if (n > 1) return(paste0(cl, z, "..."))
    return(paste0(cl, z))
  }
  return(cl)
}
