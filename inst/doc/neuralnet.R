## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(oops)

Layer <- oClass("Layer")

## -----------------------------------------------------------------------------
init.Layer <- function(self, dim){
  self$dim    <- dim
  self$beta   <- matrix(rep(0.01, prod(dim)), nrow = dim[1])
  self$output <- 0
  return(self)
}

## -----------------------------------------------------------------------------
Layer_Relu <- oClass(
  inherit = Layer,
  active_fun = function(x) ifelse(x <= 0, 0, x),
  deriv_fun  = function(x) ifelse(x <= 0, 0, 1)
)

Layer_Gcu <- oClass(
  inherit = Layer, 
  active_fun = function(x) x*cos(x),
  deriv_fun  = function(x) cos(x) - x*sin(x)
)

Layer_Softplus <- oClass(
  inherit = Layer,
  active_fun = function(x) log(1+exp(x)),
  deriv_fun  = function(x) 1/(1+exp(-x))
)


## -----------------------------------------------------------------------------
class( Layer_Relu(1) )

ls( Layer_Relu(1) )

## -----------------------------------------------------------------------------
NNetwork <- oClass("NNetwork")

## -----------------------------------------------------------------------------
init.NNetwork <- function(self, y, x){
  self$y      <- y
  self$X      <- X
  self$layers <- list(
    Layer_Gcu( dim=c(ncol(X), 4) ),
    Layer_Relu( dim=c(4, 2) ),
    Layer_Softplus( dim=c(2,1) )
  )
  train_nn(self)
}

## -----------------------------------------------------------------------------
NNetwork$mse        <- Inf
NNetwork$epoch      <- 0
NNetwork$tolerance  <- 1e-08
NNetwork$max_epoch  <- 5000
NNetwork$learn_rate <- 0.2

## -----------------------------------------------------------------------------
train_nn <- function(nn){
  while(TRUE) {
    nn$epoch <- nn$epoch + 1
    if (nn$epoch >= nn$max_epoch) break
    out  <- feed_forward(nn)          # returns output of last layer
    diff <- back_propagate(nn, out)   # returns change in mse
    if (diff < nn$tolerance){
      nn$flag <- 1
      break
    }
  }
  nn
}

## -----------------------------------------------------------------------------
feed_forward <- function(nn){
  output <- nn$X
  for (layer in nn$layers){
    output <- layer$active_fun(output %*% layer$beta)
    layer$output <- output
  }
  output
}

## -----------------------------------------------------------------------------
back_propagate <- function(nn, output){
  error  <- output - nn$y
  delta  <- 2*error/length(output)     # Loss = sum(error^2)/n => dLoss = 2/n*error
  nlayer <- length(nn$layers)
  
  for (i in nlayer:1){
    layeri <- nn$layers[[i]]
    if (i == nlayer){
      delta <- layeri$deriv_fun(output) * delta
    } else {
      delta <- (delta %*% t(nn$layers[[i+1]]$beta)) *
        layeri$deriv_fun(layeri$output)
    }
    if (i == 1){
      dweight <- t(nn$X) %*% delta
    } else {
      dweight <- t(nn$layers[[i-1]]$output) %*% delta
    }
    layeri$beta <- layeri$beta - nn$learn_rate * dweight
  }
  mse    <- mean(error^2)
  dmse   <- abs(nn$mse - mse)
  nn$mse <- mse
  return(dmse)
}

## -----------------------------------------------------------------------------
predict_nn <- function(nn, X){
  output <- X
  for (layer in nn$layers){
    output <- layer$active_fun(output %*% layer$beta)
  }
  output
}

## ----include=FALSE------------------------------------------------------------
data(cpi_data)

## ----eval=FALSE---------------------------------------------------------------
#  library(eFRED)
#  library(dplyr)
#  dat <- fred("CPIAUCSL")

## ----eval=FALSE---------------------------------------------------------------
#  cpi_data <- dat %>%  transmute(
#      date,
#      pi    = log(CPIAUCSL) - log(lag(CPIAUCSL, 12)),
#      pi.1  = lag(pi, 1),
#      pi.2  = lag(pi, 2),
#      pi.3  = lag(pi, 3),
#      pi.6  = lag(pi, 6),
#      pi.12 = lag(pi, 12)
#    ) %>%
#    na.omit

## -----------------------------------------------------------------------------
train <- cpi_data[which(cpi_data$date <= as.Date("2019-12-01")),]
test  <- cpi_data[which(cpi_data$date > as.Date("2019-12-01") & cpi_data$date < as.Date("2021-01-01")),]

## -----------------------------------------------------------------------------
y <- as.matrix(train$pi, ncol=1) 
X <- cbind(1, as.matrix(train[,3:7]))

nn <- NNetwork(y, X)

## -----------------------------------------------------------------------------
nn$epoch

nn$mse

nn$layers[[1]]$beta

## -----------------------------------------------------------------------------
Xtest <- cbind(1, as.matrix(test[, 3:7]))
prediction <- predict_nn(nn, Xtest)
prediction

mean(prediction - test$pi) # Average error

## -----------------------------------------------------------------------------
ols <- lm(y~-1+X, data = train)
mean(Xtest %*% as.matrix(ols$coeff) - test$pi)

