## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(oops)

## -----------------------------------------------------------------------------
Agent <- oClass(
  "Agent",
  cash  = 0
)

## -----------------------------------------------------------------------------
Agent

## -----------------------------------------------------------------------------
agent1 <- Agent()
agent2 <- Agent()

agent1

## -----------------------------------------------------------------------------
agent1$cash

## -----------------------------------------------------------------------------
Agent$cash <- 10
agent1$cash

## -----------------------------------------------------------------------------
agent1$cash <- 250
agent1$cash 

agent2$cash

## -----------------------------------------------------------------------------
agent2$items <- list("computer", "code")

Agent$items <- list()
agent1$items

agent2$items

## -----------------------------------------------------------------------------
init.Agent <- function(x, ...){
  x$id <- round(runif(1) * 10000)
  init_next(x, ...)
  return(x)
}

## -----------------------------------------------------------------------------
# Create new agent and check id
agent1 <- Agent()
agent1$id

# Create a new agent with extra field (initialized through init.Instance)
agent2 <- Agent(name = "Dave")
agent2$id   # different from agent1

agent2$name

## -----------------------------------------------------------------------------
print.Agent <- function(x, ...){
  text <- paste0("Agent, #", x$id, ": $", x$cash, "\n")
  cat(text)
}

agent1
agent2

## -----------------------------------------------------------------------------
transfer <- function(from, to, amount) UseMethod("transfer")
transfer.Agent <- function(from, to, amount){
  from$cash <- from$cash - amount
  to$cash   <- to$cash   + amount
}

## -----------------------------------------------------------------------------
transfer(agent1, agent2, 2)

agent1
agent2

## -----------------------------------------------------------------------------
fake_agent1 <- clone(agent1)
agent1
fake_agent1

identical(agent1, fake_agent1)

## -----------------------------------------------------------------------------
agent1[["partner"]] <- agent2
fake_agent1 <- clone(agent1)

identical(agent1$partner, fake_agent1$partner) # Same!

## -----------------------------------------------------------------------------
fake_agent1 <- clone(agent1, deep=TRUE)
identical(agent1$partner, fake_agent1$partner) # Different!

## -----------------------------------------------------------------------------
Household <- oClass(
  "Household",
  inherit = Agent,
  rent = 1000
)

## -----------------------------------------------------------------------------
house <- Household()

# Variables 
house$rent   # from "Household"
house$cash   # from "Agent"
house$id     # from "Init.Agent"

# Using an Agent Method
transfer(house, agent1, -2000)
house$cash

## -----------------------------------------------------------------------------
# create pay_rent function that reduces cash by rent amount
pay_rent <- function(x) UseMethod("pay_rent")
pay_rent.Household <- function(x){
  x$cash <- x$cash - x$rent
  print(paste0("Household paid $", x$rent, " in rent and now has $", x$cash))
}

# pay rent :(
pay_rent(house)

## -----------------------------------------------------------------------------
## DON'T RUN:
## pay_rent(agent1)
##
## Error in UseMethod("pay_rent") :  
##   no applicable method for 'pay_rent' applied to an object of class "c('Agent', 'Instance')"

## -----------------------------------------------------------------------------
Household <- oClass(
  "Household",
  inherit = Agent,
  formals = list(rent, ...)
)

Household

## -----------------------------------------------------------------------------
init.Household <- function(x, rent, ...){
  x$rent <- rent 
  init_next(x, ...)
  x
}

house <- Household(500)
house$rent

## -----------------------------------------------------------------------------
# Update the 'init' method
init.Household <- function(x, rent = 1000, cash = 5000){
  x$rent <- rent
  x$cash <- cash
  x
}

# Change the formals list
Household <- change_formals(Household, from_init=init.Household)
Household 

# Create new instance
house <- Household()
house$rent

## -----------------------------------------------------------------------------
Agent <- oClass(
  "Agent",
  cash  = 0,
  portable = TRUE
)

agent1 <- Agent()
ls(agent1)        # includes "cash" from "Agent"

parent.env(agent1)

