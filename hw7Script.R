library(tidyverse)
#Function for part 1
pois.prob = function(x, type, rate)
{
  #Each if statement accounts for all of the potential type arguments
  if(type == "==")
  {
    dpois(x, rate)
  }
  else if(type == "!=")
  {
    1 - dpois(x, rate)
  }
  else if(type == "<=")
  {
    ppois(x, rate, lower.tail = TRUE)
  }
  else if(type == ">=")
  {
    ppois(x, rate, lower.tail = FALSE) + dpois(x,rate) 
  }
  else if(type == "<")
  {
    ppois(x, rate, lower.tail = TRUE) - dpois(x,rate)
  }
  else if(type == ">")
  {
    ppois(x,rate, lower.tail = FALSE)
  }
}
#Rate modifies how spread out the data is
print(pois.prob(4, ">=", 2)) #Call for testing poissan
###############################################################################
#Function for part two
beta.prob = function(x, type, rate, alpha, beta)
{
  #Each if statement accounts for all of the potential type arguments
  if(type == "==" | type == "!=")
  {
    0
  }
  else if(type == "<=" | type == "<")
  {
    pbeta(x, shape1 = alpha, shape2 = beta, lower.tail = TRUE) 
  }
  else if(type == ">=" | type == ">")
  {
     pbeta(x, shape1 = alpha, shape2 = beta, lower.tail = FALSE) 
  }
}
print(beta.prob(.1, "<=", 2,  2, 5)) #Call to use for testing




