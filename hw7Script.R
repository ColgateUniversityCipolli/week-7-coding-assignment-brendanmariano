library(tidyverse)
#Function for part 1
pois.prob = function(x, size, type, rate)
{
  data = 0:(size -1)
  #Each if statement accounts for all of the potential type arguments
  if(type == "==")
  {
    data = tibble(data) |> #Converts vector to tibble
       mutate(data.dist = dpois(data,rate)) |> #Adds distribution values as a column
       filter(data == x) |> #Removes all rows except for the one with the desired x-value
       pull(data.dist) #Pulls the distribution value
    data# data Returns the value
  }
  else if(type == "!=")
  {
    data = tibble(data) |>
      mutate(data.dist = 1 - dpois(data,rate)) |>
      filter(data == x) |>
      pull(data.dist)
    data
  }
  else if(type == "<=")
  {
    data = tibble(data) |>
      mutate(data.dist = ppois(data,rate, lower.tail = TRUE)) |>
      filter(data == x) |>
      pull(data.dist)
    data
  }
  else if(type == ">=")
  {
    data = tibble(data) |>
      mutate(data.dist = ppois(data,rate, lower.tail = FALSE) + dpois(data,rate)) |>
      filter(data == x) |>
      pull(data.dist)
    data
  }
  else if(type == "<")
  {
    data = tibble(data) |>
      mutate(data.dist = ppois(data,rate, lower.tail = TRUE) - dpois(data,rate)) |>
      filter(data == x) |>
      pull(data.dist)
    data
  }
  else if(type == ">")
  {
    data = tibble(data) |>
      mutate(data.dist = ppois(data,rate, lower.tail = FALSE)) |>
      filter(data == x) |>
      pull(data.dist)
    data
  }
}
print(pois.prob(0, 500, "==", 2))
###############################################################################
#Function for part two
beta.prob = function(x, size, type, rate, alpha, beta)
{
  data = 0:(size-1)
  
  #Each if statement accounts for all of the potential type arguments
  if(type == "==")
  {
    data = tibble(data) %>% #Converts vector to tibble
      mutate(beta.distrib.mass = dbeta(x = data, shape1 = alpha, shape2 = beta)) |> #Adds column
      filter(data == x) |> #Removes all rows except for the one with the desired x-value
      pull(beta.distrib.mass) #Pulls the distribution value
    data #Returns the value
  }
  else if(type == "!=")
  {
    data = tibble(data) |> #Converts vector to tibble
      mutate(beta.distrib.mass = 1-dbeta(alpha,beta)) |> 
      filter(data == x) |> #Removes all rows except for the one with the desired x-value
      pull(data.dist.mass) #Pulls the distribution value
    data #Returns the value
  }
  else if(type == "<=")
  {
    data = tibble(data) |> #Converts vector to tibble
      mutate(beta.distrib = pbeta(alpha,beta,lower.tail = TRUE)) |>
      filter(data == x) |> #Removes all rows except for the one with the desired x-value
      pull(data.distrib) #Pulls the distribution value
    data #Returns the value
  }
  else if(type == ">=")
  {
    data = tibble(data) |> #Converts vector to tibble
      mutate(beta.distrib = pbeta(alpha,beta, lower.tail = FALSE) + dbeta(alpha,beta)) |>
      filter(data == x) |> #Removes all rows except for the one with the desired x-value
      pull(data.distrib) #Pulls the distribution value
    data #Returns the value
  }
  else if(type == "<")
  {
    data = tibble(data) |> #Converts vector to tibble
      mutate(beta.distrib = pbeta(alpha,beta, lower.tail = TRUE) - dbeta(alpha, beta)) |>
      filter(data == x) |> #Removes all rows except for the one with the desired x-value
      pull(data.distrib) #Pulls the distribution value
    data #Returns the value

  }
  else if(type == ">")
  {
    data = tibble(data) |> #Converts vector to tibble
      mutate(beta.distrib = pbeta(alpha,beta, lower.tail = FALSE)) |>
      filter(data == x) |> #Removes all rows except for the one with the desired x-value
      pull(data.distrib) #Pulls the distribution value
    data #Returns the value
  }
}
print(beta.prob(5, 55, "==",2,  2, 5))




