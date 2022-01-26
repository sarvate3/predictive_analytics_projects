optimize_param <- function(N, product_class_units_sold, resource_allocation_by_class, profit_per_product_class, L)
{
  step_size <- 10
  epsilon <- 1e-5
  iterations <- 1000
  gradient <- vector()
  profit_history <- vector()

  for (i in 1:iterations)
  {
    # calculate gradient vector
    for (j in 1:N-1)
    {
      product_class_units_sold_prime <- product_class_units_sold
      product_class_units_sold_prime[j] <- product_class_units_sold_prime[j] + epsilon
      gradient[j] <- (calc_profit(product_class_units_sold_prime, resource_allocation_by_class, 
                                  profit_per_product_class) 
                      - calc_profit(product_class_units_sold, resource_allocation_by_class, 
                                    profit_per_product_class))/epsilon
    }
    
    # final product class units term is always zero, as it is defined by resource limit constraint
    
    gradient[N] <- 0
    
    # step in direction of maximizing objective function (profit)
    
    profit_history[i] <- calc_profit(product_class_units_sold, resource_allocation_by_class, profit_per_product_class)
    
    product_class_units_sold <- normalize_units(N, L, product_class_units_sold + step_size * gradient)
  }
  
  print(product_class_units_sold)
  return(profit_history)
}

# adjust last stock purchase amount to meet resource limit constraint
normalize_units <- function(N, L, product_class_units_sold)
{
  product_class_units_sold[N] <- (L - sum(product_class_units_sold[1:N-1]*resource_allocation_by_class[1:N-1]))/
    resource_allocation_by_class[N]
  return(product_class_units_sold)
}

# calculate total profit
calc_profit <- function(product_class_units_sold, resource_allocation_by_class, profit_per_product_class)
{
  return(product_class_units_sold*resource_allocation_by_class*profit_per_product_class)  
}

# initialize model parameters

L = 10000 # $10,000 to invest in stock portfolio
N = 10 # 10 different choices of possible stocks
var_ret <- 0.1 # variance in returns among possible stocks
var_pri <- 50 # variance in price among possible stocks
var_shar <- 10 # variance in number of shares of each stock purchased
#resource_allocation_by_class <- abs(rnorm(N, 30, var_pri)) # prices of each stock 
#product_class_units_sold <- normalize_units(N, L, abs(rnorm(N, 0, var_shar))) # shares of each stock purchased
#profit_per_product_class <- abs(rnorm(N,0,var_ret)) # expected yearly return by stock

resource_allocation_by_class <- c(10, 15, 20, 55, 10, 5, 100, 300, 80, 100)
product_class_units_sold <- normalize_units(N, L, c(10, 10, 10, 10, 10, 10, 10, 10, 10, 10))
profit_per_product_class <- c(rep(0.05, 10))

plot(optimize_param(N, product_class_units_sold, resource_allocation_by_class, profit_per_product_class, L),
     ylab = "profit")
