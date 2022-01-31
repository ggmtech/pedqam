# create flow diag

is_prime <- function(num) {
  if (num == 2) {
    TRUE
  } else if (any(num %% 2:(num-1) == 0)) {
    FALSE
  } else { 
    TRUE
  }
}

is_prime(997)


library(flow)
flow::flow_view(is_prime)
