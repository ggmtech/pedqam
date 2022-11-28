# create flow diag

library(flow)
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



flow::flow_view(is_prime)
