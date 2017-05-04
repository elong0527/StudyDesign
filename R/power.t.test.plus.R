#' Enhanced output for Power calculations for one and two sample t tests
#'
#' All options is same as the 'power.t.test' We also generate Half width for 90% and 95% Confidence interval.
#'
#' @param ... options in 'power.t.test'
#' @param complete_rate the complete rate of the clinical trail
#' @export
#' @examples
#' power.t.test.plus(n = 100, delta = 0.5)
power.t.test.plus <- function(..., complete_rate = 0.8){

  tmp <- power.t.test(...)
  tmp$complete_rate = complete_rate
  tmp$n_random_per_grp = round(tmp$n / complete_rate,0)
  tmp$n_random  = tmp$n_random_per_grp * 2
  tmp$hf_95 <- - qnorm(0.025) * tmp$sd * sqrt(2) / sqrt(tmp$n)
  tmp$hf_90 <- - qnorm(0.05)  * tmp$sd * sqrt(2) / sqrt(tmp$n)
  tmp$note  <- paste("Note: n is effective subjects per group",
                     "\nNOTE: n_random_per_grp is the randomzied subjects per gorup after considering loss of follow up",
                     "\nNOTE: n_random is the total randomized subjects after considering loss of follow up")
  tmp
}

