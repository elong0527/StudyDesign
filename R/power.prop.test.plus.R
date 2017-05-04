#' Enhanced output for Power calculations for two sample proportion tests
#'
#' All options is same as the 'power.prop.test' We also generate Half width for 90% and 95% Confidence interval.
#'
#' @param ... options in 'power.prop.test'
#' @param complete_rate the complete rate of the clinical trail
#' @export
#' @examples
#' The power is consistent with PASS 2008 "Two Independent Proportions (Null Case) Power Analysis"
#' power.prop.test.plus(n = 175, p1 = 0.01, p2 = 0.05, alternative = "two.sided", complete_rate = 0.85)
power.prop.test.plus <- function(..., complete_rate = 0.8){

  tmp <- power.prop.test(...)

  if(tmp$alternative == "two.sided"){
    n  <- tmp$n
    p1 <- tmp$p1
    p2 <- tmp$p2
    v1 <- p1 * (1 - p1) / n
    v2 <- p2 * (1 - p2) / n
    se <- sqrt(v1 + v2)
  }

  tmp$complete_rate = complete_rate
  tmp$n_random_per_grp = round(tmp$n / complete_rate,0)
  tmp$n_random  = tmp$n_random_per_grp * 2
  tmp$hf_95 <- - qnorm(0.025) * se
  tmp$hf_90 <- - qnorm(0.05)  * se
  tmp$note  <- paste("Note: n is effective subjects per group",
                     "\nNOTE: n_random_per_grp is the randomzied subjects per gorup after considering loss of follow up",
                     "\nNOTE: n_random is the total randomized subjects after considering loss of follow up",
                     "Margin of Error / Half Width reference: https://onlinecourses.science.psu.edu/stat500/node/31")
  tmp
}


