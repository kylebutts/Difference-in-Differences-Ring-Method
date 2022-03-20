## helper-nonparametric_rings_estimator.R --------------------------------------
## Kyle Butts, CU Boulder Economics
##
## This implements the nonparametric rings estimator

#' Panel nonparametric rings estimator
#'
#' @param y Numeric vector. First differenced outcome variable
#' @param dist Numeric vector. Distance to treatment
#'
nonparametric_ring_panel <- function(y, dist) {
	# Keep from drawing plots
	pdf(NULL)
  est <- binsreg::binsreg(
    y = y, x = dist,
    # 0th degree polynomial and 0th degree standard errors
    line = c(0, 0), ci = c(0, 0)
  )
  dev.off()

  line <- as.data.table(est$data.plot$`Group Full Sample`$data.line)
  line <- line[, .(x, bin, tau = fit)]

  se <- as.data.table(est$data.plot$`Group Full Sample`$data.ci)
  se <- se[
    , se := (ci.r - ci.l) / 2 / 1.96
  ][
    , .(bin, se)
  ]

  line <- merge(line, se, by = "bin")

  line[,
	  let(
	    ci_lower = tau - 1.96 * se,
	    ci_upper = tau + 1.96 * se
  	)
	]

  # Counterfactual Trend
  count_trend <- line[bin == max(bin) & !is.na(tau)][1, ]$tau

  line[, let(
    tau = tau - count_trend,
    ci_lower = ci_lower - count_trend,
    ci_upper = ci_upper - count_trend
  )]

  line[line$bin == max(line$bin), let(se = 0, ci_lower = 0, ci_upper = 0)]

  # Right-most endpoint needs NA column
  line <- rbind(line, data.table(
  	bin = max(line$bin), x = max(line$x), tau = NA, se = 0, ci_lower = NA, ci_upper = NA
  ))

  # Subset to left and right endpoints
  line <- line[, .SD[c(1, .N-1, .N), ], by=bin]


  return(line)
}

#' Cross-sectional nonparametric rings estimator
#'
#' @param y Numeric vector. Outcome variable
#' @param dist Numeric vector. Distance to treatment
#' @param post Logical vector. True = post; false = pre.
#'
nonparametric_ring_cs <- function(y, dist, post) {
  est <- binsreg::binsreg(
    y = y, x = dist, by = as.logical(post), samebinsby = T,
    # 0th degree polynomial and 0th degree standard errors
    line = c(0, 0), ci = c(0, 0)
  )

  post_line <- data.table::as.data.table(est$data.plot$`Group TRUE`$data.line)
  post_line <- post_line[, .(x, bin, post_fit = fit)]
  pre_line <- data.table::as.data.table(est$data.plot$`Group FALSE`$data.line)
  pre_line <- pre_line[, .(x, bin, pre_fit = fit)]

  post_se <- data.table::as.data.table(est$data.plot$`Group TRUE`$data.ci)
  post_se <- post_se[
    , post_se := (ci.r - ci.l) / 2 / 1.96
  ][
    , .(bin, post_se)
  ]


  pre_se <- data.table::as.data.table(est$data.plot$`Group FALSE`$data.ci)
  pre_se <- pre_se[
    , pre_se := (ci.r - ci.l) / 2 / 1.96
  ][
    , .(bin, pre_se)
  ]

  post_line <- merge(post_line, post_se, by = "bin")
  pre_line <- merge(pre_line, pre_se, by = "bin")

  line <- merge(pre_line, post_line, by = c("x", "bin"))

  line[, let(
    tau = post_fit - pre_fit,
    se = sqrt(pre_se^2 + post_se^2)
  )][, let(
    ci_lower = tau - 1.96 * se,
    ci_upper = tau + 1.96 * se
  )]

  # Counterfactual Trend
  count_trend <- line[bin == max(bin) & !is.na(tau)][1, ]$tau

  line[, let(
    tau = tau - count_trend,
    ci_lower = ci_lower - count_trend,
    ci_upper = ci_upper - count_trend
  )]

  line[line$bin == max(line$bin), let(se = 0, ci_lower = 0, ci_upper = 0)]

  # Right-most endpoint needs NA column
  line <- rbind(line[, .(bin, x, tau, se, ci_lower, ci_upper)], data.table(
  	bin = max(line$bin), x = max(line$x), tau = NA, se = 0, ci_lower = NA, ci_upper = NA
  ))

  # Subset to left and right endpoints
  line <- line[, .SD[c(1, .N-1, .N), ], by=bin]

  return(line)
}
