## helper-parametric_rings_estimator.R -----------------------------------------
## Kyle Butts, CU Boulder Economics
##
## This implements the nonparametric rings estimator

#' Panel nonparametric rings estimator
#'
#' @param y Numeric vector. First differenced outcome variable
#' @param dist Numeric vector. Distance to treatment
#' @param rings Numeric vector. End points to construct rings from, in order.
#'
parametric_ring_panel <- function(y, dist, rings) {
	df <- data.table(y = y, dist = dist)

	# Subsample to rings
	df <- df[dist <= max(rings) & dist >= min(rings), ]

	df[, rings := as.character(cut(dist, breaks = rings))]
	last_ring <- "({rings[length(rings)-1]},{rings[length(rings)]}]" |>
		glue::glue() |>
		as.character()

	# Estimate rings
	est <- fixest::feols(y ~ i(rings, ref=last_ring), df)

	# Extract rings
	coefs <- coef(est, keep = "rings::.*")
	se <- se(est, keep = "rings::.*")

	results <- purrr::map_df(seq_along(coefs), \(i) {
		interval <- stringr::str_match(names(coefs)[i], r"(rings::\((.*?),(.*?)\].*)")
		dt <- data.table(
			bin = i,
			x = as.numeric(c(interval[2:3], interval[3])),
			tau = c(coefs[i], coefs[i], NA),
			se = c(se[i], se[i], se[i])
		)
		return(dt)
	})

	results <- rbind(results, data.table(
		bin=c(length(rings)-1, length(rings)-1, length(rings)-1),
		x = c(rings[length(rings)-1], rings[length(rings)], rings[length(rings)]),
		tau = c(0,0, NA),
		se = c(0,0, 0)
	))

	results[, let(
		ci_lower = tau - 1.96 * se,
		ci_upper = tau + 1.96 * se
	)]

	return(results)
}
