## analysis-simulations.R ------------------------------------------------------
## Kyle Butts, CU Boulder Economics
##
## Monte-carlo simulations following DGP of Diamond and McQuade (2019)

library(data.table)
library(fixest)
library(binsreg)
library(sf)
library(ggplot2)
library(furrr)
library(kfbmisc)

# Rings estimator
source(here::here("helper-nonparametric_rings_estimator.R"))
source(here::here("helper-parametric_rings_estimator.R"))

# ---- Helper Functions --------------------------------------------------------

# Best case scenario: flat but incorrect distance
te1 <- \(dist, t) {
	te <- 0.15 * (dist < 0.8) * (t == 1)
	return(te)
}

# From Diamond and McQuad (2019)

te2 <- \(dist, t) {
	te <- 0.5 * (0.8 - dist)^2 * (dist < 0.8) * (t == 1)
	return(te)
}

# Additive effects: negative local and positive more widely
# parabola passing through {(0,-0.15), (0.5, 0.15), (0.8, 0)}
te3 <- \(dist, t) {
	return(
		(-0.15 + 1.2875 * dist - 1.375 * dist^2) * (dist < 0.8) * (t == 1)
	)
}


# Short treatment effect distance
te4 <- \(dist, t) {
	te <- 0.5 * (0.8 - dist)^2 * (dist < 0.25) * (t == 1)
	return(te)
}


# Simulate data
simulate_data <- function(te_function, n_units) {
	df <- data.table()
	# Sample from unit circle
	lat_out <- c()
	lon_out <- c()

	# Should only run once
	while(length(lat_out) < n_units) {
		lat  = runif(3*n_units, -1.2, 1.2)
		lon  = runif(3*n_units, -1.2, 1.2)
		dist = sqrt(lat^2 + lon^2)

		lat_out = c(lat_out, lat[dist < 1.2])
		lon_out = c(lon_out, lon[dist < 1.2])
	}

	df <- data.table(lat = lat_out[1:n_units], lon = lon_out[1:n_units])

	df$dist <- sqrt(df$lat^2 + df$lon^2)
	hist(df$dist)

	df$id <- 1:n_units

	df <- df |>
		DT(
			j = let(
				dist  = sqrt(lat^2 + lon^2),
				alpha = rnorm(n_units, 0, sqrt(0.025)),
				beta_lat = rnorm(1, 0, sqrt(0.036)),
				beta_lon = rnorm(1, 0, sqrt(0.036))
			)
		) |>
		# Two periods
		(\(df) {
			df1 <- df
			df2 <- df
			df1$t <- 0
			df2$t <- 1
			rbind(df1, df2)
		})() |>
		DT(
			j = let(
				te = te_function(dist, t),
				time_trend = alpha * t,
				location_effect = beta_lat * lat + beta_lon * lon,
				eps = rnorm(.N, 0, sqrt(0.036))
			)
		) |>
		DT(
			j = let(
				y = 1 + te + time_trend + location_effect + eps
			)
		)

	df <- df[
		order(id, t),
	][,
		D1_y := c(NA, diff(y)),
		by = id
	][
		t == 1,
	]

	return(df)
}

# Given result of nonparametric/parametric, predict TE at given distances
predict_te <- function(dist, line) {
	line <- as.data.table(line[!is.na(line$tau), ])

	# Make line connected
	line[1, "x"] <- 0
	line[nrow(line), "x"] <- 1.2

	evens <- seq(2, nrow(line) - 1, by = 2)
	odds <- evens + 1
	line[evens, "x"] <- (line[evens, "x"] + line[odds, "x"])/2
	line[odds, "x"] <- line[evens, "x"]


	# Uses the fact that I made sure there are only 2 points per bin
	bins <- base::findInterval(dist, unique(line$x))
	tau_hat <- line[bins*2, ]$tau

	return(tau_hat)
}

# Calculate Mean Absolute Bias
get_mean_abs_bias <- function(te_function, te_lim, line) {
	dist_seq <- seq(0, te_lim, by = 0.01)
	return(
		mean(abs(
			te_function(dist_seq, 1) - predict_te(dist_seq, line)
		))
	)
}

# Calculate MSPE
get_mspe <- function(te, dist, te_lim, line) {
	# Subset to observations with te > 0
	te   <-   te[dist <= te_lim]
	dist <- dist[dist <= te_lim]

	return(
		mean( (te - predict_te(dist, line))^2 )
	)
}


# Run simulation and report results
simulation <- function(n_units, te_name, te_function, times) {

	# Determine extent of treatment effects
	dist <- seq(0, 1, 0.05)
	te_lim <- dist[(te_function(dist, 1) == 0) |> which() |> min()]
	rm(dist)

	cli::cli({
		cli::cli_text("Starting on simulation with the following parameters")
		cli::cli_ul()
		cli::cli_li("n_units {.var {n_units}}")
		cli::cli_li("te_function {.var {te_name}}")
		cli::cli_li("te_lim {.var {te_lim}}")
		cli::cli_li("times {.var {times}}")
		cli::cat_line()
	})
	cli::cli_progress_step("Running")


	results <- lapply(1:times, \(i) {

		df <- simulate_data(te_function, n_units)

		# Estimate non-parametric and rings
		line_2rings = parametric_ring_panel(df$D1_y, df$dist, rings = c(0, 0.8, 1))
		line_3rings = parametric_ring_panel(df$D1_y, df$dist, rings = c(0, 0.4, 0.8, 1))
		line_np     = nonparametric_ring_panel(df$D1_y, df$dist)

		# Plot predictions
		# df$prediction_2rings = predict_te(df$dist, line_2rings)
		# df$prediction_3rings = predict_te(df$dist, line_3rings)
		# df$prediction_np = predict_te(df$dist, line_np)
		#
		# ggplot(df) +
		# 	geom_point(aes(x = dist, y = te), color = "black") +
		# 	geom_point(aes(x = dist, y = prediction_np), color = "red") +
		# 	geom_point(aes(x = dist, y = prediction_2rings), color = "blue") +
		# 	geom_point(aes(x = dist, y = prediction_3rings), color = "green")

		mspe_2rings = get_mspe(df$te, df$dist, te_lim, line_2rings)
		mspe_3rings = get_mspe(df$te, df$dist, te_lim, line_3rings)
		mspe_np     = get_mspe(df$te, df$dist, te_lim, line_np)

		mean_abs_bias_2rings = get_mean_abs_bias(te_function, te_lim, line_2rings)
		mean_abs_bias_3rings = get_mean_abs_bias(te_function, te_lim, line_3rings)
		mean_abs_bias_np     = get_mean_abs_bias(te_function, te_lim, line_np)

		abs_bias_at_0.4_2rings = abs(te_function(0.1, 1) - predict_te(0.1, line_2rings))
		abs_bias_at_0.4_3rings = abs(te_function(0.1, 1) - predict_te(0.1, line_3rings))
		abs_bias_at_0.4_np     = abs(te_function(0.1, 1) - predict_te(0.1, line_np))

		return(
			data.table(
				n_units = n_units,
				te_name = te_name,
				mspe_2rings = mspe_2rings,
				mspe_3rings = mspe_3rings,
				mspe_np = mspe_np,
				mean_abs_bias_2rings = mean_abs_bias_2rings,
				mean_abs_bias_3rings = mean_abs_bias_3rings,
				mean_abs_bias_np = mean_abs_bias_np,
				abs_bias_at_0.4_2rings = abs_bias_at_0.4_2rings,
				abs_bias_at_0.4_3rings = abs_bias_at_0.4_3rings,
				abs_bias_at_0.4_np = abs_bias_at_0.4_np
			)
		)
	})

	return(rbindlist(results))
}


# ---- Figure ------------------------------------------------------------------

colors <- c("#EBC944", "#9A2515", "#3e3788", "#829356")
(dgps <- ggplot() +
	xlim(0, 1.2) +
	# Plot tau's
	geom_function(
		fun = \(x) te4(x, 1),
		xlim = c(0.25, 1.2),
		size = 1.6, color = colors[4]
	) +
	geom_function(
		fun = \(x) te1(x, 1),
		xlim = c(0, 0.795),
		size = 1.6, color = colors[1]
	) +
	geom_function(
		fun = \(x) te1(x, 1),
		xlim = c(0.8, 1.2),
		size = 1.6, color = colors[1]
	) +
	geom_function(
		fun = \(x) te2(x, 1),
		size = 1.6, color = colors[2]
	) +
	geom_function(
		fun = \(x) te3(x, 1),
		size = 1.6, color = colors[3]
	) +
	geom_function(
		fun = \(x) te4(x, 1),
		xlim = c(0, 0.245),
		size = 1.6, color = colors[4]
	) +

	# Label tau's
	annotate("text",
		x = 0.05, y = 0.165, label = "tau[1]",
		parse = T, hjust = 1, size = 6, color = colors[1]
	) +
	annotate("text",
	  x = 0.05, y = 0.315, label = "tau[2]",
		parse = T, hjust = 1, size = 6, color = colors[2]
	) +
	annotate("text",
		x = 0.05, y = -0.065, label = "tau[3]",
		parse = T, hjust = 1, size = 6, color = colors[3]
	) +
	annotate("text",
	  x = 0.1, y = 0.315, label = "tau[4]",
		parse = T, hjust = 1, size = 6, color = colors[4]
	) +
	kfbmisc::theme_kyle(base_size = 14) +
	labs(y = "Treatment Effect", x = "Distance to Treatment"))

# kfbmisc::ggpreview(dgps, device = "pdf", width = 8, height = 5)
ggsave(here::here("figures", "monte_carlo_dgps.pdf"), dgps, width = 8, height = 5)


# ---- Simulations -------------------------------------------------------------


params = CJ(
	te_name = c("te1", "te2", "te3", "te4"),
	n_units = c(250, 500, 1000, 5000),
	times = 2000,
	sorted = F
)

params[, te_function := lapply(te_name, \(x) { get(x) })]

plan(multisession, workers = 12)

results <- params |>
	split(1:nrow(params)) |>
	future_map(\(row) {
			return(simulation(row$n_units, row$te_name, row$te_function[[1]], row$times))
		},
		.options = furrr_options(seed = 123)
	)


results_clean <- results |>
	rbindlist() |>
	DT(j = lapply(.SD, mean), by = .(n_units, te_name))

results_clean[, let(
	mspe_2rings = mspe_2rings / mspe_np,
	mspe_3rings = mspe_3rings / mspe_np,
	mspe_np = mspe_np / mspe_np,
	mean_abs_bias_2rings = mean_abs_bias_2rings / mean_abs_bias_np,
	mean_abs_bias_3rings = mean_abs_bias_3rings / mean_abs_bias_np,
	mean_abs_bias_np = mean_abs_bias_np / mean_abs_bias_np,
	abs_bias_at_0.4_2rings = abs_bias_at_0.4_2rings / abs_bias_at_0.4_np,
	abs_bias_at_0.4_3rings = abs_bias_at_0.4_3rings / abs_bias_at_0.4_np,
	abs_bias_at_0.4_np = abs_bias_at_0.4_np / abs_bias_at_0.4_np
)]

results_clean

arrow::write_parquet(results_clean, here::here("data/simulations.parquet"))
# results_clean <- arrow::read_parquet(here::here("data/simulations.parquet"))


# ---- Export as Table ---------------------------------------------------------

#' Prepares table body from dataframe
#'
#' @description Note this will copy the latex code into your clipboard. This is
#'   for my ease of use
#'
#' @param df Dataframe to convert to latex
#' @param col_formatters List of length `ncol(df)`. Use `NULL` to not format
#'   the column, otherwise provide a function that produces a string to that column.
#'
#' @return String containing the tex output
#' @export
df_to_table_body <- function(df, col_formatters = NULL) {
	if(is.null(col_formatters)) {
		col_formatters <- lapply(1:ncol(df), \(x) { return(NULL) })
	}

	# Figure out maximum length of each column (for pretty layout)
	max_col_lengths <- rep(0, times = ncol(df))
	for(i in 1:nrow(df)) {

		col_lengths <- c()

		for(j in 1:ncol(df)) {
			frmtr <- col_formatters[[j]]

			if(is.null(frmtr)) {
				col_lengths <- c(col_lengths, df[i,][[j]] |> as.character() |> nchar())
			} else {
				col_lengths <- c(col_lengths, df[i,][[j]] |> frmtr() |> nchar())
			}

		}

		max_col_lengths <- pmax(max_col_lengths, col_lengths)
	}


	# Go through row and construct latex string
	table = ""

	for(i in 1:nrow(df)) {
		row <- c()

		for(j in 1:ncol(df)) {
			frmtr <- col_formatters[[j]]

			if(is.null(frmtr)) {
				row <- c(row, df[i,][[j]] |> as.character())
			} else {
				row <- c(row, df[i,][[j]] |> frmtr())
			}

		}

		row <- row |>
			stringr::str_pad(max_col_lengths, side = "right", pad = " ") |>
			paste(collapse = " & ") |>
			(\(row) paste(row, "\\\\ \n"))()

		table <- paste0(table, row)
	}

	cat(table)
	clipr::write_clip(table)
	return(table)
}

wrap_latex <- \(x) { paste0("$", x, "$") }
process_2decimals <- \(x) { sprintf("$%.2f$", x) }

results_clean[te_name == "te1",][, .SD, .SDcols = patterns("n_units|rings$")] |>
	df_to_table_body(col_formatters = list(
		wrap_latex, process_2decimals, process_2decimals, process_2decimals,
		process_2decimals, process_2decimals, process_2decimals
	)) |>
	cat(file=here::here("tables/monte_te1.tex"))

results_clean[te_name == "te2", ][, .SD, .SDcols = patterns("n_units|rings$")] |>
	df_to_table_body(col_formatters = list(
		wrap_latex, process_2decimals, process_2decimals, process_2decimals,
		process_2decimals, process_2decimals, process_2decimals
	)) |>
	cat(file=here::here("tables/monte_te2.tex"))
results_clean[te_name == "te3", ][, .SD, .SDcols = patterns("n_units|rings$")] |>
	df_to_table_body(col_formatters = list(
		wrap_latex, process_2decimals, process_2decimals, process_2decimals,
		process_2decimals, process_2decimals, process_2decimals
	)) |>
	cat(file=here::here("tables/monte_te3.tex"))
results_clean[te_name == "te4", ][, .SD, .SDcols = patterns("n_units|rings$")] |>
	df_to_table_body(col_formatters = list(
		wrap_latex, process_2decimals, process_2decimals, process_2decimals,
		process_2decimals, process_2decimals, process_2decimals
	)) |>
	cat(file=here::here("tables/monte_te4.tex"))

