## -----------------------------------------------------------------------------
## figure-example_problems.R
## Kyle Butts, CU Boulder Economics
##
## Problem with ad-hoc approach to selecting ring
## -----------------------------------------------------------------------------

library(tidyverse)
library(glue)

# Load Custom Theme
source("https://raw.githubusercontent.com/kylebutts/templates/master/ggplot_theme/theme_kyle.R")

# ggpreview
ggpreview <- function(..., device = "png", cairo = TRUE) {
	fname <- tempfile(fileext = paste0(".", device))

	if (cairo & device == "pdf") {
		ggplot2::ggsave(filename = fname, device = cairo_pdf, ...)
	} else if (cairo & device == "png") {
		ggplot2::ggsave(filename = fname, device = device, type = "cairo", ...)
	} else {
		ggplot2::ggsave(filename = fname, device = device, ...)
	}

	system2("open", fname)
	invisible(NULL)
}


# ---- Generate Data -----------------------------------------------------------

df1 <- tibble(id = 1:500) %>%
	# Generate Treatment and Distance
	mutate(
		dist = runif(n(), 0, 2),
		dist = dist
	) %>%
	# Treatment Effects
	mutate(
		spill1 = 1.5 * exp(-2 * dist) * (dist <= 1),
		eps = rnorm(n(), mean = 0, sd = 0.05),
		`Treatment Effect` = spill1,
		`Counterfactual Trend` = 0, # dist * 0.075,
		`Observed Change` = `Treatment Effect` + `Counterfactual Trend`
	)


df1_long = df1 %>% pivot_longer(
		cols = c(`Treatment Effect`, `Counterfactual Trend`)
	)

# Preview of Data
ggplot() +
	geom_point(data = df1_long, aes(x=dist, y=value, color=name), shape = 19, alpha = 0.8) +
	scale_color_manual(
		values = c("#374E55", "#B24745", "#00A1D5", "#DF8F44",  "#79AF97", "#6A6599")
	) +
	labs(
		y = "Change in Outcome", shape = NULL, color = NULL, x = "Distance (miles)"
	) +
	theme_kyle(base_size = 16) +
	theme(legend.position = "bottom")


# ---- Plot function -----------------------------------------------------------

plot_est = function(df, dist_t, dist_c) {
	df = df %>% mutate(
		group = case_when(
			dist <= dist_t ~ "Treated",
			dist <= dist_c ~ "Control",
			TRUE ~ NA_character_
		)
	) %>%
		drop_na(group)

	mean_t = mean(df[df$group == "Treated",]$`Observed Change`)
	mean_c = mean(df[df$group == "Control",]$`Observed Change`)

	avg_pt = tribble(
		~x, ~y, ~group,
		0, mean_t, "Treat",
		dist_t, mean_t, "Treat",
		dist_t, mean_c, "Control",
		dist_c, mean_c, "Control"
	)

	rect = tribble(
		~xmin, ~xmax, ~ymin, ~ymax, ~group,
		0, dist_t, -Inf, Inf, "Treat",
		dist_t, dist_c, -Inf, Inf, "Control"
	)

	max_y = max(df$`Observed Change`)
	labs = tribble(
		~x, ~y, ~label,
		dist_t/2, max_y, "\u2190 Treated Units \u2192",
		dist_t + (dist_c - dist_t)/2, max_y, "\u2190 Control Units \u2192",
	)

	ggplot() +
		geom_rect(data = rect %>% filter(group == "Treat"),
							mapping = aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
							alpha = 0.2, fill = "#6A6599", color = NA
		) +
		geom_rect(data = rect %>% filter(group == "Control"),
							mapping = aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
							alpha = 0.2, fill = "#79AF97", color = NA
		) +
		geom_point(data = df1_long,
							 mapping = aes(x=dist, y=value, color=name, shape=name)
		) +
		geom_line(data = avg_pt %>% filter(group == "Treat"), mapping = aes(x = x, y = y),
							color = "#6A6599", size = 1.1
		) +
		geom_line(data = avg_pt %>% filter(group == "Control"), mapping = aes(x = x, y = y),
							color = "#79AF97", size = 1.1
		) +
		# Treated and Control Unit Labels
		geom_label(data = labs,
							 mapping = aes(x = x, y = y, label = label), size = 5, fill = NA
		) +
		# Mean Labels
		annotate("label", x = dist_t - 0.02, y = mean_t + 0.1, label = "bar(Delta*y)[T]",
						 parse=T, hjust = 1, size = 5, color = "#6A6599", fill = NA) +
		annotate("label", x = dist_c - 0.02, y = mean_c + 0.1, label = "bar(Delta*y)[C]",
						parse=T, hjust = 1, size = 5, color = "#79AF97", fill = NA) +
		# Difference-in-differences Labels
		annotate("point", x = dist_t, y = c(mean_t, mean_c), size = 2) +
		annotate("segment", x = dist_t, xend = dist_t, y = mean_t, yend = mean_c,
						 linetype = "dashed", size = 1.1) +
		annotate("text", x = dist_t + 0.03, y = mean_c + (mean_t - mean_c)*2/3,
						 label = "hat(tau)",
						 parse=T, hjust = 0, size = 7) +
		scale_color_manual(
			values = c("Counterfactual Trend" = "grey60", "Treatment Effect" = "grey20")
		) +
		scale_shape_manual(
			values = c("Counterfactual Trend" = 4, "Treatment Effect" = 16)
		) +
		scale_x_continuous(expand = expansion(mult = c(0, 0)), limits = c(0, 2)) +
		labs(
			y = "Change in Outcome", x = "Distance from Treatment (miles)", shape = NULL, color = NULL
		) +
		theme_kyle(base_size = 16) +
		theme(legend.position = "bottom")

}


# ---- (a) Correct -------------------------------------------------------------

(plot_a <- plot_est(df1, 1, 1.5))

# ---- (b) Treated too wide ----------------------------------------------------

(plot_b <- plot_est(df1, 1.5, 2))

# ---- (c) Treated too small ----------------------------------------------------

(plot_c <- plot_est(df1, 0.5, 1.5))

# ---- Export ------------------------------------------------------------------

ggsave("figures/example_a.jpeg", plot_a, dpi = 300, width = 10, height = 5, bg="white")
ggsave("figures/example_b.jpeg", plot_b, dpi = 300, width = 10, height = 5, bg="white")
ggsave("figures/example_c.jpeg", plot_c, dpi = 300, width = 10, height = 5, bg="white")



