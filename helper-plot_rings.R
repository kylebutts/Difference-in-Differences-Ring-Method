# Plot rings
plot_rings <- function(line) {
	p <- ggplot() +
		# 0
		geom_hline(yintercept = 0, linetype = "dashed") +
		# Line
		geom_line(
			data = line, mapping = aes(x = x, y = tau),
			color = "#3e3788", size = 1.2
		) +
		# CI
		geom_ribbon(
			data = line, mapping = aes(x = x, ymin = ci_lower, ymax = ci_upper),
			color = "#3e3788", fill = "#3e3788", alpha = 0.2
		) +
		kfbmisc::theme_kyle(base_size = 14) +
		labs(x = "Distance to Treatment", y = "Change in Y")
}
