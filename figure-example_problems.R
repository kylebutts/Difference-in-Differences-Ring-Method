## figure-example_problems.R ---------------------------------------------------
## Kyle Butts, CU Boulder Economics
##
## Problem with ad-hoc approach to selecting ring

library(tidyverse)
library(glue)
library(sf)
library(patchwork)
library(fixest)
# theme_kyle and ggpreview
library(kfbmisc)



# ---- Example of ID strategy --------------------------------------------------

set.seed(2021)

# Full rectangle
rectangle <- st_sf(
    id = 1,
    geometry = st_sfc(
        st_polygon(list(rbind(c(0, 0), c(1.5, 0), c(1.5, 1.5), c(0, 1.5), c(0, 0))))
    )
)

# Treatment point
treat <- st_sf(
    id = 1,
    geometry = st_sfc(
        st_point(c(0.75, 0.75))
    )
)

# Rings
treat_ring <- st_buffer(treat, dist = 0.2)
control_ring <- st_difference(st_buffer(treat, dist = 0.5), treat_ring)

# Random sample of points
points <- st_sf(id = 1:2000, st_sample(rectangle, 2000)) |>
    mutate(
        group = case_when(
            st_within(., treat_ring, sparse = F) ~ "Treated",
            st_within(., control_ring, sparse = F) ~ "Control",
            TRUE ~ NA_character_
        )
    )

# Plot figure
(plot_id <- ggplot() +
    geom_sf(data = rectangle, fill = NA) +
    geom_sf(data = treat_ring, fill = NA, alpha = 0.2) +
    geom_sf(data = control_ring, fill = NA, alpha = 0.2) +
    geom_sf(data = points, mapping = aes(color = group, shape = group)) +
    geom_sf(data = treat, color = "white", shape = 17, size = 4) +
    geom_sf(data = treat, color = "black", shape = 17, size = 3) +
    coord_sf(datum = NULL) +
    scale_color_manual(
        values = c("Treated" = "#6A6599", "Control" = "#79AF97"),
        na.value = "grey50"
    ) +
    scale_shape_manual(
        values = c("Treated" = 19, "Control" = 17),
        na.value = 15
    ) +
    labs(color = "Treatment Status", shape = "Treatment Status") +
    # theme_minimal(base_size = 12) +
    theme_kyle(base_size = 12) +
    theme(
        legend.position = c(0.25, 0.175),
        legend.background = element_rect(fill = "white")
    ))

ggsave("figures/example_id.pdf", plot_id, dpi = 300, width = 5, height = 5, bg = "white")



# ---- Generate Data -----------------------------------------------------------

set.seed(20210708)

df <- tibble(id = 1:10000) |>
    # Generate Treatment and Distance
    mutate(
        dist = runif(n(), 0, 1.5),
        dist = dist
    ) |>
    # Treatment Effects
    mutate(
        spill1 = 1.5 * exp(-2.3 * dist) * (dist <= 0.75),
        eps = rnorm(n(), mean = 0, sd = 0.05),
        `Treatment Effect` = spill1,
        `Counterfactual Trend` = 0, #dist * 0.075,
        `Observed Change` = `Treatment Effect` + `Counterfactual Trend`
    )


df_long <- df |> pivot_longer(
    cols = c(`Treatment Effect`, `Counterfactual Trend`, `Observed Change`)
)

true_te <- df |> filter(dist <= 0.75) |> pull(`Treatment Effect`) |> mean()


# Data-generating Process Plot

# avoid the line in the drop
df_long <- bind_rows(
    df_long |> filter(dist <= 0.75),
    tibble(dist = 0.75, name = "Treatment Effect", value = NA),
    df_long |> filter(dist > 0.75),
)

(plot_dgp <- ggplot() +
        # Potential outcomes
        geom_line(
            data = df_long |> filter(name != "Observed Change"),
            mapping = aes(x = dist, y = value, color = name),
            size = 1.5
        ) +
        # Color of lines
        scale_color_manual(
            values = c("Counterfactual Trend" = "grey60", "Treatment Effect" = "grey20")
            # values = c("Counterfactual Trend" = "#374E55", "Treatment Effect" = "#B24745")
        ) +
        # Make x-axis start at 0
        scale_x_continuous(expand = expansion(mult = c(0, 0)), limits = c(0, 1.5)) +
        # Labels
        labs(
            title = "Toy Example - Simulated Data",
            x = "Distance from Treatment (miles)",
            y = "Change in Outcome",
            color = NULL
        ) +
        # Themeing
        theme_kyle(base_size = 15, title_pos = "center", has_subtitle = T) +
        theme(
            legend.position = "bottom",
            plot.title = element_text(margin = margin(b = 12, unit = "pt"), size = 14),
            axis.title.y = element_text(margin = margin(r = 4), hjust = 0),
            axis.title.x = element_text(margin = margin(t = 4))
        ))


# ---- Plot function -----------------------------------------------------------

plot_est <- function(df, df_long, dist_t, dist_c, title = NULL, include_labs = F) {
    df <- df |>
        mutate(
            group = case_when(
                dist <= dist_t ~ "Treated",
                dist <= dist_c ~ "Control",
                TRUE ~ NA_character_
            )
        ) |>
        drop_na(group)

    mean_t <- mean(df[df$group == "Treated", ]$`Observed Change`)
    mean_c <- mean(df[df$group == "Control", ]$`Observed Change`)

    avg_pt <- tribble(
        ~x, ~y, ~group,
        0, mean_t, "Treat",
        dist_t, mean_t, "Treat",
        dist_t, mean_c, "Control",
        dist_c, mean_c, "Control"
    )

    rect <- tribble(
        ~xmin, ~xmax, ~ymin, ~ymax, ~group,
        0, dist_t, -Inf, Inf, "Treated Ring",
        dist_t, dist_c, -Inf, Inf, "Control Ring"
    )

    max_y <- max(df$`Observed Change`) * 0.98
    labs <- tribble(
        ~x, ~y, ~label,
        # dist_t/2, max_y, "\u2190 Treated Units \u2192",
        # dist_t + (dist_c - dist_t)/2, max_y, "\u2190 Control Units \u2192",
        dist_t / 2, max_y, "Treated Units",
        dist_t + (dist_c - dist_t) / 2, max_y, "Control Units",
    )

    # avoid the line in the drop
    df_long <- bind_rows(
        df_long |> filter(dist <= 0.75),
        tibble(dist = 0.75, name = "Treatment Effect", value = NA),
        df_long |> filter(dist > 0.75),
    )

    (plot <- ggplot() +
        # Shaded regions
        geom_rect(
            data = rect,
            mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = group),
            alpha = 0.2, color = NA
        ) +
        # Potential outcomes
        geom_line(
            data = df_long |> filter(name != "Observed Change"),
            mapping = aes(x = dist, y = value, color = name),
            size = 1.5
        ) +
        # Mean of treated and untreated
        geom_line(
            data = avg_pt |> filter(group == "Treat"), mapping = aes(x = x, y = y),
            color = "#6A6599", size = 1.1
        ) +
        geom_line(
            data = avg_pt |> filter(group == "Control"), mapping = aes(x = x, y = y),
            color = "#79AF97", size = 1.1
        ) +
        # Treated and Control Unit Labels
        {
            if (include_labs) {
                geom_text(
                    data = labs,
                    mapping = aes(x = x, y = y, label = label), size = 5, family = "merriweather"
                )
            }
        } +
        # Mean Labels
        annotate("text",
            x = dist_t - 0.025, y = mean_t + 0.15, label = "bar(Delta*y[T])",
            parse = T, hjust = 1, size = 6, color = "#6A6599", family = "merriweather"
        ) +
        annotate("text",
            x = dist_c - 0.025, y = mean_c + 0.15, label = "bar(Delta*y[C])",
            parse = T, hjust = 1, size = 6, color = "#79AF97", family = "merriweather"
        ) +
        # Difference-in-differences Labels
        annotate("point", x = dist_t, y = c(mean_t, mean_c), size = 2) +
        annotate("segment",
            x = dist_t, xend = dist_t, y = mean_t, yend = mean_c,
            linetype = "dashed", size = 1.1
        ) +
        annotate("text",
            x = dist_t + 0.03, y = mean_c + (mean_t - mean_c) * 2 / 3,
            label = paste0("hat(tau) == ", round(mean_t - mean_c, 2)),
            parse = T, hjust = 0, size = 6
        ) +
        # Color of lines
        scale_color_manual(
            values = c("Counterfactual Trend" = "grey60", "Treatment Effect" = "grey20")
            # values = c("Counterfactual Trend" = "#374E55", "Treatment Effect" = "#B24745")
        ) +
        # Fill of shaded regions
        scale_fill_manual(
            values = c("Treated Ring" = "#6A6599", "Control Ring" = "#79AF97")
        ) +
        # Make x-axis start at 0
        scale_x_continuous(expand = expansion(mult = c(0, 0)), limits = c(0, 1.5)) +
        # Labels
        labs(
            title = title,
            x = "Distance from Treatment (miles)",
            y = "Change in Outcome",
            color = NULL, fill = NULL,
        ) +
        # Themeing
        theme_kyle(base_size = 15, title_pos = "center", has_subtitle = T) +
        theme(
            legend.position = "bottom",
            plot.title = element_text(margin = margin(b = 12, unit = "pt"), size = 14),
            axis.title.y = element_text(margin = margin(r = 4), hjust = 0),
            axis.title.x = element_text(margin = margin(t = 4))
        ))
}



## (a) Correct

(plot_a <- plot_est(df, df_long, 0.75, 1.5, "(a) Correct Specification", include_labs = T))

## (b) Treated too wide

(plot_b <- plot_est(df, df_long, 1, 1.5, "(b) Treated is Too Wide", include_labs = F))

## (c) Treated too small

(plot_c <- plot_est(df, df_long, 0.5, 1.5, "(c) Treated is Too Narrow", include_labs = F))

## (d) Robustness check equals (c)

(plot_d <- plot_est(df, df_long, 0.25, 1, "(d) Robustness Check equals (c)", include_labs = F))


# Patchwork combine

(plot <- (plot_a + plot_b) / (plot_c + plot_d) + plot_layout(guides = "collect") &
    theme(
        legend.position = "bottom",
        plot.background = element_rect(fill = "transparent")
    ) &
    guides(color = guide_legend(override.aes = list(size = 1.5)))
)

# Export

# For Slides
# ggpreview(plot_a, width = 8, height = 6, device = "pdf", cairo = FALSE, bg = "white")
ggsave("figures/slides-example_dgp.pdf", plot_dgp, width = 8, height = 6, bg = "white")
ggsave("figures/slides-example_a.pdf", plot_a, width = 8, height = 6, bg = "white")
ggsave("figures/slides-example_b.pdf", plot_b, width = 8, height = 6, bg = "white")
ggsave("figures/slides-example_c.pdf", plot_c, width = 8, height = 6, bg = "white")
ggsave("figures/slides-example_d.pdf", plot_d, width = 8, height = 6, bg = "white")


# For Paper
# ggpreview(plot, width = 13, height = 8, device = "png", cairo = FALSE, bg = "white")
ggsave("figures/example.pdf", plot, dpi = 300, width = 13, height = 8, bg = "white")

## For web
ggsave("figures/rings_ex.svg", plot, dpi = 300, width = 13, height = 8)







# ---- Example did vs. Partition -----------------------------------------------

## Generate Data from DGP
set.seed(2)
df_binsreg <- df_long[df_long$name == "Observed Change", ]
df_binsreg <- df_binsreg[sample.int(nrow(df_binsreg), size = 300), ]
df_binsreg$value <- df_binsreg$value + rnorm(nrow(df_binsreg), 0, 1.2)


## Diff-in-diff

df_binsreg$treat <- df_binsreg$dist <= 0.75

did <- feols(value ~ 1 + i(treat), data = df_binsreg)

coef <- coef(did)[["treat::TRUE"]]
se <- se(did)[["treat::TRUE"]]

line <- tibble(
    x = c(0, 0.75, 0.75, .75, 1.5),
    diff = c(coef, coef, NA, 0, 0),
    diff_ci_lower = c(coef - 1.96*se, coef - 1.96*se, NA, 0, 0),
    diff_ci_upper = c(coef + 1.96*se, coef + 1.96*se, NA, 0, 0),
)


# Plot
(p_did <- ggplot() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    # Line
    geom_line(
        data = line, mapping = aes(x = x, y = diff),
        color = "#3e3788", size = 1.2
    ) +
    # CI
    geom_ribbon(
        data = line, mapping = aes(x = x, ymin = diff_ci_lower, ymax = diff_ci_upper),
        color = "#3e3788", fill = "#3e3788", alpha = 0.2
    ) +
    theme_kyle(base_size = 14) +
    labs(x = "Distance to Treatment", y = "Change in Y"))


## Binsreg

est <- binsreg::binsreg(
    y = df_binsreg$value, x = df_binsreg$dist, binspos = "es",
    # 0th degree polynomial and 0th degree standard errors
    line = c(0,0), ci = c(3,3)
)


# Converting things into a nice format
line <- tibble(est$data.plot$`Group Full Sample`$data.line) |>
    select(x, bin, fit = fit)

se <- tibble(est$data.plot$`Group Full Sample`$data.ci) |>
    mutate(se = (ci.r - ci.l)/2/1.96) |>
    select(bin, se)

line <- left_join(line, se, by = c("bin"))

line <- line  |>
    mutate(
        tau = fit,
        ci_lower = tau - 1.96 * se,
        ci_upper = tau + 1.96 * se
    )

# Counterfactual Trend
count_trend <- line |>
    dplyr::filter(bin == max(bin) & !is.na(tau)) |>
    slice(1) |>
    pull(tau)

line <- line |> mutate(
    tau = tau - count_trend,
    ci_lower = ci_lower - count_trend,
    ci_upper = ci_upper - count_trend,
)

(p_partition <- ggplot() +
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
        theme_kyle(base_size = 14) +
        labs(x = "Distance to Treatment", y = "Change in Y"))


library(patchwork)
p_did / p_partition

ggsave("figures/slides-example_did.pdf", p_did, width = 8, height = 6, bg = "white")
ggsave("figures/slides-example_partition.pdf", p_partition, width = 8, height = 6, bg = "white")




