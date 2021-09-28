## analysis-linden_rockoff.R --------------------------------------------------------------------------
## Kyle Butts, CU Boulder Economics
##
## Relooking at Linden and Rockoff to try and estimate treatment effect curve

library(tidyverse)
library(here)
library(fixest)
library(kfbmisc)
library(patchwork)
library(lpridge)
library(KernSmooth)


raw <- haven::read_dta(here("data", "linden_rockoff.dta"))

df <- raw %>%
    filter(offender == 1) %>%
    mutate(
        distance = distance / 3,
        dist_post = distance * 10 * close_post_move,
        post = ifelse(post_move, "Post", "Pre"),
        srn_year = paste(srn, sale_year, sep = "-"),
        offdays = sale_date - offender_address_date
    )

df_short <- df %>% filter(distance <= 0.3)

# ---- Replicating Figure 2 ----------------------------------------------------

plot_kernel <- function(bandwidth, kernel, xint) {

    # Pre
    temp <- df %>% filter(abs(offdays) <= 365 & offdays < 0)

    if(kernel == "Epanechnikov") {
        pre <- lpridge::lpepa(temp$distance, temp$amt_Price, bandwidth = bandwidth)
        pre <- data.frame(x = pre$x.out, y = pre$est)
    } else {
        pre <- KernSmooth::locpoly(temp$distance, temp$amt_Price, bandwidth = bandwidth)
        pre <- as.data.frame(pre)
    }

    # Post
    temp <- df %>% filter(abs(offdays) <= 365 & offdays >= 0 & offender == 1)

    if(kernel == "Epanechnikov") {
        post <- lpridge::lpepa(temp$distance, temp$amt_Price, bandwidth = bandwidth)
        post <- data.frame(x = post$x.out, y = post$est)
    } else {
        post <- KernSmooth::locpoly(temp$distance, temp$amt_Price, bandwidth = bandwidth)
        post <- as.data.frame(post)
    }

    data = bind_rows(
        pre %>% mutate(name = "Average Home Price Before Offender Arrives"),
        post %>% mutate(name = "Average Home Price After Offender Arrives"),
    )

    # Plot
    title = glue::glue("Bandwidth of {bandwidth}")

    ggplot() +
        geom_vline(xintercept = xint, size = 1.2, linetype = "dotted") +
        geom_line(
            data = data, aes(x = x, y = y/1000, color = name, linetype = name), size = 1.2
        ) +
        labs(
            x = "Distance from Offender (mi.)", # title = title,
            y = NULL, color = NULL, linetype = NULL
        ) +
        scale_y_continuous(
            limits = c(120, 155),
            breaks = c(120, 130, 140, 150),
            labels = c("$120K", "$130K", "$140K", "$150K")
        ) +
        scale_color_manual(
            values = c("#0a4859", "#f69964"),
            guide = guide_legend(nrow = 1)
        ) +
        kfbmisc::theme_kyle(base_size = 14) +
        theme(
            plot.title = element_text(
                family = "fira_sans", size = 18, margin = margin(b = 4, unit = "pt")
            ),
            legend.position = c(0.6, 0.2),
            legend.background = element_rect(fill = "white")
        )
}

p1 <- plot_kernel(0.025, "Epanechnikov", 0.1)
p2 <- plot_kernel(0.075, "Epanechnikov", 0.15)
p3 <- plot_kernel(0.125, "Epanechnikov", 0.2)

# Remove y-axis for subfigures (b) and (c)
no_y <- theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.title.y = element_blank(),
              axis.line.y =  element_blank())

p <- (p1 + (p2 + no_y) + (p3 + no_y) +
          plot_layout(guides = "collect") &
          theme(
              legend.position = "bottom",
              plot.margin = margin(t = 4, l = 4, r = 4, b = 4, unit = "pt")
          ))



# kfbmisc::ggpreview(p1, width = 8, height = 5, device = "pdf")
#kfbmisc::ggpreview(p, width = 12, height = 4, device = "pdf")

ggsave(here::here("figures", "linden_rockoff_epa_025.pdf"), p1, width = 8, height = 5)
ggsave(here::here("figures", "linden_rockoff_epa_075.pdf"), p2, width = 8, height = 5)
ggsave(here::here("figures", "linden_rockoff_epa_125.pdf"), p3, width = 8, height = 5)
ggsave(here::here("figures", "linden_rockoff_nonparametric.pdf"), p, width = 12, height = 4)

#




# ---- diff-in-diff approach ---------------------------------------------------

# Table 3 column (5) using Offender Area
# did <- feols(log_price ~ close_offender + post_move + close_post_move
#     + HEATED + AGE + NEW + AIRCOND + BEDROOMS + BATHS + BQM2
#     + CNTLHEIGHT_2 + CNTLHEIGHT_3 + CNTLHEIGHT_4 + CNTLHEIGHT_5 + CNTLHEIGHT_6
#     + CNTLWALL_2 + CNTLWALL_3 + CNTLWALL_4 + CNTLWALL_5 + CNTLWALL_6
#     + CNTLWALL_7 + CNTLWALL_8 + CNTLWALL_9 + CNTLWALL_10
#     + CNTLBQM1_2 + CNTLBQM1_3 + CNTLBQM1_4 + CNTLBQM1_5 + CNTLBQM1_6 |
#     srn_year,
# data = df %>% filter(offender == 1), cluster = "neighborhood"
# )

did <- feols(
    log_price ~ close_offender + post_move + close_post_move | srn_year,
    data = df %>% filter(offender == 1), cluster = "neighborhood"
)

coef <- coef(did)[["close_post_move"]]
se <- se(did)[["close_post_move"]]


# Treated circle = 1/10 miles
summary(df[df$close_offender == 1, ][["distance"]])

# Control circle = 3/10 miles
summary(df[df$offender == 1, ][["distance"]])


line <- tibble(
    x = c(0, 0.1, 0.1, 0.1, 0.3),
    diff = c(coef, coef, NA, 0, 0),
    diff_ci_lower = c(coef - 1.96*se, coef - 1.96*se, NA, 0, 0),
    diff_ci_upper = c(coef + 1.96*se, coef + 1.96*se, NA, 0, 0),
)


# Plot
(p <- ggplot() +
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
    labs(x = "Distance to Offender", y = "Change in log(Home Price)") +
    scale_y_continuous())

ggsave(here("figures", "linden_rockoff_did.pdf"), width = 8, height = 2.5)




# ---- binsreg -----------------------------------------------------------------

`Log Price` <- df_short$log_price
`Distance to Offender` <- df_short$distance
`Post Move` <- df_short$post


# Symmetric Confidence Intervals
est <- binsreg::binsreg(
    y = `Log Price`, x = `Distance to Offender`, by = `Post Move`, samebinsby = T,
    # 0th degree polynomial and 0th degree standard errors
    line = c(0,0), ci = c(0,0)
)

post_line <- tibble(est$data.plot$`Group Post`$data.line) %>%
    select(x, bin, post_fit = fit)
pre_line  <- tibble(est$data.plot$`Group Pre`$data.line) %>%
    select(x, bin, pre_fit = fit)

post_se <- tibble(est$data.plot$`Group Post`$data.ci) %>%
    mutate(post_se = (ci.r - ci.l)/2/1.96) %>%
    select(bin, post_se)

pre_se  <- tibble(est$data.plot$`Group Pre`$data.ci) %>%
    mutate(pre_se = (ci.r - ci.l)/2/1.96) %>%
    select(bin, pre_se)

post_line <- left_join(post_line, post_se, by = c("bin"))
pre_line <- left_join(pre_line, pre_se, by = c("bin"))

line <- left_join(pre_line, post_line, by = c("x", "bin"))

line <- line %>%
    mutate(
        tau = post_fit - pre_fit,
        se = sqrt(pre_se^2 + post_se^2),
        ci_lower = tau - 1.96 * se,
        ci_upper = tau + 1.96 * se
    )

# Counterfactual Trend
count_trend <- line %>%
    dplyr::filter(bin == max(bin) & !is.na(tau)) %>%
    .[["tau"]]

line <- line %>% mutate(
    tau = tau - count_trend,
    ci_lower = ci_lower - count_trend,
    ci_upper = ci_upper - count_trend,
)

line[line$bin == max(line$bin), "se"] <- 0
line[line$bin == max(line$bin), "ci_lower"] <- 0
line[line$bin == max(line$bin), "ci_upper"] <- 0


(p <- ggplot() +
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
        labs(x = "Distance to Offender", y = "Change in log(Home Price)") +
        scale_y_continuous(limits = c(-0.35, 0.3), breaks = c(-0.3, -0.2, -0.1, 0.0, 0.1, 0.2, 0.3)))

# kfbmisc::ggpreview(p, device = "pdf", width = 8, height = 3)

ggsave(here("figures", "linden_rockoff.pdf"), width = 8, height = 2.5)




