## analysis-linden_rockoff.R --------------------------------------------------------------------------
## Kyle Butts, CU Boulder Economics
##
## Relooking at Linden and Rockoff to try and estimate treatment effect curve

library(tidyverse)
library(here)
library(fixest)
library(kfbmisc)


raw <- haven::read_dta(here("data", "linden_rockoff.dta"))

df <- raw %>%
    filter(offender == 1) %>%
    mutate(
        distance = distance / 3,
        dist_post = distance * 10 * close_post_move,
        post = ifelse(post_move, "Post", "Pre"),
        srn_year = paste(srn, sale_year, sep = "-")
    )

df_short <- df %>% filter(distance <= 0.3)



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
p <- ggplot() +
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
    labs(x = "Distance to Offender", y = "Change in log(Home Price)")





# ---- lspartition -------------------------------------------------------------

# Manual bug fix
source("code/lspartition/lspfunctions.R")
source("code/lspartition/lspkselect.R")
source("code/lspartition/lsplincom.R")
source("code/lspartition/lsprobust.R")

`Log Price` <- df_short$log_price
`Distance to Offender` <- df_short$distance
`Post Move` <- df_short$post

# est <- lsplincom(
#         y = `Log Price`, x = `Distance to Offender`,
#         # Groups
#         G = `Post Move`,
#         # Post - Pre
#         R = c(1, -1),
#         # Options passed to lspartition::lsprobust
#         # Piecewise constant
#         m = 1, method = "pp"
#     ) %>%
#     .[["Estimate"]] %>%
#     as.data.frame()


knots <- quantile(`Distance to Offender`, probs = seq(0, 1, by = 1/12))
knots <- c(min(`Distance to Offender`), 0.05, 0.1, 0.15, 0.2, max(`Distance to Offender`))

est <- lsplincom(
        y = `Log Price`, x = `Distance to Offender`,
        # Groups
        G = `Post Move`,
        # Post - Pre
        R = c(1, -1),
        # Manually specify knots
        knot = knots,
        # Options passed to lspartition::lsprobust
        # Piecewise constant
        m = 1, method = "pp"
    ) %>%
    .[["Estimate"]] %>%
    as.data.frame() %>%
    mutate(
        x = X1,
        tau = tau.cl, se = se.cl,
        ci_upper = tau + 1.96 * se,
        ci_lower = tau - 1.96 * se,
    )


est <- est %>% group_by(tau) %>% slice(1)


# Create lines for ggplot
line <- NULL

for(i in 1:nrow(est)) {
    line <- bind_rows(line,
        tibble(
            x = c(knots[i], knots[i+1], knots[i+1]),
            bin = i,
            tau = c(est[i,][["tau"]], est[i,][["tau"]], NA),
            se =  c(est[i,][["se"]], est[i,][["se"]], NA),
            ci_upper =  c(est[i,][["ci_upper"]], est[i,][["ci_upper"]], NA),
            ci_lower =  c(est[i,][["ci_lower"]], est[i,][["ci_lower"]], NA),
        )
    )
}


# Counterfactual Trend
count_trend <- line %>%
    dplyr::filter(bin == max(bin) & !is.na(tau)) %>%
    .[["tau"]]

line <- line %>% mutate(
    tau = tau - count_trend,
    ci_lower = ci_lower - count_trend,
    ci_upper = ci_upper - count_trend,
)

# Remove standard errors from last estimate
line[line$bin == max(line$bin) & !is.na(line$tau), c("ci_upper", "ci_lower")] <- 0


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
    labs(x = "Distance to Offender", y = "Change in log(Home Price)"))


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

# Uniform comparison
# binsreg::binspwc(
# 	y = `Log Price`, x = `Distance to Brothel`, by = `Post`, samebinsby = T,
# 	pwc = c(0,0)
# )

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
        labs(x = "Distance to Offender", y = "Change in log(Home Price)"))

# kfbmisc::ggpreview(p, device = "pdf", width = 8, height = 3)

ggsave(here("figures", "linden_rockoff.pdf"), width = 8, height = 2.5)




