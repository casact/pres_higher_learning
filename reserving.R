library(raw)
library(tidyverse)
library(lme4)
source("common.R")

#==============================================
#' This function will:
#'   1. Form prior paid and incurred columns
#'   2. Calc incremental paid and incurred
#'   3. Form a column which combines lags 7 and higher
#'   4. Convert Lag and AY to factors
#==============================================
adjust_triangle <- function(tbl_in) {
  
  tbl_in %>% 
    group_by(AccidentYear, .add = TRUE) %>% 
    arrange(Lag, .by_group = TRUE) %>% 
    mutate(
      prior_paid = lag(CumulativePaid)
      , incremental_paid = coalesce(
        CumulativePaid - prior_paid
        , prior_paid
      )
      , prior_incurred = lag(CumulativeIncurred)
      , incremental_incurred = coalesce(
        CumulativeIncurred - prior_incurred
        , prior_incurred
      )
      , upper = DevelopmentYear <= 1997
    ) %>% 
    mutate(
      lag_tail = ifelse(Lag >= 7, 'tail', Lag) %>% as.factor()
      , Lag = as.factor(Lag)
      , AY = as.factor(AccidentYear)
    ) %>% 
    ungroup()
  
}

#=========================================
# New Jersey manufacturing
#=========================================

tbl_njm <- raw::NJM_WC %>% 
  adjust_triangle()

tbl_njm_upper <- tbl_njm %>% 
  filter(upper)

tbl_njm_lower <- tbl_njm %>% 
  filter(!upper)

fit_all <- tbl_njm_upper %>% 
  filter(Lag != '1') %>% 
  lm(formula = incremental_paid ~ 1 + prior_paid, data = .)

lower_y <- min(predict(fit_all))

plt_njm_all <- tbl_njm_upper %>% 
  filter(Lag != '1') %>% 
  ggplot(aes(prior_paid, incremental_paid)) +
  geom_point() +
  scale_y_continuous(lim = c(lower_y, NA), labels = scales::comma) + 
  scale_x_continuous(labels = scales::comma) + 
  labs(
    title = "Upper triangle of New Jersey Manufacturing Workers Comp"
    , x = "Cumulative prior paid"
    , y = "Incremental paid"
  ) +
  theme_minimal()

plt_njm_all

ggsave('images/plt_njm_all.png', dpi = 300, width = 13.2, height = 4.7, units = 'in')

plt_njm_all_fit <- tbl_njm_upper %>% 
  filter(Lag != '1') %>% 
  ggplot(aes(prior_paid, incremental_paid)) +
  geom_smooth(method = lm) +
  geom_point() +
  scale_y_continuous(lim = c(lower_y, NA), labels = scales::comma) + 
  scale_x_continuous(labels = scales::comma) + 
  labs(
    title = "Upper triangle of New Jersey Manufacturing Workers Comp"
    , subtitle = "Pooled fit"
    , x = "Cumulative prior paid"
    , y = "Incremental paid"
  ) +
  theme_minimal()

plt_njm_all_fit

ggsave('images/plt_njm_all_fit.png', dpi = 300, width = 13.2, height = 4.7, units = 'in')

plt_simpsons_paradox <- tbl_njm_upper %>% 
  filter(Lag != '1') %>% 
  ggplot(aes(prior_paid, incremental_paid)) +
  geom_point(aes(color = Lag)) +
  geom_smooth(se = FALSE, method = lm, aes(color = Lag)) +
  geom_smooth(se = FALSE, method = lm, color = 'black') +
  scale_y_continuous(lim = c(lower_y, NA), labels = scales::comma) + 
  scale_x_continuous(labels = scales::comma) + 
  labs(
    title = "Upper triangle of New Jersey Manufacturing Workers Comp"
    , subtitle = "Individual and pooled fit. Also: Simpson's paradox!"
    , x = "Cumulative prior paid"
    , y = "Incremental paid"
  ) +
  theme_minimal()

plt_simpsons_paradox

ggsave('images/plt_simpsons_paradox.png', dpi = 300, width = 13.2, height = 4.7, units = 'in')

plt_njm_by_ay_focus <- tbl_njm_upper %>% 
  filter(Lag %in% c('2', '7', '8', '9', '10')) %>% 
  ggplot(aes(prior_paid, incremental_paid, group = Lag)) +
  geom_point(aes(color = Lag)) +
  geom_smooth(se = FALSE, method = lm, aes(color = Lag)) +
  scale_y_continuous(lim = c(lower_y, NA), labels = scales::comma) + 
  scale_x_continuous(labels = scales::comma) + 
  labs(
    title = "Upper triangle of New Jersey Manufacturing Workers Comp"
    , subtitle = "Lag 2 and lags 7 through 10"
    , x = "Cumulative prior paid"
    , y = "Incremental paid"
  ) +
  theme_minimal()

plt_njm_by_ay_focus
ggsave('images/plt_njm_by_ay_focus.png', dpi = 300, width = 13.2, height = 4.7, units = 'in')

#===================================================================
#' Fit NJM
#' 
#' We fit several models. Note that we will always use the lag as 
#' an interaction term. Different slope = interaction
#====================================================================

fit_njm_no_intercept <- lm(
  incremental_paid ~ 0 + prior_paid:Lag
  , data = tbl_njm_upper %>% filter(Lag != 1)
)

fit_njm_no_intercept_tail <- lm(
  incremental_paid ~ 0 + prior_paid:lag_tail
  , data = tbl_njm_upper %>% filter(Lag != 1)
)

# This code is probably fine, but I'm tempted to re-do it. I might 
# prefer to have a zero so that the lag one intercept doesn't get swallowed
# by the global intercept. Probably doesn't matter, but tempted to give it 
# another look.s
fit_njm_intercept <- lm(
  incremental_paid ~ 1 + lag_tail + prior_paid:lag_tail
  , data = tbl_njm_upper %>% filter(Lag != 1)
)

# This may be problematic! The prior_paid is not conditioned on the lag.
fit_njm_no_intercept_mixed <- lmer(
  incremental_paid ~  0 + prior_paid + (0 + prior_paid | lag_tail)
  , data = tbl_njm_upper %>% filter(Lag != 1)
)

ranef(fit_njm_no_intercept_mixed)
fixef(fit_njm_no_intercept_mixed)

tbl_njm_oos <- calc_prediction_error(tbl_njm_lower, fit_njm_no_intercept, 'no intercept', 'incremental_paid') %>% 
  bind_rows(
    calc_prediction_error(tbl_njm_lower, fit_njm_no_intercept_tail, 'no intercept tail', 'incremental_paid')
  ) %>% 
  bind_rows(
    calc_prediction_error(tbl_njm_lower, fit_njm_intercept, 'intercept', 'incremental_paid')
  )

tbl_njm_oos %>% 
  filter(DevelopmentYear == 1998) %>% 
  model_performance()

tbl_njm_upper <- tbl_njm_upper %>% 
  mutate(use_intercept = lag_tail == 'tail')

tbl_njm_lower <- tbl_njm_lower %>% 
  mutate(use_intercept = lag_tail == 'tail')

# I have no memory of what I was trying to do here
fit_njm_simple_intercept_tail <- lm(
  incremental_paid ~ 1 + prior_paid
  , data = tbl_njm_upper %>% filter(Lag != 1, use_intercept)
)

mojo <- tbl_njm_oos %>% 
  bind_rows(
    tbl_njm_lower %>% 
      filter(use_intercept) %>% 
      calc_prediction_error(fit_njm_simple_intercept_tail, 'simple intercept', 'incremental_paid')
  ) %>% 
  bind_rows(
    tbl_njm_lower %>% 
      filter(!use_intercept) %>% 
      calc_prediction_error(fit_njm_no_intercept, 'simple intercept', 'incremental_paid')
  )

mojo %>% 
  filter(DevelopmentYear == 1998) %>% 
  model_performance()

#=========================================
#' Workers comp
#' 
#' We extend the analysis to every company writing WC in the database
#=========================================

tbl_wc <- raw::wkcomp %>% 
  group_by(Company) %>% 
  adjust_triangle()

tbl_wc_upper <- tbl_wc %>% 
  filter(upper)

tbl_wc_lower <- tbl_wc %>% 
  filter(!upper)

fit_wc_no_intercept <- lm(
  incremental_paid ~ 0 + prior_paid:Lag
  , data = tbl_wc_upper %>% filter(Lag != 1)
)

fit_wc_no_intercept_tail <- lm(
  incremental_paid ~ 0 + prior_paid:lag_tail
  , data = tbl_wc_upper %>% filter(Lag != 1)
)

# The warning we get here is something worth exploring!
fit_wc_mixed <- lmer(
  incremental_paid ~ 0 + prior_paid:lag_tail + (0 + prior_paid:lag_tail | Company)
  , data = tbl_wc_upper %>% filter(Lag != 1)
)

tbl_wc_oos <- calc_prediction_error(tbl_wc_lower, fit_wc_no_intercept, 'no intercept', 'incremental_paid') %>% 
  bind_rows(
    calc_prediction_error(tbl_wc_lower, fit_wc_no_intercept_tail, 'pooled tail', 'incremental_paid')
  ) %>% 
  bind_rows(
    calc_prediction_error(tbl_wc_lower, fit_wc_mixed, 'mixed', 'incremental_paid')
  )

tbl_wc_oos %>% 
  filter(DevelopmentYear == 1998) %>% 
  model_performance()

plt_wc_model_box <- tbl_wc_oos %>% 
  filter(DevelopmentYear == 1998) %>% 
  model_compare_box() + 
  scale_y_continuous(labels = scales::comma)

plt_wc_model_box
ggsave('images/plt_wc_model_box.png', dpi = 300, width = 13.2, height = 4.7, units = 'in')

# And again, I'm not sure what I was on about here. Didn't make it into the 
# presentation, but could be interesting.
fit_wc_company <- lmer(
  incremental_paid ~ 1 + prior_paid + (1 | Company/lag_tail)
  , data = tbl_wc_upper %>% filter(Lag != 1)
)

mojo <- tbl_wc_lower %>% 
  bind_rows(
    calc_prediction_error(tbl_wc_lower, fit_wc_company, 'company', 'incremental_paid')
  )

mojo %>% 
  model_performance()

fit_wc_mixed_company <- lmer(
  incremental_paid ~ 1 + prior_paid + (1 | Company/lag_tail) + (prior_paid | Company/lag_tail)
  , data = tbl_wc_upper
)

mojo <- tbl_wc_lower %>% 
  calc_prediction_error(fit_wc_mixed_company, 'company', 'incremental_paid')

mojo %>% 
  model_performance()
