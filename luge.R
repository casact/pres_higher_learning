library(tidyverse)
library(ggbeeswarm)
library(lme4)
library(gridExtra)

source("common.R")

#==================================================
# Read in and minor wrangle
#
# Times were taken from this site:
# https://olympics.com/beijing-2022/olympic-games/en/results/luge/results-men-s-singles-fnl-000100-.htm
#==================================================

tbl_men <- read_csv('data/tbl_men.csv')

# The summmary table will include only those competitors who made all four runs
tbl_finalists_summary <- tbl_men %>% 
  group_by(name_full, name_first, name_last) %>% 
  summarise(
    time_total = sum(time_run)
    , time_mean = mean(time_run)
    , time_sd = sd(time_run)
    , n_runs = n()
  ) %>% 
  ungroup() %>% 
  filter(n_runs == 4) %>% 
  arrange(time_total)

# Remove the non-finalists with a semi-join
tbl_men <- semi_join(tbl_men, tbl_finalists_summary, by = 'name_full')

#==================================================
# Plots
#
# Note that I'm saving the plots to be inserted into the .PPTX
#==================================================

#' Not terribly useful. One of these days I'll figure out how to get more than 
#' one histogram on the same plot without having to facet.
tbl_men %>% 
  ggplot(aes(time_run)) + 
  geom_histogram(binwidth = 0.1) + 
  facet_wrap(~ run) + 
  theme_minimal()

plt_histo <- tbl_men %>% 
  ggplot(aes(time_run)) + 
  geom_histogram(binwidth = 0.1) + 
  theme_minimal() + 
  labs(
    title = "Run times for the top 20 lugers"
    , subtitle = "Four runs per luger"
    , x = 'Run time'
  )

plt_histo
ggsave('images/plt_histo.png', dpi = 300, width = 13.2, height = 4.7, units = 'in')

tbl_men %>% 
  ggplot(aes(run, time_run)) + 
  geom_boxplot() + 
  theme_minimal() + 
  labs(
    title = "Run times for the top 20 lugers, by run"
    , x = 'Run'
    , y = 'Run time'
  )

plt_beeswarm_by_run <- tbl_men %>% 
  ggplot(aes(run, time_run)) + 
  geom_beeswarm() +
  theme_minimal() + 
  labs(
    title = "Run times for the top 20 lugers, by run"
    , x = 'Run'
    , y = 'Run time'
  )

plt_beeswarm_by_run
ggsave('images/plt_beeswarm_by_run.png', dpi = 300, width = 13.2, height = 4.7, units = 'in')

tbl_top_men <- tbl_men %>% 
  filter(name_full %in% tbl_finalists_summary$name_full[1:5])

plt_top_men <- tbl_top_men %>% 
  ggplot(aes(run, time_run, color = name_full)) + 
  geom_point() + 
  geom_line(aes(group = name_full)) + 
  geom_label(
    aes(label = name_last)
    , data = tbl_top_men %>% filter(run == 4)
      , nudge_x = 0.25) +
  guides(color = 'none') +
  scale_y_continuous(limits = c(57, 59.5)) +
  theme_minimal() + 
  labs(
    title = "Run times for the top 5 lugers, by run"
    , x = 'Run'
    , y = 'Run time'
  )

plt_top_men
ggsave('images/plt_top_men.png', dpi = 300, width = 13.2, height = 4.7, units = 'in')

tbl_next_men <- tbl_men %>% 
  filter(name_full %in% tbl_finalists_summary$name_full[6:10])

plt_next_men <- tbl_next_men %>% 
  ggplot(aes(run, time_run, color = name_full)) + 
  geom_point() + 
  geom_line(aes(group = name_full)) + 
  geom_label(aes(label = name_last), data = filter(tbl_next_men, run == 4), nudge_x = 0.25) +
  scale_y_continuous(limits = c(57, 59.5)) +
  guides(color = 'none') +
  theme_minimal() +
  labs(
    title = "Run times for the next 5 lugers, by run"
    , x = 'Run'
    , y = 'Run time'
  )

plt_next_men
ggsave('images/plt_next_men.png', dpi = 300, width = 13.2, height = 4.7, units = 'in')

plt_sd <- tbl_finalists_summary %>% 
  ggplot(aes(name_last, time_sd)) + 
  geom_point() + 
  geom_label(aes(label = name_last)) +
  theme_minimal() + 
  scale_x_discrete(labels = NULL) + 
  labs(
    title = 'Standard deviation by luger'
    , x = 'Luger'
    , y = 'Standard deviation'
  )

plt_sd
ggsave('images/plt_sd.png', dpi = 300, width = 13.2, height = 4.7, units = 'in')

plt_sd_by_time <- tbl_finalists_summary %>% 
  ggplot(aes(time_mean, time_sd)) + 
  geom_point() + 
  geom_label(aes(label = name_last)) +
  theme_minimal() +
  labs(
    title = 'Standard deviation vs. time'
    , x = 'Mean Time'
    , y = 'Standard deviation'
  )

plt_sd_by_time
ggsave('images/plt_sd_by_time.png', dpi = 300, width = 13.2, height = 4.7, units = 'in')

#==================================================
# Model
# 
#' Training and test sets are straightforward in this case. We're simply dropping
#' the fourth run for our training set. It would be fun to do cross validation
#' as well.
#==================================================

tbl_train <- filter(tbl_men, run != 4)
tbl_test <- filter(tbl_men, run == 4)

fit_pooled <- lm(
  time_run ~ 1
  , data = tbl_train
)

summary(fit_pooled)

fit_individual <- lm(
  time_run ~ 0 + name_full
  , data = tbl_train
)

summary(fit_individual)

#' Another one that didn't make the cut. I ran it once using `nlme`
fit_mixed_nlme <- nlme::lme(
  time_run ~ 1
  , random = ~ 1 | name_full
  , data = tbl_train
)

summary(fit_mixed_nlme)

fit_mixed <- lme4::lmer(
  time_run ~ 1 + (1 | name_full)
  , data = tbl_train
)

summary(fit_mixed)

lme4::fixef(fit_mixed)
lme4::ranef(fit_mixed)$name_full + lme4::fixef(fit_mixed)

tbl_oos_long <- calc_prediction_error(tbl_test, fit_pooled, 'pooled', 'time_run') %>% 
  bind_rows(
    calc_prediction_error(tbl_test, fit_individual, 'individual', 'time_run')
  ) %>% 
  bind_rows(
    calc_prediction_error(tbl_test, fit_mixed, 'mixed', 'time_run')
  ) %>% 
  mutate(
    model = as_factor(model)
  )

tbl_oos_long %>% 
  model_performance()

tbl_in_sample_long <- calc_prediction_error(tbl_train, fit_pooled, 'pooled', 'time_run') %>% 
  bind_rows(
    calc_prediction_error(tbl_train, fit_individual, 'individual', 'time_run')
  ) %>% 
  bind_rows(
    calc_prediction_error(tbl_train, fit_mixed, 'mixed', 'time_run')
  ) %>% 
  mutate(
    model = as_factor(model)
  )

tbl_in_sample_long %>% 
  model_performance()

tbl_oos_long %>% 
  ggplot(aes(prediction, residual, color = model)) + 
  geom_point() + 
  geom_hline(yintercept = 0, color = 'black') + 
  theme_minimal()

tbl_oos_long %>% 
  ggplot(aes(name_full, residual, color = model)) + 
  geom_point() + 
  geom_hline(yintercept = 0, color = 'black') + 
  theme_minimal()

tbl_oos_long %>% 
  ggplot(aes(model, residual)) + 
  geom_beeswarm() + 
  geom_hline(yintercept = 0, color = 'black') + 
  theme_minimal()

plt_residual_by_model <- tbl_oos_long %>% 
  ggplot(aes(model, residual)) + 
  geom_boxplot() +
  labs(
    title = 'Out-of-sample residuals by model'
    , x = NULL
    , y = 'Residual'
  ) +
  theme_minimal()

plt_residual_by_model
ggsave('images/plt_residual_by_model.png', dpi = 300, width = 13.2, height = 4.7, units = 'in')

#==================================================
# Bootstrap model
#
#' This is the part where we shuffle the names of the competitors. I had to 
#' play with the random number seed to get to a mixed effects model that would
#' fit.
#==================================================

set.seed(123456)
tbl_train <- tbl_train %>% 
  group_by(run) %>% 
  mutate(
    shuffled = sample(tbl_finalists_summary$name_full) %>% as.factor()
  ) %>% 
  ungroup()

tbl_test <- tbl_test %>% 
  mutate(
    shuffled = sample(tbl_finalists_summary$name_full) %>% as.factor()
  )

tbl_shuffled <- bind_rows(tbl_train, tbl_test)

tbl_top_shuffled <- tbl_shuffled %>% 
  group_by(shuffled) %>% 
  summarise(total_time = sum(time_run)) %>% 
  arrange(desc(total_time)) %>% 
  slice(1:5)

plt_top_shuffled <- tbl_shuffled %>% 
  filter(tbl_shuffled$shuffled %in% tbl_top_shuffled$shuffled[1:5]) %>% 
  ggplot(aes(run, time_run, color = shuffled)) + 
  geom_point() + 
  geom_line(aes(group = shuffled)) + 
  geom_label(
    aes(label = shuffled)
    , data = tbl_shuffled %>% filter(tbl_shuffled$shuffled %in% tbl_top_shuffled$shuffled[1:5]) %>% filter(run == 4)
    , nudge_x = 0.25) +
  guides(color = 'none') +
  scale_y_continuous(limits = c(57, 59.5)) +
  theme_minimal() + 
  labs(
    title = "Run times for the top 5 shuffled lugers, by run"
    , x = 'Run'
    , y = 'Run time'
  )

plt_top_shuffled

plt_shuffled_arrange <- arrangeGrob(
  plt_top_men, plt_top_shuffled
  , nrow = 1
)

ggsave('images/plt_top_shuffled.png' , dpi = 300, width = 13.2, height = 4.7, units = 'in', plt_shuffled_arrange)

fit_individual_shuffled <- lm(
  time_run ~ 0 + shuffled
  , data = tbl_train
)

fit_mixed_shuffled <- lme4::lmer(
  time_run ~ 1 + (1 | shuffled)
  , data = tbl_train
)

tbl_oos_long_shuffled <- tbl_oos_long %>% 
  bind_rows(
    calc_prediction_error(tbl_test, fit_individual_shuffled, 'individual_shuffled', 'time_run')
  ) %>% 
  bind_rows(
    calc_prediction_error(tbl_test, fit_mixed_shuffled, 'mixed_shuffled', 'time_run')
  )

tbl_oos_long_shuffled %>% 
  filter(model %in% c('pooled', 'mixed', 'mixed_shuffled')) %>% 
  model_performance()

plt_residual_by_model_shuffled <- tbl_oos_long_shuffled %>% 
  filter(model != 'individual_shuffled') %>% 
  ggplot(aes(model, residual)) + 
  geom_boxplot() +
  labs(
    title = 'Out-of-sample residuals by model'
    , subtitle = 'Shuffled groups'
    , x = NULL
    , y = 'Residual'
  ) +
  theme_minimal()

plt_residual_by_model_shuffled
ggsave('images/plt_residual_by_model_shuffled.png', dpi = 300, width = 13.2, height = 4.7, units = 'in')

#==================================================
# Model by  country
#
#' This one didn't make the cut, but it's pretty interesting. Germany has a 
#' modest, but detectable impact on times. 'Schland!
#==================================================

# By country
tbl_men %>% 
  ggplot(aes(noc, time_run)) + 
  geom_beeswarm() + 
  theme_minimal()

fit_mixed_country <- lme4::lmer(
  time_run ~ 1 + (1 | noc)
  , data = tbl_train
)

tbl_oos_long <- tbl_oos_long %>% 
  bind_rows(
    calc_prediction_error(tbl_test, fit_mixed_country, 'mixed_country', 'time_run')
  ) %>% 
  mutate(
    model = as_factor(model)
  )

tbl_oos_long %>% 
  model_performance()

plt_residual_by_model_country <- tbl_oos_long %>% 
  ggplot(aes(model, residual)) + 
  geom_boxplot() +
  labs(
    title = 'Out-of-sample residuals by model'
    , x = NULL
    , y = 'Residual'
  ) +
  theme_minimal()

plt_residual_by_model_country
ggsave('images/plt_residual_by_model_country.png', dpi = 300, width = 13.2, height = 4.7, units = 'in')

fit_mixed_country_luger <- lme4::lmer(
  time_run ~ 1 + (1 | noc) + (1 | name_full)
  , data = tbl_train
)

summary(fit_mixed_country_luger)

ranef(fit_mixed_country_luger)

tbl_oos_long <- tbl_oos_long %>% 
  bind_rows(
    calc_prediction_error(tbl_test, fit_mixed_country_luger, 'mixed_country_luger', 'time_run')
  ) %>% 
  mutate(
    model = as_factor(model)
  )

tbl_oos_long %>% 
  model_performance()

tbl_train <- tbl_train %>% 
  mutate(
    noc_germany = ifelse(noc == 'GER', 'Germany', 'Other')
  )

tbl_test <- tbl_test %>% 
  mutate(
    noc_germany = ifelse(noc == 'GER', 'Germany', 'Other')
  )

fit_mixed_germany <- lme4::lmer(
  time_run ~ 1 + (1 | noc_germany) + (1 | name_full)
  , data = tbl_train
)

tbl_oos_long <- tbl_oos_long %>% 
  bind_rows(
    calc_prediction_error(tbl_test, fit_mixed_germany, 'mixed_germany', 'time_run')
  ) %>% 
  mutate(
    model = as_factor(model)
  )

tbl_oos_long %>% 
  model_performance()

tbl_oos_long %>% 
  filter(!model %in% c('mixed_country', 'mixed_country_luger')) %>% 
  ggplot(aes(model, residual)) + 
  geom_boxplot() +
  labs(
    title = 'Out-of-sample residuals by model'
    , x = NULL
    , y = 'Residual'
  ) +
  theme_minimal()

anova(
  fit_mixed_country_luger
  , fit_mixed
)

anova(fit_mixed)

#==============================
# Z
# 
#' These bits didn't make the cut for the presentation. Here I'm calculating
#' the standard deviations by competitor, over the whole population (needlessly)
#' and between the lugers. Fairly certain that I've made some sort of mistake, 
#' possibly in the sample estimate of the between-group sd.
#==============================

tbl_summary_3 <- tbl_men %>% 
  filter(run != 4) %>% 
  group_by(name_full) %>% 
  summarise(
    time_total = sum(time_run)
    , time_mean = mean(time_run)
    , time_sd = sd(time_run)
    , n_runs = n()
  ) %>% 
  arrange(time_total)

sd_pop <- tbl_men %>% 
  filter(run != 4) %>% 
  pull(time_run) %>% 
  sd()

sd_luger <- tbl_summary_3$time_mean %>% sd()

sd_luger / (sd_luger + sd_pop)
sd_pop / (sd_luger + sd_pop)

tbl_oos_wide <- tbl_oos_long %>% 
  select(model, name_full, prediction) %>% 
  filter(model %in% c('pooled', 'individual', 'mixed')) %>% 
  pivot_wider(names_from = 'model', values_from = 'prediction') %>% 
  inner_join(
    select(tbl_summary_3, name_full, time_sd, n_runs)
    , by = 'name_full'
  ) %>% 
  mutate(
    z = (mixed - pooled) / (individual - pooled)
    , z_prime = n_runs / (n_runs + time_sd / sd_luger)
  )
