rm(list = ls())
library(caret)
library(plotROC)
library(performance)
library(pROC)
library(broom)
library(pdp)
library(ggpubr)
conflict_prefer("partial", "pdp")
conflict_prefer("lag", "dplyr")
conflict_prefer("filter", "dplyr")

# read data--------------
# load("C:/Users/javiera/OneDrive - Cawthron/UA/Gloria2/data/glms.RData")
d <- 
  read_csv('data/anomalies_data.csv') %>% 
  mutate(anomaly = factor(anomaly))

d %>% 
  group_by(fish_market, species, anomaly) %>% 
  count() %>% 
  print(n = Inf) %>% 
  arrange(n)

janitor::tabyl(d, anomaly)

# define control-------------
set.seed(1234567)
train_control <-
  trainControl(
    method = "repeatedcv",
    number = 5,
    repeats = 3,
    savePredictions =  TRUE,
    classProbs = TRUE,
    selectionFunction = "tolerance",
    sampling = "smote", ## sub-sampling for class imbalance
    summaryFunction = twoClassSummary)


# fit the models---------
glms <-
  d %>%
  # filter(species == "Seabream") %>% 
  group_by(fish_market, species) %>%
  nest() %>% 
  mutate(
    model = map(data,   ~train(
      anomaly  ~ .,
      data = .x,
      preProcess = c("center", "scale", "YeoJohnson"),
      method = "glm",
      metric = "ROC",
      trControl=train_control
    )),
    glance_mod = map(model, ~glance(.x$finalModel)),
    tidy_mod = map(model, ~tidy(.x$finalModel)),
    # r2_model = map(model, ~r2_tjur(.x$finalModel) %>% as_tibble()),
    roc_model = map(model, ~as_tibble(.x$results)))


write_rds(glms, 'data/classification_models.rds')

# plot regression estimates by fish_market and species------------
coef_plot <- 
glms %>% 
  select(tidy_mod) %>% 
  unnest(cols = c(tidy_mod)) %>% 
  filter(term != "(Intercept)") %>% 
  mutate(term = fct_recode(term, 
        `Wave height`=  "wave_height_m", 
        `Lag 1 wave height`= "wave_height_lag1",
        `Lag 2 wave height`= "wave_height_lag2",
        `Lag 3 wave height`="wave_height_lag3",
        `Lag 1 tonnes`="tonnes_lag1",     
        `Lag 2 tonnes`="tonnes_lag2",
        `Lag 3 tonnes`= "tonnes_lag3" )) %>% 
  ggplot(aes(term, color = fish_market)) +
  geom_pointrange(
    aes(
      y = estimate,
      ymin = estimate + std.error*1.96,
      ymax = estimate - std.error*1.96
    ),
    alpha = .7,
    size = .1,
    position = position_dodge2(width = .2)
  ) +
  coord_flip() +
  labs(x = "Term", y = "Estimate") +
  facet_wrap( ~ species, scales = "free") +
  theme_minimal(base_size = 8) +
  geom_hline(yintercept = 0, lty = 2) +
  scale_color_viridis_d(name = NULL)

coef_plot

# save coefficient plots -----------
ggsave(
  last_plot(),
  filename = 'figures/coef_plot_glm.png',
  width = 7,
  height = 3.5,
  device = "png",
  dpi = 300,
  bg = 'white'
)

# coef tables--------
coef_table_glm <- 
glms %>%
  select(tidy_mod) %>%
  unnest(cols = c(tidy_mod)) %>%
  mutate(across(is.numeric, ~ round(.x, 2))) %>%
  mutate(
    term = fct_recode(
      term,
      `Wave height` =  "wave_height_m",
      `Lag 1 wave height` = "wave_height_lag1",
      `Lag 2 wave height` = "wave_height_lag2",
      `Lag 3 wave height` = "wave_height_lag3",
      `Lag 1 tonnes` = "tonnes_lag1",
      `Lag 2 tonnes` = "tonnes_lag2",
      `Lag 3 tonnes` = "tonnes_lag3"
    )
  ) %>% 
  select(-statistic) %>%
  mutate(p.value = scales::pvalue(p.value)) %>%
  rename(P = p.value,
         SE = std.error)


write_excel_csv(coef_table_glm, 'tables/coef_table_glms.csv')

# r2 tables---
r2_glms <- 
glms %>% 
  select(r2_model) %>% 
  unnest(cols = c(r2_model)) %>% 
  rename(Rsq = value)
  
# Rsq, roc, sensitivity and accuracy values---
glm_performance <- 
  glms %>% 
  select(roc_model) %>% 
  unnest(cols = c(roc_model)) %>% 
  select(-parameter) %>% 
  left_join(r2_glms) %>% 
  mutate(across(is.numeric, ~round(.x, 3))) %>% 
  relocate(Rsq, .after = species)

write_excel_csv(glm_performance, 'tables/glm_performance_results.csv')

glm_performance %>%
  group_by(species) %>%
  summarise(across(ROC:Spec,
                   list(
                     mean = mean,
                     min = min,
                     max = max
                   ),
                   na.rm = T))

# plot of model performances ROC, Sens, Spec----------
left_join(
  glm_performance %>%
    pivot_longer(
      cols = c("ROC", "Sens", "Spec"),
      names_to = "name",
      values_to = "mean"
    ) %>%
    select(fish_market, species, name, mean),
  glm_performance %>%
    pivot_longer(
      cols = c("ROCSD", "SensSD", "SpecSD"),
      names_to = "name",
      values_to = "sd"
    ) %>%
    select(fish_market, species, name, sd) %>%
    mutate(name = str_remove_all(name, "SD")),
  by = join_by(fish_market, species, name)
) %>%
  mutate(name = fct_recode(name, AUC = "ROC",  Sensitivity = "Sens", Specificity = "Spec")) %>% 
  # Create plot
  ggplot(aes(x = fish_market, y = mean, color = species)) +
  geom_pointrange(aes(ymin = mean - sd ,
                      ymax = mean + sd),
                  alpha = .7,
                  size = .1,
                  position = position_dodge2(width = .2)) +
  facet_wrap( ~ name, scales = "fixed") +
  labs(x = NULL, y = NULL) +
  coord_flip() +
  theme_minimal()

# coefficients tables----
coef_table_glm <- 
glms %>% 
  select(tidy_mod) %>% 
  unnest(cols = c(tidy_mod))

coef_table_glm

write_csv(coef_table_glm, 'tables/coef_table_glm.csv')


# model summaries-----
glm_summaries <- 
  glms %>% 
  select(glance_mod) %>% 
  unnest(cols = c(glance_mod))

write_csv(glm_summaries, 'tables/glm_summary_table_lms.csv')

# variable importance----------
var_imp <- 
glms %>% 
  select(model) %>% 
  mutate(varimp = map(model, ~varImp(.x)$importance %>% as_tibble(rownames = 'term')))

var_imp_table <- 
var_imp %>% 
  select(varimp) %>% 
  unnest(cols = c(varimp)) %>% 
  ungroup()


var_imp_table %>% 
  group_by(term, species) %>% 
  summarise(mean = mean(Overall), max = max(Overall), min = min(Overall))

varimp_plts <-
  ggplot(var_imp_table, aes(
    x = fct_reorder(term, Overall),
    y = Overall,
    color = species
  )) +
  stat_summary(
    fun = "mean",
    geom = "point",
    size = 3,
    position = position_dodge2(width = .2)
  ) +
  stat_summary(
    fun.data = "mean_cl_boot",
    geom = "errorbar",
    width = 0.1,
    position = position_dodge2(width = .2)
  ) +
  labs(x = "Term", y = "Overall", color = NULL) +
  coord_flip() +
  theme_minimal(base_size = 9)

varimp_plts

# save var imp plots -----------
ggsave(varimp_plts,
       filename = 'figures/var_imp_plot.png',
       dpi = 300,
       width = 4,
       height = 4)


# get predictions-------
preds <- 
glms %>%
  mutate(preds = map2(
    .x = model,
    .y = data,
    ~ augment(.x$finalModel, newdata = .y, type.predict = "response",  se_fit = T)
  )) %>%
  select(preds) %>%
  unnest(cols = preds)

# Partial plots wave ----------
pdp_func <-
  function(model) {
    model %>%
      partial(pred.var = "wave_height_lag1",
              prob = TRUE,
              which.class = "Yes")
  }

pdp_dat <- 
  glms %>%
  select(model) %>% 
  mutate(pdps = map(.x = model, pdp_func)) 

# write_rds(pdp_dat, 'data/pdp_dat.rds') 
pdp_dat <- read_rds('data/pdp_dat.rds')

pdp_plts <- 
pdp_dat %>%
  select(pdps) %>%
  unnest(cols = pdps) %>%
  ggplot(aes(x = wave_height_lag1, y = yhat, color = species)) +
  geom_line() +
  xlim(0, 5) +
  ylim(0, 1) +
  stat_smooth(method = "gam",
              fullrange = TRUE,
              linewidth = .7, 
              # formula = y ~ s(x, 5, bs = 'cs'),
              se = F) +
  facet_wrap(~ fish_market, scales = 'fixed') +
  theme_minimal(base_size = 9) +
  labs(x = "Wave height lagged 1 day (m)", y = "Anomaly probability", color = NULL) +
  theme(legend.position = c(.9, .1))
      

pdp_plts

# save partial plots -----------
ggsave(pdp_plts,
       filename = 'figures/pdp_plts.png',
       dpi = 300,
       width = 6,
       height = 4)


# Partial plots tonnes_lag1----------
pdp_func_tonnes <-
  function(model) {
    model %>%
      partial(pred.var = "tonnes_lag1",
              prob = TRUE,
              which.class = "Yes")
  }

pdp_dat_tonnes <- 
  glms %>%
  select(model) %>% 
  mutate(pdps = map(.x = model, pdp_func_tonnes)) %>%
  select(pdps) %>%
  unnest(cols = pdps) 


pdp_plts_tonnes <- 
  pdp_dat_tonnes %>% 
  ungroup() %>% 
  ggplot(aes(x = tonnes_lag1, y = yhat, color = species)) +
  geom_line() +
  xlim(0, 2) +
  ylim(0, 1) +
  stat_smooth(method = "gam",
              fullrange = TRUE,
              linewidth = .7, 
              se = F) +
  facet_wrap(~ fish_market, scales = 'fixed') +
  theme_minimal(base_size = 9) +
  labs(x = "Landings lagged 1 day (tonnes)", y = "Anomaly probability", color = NULL) +
  theme(legend.position = c(.9, .1))


pdp_plts_tonnes

ggarrange(pdp_plts_tonnes, pdp_plts)

# save partial plots -----------
ggsave(pdp_plts_tonnes,
       filename = 'figures/pdp_plts_tonnes.png',
       dpi = 300,
       width = 6,
       height = 4)
