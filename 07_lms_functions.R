# data split
split_func <-
  function(data) {
    data %>%
      time_series_split(assess = "24 months",
                        cumulative = TRUE,
                        date_var = date)
  }


fit_func <-
  function(data, splits) {
    # Add time series signature
    recipe_spec_timeseries <-
      recipe(log10_tonnes  ~ . , data = training(splits)) %>%
      step_rm(date) %>%
      step_lag(wave_height_m,
               log10_tonnes,
               # wave_period_s,
               # peak_wave_period_s,
               # swell_height_m,
               # wind_speed_m_s,
               lag = 1:3) %>%
      step_scale(all_numeric()) %>%
      step_normalize(all_numeric()) %>%
      # step_YeoJohnson(all_numeric()) %>%
      step_naomit() 
    
    # linear model
    model_spec <-
      linear_reg() %>%
      set_engine('lm') %>% # adds lm implementation of linear regression
      set_mode('regression')
    
    workflow_fit <-
      workflow() %>%
      add_model(model_spec) %>%
      add_recipe(recipe_spec_timeseries) %>%
      fit(training(splits))
    
  }

refit_func <-
  function(model, data, splits) {
    modeltime_table(model) %>%
      modeltime_calibrate(testing(splits)) %>%
      modeltime_refit(data)
  }

# plot_ts_func <-
#   function(refit, data, newdata) {
#     refit %>% 
#       modeltime_forecast(actual_data = data, new_data = newdata) %>%
#       ggplot(aes(.index, exp(.value), color = .key)) +
#       geom_line(alpha = .8) +
#       theme_minimal() +
#       labs(title = NULL, y = "tonnes", x = NULL) +
#       scale_color_tableau(name = NULL)
#   }
