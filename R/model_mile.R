library(tidymodels)

mile_recipe <- recipe(mile_time ~ ., data = training_data) %>%
  step_normalize(all_numeric_predictors())

mile_model <- boost_tree(
  trees = 1000,
  learn_rate = 0.01
) %>% 
  set_engine("xgboost") %>%
  set_mode("regression")

mile_workflow <- workflow() %>%
  add_recipe(mile_recipe) %>%
  add_model(mile_model)

mile_fit <- fit(mile_workflow, data = training_data)
saveRDS(mile_fit, "models/mile_model.rds")