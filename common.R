calc_prediction_error <- function(tbl_test, obj_model, model_name, response_col) {
  
  tbl_test <- tbl_test %>% 
    mutate(
      prediction = predict(obj_model, newdata = .)
      , model = as.factor(model_name)
    )
  
  residual_val = tbl_test[[response_col]] - tbl_test[['prediction']]
  tbl_test %>% 
    mutate(
      residual = residual_val
    )
    
}

model_performance <- function(tbl_in){
  
  tbl_in %>% 
    group_by(model) %>% 
    summarise(
      mae = mean(abs(residual))
      , rmse = mean(residual^2)^(1/2)
    )
}

model_compare_box <- function(tbl_in) {

  tbl_in %>% 
    ggplot(aes(model, residual)) + 
    geom_boxplot() +
    labs(
      title = 'Out-of-sample residuals by model'
      , x = NULL
      , y = 'Residual'
    ) +
    theme_minimal()
}
