library(data.table)

# Modify this variable to your group name, which is formatted as FirstName1_LastName1-FirstName2_LastName2-...
GROUP_NAME <- "Phillip_Hovgesen-Emil_Sükrüoglu-Emil_Knudsen-Christian_Niekrenz"

# Define the train function, assume that X is a data.table and y is a vector of responses of the same length as the number of rows in X
train_model <- function(X, y) {
  # Do your training here, do hyperparameter tuning, etc.
  # The following code is an example of how to train a simple xgboost model in mlr3

  # ---- MODIFY THIS SECTION ----
  library(mlr3verse)
  library(mlr3extralearners)
  library(dplyr)
  library(lubridate)
  
  # Functions for data transformation
  reference_date <- as.Date("2015-01-01")
  
  normalize_date_to_days <- function(date_vector) {
    as.numeric(date_vector - reference_date)
  }
  
  format_data <- function(data) {
    # Convert categorical variables to factors
    data <- data %>% mutate(
      ID = as.numeric(ID),
      Date_start_contract = dmy(Date_start_contract),
      Date_last_renewal = dmy(Date_last_renewal),
      Date_next_renewal = dmy(Date_next_renewal),
      Distribution_channel = factor(
        Distribution_channel,
        levels = c(0, 1),
        labels = c("Agent", "Broker")
      ),
      Date_birth = dmy(Date_birth),
      Date_driving_licence = dmy(Date_driving_licence),
      Seniority = as.numeric(Seniority),
      Policies_in_force = as.numeric(Policies_in_force),
      Max_policies = as.numeric(Max_policies),
      Max_products = as.numeric(Max_products),
      Lapse = as.numeric(Lapse),
      Date_lapse = dmy(Date_lapse),
      Payment = factor(
        Payment,
        levels = c(0, 1),
        labels = c("Annual", "Half-yearly")
      ),
      Premium = as.numeric(Premium),
      N_claims_year = as.numeric(N_claims_year),
      N_claims_history = as.numeric(N_claims_history),
      R_Claims_history = as.numeric(R_Claims_history),
      Type_risk = factor(
        Type_risk,
        levels = c(1, 2, 3, 4),
        labels = c(
          "Motorbikes",
          "Vans",
          "Passenger cars",
          "Agricultural vehicles"
        )
      ),
      Area = factor(
        Area,
        levels = c(0, 1),
        labels = c("Rural", "Urban")
      ),
      Second_driver = factor(
        Second_driver,
        levels = c(0, 1),
        labels = c("Single", "Multiple")
      ),
      Year_matriculation = as.numeric(Year_matriculation),
      Power = as.numeric(Power),
      Cylinder_capacity = as.numeric(Cylinder_capacity),
      Value_vehicle = as.numeric(Value_vehicle),
      N_doors = as.numeric(N_doors),
      Type_fuel = factor(
        Type_fuel,
        levels = c("P", "D"),
        labels = c("Petrol", "Diesel")
      ),
      Length = as.numeric(Length),
      Weight = as.numeric(Weight)
    ) %>%
      mutate(
        Age = year(as.period(
          interval(start = Date_birth, end = Date_last_renewal)
        )),
        Age_driving_licence = year(as.period(
          interval(start = Date_driving_licence, end = Date_last_renewal)
        )),
        Vehicle_age = as.numeric(format(Date_last_renewal, "%Y")) - as.numeric(Year_matriculation),
        Exposure = as.numeric(
          if_else(
            is.na(Date_lapse) |
              Date_next_renewal < Date_lapse,
            Date_next_renewal,
            Date_lapse
          ) - Date_last_renewal
        ) / 365,
      ) %>% mutate(Exposure = if_else(Exposure > 1, 1, Exposure)) %>%
      filter(Exposure > 0) %>%
      select(-c(N_claims_year, Date_next_renewal, Date_lapse, Lapse, Year_matriculation, Date_driving_licence, Date_birth, ID))
    
    date_cols <- data %>%
      select(where(is.Date)) %>%
      colnames()
    
    data <- data %>%
      mutate(across(all_of(date_cols), ~ year(.x), .names = "{.col}_year")) %>%
      mutate(across(all_of(date_cols), ~ month(.x), .names = "{.col}_month")) %>%
      mutate(across(all_of(date_cols), ~ day(.x), .names = "{.col}_day")) %>%
      mutate(across(all_of(date_cols), normalize_date_to_days))
    
    return(data)
  }
  
  X <- format_data(X)
  
  task <- as_task_regr(data.table(X, y = y), target = "y")
  
  learner <- lrn(
    "regr.lightgbm",
    id = "lightgbm",
    objective = "tweedie",
    num_iterations = to_tune(100,5000),
    learning_rate = to_tune(0.005, 0.1),
    num_leaves = to_tune(10, 512),
    max_depth = to_tune(3, 15),
    feature_fraction = to_tune(0.6, 1.0),
    bagging_fraction = to_tune(0.6, 1.0),
    min_data_in_leaf = to_tune(1, 100)
  )


  at <- auto_tuner(
    tuner = tnr("grid_search"),
    learner = learner,
    resampling = rsmp("cv", folds = 5),
    measure = msr("regr.mse"),
    term_evals = 100
  )
  at$train(task)
  model <- at$learner

  # ---- END OF MODIFY THIS SECTION ----

  predict_fun <- function(test_X) {
    # assume that test_X is the same format as X
    # call your model here, e.g. in mlr3 you can use the predict function
    # ---- MODIFY THIS SECTION ----
    test_X <- format_data(test_X)
    preds <- model$predict_newdata(test_X)
    return(preds$response)
    # ---- END OF MODIFY THIS SECTION ----
  }

  return(predict_fun)
}

# run the following function to check if your train_model function is working
# do not modify this function
test_model_passes <- function() {
  data <- fread("data/Motor vehicle insurance data.csv", sep = ";")

  X <- data[, -c("Cost_claims_year")]
  y <- data$Cost_claims_year

  train_idx <- sample(seq_along(y), 10000)
  test_idx <- sample(setdiff(seq_along(y), train_idx), 10000)

  train_X <- X[train_idx]
  train_y <- y[train_idx]
  test_X <- X[test_idx]
  test_y <- y[test_idx]

  predict_fun <- train_model(train_X, train_y)

  pred <- predict_fun(test_X)
  pred_baseline <- mean(train_y)
  MSE <- mean((pred - test_y)^2)
  baseline_MSE <- mean((pred_baseline - test_y)^2)

  print(paste("MSE:", MSE))
  print(paste("Baseline MSE:", baseline_MSE))
}

# uncomment this line to test your model
test_model_passes()
