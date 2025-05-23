mv_data_tmp <- read_delim(
  "data/Motor vehicle insurance data.csv",
  delim = ";",
  escape_double = FALSE,
  trim_ws = TRUE
) %>%
  mutate(
    ID = as.character(ID),
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
    Cost_claims_year = as.numeric(Cost_claims_year),
    N_claims_year = as.numeric(N_claims_year),
    N_claims_history = as.numeric(N_claims_history),
    R_Claims_history = as.numeric(R_Claims_history),
    Type_risk = factor(
      Type_risk,
      levels = c(1, 2, 3, 4),
      labels = c("Motorbikes", "Vans", "Passenger cars", "Agricultural vehicles")
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
  )

mv_data_tmp <- mv_data_tmp %>% 
  mutate(
    Exposure = if_else(
      Exposure > 1,
      1,
      Exposure
    )
  )

# Remove rows with negative exposure

mv_data <- mv_data_tmp %>%
  filter(Exposure > 0) %>%
  select(-Date_birth, -Date_driving_licence, -Year_matriculation)
