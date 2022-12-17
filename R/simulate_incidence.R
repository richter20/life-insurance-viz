
## The intention of this is to simulate life insurance data.
## The simulated product will have a long reporting period (e.g. like TPD).
## This function doesn't intend to simulate realistic experience, but gives us something to demonstrate reporting and visualisation functionality

## Define function ----
simulate_incidence <- function(){ ## We may choose to parametrise later, e.g. varying volume, incidence prob, rating factors etc
  
  ## Assume there are initially 10,000 policyholders, of which, each month some will claim
  ## Assume also that there are new policyholders each month
  
  ## Define global variables TODO: make these arguments to the function
  EI_start_date <- "2017-07-01"
  EI_end_date <- "2022-06-30"
  save_loc <- "~/life_viz_outputs/data"
  
  ## Check that the save location exists and is writable
  if(!dir.exists(save_loc)){
    dir.create(save_loc, recursive = T)
  }
  
  ## Incidence rates
  male_month_inc <- 1-(1-0.005)^(1/12) ## An annual rate of 0.5%
  female_month_inc <- 1-(1-0.003)^(1/12) ## An annual rate of 0.3%
  
  ## Policyholders at start of experience investigation
  pol_init <- data.frame(
    pol_ref = 1:10000,
    exposure_start = as.Date(EI_start_date),
    gender = sample(c("M", "F"), 10000, replace = T),
    policy_duration_at_start = sample(1:10, 10000, replace = T)
  )
  
  ## Policyholders who entered during the experience investigation
  pol_new <- data.frame(
    pol_ref = 10001:12500,
    exposure_start = as.Date(EI_start_date) %m+% months(sample(1:(5*12-1), 2500, replace = T)),
    gender = sample(c("M", "F"), 2500, replace = T),
    policy_duration_at_start = rep(1, 2500)
  )
  
  ## Combine the existing and new policyholders into one dataset
  all_pol <- rbind(pol_init, pol_new)
  
  ## Merge incidence rates to policy data
  incidence_rates <- data.frame(gender = c("M", "F"), inc_month_rate = c(male_month_inc, female_month_inc))
  all_pol <- merge(all_pol, incidence_rates, by = "gender", all.x = T)
  
  ## Create new variables to simulate the incidence
  all_pol %<>% mutate(total_exposure_months = interval(exposure_start, as.Date(EI_end_date)) %/% months(1), ## Total monthly exposure
                      expected_inc = inc_month_rate * total_exposure_months, ## Expected incidence
                      random_number = runif(dim(all_pol)[1],0,1), ## Create random number between zero and one to create incidence random variable
                      incidence_ind = ifelse(random_number > expected_inc, 0, 1)  ## Incidence random value, 0 is no incidence, 1 is incidence
                        )
  
  ## Create claims dataset
  claims <- all_pol %>% filter(incidence_ind == 1)
  claims %<>% mutate(months_until_incidence = floor(runif(dim(claims)[1],0,1)*total_exposure_months), ## Using a uniform distribution, for simplicity
                     incidence_month = exposure_start %m+% months(months_until_incidence + 1) -1,
                     number_mths_to_report = sample(1:24, dim(claims)[1], replace = T), ## Assume it takes between 1 and 24 months for claims to be reported
                     report_month = incidence_month %m+% months(number_mths_to_report)
  )
  
  ## Join claims back into policy data
  policy_clms <- merge(all_pol, claims, all.x = T)
  
  ## Adjust policy dates
  policy_clms %<>% mutate(total_exposure_months = interval(exposure_start, coalesce(incidence_month,as.Date(EI_end_date))) %/% months(1)
  )
  
  ## Save output
  fwrite(policy_clms, file.path(save_loc, "exposure.csv"))
  fwrite(claims, file.path(save_loc,"incidence.csv"))
}




