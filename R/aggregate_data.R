## Usually you would have to aggregate monthly/quarterly/annual observations
## We have constructed the dataset to not have to access retrospective data

## Define function ----
aggregate_exposure <- function( key_vars = c("fy", "fy_period_label"), ## Key variables for final extract
                                aggregate_by = c(), ## Variable names you wish to aggregate interactively by
                                summary_vars = c("expected_inc", "incidence_ind", "incidence_reported", "IBNR", "period_exposure_months"),
                                months_to_aggregate = 6 ## Number of months in each aggregation period, between 1 and 12
    ){
  
  ## Define global variables
  EI_start_date <- as.Date("2017-07-01")
  EI_end_date <- "2022-06-30"
  read_loc <- "~/life_viz_outputs/data"
  save_loc <- "~/life_viz_outputs/data"
  
  ## Read in IBNR data
  IBNR_factors <- read.csv(file.path(read_loc, "ibnr_factors.csv"))
  
  ## Read exposure data
  exposure <- read.csv(file.path(read_loc, "exposure.csv"))
  exposure %<>% mutate(exposure_start = as.Date(exposure_start),
                       incidence_month = as.character(incidence_month),
                       report_month = as.character(report_month)
                       )
  exposure %<>% mutate(incidence_month = ifelse(incidence_month == "", NA_character_, incidence_month))
  exposure %<>% mutate(report_month = ifelse(report_month == "", NA_character_, report_month))
  
  ## Cut data into monthly periods
  #### Number of months
  exposure %<>% mutate( exposure_end = as.Date(coalesce(incidence_month, EI_end_date)),
    no_reporting_periods = floor((interval(EI_start_date, exposure_end) %/% months(1))) -
                         floor((interval(EI_start_date, exposure_start) %/% months(1))) + 1 ## This is not necessarily the total exposure divided by the months to aggregate, unless the months to aggregate is 1
  )
  
  #### Create one record for each reporting period
  #### This is more efficient than iterating through each reporting period 
  #### Sourced from https://www.tutorialspoint.com/how-to-create-a-data-frame-in-r-with-repeated-rows-by-a-sequence-of-number-of-times-or-by-a-fixed-number-of-times
  exposure_rp <- exposure[rep.int(seq_len(nrow(exposure)), times = exposure$no_reporting_periods), ]
  
  #### Create report period count variable
  exposure_rp %<>% group_by(pol_ref) %>% 
    mutate(reporting_period_count = row_number()) %>%
    ungroup()
  
  ## Create exposure start and end date for each period
  exposure_rp %<>% mutate(period_exposure_start = exposure_start + months((reporting_period_count - 1)*months_to_aggregate),
                          period_exposure_end = exposure_start + months(reporting_period_count*months_to_aggregate) - 1
                          )
  
  #### Create reporting group data.frame with rp names
  rp_groups <- data.frame(exposure_start = unique(exposure_rp$exposure_start) %>% ymd %>% sort)
  rp_groups %<>% mutate(temp_period_exposure_start = EI_start_date + months(interval(EI_start_date, rp_groups$exposure_start) %/% months(1)))
  
  ## Join and create exposure for each period
  exposure_rp_2 <- merge(exposure_rp, rp_groups, by = "exposure_start", all.x = T)
  exposure_rp_2 %<>% mutate(period_exposure_start = temp_period_exposure_start + months((reporting_period_count - 1)),
                          period_exposure_end = temp_period_exposure_start + months(reporting_period_count) - 1,
                          period_exposure_months =  interval(pmax(period_exposure_start,exposure_start), pmin(period_exposure_end, exposure_end) +1) %/% months(1),
                          fy = year(period_exposure_start) - (month(period_exposure_start) <= 6), ## TODO: Add functionality to allow FY to start in any month
                          # months_dev = interval(period_exposure_end, as.Date(EI_end_date)) %/% months(1),
                          fy_period = (floor((month(period_exposure_start)+5)/months_to_aggregate) %% (12/months_to_aggregate)) + 1, ## Complicated code to generically create period 1,2, ..., n within the FY
                          fy_period_label = case_when(months_to_aggregate == 12 ~ as.character(fy)
                                                      , months_to_aggregate == 6 ~ paste0(fy, "H", fy_period)
                                                      , months_to_aggregate == 3 ~ paste0(fy, "Q", fy_period)
                                                      , months_to_aggregate == 1 ~ paste0(fy, "M", fy_period)
                          )
  )
  
  ## Fix incidence ind to only occur in the period of incidence
  exposure_rp_2 %<>% mutate(incidence_ind = case_when(between(incidence_month, period_exposure_start, period_exposure_end) ~ 1,
                                                      T ~ 0
                                                      )
  )
  
  ## Create crosstab of incidence date by reporting date
  incident_dates <- exposure_rp_2 %>% select(fy, fy_period_label,period_exposure_end) %>% 
    unique() %>% arrange(fy_period_label)
  report_dates <- incident_dates %>% 
    group_by(fy, fy_period_label) %>% 
    summarise(period_exposure_end = last(period_exposure_end)) %>%
    ungroup() %>%
    filter(row_number() >= (n() - 4))
  
  incidence <- crossing(incident_dates %>% select(fy),
            report_dates %>% select(fy_period_label, period_exposure_end)
            )
  incidence %<>% filter(year(period_exposure_end %m-% months(6)) >= fy)
  
  exposure_rp_2 %<>% select(-fy_period_label, -period_exposure_end) ## Remove these so they do not conflict with the join
  
  ## Left join incidence to exposure to duplicate records for each reporting period
  incidence_2 <- merge(exposure_rp_2, incidence, all.x = T, by = "fy") %>% 
    filter(fy_period_label %in% incidence$fy_period_label)
  
  ## Redefine incidence according to reporting date
  incidence_2 %<>% mutate(incidence_reported = case_when(incidence_ind == 1 & report_month <= period_exposure_end ~ 1,
                                                         T ~ 0))
  
  ## Join in IBNR factors and create IBNR variable
  incidence_2 %<>% mutate(months_dev = ifelse(incidence_reported == 1
                                                    , pmin(interval(incidence_month, period_exposure_end) %/% months(1),24)
                                                    , NA_real_))
  incidence_ibnr <- merge(incidence_2, IBNR_factors, all.x = T, by = "months_dev")
  incidence_ibnr %<>% mutate(IBNR = ifelse(incidence_reported == 0
                                       , 0
                                       , incidence_reported * cum_prob - 1))
  
  ## Aggregate
  
  aggregate_incidence <- incidence_ibnr %>% 
    group_by_at(key_vars) %>%
    summarise_at(vars(all_of(summary_vars)), sum
    )
  
  ## Save output
  fwrite(aggregate_incidence, file.path(save_loc, "aggregate_incidence.csv"))
}
