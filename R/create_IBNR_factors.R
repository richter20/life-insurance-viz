
## Create simple IBNR factors
save_loc <- "~/life_viz_outputs/data"

IBNR_factors <-  data.frame(months_dev = 1:24)
IBNR_factors %<>% mutate(cum_prob = 24/months_dev)

## Save output
fwrite(IBNR_factors, file.path(save_loc, "ibnr_factors.csv"))