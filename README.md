# life-insurance-viz
Simulates life insurance incidence data and experiments with ways of presenting incidence experience that encapsulates IBNR/RBNA development and uncertainty in the central estimate.

## Example usage
## 1. Simulate incidence data to use in viz (you can skip this step if you have actual incidence data to use)
simulate_incidence()
create_IBNR_factors()

## 2. Aggregate data to ingest into graph function
aggregate_exposure()

## 3. Graph incidence
data_loc <- "~/life_viz_outputs/data"
aggregate_incidence <- fread(file.path(data_loc, "aggregate_incidence.csv"))

plot_incidence_development(aggregate_incidence)
