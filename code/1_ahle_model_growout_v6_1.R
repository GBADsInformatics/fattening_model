##################
# file name: ahle_model_growout_v6.R
# created by: Anne Meyer
# created on: 2024-01-19
##################

# =================================================================
# GBADs AHLE model for grow-out farms - v6
# This code reads an Excel file which defines the scenarios to run.
# Model definition :
# =================================================================

grow_out_model <- function(
nruns ## number of model iterations
, cycle_length ## length of production cycle in days
, inputs ## input data frame for this scenario
, mortality ## mortality matrix
, growth ## growth (daily gain) matrix
)
{

# --------------------------------------------------------------
# Simulate population and weight at each time step for all iterations 
# --------------------------------------------------------------

population <- matrix(0, nrow = nruns, ncol = cycle_length)
liveweight <- matrix(0, nrow = nruns, ncol = cycle_length)
mortality_weight<- matrix(0, nrow = nruns, ncol = cycle_length)

population[,1] <- inputs$num_stocked
liveweight[,1] <- inputs$stocking_weight
mortality_weight [,1] <- 0

for(day in c(2:cycle_length)) {
population[,day] <- population[,day-1]*(1-mortality[,day-1])
liveweight[,day] <- liveweight[,day-1]*(1+growth[,day-1])
mortality_weight[,day] <- population[,day-1]*mortality[,day-1]*liveweight[,day-1]
}

biomass <- liveweight*population

# --------------------------------------------------------------
# Calculate costs and production for each iteration 
# --------------------------------------------------------------

outputs <- data.frame(iteration = seq(1:nruns))

## Cycle
outputs$cycle_length <- inputs$cycle_length 
outputs$area_used <- inputs$area_used 
outputs$n_cycles_farm <- inputs$n_cycles_farm 

## Production
outputs$harvest_live_weight <- liveweight[,cycle_length]
outputs$average_daily_gain <- 1000*(outputs$harvest_live_weight-inputs$stocking_weight)/inputs$cycle_length
outputs$harvest_dressed_weight <- outputs$harvest_live_weight*inputs$dressing_prop
outputs$harvest_number <- population[,cycle_length]
outputs$harvest_biomass <- outputs$harvest_live_weight*outputs$harvest_number
outputs$sold_number <- outputs$harvest_number*(1-inputs$rejection_rate)
outputs$sold_biomass <- outputs$harvest_dressed_weight*outputs$sold_number

outputs$harvest_value <- outputs$sold_biomass*inputs$sale_price 

## Variable costs

# Stocking costs: per cycle
outputs$stocking_cost <- inputs$num_stocked * inputs$seed_price

# Feed consumption: by day and weight unit
outputs$total_weight_days <- apply(biomass,1,sum)
outputs$lost_biomass <- rowSums(mortality_weight)
outputs$initial_biomass <- inputs$num_stocked*inputs$stocking_weight

outputs$feed_amount <- inputs$fcr*((outputs$harvest_biomass+outputs$lost_biomass)-outputs$initial_biomass)
outputs$feed_cost <- outputs$feed_amount*inputs$feed_price 
outputs$fcr <- inputs$fcr

# Non-health related labour costs: per unit of weight produced
outputs$labour_amount <- outputs$harvest_biomass*inputs$labour_amount_non_health
outputs$labour_cost <- outputs$labour_amount*inputs$labour_price 

# Health expenditure: per unit of weight produced, includes health products, interventions and labour
outputs$health_cost = outputs$harvest_biomass*(inputs$health_exp+inputs$labour_amount_health*inputs$labour_price)

# Other variable costs
outputs$other_cost = outputs$harvest_biomass*inputs$other_price 

# Total variable costs 
outputs$variable_costs <- outputs$stocking_cost + outputs$feed_cost + outputs$labour_cost + outputs$health_cost + outputs$other_cost

## Gross margin
outputs$gross_margin<- outputs$harvest_value - outputs$variable_costs
outputs$gm_proportion<- 100*outputs$gross_margin/outputs$harvest_value


# --------------------------------------------------------------
# Package the outputs
# --------------------------------------------------------------

model_results <- list(population,liveweight, outputs)
names(model_results) <-c("population","liveweight","outputs")
return(model_results)

}
