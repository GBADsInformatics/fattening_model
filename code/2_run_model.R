##################
# file name: run_model.R
# created by: Anne Meyer
# created on: 2024-01-19
##################

set.seed(1234)

# Base parameters : model iterations, cycle length and prices
base_params <- readxl::read_excel(scenario_file ,'Base_params')
nruns <- as.numeric(base_params$value[base_params$'parameter'=="nruns"])
exchange_rate <- as.numeric(base_params$value[base_params$'parameter'=="exchange_rate"])
base_params <- subset(base_params,!(parameter %in% c("nruns","exchange_rate")))

common_inputs_df <- data.frame(iteration = seq(1:nruns))
for (i in 1:nrow(base_params)) {
  sample_values <- eval(parse(text=base_params[i,"value"]))
  if (length(sample_values)==1) {
    sample_values=rep(sample_values,nruns)
  }
  common_inputs_df[,i] <- sample_values
}
names(common_inputs_df) <- base_params$parameter

# Scenario parameters
## Read control table
ahle_scenarios <- readxl::read_excel(scenario_file ,'Scenarios')
growth_rates <- readxl::read_excel(scenario_file ,'daily_gain')
mortality_rates <- readxl::read_excel(scenario_file ,'daily_mort')

## Drop rows where parameter name is empty or commented
ahle_scenarios <- ahle_scenarios[!is.na(ahle_scenarios$'parameter') ,]

## Get scenario names
remove_cols <- c('parameter_name' ,'parameter')
ahle_scenarios_cln <- names(ahle_scenarios[!(names(ahle_scenarios) %in% remove_cols)])

## Create data frames to store inputs and outputs 
all_inputs <- data.frame()
all_population <- data.frame()
all_liveweight <- data.frame()
all_biomass <- data.frame()
all_outputs <- data.frame()
all_mortality <- data.frame()
all_growth <- data.frame()

## Get the longest cycle for sizing the data frames
cycle_length_max <- max(as.numeric(ahle_scenarios[ahle_scenarios$'parameter'=="cycle_length", ahle_scenarios_cln]))

# Loop through scenario columns, calling the function for each
for (COLNAME in ahle_scenarios_cln){
  print('> Running AHLE scenario:')
  print(COLNAME)
  
  scenario_data <- ahle_scenarios[c("parameter", COLNAME)]
  scenario_length <- as.numeric(scenario_data %>% subset(parameter=="cycle_length") %>% select(2))
      
  ## Construct input data frame with other arguments 
  input_df <- data.frame(iteration = seq(1:nruns))
  for (i in 1:nrow(scenario_data)) {
    sample_values <- eval(parse(text=scenario_data[i,2]))
    if (length(sample_values)==1) {
      sample_values=rep(sample_values,nruns)
      }
    input_df[,i] <- sample_values
    }
  names(input_df) <- scenario_data$parameter
  
 ## add the common input parameters back to it
  input_df <- cbind(input_df, common_inputs_df)

  ## Construct growth rate data frame  
  growth_scenario <- matrix(0,nrow = nruns, ncol = scenario_length)

    for (j in 1:scenario_length) {
    sample_values <- eval(parse(text=growth_rates[growth_rates$production_day==j, COLNAME]))
    if (length(sample_values)==1) {
      sample_values=rep(sample_values,nruns)
      }
    growth_scenario[,j] <- sample_values
    }

  ## Construct mortality rate data frame  
  mortality_scenario <- matrix(0,nrow = nruns, ncol = scenario_length)

    for (j in 1:scenario_length) {
    sample_values <- eval(parse(text=mortality_rates[mortality_rates$production_day==j, COLNAME]))
    if (length(sample_values)==1) {
      sample_values=rep(sample_values,nruns)
      }
    mortality_scenario[,j] <- sample_values
    }

  ## Call function 
  result <- grow_out_model(nruns=nruns, cycle_length=scenario_length, inputs=input_df, mortality=mortality_scenario, growth=growth_scenario)
  
  ## Compile inputs and outputs
  #a/ input parameters
  input_df$scenario <- COLNAME
  all_inputs <- rbind(all_inputs, input_df)
  
  #b/ population output
  population <- result[[1]]

  population <- population %>%
    data.frame %>%
    pivot_longer(everything(), names_to="production_day", values_to="pop") %>%
    mutate(production_day=as.numeric(str_replace(production_day,"X",""))) %>% 
    group_by(production_day) %>% 
    summarize(mean=mean(pop), sd=sd(pop) , q50=quantile(pop, 0.5), q2_5=quantile(pop,0.025), q97_5=quantile(pop,0.975)) %>% 
    ungroup %>% 
    mutate(scenario = COLNAME)
    
  all_population <- rbind(all_population, population)

  #c/ liveweight output
  liveweight <- result[[2]]

  liveweight <- liveweight %>%
    data.frame %>%
    pivot_longer(everything(), names_to="production_day", values_to="weight") %>%
    mutate(production_day=as.numeric(str_replace(production_day,"X",""))) %>% 
    group_by(production_day) %>% 
    summarize(mean=mean(weight), sd=sd(weight) , q50=quantile(weight, 0.5), q2_5=quantile(weight,0.025), q97_5=quantile(weight,0.975)) %>% 
    ungroup %>% 
    mutate(scenario = COLNAME)

  all_liveweight <- rbind(all_liveweight, liveweight)

  #d/ total weight output
  biomass <- result[[1]]*result[[2]]

  biomass <- biomass %>%
    data.frame %>%
    pivot_longer(everything(), names_to="production_day", values_to="weight") %>%
    mutate(production_day=as.numeric(str_replace(production_day,"X",""))) %>% 
    group_by(production_day) %>% 
    summarize(mean=mean(weight), sd=sd(weight) , q50=quantile(weight, 0.5), q2_5=quantile(weight,0.025), q97_5=quantile(weight,0.975)) %>% 
    ungroup %>% 
    mutate(scenario = COLNAME)

  all_biomass <- rbind(all_biomass, biomass)

  #f/ other outputs (not storing everything for the sake of size, but could add if useful)
  outputs <- result[[3]]
  outputs <- outputs %>% 
    select(iteration, feed_cost, fcr, health_cost, labour_cost, stocking_cost, other_cost, harvest_value, harvest_biomass, cycle_length, gross_margin, gm_proportion, harvest_live_weight, harvest_number, feed_amount,area_used, n_cycles_farm,variable_costs, harvest_dressed_weight, sold_number, sold_biomass,average_daily_gain) %>%
    mutate(scenario = COLNAME)
  # dropping these fields: initial_biomass, total_weight_days, lost_biomass, labour_amount  
  all_outputs <- rbind(all_outputs, outputs)

  #g/ mortality rates
  mortality_scenario <- mortality_scenario %>%
  data.frame %>%
    pivot_longer(everything(), names_to="production_day", values_to="daily_mort") %>%
    mutate(production_day=as.numeric(str_replace(production_day,"X",""))) %>% 
    group_by(production_day) %>% 
    summarize(mean=mean(daily_mort), sd=sd(daily_mort) , q50=quantile(daily_mort, 0.5), q2_5=quantile(daily_mort,0.025), q97_5=quantile(daily_mort,0.975)) %>% 
    ungroup %>% 
    mutate(scenario = COLNAME)

  all_mortality <- rbind(all_mortality, mortality_scenario)
  
  #h/ growth rates
  growth_scenario <- growth_scenario %>%
  data.frame %>%
    pivot_longer(everything(), names_to="production_day", values_to="daily_gain") %>%
    mutate(production_day=as.numeric(str_replace(production_day,"X",""))) %>% 
    group_by(production_day) %>% 
    summarize(mean=mean(daily_gain), sd=sd(daily_gain) , q50=quantile(daily_gain, 0.5), q2_5=quantile(daily_gain,0.025), q97_5=quantile(daily_gain,0.975)) %>% 
    ungroup %>% 
    mutate(scenario = COLNAME)

  all_growth <- rbind(all_growth, growth_scenario)

}


# Simplify the input parameters for storage (we don't need all iterations for the density graphs)

all_inputs <- all_inputs %>% pivot_longer(!scenario, values_to="value", names_to="variable")
my_params <- unique(all_inputs$variable)
my_scenarios <- unique(all_inputs$scenario)

# aggregate data in bins for distribution to save space
summary_inputs <- data.frame()
for (i in my_params) { 
  for (j in my_scenarios) {
my_values <- unique(subset(all_inputs,variable==i&scenario==j))

if (nrow(my_values)>1) { 
  br <- seq(min(my_values$value), max(my_values$value), length.out=500)
  dx <- median(diff(br))
  aggregated <- my_values %>% 
  count(scenario = unique(scenario),
        variable = unique(variable),
        value = cut(value, breaks=br, labels=(br + dx)[-length(br)], 
                  include.lowest=TRUE),
        value = as.numeric(as.character(value)),
        type = "Distribution")
} else {
  aggregated <- my_values %>% 
  mutate (n=1, type="Point")
  }
 summary_inputs <-rbind(summary_inputs, aggregated)
  }
}

# add in the provided exchange rate for USD equivalent
summary_inputs$exchange_rate<-exchange_rate

## Save inputs and outputs to files
write.csv(summary_inputs, paste0(local_folder,"outputs/summary_inputs.csv"), row.names=FALSE)
write.csv(all_outputs, paste0(local_folder,"outputs/all_outputs.csv"), row.names=FALSE)
write.csv(all_mortality, paste0(local_folder,"outputs/all_mortality.csv"), row.names=FALSE)
write.csv(all_growth, paste0(local_folder,"outputs/all_growth.csv"), row.names=FALSE)
write.csv(all_liveweight, paste0(local_folder,"outputs/all_liveweight.csv"), row.names=FALSE)
write.csv(all_biomass, paste0(local_folder,"outputs/all_biomass.csv"), row.names=FALSE)
write.csv(all_population, paste0(local_folder,"outputs/all_population.csv"), row.names=FALSE)