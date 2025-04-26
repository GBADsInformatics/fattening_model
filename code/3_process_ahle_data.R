##################
# file name: process_ahle_data_country.R
# Prepare the AHLE data for reporting
# created by: Anne Meyer
# created on: 2024-08-27
##################

# ---------------------------------------------------------------------------------------------------------------------------------------------------
# Read and prepare the datasets
data_full <- read.csv("outputs/all_outputs.csv")
param_names <- read.csv("data/ahle_params_english_names.csv")
input_params <- read.csv("outputs/summary_inputs.csv")
conv_rate <- unique(input_params$exchange_rate) # local currency unit to USD, average exchange rate for year of interest
input_params$exchange_rate <- NULL

# ---------------------------------------------------------------------------------------------------------------------------------------------------
# Colours
my_scenario_colours <-  c("#9057C6","#71B48D","#53B3CB","#EEE3AB")  # current - ideal  + 2 others
ahle_components_colours <- c("#5E8C61","#C0CBDC","#EF5FAF","#AE8EAC") # AHE - direct morbidity - direct mortality - joint morbidity/mortality 

ahle_colour <- "#F4941C"
ahle_colour_shades <- colorRampPalette(c(ahle_colour, "white"))(4)
ahle_colour_shades <- ahle_colour_shades[1:3]

# ---------------------------------------------------------------------------
# Prepare dataset

## Process full data set and flag costs and values
data_full <- data_full %>% 
  pivot_longer(!c(iteration,scenario),names_to="variable",values_to="value") %>% 
  left_join(param_names, by="variable") %>% 
  mutate(sign=ifelse(parameter=='Gross margin', 0,
    ifelse(parameter %in% c('Stocking costs', 'Animal health costs', 'Labour costs', 'Feed costs', 'Other variable costs'), -1, 
    ifelse(parameter %in% c('Gross margin', 'Harvest revenue'), +1, 
    NA))))

## Rank iterations by gross margin, to avoid comparing very bad Ideal scenarios with very good Current scenarios
data_subset <- data_full %>% 
  filter(variable=="gross_margin") %>%
  group_by(scenario) %>%
  mutate(iteration_rank=rank(-value, ties.method="min")) 

data_full <- data_full %>% 
  left_join(data_subset %>% select(-variable, -value, -parameter, -sign), by=c("scenario","iteration")) %>%
  select(-iteration) %>%
  rename(iteration=iteration_rank) 

## Extract area for biomasse density calculation
area_used <- data_full %>% 
  filter(variable=="area_used") %>% 
  group_by(scenario) %>%
  summarize(value=mean(value)) %>%
  ungroup

## Summarize data by mean and 95% prediction interval 
data_summary <- data_full %>% 
  group_by(variable,parameter,sign,scenario) %>% 
  summarize(mean=mean(value), q2_5=quantile(value,0.025), q97_5=quantile(value,0.975)) %>%
  mutate(mean_pretty=ifelse(
    mean<0.01,formatC(mean, digits=3, format="f"),
    ifelse(mean<20, formatC(mean, digits=2, format="f"), formatC(mean, digits=0, format="f",big.mark=",")))) %>%
  ungroup

## Summary tables to compare high-level scenario results
## note that the number of animals stocked per cycle in the scenario parameters may be provided for a single farm or for the whole sector
## results below describe a single farm or the whole sector, accordingly, for a single production cycle
data_table0 <- data_summary %>% 
  filter(is.na(sign)) %>%
  select(scenario, mean_pretty,parameter, variable) %>%
  rename(column_name=scenario) 
  
my_column_names <- unique(data_table0$column_name)

feed_summary_ft <- data_table0 %>%
  filter(variable %in% c("average_daily_gain","fcr","feed_amount")) %>%
  pivot_wider(names_from="column_name",values_from="mean_pretty") %>%
  flextable(col_keys= c("parameter",my_column_names)) %>% 
  theme_vanilla() 

production_summary_ft <- data_table0 %>%
  filter(variable %in% c("harvest_live_weight","harvest_number","harvest_biomass","harvest_dressed_weight","sold_number","sold_biomass")) %>%
  pivot_wider(names_from="column_name",values_from="mean_pretty") %>%
  flextable(col_keys= c("parameter",my_column_names)) %>% 
  theme_vanilla() 

# ---------------------------------------------------------------------------
# Plot costs, revenue and gross margins per scenario

## Gross margin per cycle and its elements as a dodged bar chart for each scenario
data_summary <- data_summary %>%
  filter(!is.na(sign)) %>%
  mutate(mean_sign=ifelse(sign==0,1,sign)*mean,
         q2_5_sign=ifelse(sign==0,1,sign)*q2_5,
         q97_5_sign=ifelse(sign==0,1,sign)*q97_5,
         mean_sign_label=ifelse(abs(mean)<(10^8),
                                paste0(formatC(mean_sign*10^-6,format = "f", digits = 1), " M"), 
                                paste0(formatC(mean_sign*10^-9, format = "f", digits = 1), " B"))
  )

param_factor_levels <- c('Stocking costs', 'Other variable costs', 'Feed costs', 'Labour costs', 'Animal health costs','Harvest revenue','Gross margin','Total weight harvested (kg)')
data_summary$parameter <- factor(data_summary$parameter, levels=param_factor_levels)

gross_margin_plot <- 
      data_summary %>% filter(scenario %in% c("Current","Ideal","ZeroMort","ZeroMorb")) %>%
      ggplot(aes(y=mean_sign, x=parameter, group = scenario, fill = scenario)) +
      geom_bar(stat = "identity", position = position_dodge(width = 1)) +
      geom_errorbar(aes(ymin=q2_5_sign, ymax=q97_5_sign), width=.2, position=position_dodge(width = 1)) +
      coord_flip() +
      labs(y="Value (local currency unit)", x="", fill="Scenario") +
      geom_text(aes(label = mean_sign_label, hjust = -0.2, vjust = -.3), position = position_dodge(width = 1), size=5) +
      scale_fill_manual(values=my_scenario_colours) +
      scale_y_continuous(labels = label_number(accuracy = NULL, scale = 10^-9, suffix = "B")) +
      theme_bw() +
      theme(text = element_text(size = 16))

ggsave(filename = "figures/gross_margin_plot.tiff", plot = gross_margin_plot, dpi = 300, width = 26, height = 20, units = "cm")

## Cost categories as a stacked bar chart for each scenario 
cost_plot <- 
      data_summary %>% filter(sign==-1, scenario %in% c("Current","Ideal","ZeroMort","ZeroMorb")) %>%
      ggplot(aes(y = mean, x = scenario, fill = parameter)) +
      geom_bar(stat = "identity") +
      labs(y="Value in local currency unit", x="", fill="Cost category") +
      scale_fill_manual(values=c("#E69F00","#56B4E9","#999999","#F0E442","#CC79A7")) +
      scale_y_continuous(labels = label_number(accuracy = NULL, scale = 10^-9, suffix = "B")) +
      theme_bw() +
      theme(text = element_text(size = 20)) 

ggsave(filename = "figures/cost_plot.tiff", plot = cost_plot, dpi = 300, width = 20, height = 10, units = "cm")

# ---------------------------------------------------------------------------------------------------------------------------------------------------
# Proceed with AHLE calculations per cycle or per year

## Extract and apply multiplier from cycle to yearly output
scenario_sizes <- data_full %>% 
  filter(variable=="n_cycles_farm") %>% 
  group_by(scenario) %>%
  summarize(n_cycles_farm=mean(value)) 

data_full <- data_full %>% filter(!is.na(sign)|variable=="sold_biomass")

data_full_year <- data_full %>% 
  left_join(scenario_sizes, by="scenario") %>%
  mutate(value=value*n_cycles_farm) 

## Calculate AHLE per year
## Not running AHLE for one cycle here, but could do by doing the same process as below using data_full instead of data_full_year
allcause_ahle_results_year <- data_full_year %>%
  filter(variable=="gross_margin", scenario %in% c("Current","Ideal","ZeroMort","ZeroMorb")) %>%
  select(iteration, scenario, value) %>% 
  pivot_wider(names_from=scenario, values_from=value) %>% 
  left_join(data_full_year %>% filter(scenario=="Current"&variable=="health_cost") %>% select(iteration,value), by="iteration") %>% 
  rename(AHLEahe=value) %>% 
  left_join(data_full_year %>% filter(scenario=="Current"&variable=="sold_biomass") %>% select(iteration,value), by="iteration") %>% 
  rename(sold_biomass=value) %>% 
  mutate(AHLEmort=ZeroMort-Current-AHLEahe,
    AHLEmorb=ZeroMorb-Current-AHLEahe,
    AHLEtot=Ideal-Current,
    AHLEjoint=AHLEtot-AHLEahe-AHLEmorb-AHLEmort,
    AHLEtot_ton=AHLEtot/(sold_biomass/1000)) %>%
  select(AHLEmort,AHLEmorb,AHLEjoint,AHLEahe,AHLEtot,AHLEtot_ton) %>%
  pivot_longer(AHLEmort:AHLEtot_ton, names_to="AHLE_type", values_to="value") %>%
  group_by(AHLE_type) %>%
  summarize(mean_value=mean(value), q2_5=quantile(value, 0.025), q97_5=quantile(value, 0.975), sd=sd(value)) %>% 
  mutate(perc=ifelse(AHLE_type %in% c("AHLEahe","AHLEmorb","AHLEmort","AHLEjoint"), 100*mean_value/mean_value[AHLE_type=="AHLEtot"], NA),
      disease="All causes"
  ) %>% 
  ungroup

## Cause-specific burden estimation for "CauseX", using approach from current scenario (repeat for other individual causes)
causeX_ahle_results_year <- data_full_year %>%
  filter(variable=="gross_margin", scenario %in% c("Minus_CauseX","Current","Minus_CauseX_zeroEMort","Minus_CauseX_zeroEMorb","Minus_CauseX_zeroE")) %>%
  select(iteration, scenario, value) %>% 
  pivot_wider(names_from=scenario, values_from=value) %>% 
  left_join(data_full_year %>% filter(scenario=="Current"&variable=="sold_biomass") %>% select(iteration,value), by="iteration") %>% 
  rename(sold_biomass=value) %>% 
  mutate(AHLEahe=Minus_CauseX_zeroE-Current,
    AHLEmort=Minus_CauseX_zeroEMort-Minus_CauseX_zeroE,
    AHLEmorb=Minus_CauseX_zeroEMorb-Minus_CauseX_zeroE,
    AHLEtot=Minus_CauseX-Current,
    AHLEjoint=AHLEtot-AHLEahe-AHLEmort-AHLEmorb,
    AHLEtot_ton=AHLEtot/(sold_biomass/1000)) %>%
  select(AHLEmort,AHLEmorb,AHLEjoint,AHLEahe,AHLEtot,AHLEtot_ton) %>%
  pivot_longer(AHLEmort:AHLEtot_ton, names_to="AHLE_type", values_to="value") %>%
  group_by(AHLE_type) %>%
  summarize(mean_value=mean(value), q2_5=quantile(value, 0.025), q97_5=quantile(value, 0.975), sd=sd(value)) %>% 
  mutate(perc=ifelse(AHLE_type %in% c("AHLEahe","AHLEmorb","AHLEmort","AHLEjoint"), 100*mean_value/mean_value[AHLE_type=="AHLEtot"], NA),
      disease="CauseX"
  ) %>% 
  ungroup

## Combine and process
ahle_results_year <- rbind(allcause_ahle_results_year, causeX_ahle_results_year)

ahle_results_year <- ahle_results_year %>% 
  mutate(mean_value_usd=mean_value/conv_rate, 
        q2_5_usd=q2_5/conv_rate, 
        q97_5_usd=q97_5/conv_rate,
        sd_usd=sd/conv_rate,
        AHLE_type_text=recode(AHLE_type, 
          AHLEahe = "Animal health expenditure", 
          AHLEmorb = "Direct morbidity losses", 
          AHLEmort = "Direct mortality losses",
          AHLEjoint = "Joint morbidity and mortality losses",
          AHLEtot = "Total AHLE",
          AHLEtot_ton = "AHLE per ton of meat produced")
    )

## Separate CauseX (and other attributed causes) from unattributed AHLE (other causes) for plots
ahle_results_year <- ahle_results_year %>% 
  bind_rows(ahle_results_year %>% 
              group_by(AHLE_type_text) %>%
              mutate(mean_value=ifelse(disease=="All causes",
                  mean_value[disease=="All causes"]-mean_value[disease=="CauseX"],
                  mean_value),
                sd=ifelse(disease=="All causes",
                  sqrt(sd[disease=="All causes"]^2+sd[disease=="CauseX"]^2),
                  sd),
                mean_value_usd=ifelse(disease=="All causes",
                  mean_value_usd[disease=="All causes"]-mean_value_usd[disease=="CauseX"],
                  mean_value_usd),
                q2_5=NA,q97_5=NA, perc=NA, q2_5_usd=NA, q97_5_usd=NA
                ) %>%
              filter(disease=="All causes") %>% 
              mutate(disease="Other causes")
  )

# ---------------------------------------------------------------------------------------------------------------------------------------------------
# Display generic AHLE results with plots and tables 

## Summarize AHLE results as a table
ahle_results_year_ft <- ahle_results_year %>% 
  filter(disease!="Other causes") %>%
  flextable(col_keys= c("disease","AHLE_type_text", "perc", "mean_value", "q2_5","q97_5","mean_value_usd", "q2_5_usd","q97_5_usd")) %>% 
  theme_booktabs() %>% 
  colformat_double(j = 3, digits = 1, suffix=" %") %>%
  colformat_double(j = 4:9, big.mark = ",", digits = 0) %>%
  colformat_double(i = 5, j = 4:9, big.mark = ",", digits = 1) %>%
  bg(i = ~AHLE_type == "AHLEtot", bg=ahle_colour) %>% 
  bg(i = ~AHLE_type == "AHLEtot_ton", bg='#fee0d2') %>% 
  set_header_labels(disease = "Hazard",
    AHLE_type_text = "AHLE",
    perc="Proportion of total AHLE",
    mean_value = "Mean estimate", 
    q2_5 = "Lower bound of the 95% prediction interval",
    q97_5 = "Upper bound of the 95% prediction interval",
    mean_value_usd = "Mean estimate", 
    q2_5_usd = "Lower bound of the 95% prediction interval",
    q97_5_usd = "Upper bound of the 95% prediction interval") %>%
  add_header_row(top = TRUE, values = c("Hazard","AHLE","Proportion of total AHLE","Local currency estimates","Local currency estimates","Local currency estimates","USD equivalents", "USD equivalents", "USD equivalents"))  %>%
  merge_v(part = "header") %>% 
  merge_h(part = "header") 

## Generic plots
ahle_generic_plot_usd <- ahle_results_year %>%
  filter(disease!="Other causes", AHLE_type=="AHLEtot") %>%
  ggplot(aes(x=disease, y=mean_value_usd, fill=disease)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=ahle_colour_shades) +
  geom_errorbar(aes(ymin=q2_5_usd, ymax=q97_5_usd), width=.1, colour="grey50") +
  labs(y="Annual estimate (USD)", x="", fill="Cause") +
  scale_y_continuous(labels = label_number(accuracy = NULL, scale = 10^-6, suffix = "M")) +
  theme_bw() +
  theme(text = element_text(size = 16))

ggsave(filename = "figures/ahle_generic_plot_usd.tiff", plot = ahle_generic_plot_usd, dpi = 300, width = 10, height = 10, units = "cm")

ahle_generic_plot_currency <- ahle_results_year %>%
  filter(disease!="Other causes", AHLE_type=="AHLEtot") %>%
  ggplot(aes(x=disease, y=mean_value, fill=disease)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=ahle_colour_shades) +
  geom_errorbar(aes(ymin=q2_5, ymax=q97_5), width=.1, colour="grey50") +
  labs(y="Annual estimate (local currency unit)", x="", fill="Cause") +
  scale_y_continuous(labels = label_number(accuracy = NULL, scale = 10^-9, suffix = "B")) +
  theme_bw() +
  theme(text = element_text(size = 16))

ggsave(filename = "figures/ahle_generic_plot_currency.tiff", plot = ahle_generic_plot_currency, dpi = 300, width = 10, height = 10, units = "cm")

ahle_generic_plot_currency_other <- ahle_results_year %>%
  filter(disease!="All causes", AHLE_type=="AHLEtot") %>%
  ggplot(aes(x=disease, y=mean_value, fill=disease)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=ahle_colour_shades) +
  geom_errorbar(aes(ymin=mean_value-1.96*sd, ymax=mean_value+1.96*sd), width=.1, colour="grey50") +
  labs(y="Annual burden estimate (local currency unit)", x="") +
  scale_y_continuous(labels = label_number(accuracy = NULL, scale = 10^-9, suffix = "B")) +
  theme_bw() +
  theme(text = element_text(size = 16), legend.position = "none")

ggsave(filename = "figures/ahle_generic_plot_currency_other.tiff", plot = ahle_generic_plot_currency_other, dpi = 300, width = 13, height = 10, units = "cm")

## Plots describing AHLE components
donut_plot_components <- ahle_results_year %>%
  filter(AHLE_type!="AHLEtot", AHLE_type!="AHLEtot_ton", disease!="Other causes") %>%
  mutate(disease=recode(disease, "CauseX"="CauseX only")) %>%
  group_by(disease) %>%
  mutate(ymax = cumsum(perc/100),
         ymin = c(0, head(ymax, n=-1)),
         labelPosition = (ymax + ymin) / 2,
         label = paste0(round(perc,0), "%")) %>% 
  ggplot(aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=AHLE_type_text)) +
  geom_rect() +
  #geom_label(x=3.5, aes(y=labelPosition, label=label), size=4, show.legend=F) +
  geom_label_repel(x=3, aes(y=labelPosition, label=str_wrap(label, width=16)), size=4, show.legend=F) +
  scale_fill_manual(values=ahle_components_colours) +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  labs(fill="Component") +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank()
        ) +
  facet_grid(~disease)

ggsave(filename = "figures/donut_plot_components.tiff", plot = donut_plot_components, dpi = 300, width = 20, height = 20, units = "cm")

donut_plot_all_only <- ahle_results_year %>%
  filter(AHLE_type!="AHLEtot", AHLE_type!="AHLEtot_ton", disease=="All causes") %>%
  mutate(ymax = cumsum(perc/100),
         ymin = c(0, head(ymax, n=-1)),
         labelPosition = (ymax + ymin) / 2,
         label = paste0(AHLE_type_text, ": ",round(perc,0), "%")) %>% 
  ggplot(aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=AHLE_type_text)) +
  geom_rect() +
  geom_label_repel(x=3, aes(y=labelPosition, label=str_wrap(label, width=16)), size=4, show.legend=F) +
  scale_fill_manual(values=ahle_components_colours) +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  labs(fill="Component") +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank()
        )

ggsave(filename = "figures/donut_plot_all_only.tiff", plot = donut_plot_all_only, dpi = 300, width = 20, height = 20, units = "cm")

component_attribution_plot <- ahle_results_year %>%
  filter(AHLE_type!="AHLEtot", AHLE_type!="AHLEtot_ton", disease!="All causes") %>%
  ggplot(aes(y=mean_value, x=disease, fill=AHLE_type_text)) +
  geom_bar(stat='identity') +
  scale_fill_manual(values=ahle_components_colours) +
  facet_wrap(~AHLE_type_text, ncol=2) +
  labs(y="Annual losses (in billion local currency unit)", fill="Component", x="") +
  scale_y_continuous(labels = label_number(accuracy = NULL, scale = 10^-9, suffix = "")) +
  theme_bw() +
  theme(text = element_text(size = 12), axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

ggsave(filename = "figures/component_attribution_plot.tiff", plot = component_attribution_plot, dpi = 300, width = 18, height = 15, units = "cm")

component_attribution_plot_perc <- ahle_results_year %>%
  filter(AHLE_type!="AHLEtot", AHLE_type!="AHLEtot_ton", disease!="All causes") %>%
  group_by(AHLE_type) %>% 
  mutate(perc=mean_value/sum(mean_value)*100) %>%
  ggplot(aes(y=perc, x=disease, fill=AHLE_type_text)) +
  geom_bar(stat='identity') +
  geom_label(aes(label=paste0(round(perc,0),"%")), position=position_stack(vjust=0.5), size=4) + 
  scale_fill_manual(values=ahle_components_colours) +
  facet_wrap(~AHLE_type_text, ncol=2) +
  labs(y="Proportion of envelope", fill="Component", x="") +
  theme_bw() +
  theme(text = element_text(size = 12), axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

ggsave(filename = "figures/component_attribution_plot_perc.tiff", plot = component_attribution_plot_perc, dpi = 300, width = 18, height = 15, units = "cm")

## Treemap for AHLE components
attribution_data <- ahle_results_year %>%
  filter(AHLE_type!="AHLEtot", AHLE_type!="AHLEtot_ton", disease!="All causes") %>%
  select(disease, AHLE_type_text, mean_value) %>%
  mutate(Level1=disease, 
    Level2=AHLE_type_text) 
  
attribution_summary_l1 <- attribution_data %>% mutate(Label=paste0(Level2,Level1), Text=Level1, Parent=paste0(Level2), Value=mean_value)
attribution_summary_l2 <- attribution_data %>% group_by(Level2) %>% summarize(mean_value=sum(mean_value)) %>% mutate(Value=mean_value, Label=Level2, Text=Level2, Parent="AHLE")

treemap_data <- rbind(attribution_summary_l2[,c("Label","Parent","Value","Text")],attribution_summary_l1[,c("Label","Parent","Value","Text")])

treemap_data$formatted_values <- ifelse(treemap_data$Value<(10^8),
 paste0(formatC(treemap_data$Value*10^-6,format = "f", digits = 1), " M"),
 paste0(formatC(treemap_data$Value*10^-9, format = "f", digits = 1), " B"))

tree_plot_two_levels <- plot_ly(
    type='treemap',
    branchvalues="total",
    ids=treemap_data$Label,
    labels=treemap_data$Text,
    parents=treemap_data$Parent,
    values= treemap_data$Value,
    hovertext = treemap_data$formatted_values,
    hovertemplate = "Attribution= %{label}<br>Value= %{hovertext} local currency unit") %>%
    layout(colorway=c("#C0CBDC","#EF5FAF","#5E8C61","#AE8EAC"))

## Attributed parts of the AHLE as a donut
attribution_donut_plot <- ahle_results_year %>%
  filter(AHLE_type=="AHLEtot", disease!="All causes") %>%
  mutate(perc= mean_value/sum(mean_value)*100,
        ymax = cumsum(perc/100),
         ymin = c(0, head(ymax, n=-1)),
         labelPosition = (ymax + ymin) / 2,
         label = paste0(round(perc,0), "%")) %>% 
  ggplot(aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=disease)) +
  geom_rect() +
  geom_label(x=3.5, aes(y=labelPosition, label=label), size=4, show.legend=F) +
  scale_fill_manual(values=ahle_colour_shades) +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  labs(fill="Hazard") +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        )

ggsave(filename = "figures/attribution_donut_plot.tiff", plot = attribution_donut_plot, dpi = 300, width = 10, height = 10, units = "cm")

# ---------------------------------------------------------------------------------------------------------------------------------------------------
# Input parameter graphs

input_params <- input_params %>% left_join(param_names, by="variable") 
input_params$value_formatted <- ifelse(input_params$value<1,input_params$value,formatC(input_params$value,digits=0,format="f"))

## Parameters common to all scenarios
common_params_plot <- input_params %>% 
  filter(type=="Distribution", scenario=="Current", variable %in% c("stocking_weight","feed_price","labour_price","other_price","sale_price","seed_price")) %>%
  uncount(weight=n) %>%
  ggplot(aes(x=value)) +
  geom_density(alpha=.7, fill="lightgrey") +
  facet_wrap(~variable, ncol = 3, scales="free", labeller = label_wrap_gen()) +
  labs(x="Parameter value", y="", fill="Scenario")+
  theme_bw() +
  theme(text = element_text(size = 12)) 

ggsave(filename = "figures/common_params_plot.tiff", plot = common_params_plot, dpi = 300, width = 16, height = 10, units = "cm")

## FCR
fcr_params_plot <- input_params %>% 
  filter(variable == "fcr", scenario %in% c("Current", "Ideal", "Minus_CauseX")) %>%
  uncount(weight=n) %>%
  ggplot(aes(x=value, group=scenario, fill=scenario)) +
  geom_boxplot() +
  scale_fill_manual(values=my_scenario_colours) +
  labs(x="Feed conversion ratio", y="", fill="Scenario")+
  theme_bw() +
  theme(text = element_text(size = 12), strip.text.y = element_blank()) 

ggsave(filename = "figures/fcr_params_plot.tiff", plot = fcr_params_plot, dpi = 300, width = 10, height = 6, units = "cm")

## Table of fixed parameters
fixed_params_ft <- input_params %>% filter(type=="Point") %>% 
  flextable(col_keys= c("parameter", "scenario", "value_formatted")) %>% 
  theme_box() %>% 
  set_header_labels(parameter = "Parameter", 
    scenario = "Scenario",
    value_formatted = "Value") %>%
  merge_v(j= "parameter") 

# ---------------------------------------------------------------------------------------------------------------------------------------------------
# Mortality, population and live weight graphs

## Mortality
## static in example. change plot to a line graph if varying mort rates per production day
mort_data <- read.csv("outputs/all_mortality.csv")
mortality_plot <- mort_data %>% 
  filter(production_day==1) %>%
  ggplot(aes(x=scenario, y=mean, fill=scenario)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=q2_5, ymax=q97_5), width=.2, colour="black") +
  #scale_fill_manual(values=my_scenario_colours)+
  labs(y="Daily mortality rate", x="Scenario", fill="Scenario")+
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_y_continuous(labels = label_number(accuracy = NULL, scale = 10^2, suffix = "%")) +
  theme_bw() +
  theme(text = element_text(size = 14)) 

ggsave(filename = "figures/mortality_plot.tiff", plot = mortality_plot, dpi = 300, width = 30, height = 10, units = "cm")

## ADG
## static in example. change plot to a line graph if varying growth rates per production day
growth_data <- read.csv("outputs/all_growth.csv")
growth_plot <- growth_data %>% 
  filter(production_day==1) %>% 
  ggplot(aes(x=scenario, y=mean ,fill=scenario)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=q2_5, ymax=q97_5), width=.2, colour="black") +
  #scale_fill_manual(values=my_scenario_colours)+
  labs(y="Daily growth rate as % of bodyweight", x="Scenario", fill="Scenario")+
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_y_continuous(labels = label_number(accuracy = NULL, scale = 10^2, suffix = "%")) +
  theme_bw() +
  theme(text = element_text(size = 14)) 

ggsave(filename = "figures/growth_plot.tiff", plot = growth_plot, dpi = 300, width = 30, height = 10, units = "cm")

## Individual liveweight
liveweight_data <- read.csv("outputs/all_liveweight.csv")
liveweight_plot <- liveweight_data %>% 
  filter(scenario %in% c("Current", "Ideal", "Minus_CauseX")) %>%
  ggplot(aes(x=production_day, y=mean, fill=scenario, colour=scenario)) +
  geom_line() +
  geom_ribbon(aes(ymin=q2_5, ymax=q97_5), alpha=0.5) +
  scale_fill_manual(values=my_scenario_colours)+
  scale_colour_manual(values=my_scenario_colours)+
  labs(y="Liveweight (kg per head)", x="Day in the production cycle", fill="Scenario")+
  theme_bw() +
  theme(text = element_text(size = 14)) +
  #facet_wrap(~scenario) +
  guides(colour = "none")

ggsave(filename = "figures/liveweight_plot.tiff", plot = liveweight_plot, dpi = 300, width = 20, height = 20, units = "cm")
   
## Biomass present 
biomass_data <- read.csv("outputs/all_biomass.csv")
biomass_plot <- biomass_data %>% 
  filter(scenario %in% c("Current", "Ideal", "Minus_CauseX")) %>%
  left_join(area_used, by="scenario") %>%
  mutate(density=mean/value, q2_5_density=q2_5/value, q97_5_density=q97_5/value) %>%
  ggplot(aes(x=production_day, y=density, fill=scenario, colour=scenario)) +
  geom_line() +
  geom_ribbon(aes(ymin=q2_5_density, ymax=q97_5_density), alpha=0.5) +
  scale_fill_manual(values=my_scenario_colours)+
  scale_colour_manual(values=my_scenario_colours)+
  labs(y="Biomass density (kg per square meters)", x="Day in the production cycle", fill="Scenario")+
  theme_bw() +
  theme(text = element_text(size = 14)) +
  #facet_wrap(~scenario) +
  guides(colour = "none")

ggsave(filename = "figures/biomass_plot.tiff", plot = biomass_plot, dpi = 300, width = 20, height = 20, units = "cm")

