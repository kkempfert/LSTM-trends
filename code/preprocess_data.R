# Load libraries
library(ggplot2)
library(dplyr)
library(forcats)

# Set file paths
dir_path = c("/Users/kempfert/Documents/rain2025/")
data_path = paste0(dir_path, "data/")
code_path = paste0(dir_path, "code/")
results_path = paste0(dir_path, "output/results/")

source(paste0(code_path, "functions_preprocess_data.R"))

# First time running, make sure to create the dates dataframe from the commented out lines.
#dates = create_dates()
#write.csv(dates, paste0(data_path, "dates.csv"), row.names = F)
dates = read.csv(paste0(data_path, "dates.csv"))

# Read in raw data files
CA_Berkeley_raw = read.csv(paste0(data_path, "CA_Berkeley_raw.csv"))
AZ_Winslow_raw = read.csv(paste0(data_path, "AZ_Winslow_raw.csv"))
CA_Quincy_raw = read.csv(paste0(data_path, "CA_Quincy_raw.csv"))

# Preprocess station data
CA_Berkeley = make_station_df(CA_Berkeley_raw, start_date = "1919-01-01", end_date = "2020-12-31", units_raw = "mm")
CA_Quincy = make_station_df(CA_Quincy_raw, start_date = "1919-01-01", end_date = "2024-12-31", return_full = F)
AZ_Winslow = make_station_df(AZ_Winslow_raw, start_date = "1919-01-01", end_date = "2024-12-31", return_full = T) 

write.csv(CA_Berkeley, paste0(data_path, "CA_Berkeley.csv"), row.names = F)
write.csv(CA_Quincy, paste0(data_path, "CA_Quincy.csv"), row.names = F)
write.csv(AZ_Winslow$df_sub, paste0(data_path, "AZ_Winslow.csv"), row.names = F)

# Plot several meteorological variables for the Winslow weather station
full = AZ_Winslow$df_full

# Check when each variable starts being recorded
inds_vars = c(2:(ncol(full) - 8))
vars = full[, inds_vars]
vars_begin = apply(vars, 2, function(x) full$raw_year[which(!is.na(x))[1]])

# Make a dataframe summarizing the available variables
vars_names = read.csv(paste0(data_path, "AZ_Winslow_variables.csv"))
vars_begin_df = data.frame(code = names(vars), begin = vars_begin)
vars_df = merge(vars_begin_df, vars_names, by = "code")
View(vars_df)

pl_yrs = 1972:1976 # specify the subset of years to be plotted -- this is the middle of the study period
inds_pl = which(full$raw_year %in% pl_yrs)
subset = full[inds_pl, ]

# Create a dataframe for plotting
df = data.frame(Day = 1:length(inds_pl), Year = subset$raw_year, 
                Rain = subset$prcp, Snow = subset$snow, 
                MinTemp = subset$tmin, MaxTemp = subset$tmax, 
                WindSpeed = subset$wsf1)#, #awnd
# Convert snow from in to cm, to match the units of prcp
df$Snow = df$Snow * 2.54
m_long = tidyr::pivot_longer(df, c("Rain", "Snow", "MinTemp", "MaxTemp", "WindSpeed"),
                           values_to = "Amt", names_to = "Weather")

inds_yrs = sapply(pl_yrs, function(yr) df$Day[which(df$Year == yr)[1]]) 
m_long$Weather = fct_inorder(m_long$Weather)

# Plot the variables
pl_m_vars = plot_m_vars(m_long)

pdf(paste0(results_path, "AZ_Winslow_mvariables.pdf"))
print(pl_m_vars)
dev.off()

