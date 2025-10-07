dir_path = c("/Users/kempfert/Documents/rain2025/")
data_path = paste0(dir_path, "data/")

# Make CSVs from pulled GHCN data
AZ_Winslow_raw = read.csv("https://www.ncei.noaa.gov/orders/cdo/4075812.csv")
names(AZ_Winslow_raw) = tolower(names(AZ_Winslow_raw)) # make variable names lowercase
write.csv(AZ_Winslow_raw, paste0(data_path, "AZ_Winslow_raw.csv"), row.names = F)

CA_Quincy_raw = read.csv("https://www.ncei.noaa.gov/orders/cdo/4078852.csv")
names(CA_Quincy_raw) = tolower(names(CA_Quincy_raw)) # make variable names lowercase
write.csv(CA_Quincy_raw, paste0(data_path, "CA_Quincy_raw.csv"), row.names = F)

# Berkeley weather station data is not available on GHCN as of October, 2025, so 
# a previous download of that data is used. 