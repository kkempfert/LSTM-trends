# This script obtains the metrics and trend results for the three case studies.
library(ggplot2)
library(ggpubr)
library(stringr)
library(dplyr)
library(data.table)
library(forcats) # for fct_inorder function
library(ggh4x) # for arranging plots
library(patchwork) # for arranging plots
library(trend) # for computing Sen's slope
library(modifiedmk) # for calculating the Mann-Kendall statistic for testing slope

dir_path = ""
data_path = paste0(dir_path, "data/")
code_path = paste0(dir_path, "code/")
output_path = paste0(dir_path, "output/hpc/")
results_path = paste0(dir_path, "output/results/")

station_names = c("CA_Berkeley", "CA_Quincy", "AZ_Winslow")


# Set M, V, B and the number of mixture components.
M = 20 
V = 9
B = 10
num_K = 2

# If it is the first time running the code, run these lines (note that it takes a bit of time).
#source(paste0(code_path, "functions_get_metrics.R"))
#metrics = sapply(station_names, function(station) get_metrics_station(station))

source(paste0(code_path, "functions_plot_metrics.R"))


# Load saved files
CA_Quincy_metrics = load(paste0(results_path, "CA_Quincy", "_", "metrics", ".RData"))
CA_Quincy_metrics = metrics
CA_Berkeley_metrics = load(paste0(results_path, "CA_Berkeley", "_", "metrics", ".RData"))
CA_Berkeley_metrics = metrics
AZ_Winslow_metrics = load(paste0(results_path, "AZ_Winslow", "_", "metrics", ".RData"))
AZ_Winslow_metrics = metrics
rm(metrics)

CA_Quincy_metrics1 = CA_Quincy_metrics[[1]]
CA_Berkeley_metrics1 = CA_Berkeley_metrics[[1]]
AZ_Winslow_metrics1 = AZ_Winslow_metrics[[1]]
CA_Quincy_metrics2 = CA_Quincy_metrics[[2]]
CA_Berkeley_metrics2 = CA_Berkeley_metrics[[2]]
AZ_Winslow_metrics2 = AZ_Winslow_metrics[[2]]

# Define variable names for plots
names_metrics1 = c("Precipitation Intensity (cm)", "Precipitation Probability", 
                 "Precipitation Intensity (cm)", "Precipitation Probability", "Total Precipitation (cm)", 
                 "Mean Dry Spell Length", "Mean Wet Spell Length", "Mean Wet Spell Intensity (cm)",
                 "Probability > 3mm", "Probability > 10mm", "Probability > 20mm",
                 "Probability > 3mm", "Probability > 10mm", "Probability > 20mm",
                 "Probability D-D", "Probability D-M", "Probability D-W",
                 "Probability M-D", "Probability M-M", "Probability M-W",
                 "Probability W-D", "Probability W-M", "Probability W-W",
                 "Probability D-D", "Probability D-M", "Probability D-W",
                 "Probability M-D", "Probability M-M", "Probability M-W",
                 "Probability W-D", "Probability W-M", "Probability W-W")
varnames_metrics1 = c("mean-daily", "prob-daily", 
                    "mean-yearly", "prob-yearly", "total-yearly",
                    "mean-d-spell-len-yearly", "mean-w-spell-len-yearly", "mean-w-spell-amt-yearly",
                    "prob-3mm-daily", "prob-10mm-daily", "prob-20mm-daily",
                    "prob-3mm-yearly", "prob-10mm-yearly", "prob-20mm-yearly",
                    "prob-dd-daily", "prob-dm-daily", "prob-dw-daily",
                    "prob-md-daily", "prob-mm-daily", "prob_mw-daily",
                    "prob-wd-daily", "prob-wm-daily", "prob-ww-daily",
                    "prob-dd-yearly", "prob-dm-yearly", "prob-dw-yearly",
                    "prob-md-yearly", "prob-mm-yearly", "prob_mw-yearly",
                    "prob-wd-yearly", "prob-wm-yearly", "prob-ww-yearly")
num_metrics1 = length(names_metrics1)
inds_daily = c(T, T,
               F, F, F,
               F, F, F,
               T, T, T, 
               F, F, F,
               rep(T, 9),
               rep(F, 9))
num_metrics1 = length(inds_daily)

names_metrics2 = c("Mean Dry Spell Length", "Mean Wet Spell Length", "1-day Precipitation (cm)", "3-day Precipitation (cm)", "10-day Precipitation (cm)")
varnames_metrics2 = c("mean-d-spell-len-qq", "mean-w-spell-len-qq", "1-day-total-qq", "3-day-total-qq", "10-day-total-qq")
num_metrics2 = length(names_metrics2)

# Make plots of metrics for each station
lapply(1:8, function(i) plot_metric_stations1(i)) # 4 x 2 plots
plot_metric_stations2(1) # 4 x 2 plot for mean dry spell length
plot_metric_stations2(3) # 4 x 2 qqplot for daily prcp amt
plot_metric_stations2(4) # 4 x 2 qqplot for 3-day prcp amt
plot_metric_stations2(5) # 4 x 2 qqplot for 10-day prcp amt
plot_cdf_stations(ind_daily = T) # 4 x 3 plot of probability of 1 - ecdf 
plot_cdf_stations(ind_daily = F)
plot_trans_stations("D", ind_daily = T) # Transition probability plots
plot_trans_stations("M", ind_daily = T)
plot_trans_stations("W", ind_daily = T)
plot_trans_stations("D", ind_daily = F)
plot_trans_stations("M", ind_daily = F)
plot_trans_stations("W", ind_daily = F)

# Make trend plots for the stations
# Figures for Berkeley and Winslow
plot_metric_trend_BW("Berkeley")
plot_metric_trend_BW("Winslow")

# Figure for Quincy
plot_metric_trend_Q()

# Sen's slope p-values
CA_Quincy_trend = CA_Quincy_metrics[[3]]
CA_Berkeley_trend = CA_Berkeley_metrics[[3]]
AZ_Winslow_trend = AZ_Winslow_metrics[[3]]

palette = get_cat_palette(5, T)

pl_Berkeley_DJF = plot_violin_season_BW("DJF", CA_Berkeley_trend, "CA_Berkeley")
pl_Berkeley_MAM = plot_violin_season_BW("MAM", CA_Berkeley_trend, "CA_Berkeley")
pl_Berkeley_SON = plot_violin_season_BW("SON", CA_Berkeley_trend, "CA_Berkeley")

pl_Berkeley = (pl_Berkeley_DJF$pl_spell + pl_Berkeley_DJF$pl_mean) /
              (pl_Berkeley_MAM$pl_spell + pl_Berkeley_MAM$pl_mean) /
               (pl_Berkeley_SON$pl_spell + pl_Berkeley_SON$pl_mean)

pl_Winslow_DJF = plot_violin_season_BW("DJF", AZ_Winslow_trend, "AZ_Winslow")
pl_Winslow_MAM = plot_violin_season_BW("MAM", AZ_Winslow_trend, "AZ_Winslow")
pl_Winslow_JJA = plot_violin_season_BW("JJA", AZ_Winslow_trend, "AZ_Winslow")
pl_Winslow_SON = plot_violin_season_BW("SON", AZ_Winslow_trend, "AZ_Winslow")

pl_Winslow = (pl_Winslow_DJF$pl_spell + pl_Winslow_DJF$pl_mean) /
  (pl_Winslow_MAM$pl_spell + pl_Winslow_MAM$pl_mean) /
  (pl_Winslow_JJA$pl_spell + pl_Winslow_JJA$pl_mean) /
  (pl_Winslow_SON$pl_spell + pl_Winslow_SON$pl_mean)

pl_Quincy = plot_violin_Q()

pdfsave(paste0(results_path, "violins_Berkeley"), pl_Berkeley)
pdfsave(paste0(results_path, "violins_Winslow"), pl_Winslow)
pdfsave(paste0(results_path, "violins_Quincy"), pl_Quincy)


