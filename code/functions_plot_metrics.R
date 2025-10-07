#--------------------------------------- Functions for diagnostic metrics ------------------------------------------------

# This function constructs a dataframe for plotting for a given metric. 
make_df_stations = function(type = 1, ind_metric) {
  if (type == 1) {
    CA_Quincy_metric = CA_Quincy_metrics1[[ind_metric]]$df_seasons
    CA_Berkeley_metric = CA_Berkeley_metrics1[[ind_metric]]$df_seasons
    AZ_Winslow_metric = AZ_Winslow_metrics1[[ind_metric]]$df_seasons
  } else {
    CA_Quincy_metric = CA_Quincy_metrics2[[ind_metric]]$df_seasons
    CA_Berkeley_metric = CA_Berkeley_metrics2[[ind_metric]]$df_seasons
    AZ_Winslow_metric = AZ_Winslow_metrics2[[ind_metric]]$df_seasons
  }
  
  stations = rbind(AZ_Winslow_metric, CA_Berkeley_metric, CA_Quincy_metric)
  return(stations)
}

# For a given metric index, makes a 4 x 2 plot combining the stations.
# This is done for all metrics except for the transition probability ones.
plot_metric_stations1 = function(ind_metric) {
  ind_daily = inds_daily[ind_metric]
  name_metric = names_metrics1[ind_metric]
  varname_metric = varnames_metrics1[ind_metric]
  
  df_stations = make_df_stations(type = 1, ind_metric)
  
  df_stations$pl_name = fct_inorder(as.factor(df_stations$pl_name))
  
  # Make facet plots
  if (ind_daily) {
    plot = ggplot(df_stations) + 
      geom_ribbon(aes(x = Day, ymin = CI_l, ymax = CI_u), fill = "gray82") +
      geom_line(aes(x = Day, y = Values, group = Estimator, color = Estimator)) + 
      scale_colour_manual(values = c("Simulated" = "#a90076", "Empirical" = "#0f27ba")) +
      facet_wrap(~pl_name, nrow = 4, ncol = 2, scales = "free") +
      theme_classic() + 
      ylab(name_metric) + xlab("Day of Season") +
      theme( 
        strip.background = element_rect(fill = "#bbe0ed", linewidth = 0),
        strip.text = element_text(size = 10))
  } else {
    plot = ggplot(df_stations) + 
      geom_ribbon(aes(x = Year, ymin = CI_l, ymax = CI_u), fill = "gray82") +
      geom_line(aes(x = Year, y = Values, group = Estimator, color = Estimator)) + 
      scale_colour_manual(values = c("Simulated" = "#a90076", "Empirical" = "#0f27ba")) +
      facet_wrap(~pl_name, nrow = 4, ncol = 2, scales = "free_y") +
      theme_classic() + 
      ylab(name_metric) +
      theme(
        strip.background = element_rect(fill = "#bbe0ed", linewidth = 0),
        strip.text = element_text(size = 10))
  }
  
  filename = paste0("3stations_", varname_metric)
  pdf(paste0(results_path, filename, ".pdf"), width = 8, height = 10)
  print(plot)
  dev.off()
}

# For a given metric index for qq plots, makes a 4 x 2 plot combining the stations.
plot_qq_station = function(df_station, box = "top") {
  vals = c(df_station$Emp, df_station$CI_l, df_station$CI_u)
  xy_lims = range(vals)
  if (box == "top") {
    # Use facet_wrap layer with pl_name, in order to get a plot title in box at the top
    plot = ggplot(df_station) +
      geom_ribbon(aes(x = Emp, ymin = CI_l, ymax = CI_u), fill = "gray86") + #gray82
      geom_point(aes(x = Emp, y = Sim), colour = "#441074", fill = "blueviolet", alpha = .65, shape = 21) +
      geom_abline(aes(slope = 1, intercept = 0), linetype = 2) +
      coord_fixed(ratio = 1) +
      scale_x_continuous(limits = xy_lims) +
      scale_y_continuous(limits = xy_lims) +
      facet_wrap(~pl_name) +
      xlab("Simulated (cm)") + ylab("Empirical (cm)") +
      theme_classic() +
      theme(strip.background = element_rect(fill = "#bbe0ed", linewidth = 0),
            strip.text = element_text(size = 10))
  } else if (box == "right") {
    plot = ggplot(df_station) +
      geom_ribbon(aes(x = Emp, ymin = CI_l, ymax = CI_u), fill = "gray86") + #gray82
      geom_point(aes(x = Emp, y = Sim), colour = "#441074", fill = "blueviolet", alpha = .65, shape = 21) +
      geom_abline(aes(slope = 1, intercept = 0), linetype = 2) +
      coord_fixed(ratio = 1) +
      scale_x_continuous(limits = xy_lims) +
      scale_y_continuous(limits = xy_lims) +
      facet_wrap(~pl_name, strip.position = "right") +
      xlab("Simulated (cm)") + ylab("Empirical (cm)") +
      theme_classic() +
      theme(strip.background = element_rect(fill = "#bbe0ed", linewidth = 0),
            strip.text = element_text(size = 10))
  } else {
    plot = ggplot(df_station) +
      geom_ribbon(aes(x = Emp, ymin = CI_l, ymax = CI_u), fill = "gray86") + #gray82
      geom_point(aes(x = Emp, y = Sim), colour = "#441074", fill = "blueviolet", alpha = .65, shape = 21) +
      geom_abline(aes(slope = 1, intercept = 0), linetype = 2) +
      coord_fixed(ratio = 1) +
      scale_x_continuous(limits = xy_lims) +
      scale_y_continuous(limits = xy_lims) +
      xlab("Simulated") + ylab("Empirical") +
      theme_classic() +
      theme(strip.background = element_rect(fill = "#bbe0ed", linewidth = 0),
            strip.text = element_text(size = 10))
  }
  
  return(plot)
}

# Function that combines the qqplot metrics for the stations
plot_metric_stations2 = function(ind_metric) {
  name_metric = names_metrics2[ind_metric]
  varname_metric = varnames_metrics2[ind_metric]
  
  df_stations = make_df_stations(type = 2, ind_metric)
  
  plots = lapply(unique(df_stations$pl_name), function(name) plot_qq_station(df_stations[df_stations$pl_name == name, ]))
  plot = (plots[[1]] + plots[[2]]) / (plots[[3]] + plots[[4]]) / (plots[[5]] + plots[[6]]) / (plots[[7]] + plots[[8]])
  
  filename = paste0("3stations_", varname_metric)
  pdf(paste0(results_path, filename, ".pdf"), width = 8, height = 10)
  print(plot)
  dev.off()
}

# Function for plotting for the stations the probability of being between >.3, >1, and > 2 cm (1 - ecdf)
plot_cdf_stations = function(ind_daily) {
  if (ind_daily) {
    inds_vars = 9:11
  } else {
    inds_vars = 12:14
  }
  
  df_stations = lapply(inds_vars, function(i) make_df_stations(type = 1, i))
  df_stations = rbind(df_stations[[1]], df_stations[[2]], df_stations[[3]])
  
  state = paste0(c("> .3 cm", "> 1 cm", "> 2 cm"))
  len = nrow(df_stations)/3
  df_stations$state = rep(state, each = len)
  df_stations$state = fct_inorder(as.factor(df_stations$state))
  df_stations$pl_name = fct_inorder(as.factor(df_stations$pl_name))
  
  if (ind_daily) {
    plot = ggplot(df_stations) + 
      geom_ribbon(aes(x = Day, ymin = CI_l, ymax = CI_u), fill = "gray82") +
      geom_line(aes(x = Day, y = Values, group = Estimator, color = Estimator)) + 
      scale_colour_manual(values = c("Simulated" = "#a90076", "Empirical" = "#0f27ba")) +
      ggh4x::facet_grid2(pl_name~state, scales = "free", independent = "all") +
      theme_classic() + 
      ylab("Probability") + xlab("Day of Season") + 
      theme(strip.background = element_rect(fill = "#bbe0ed", linewidth = 0),
            theme(text=element_text(size=.1)), strip.text = element_text(size = 6),
            panel.border = element_rect(colour = "gray48", fill=NA),
            panel.spacing = unit(.5, "lines"))
  } else {
    plot = ggplot(df_stations) + 
      geom_ribbon(aes(x = Year, ymin = CI_l, ymax = CI_u), fill = "gray82") +
      geom_line(aes(x = Year, y = Values, group = Estimator, color = Estimator)) + 
      scale_colour_manual(values = c("Simulated" = "#a90076", "Empirical" = "#0f27ba")) +
      ggh4x::facet_grid2(pl_name~state, scales = "free_y", independent = "y") +
      theme_classic() + 
      ylab("Probability") + xlab("Year") + 
      theme(strip.background = element_rect(fill = "#bbe0ed", linewidth = 0),
            theme(text=element_text(size=.1)), strip.text = element_text(size = 6),
            panel.border = element_rect(colour = "gray48", fill=NA),
            panel.spacing = unit(.5, "lines"))
  }
  
  time_unit = ifelse(ind_daily, "-daily", "-yearly")
  filename = paste0("3stations_", "cdf", time_unit)
  pdf(paste0(results_path, filename, ".pdf"), width = 8, height = 10)
  print(plot)
  dev.off()
}

# For each conditioning state (D, M, W), makes an 8 x 3 plot combining the stations.
# This is done for the transition probability metrics.
plot_trans_stations = function(state_prev, ind_daily) {
  inds_state_prev = grep(paste0(state_prev, "-"), names_metrics1)
  if (ind_daily) {
    inds_state_prev = inds_state_prev[1:3]
  } else {
    inds_state_prev = inds_state_prev[4:6]
  }
  
  df_stations = lapply(inds_state_prev, function(i) make_df_stations(type = 1, i))
  df_stations = rbind(df_stations[[1]], df_stations[[2]], df_stations[[3]])
  
  state_current = paste0(state_prev, "-", c("D", "M", "W"))
  len = nrow(df_stations)/3
  df_stations$trans = rep(state_current, each = len)
  df_stations$trans = fct_inorder(as.factor(df_stations$trans))
  df_stations$pl_name = fct_inorder(as.factor(df_stations$pl_name))
  
  if (ind_daily) {
    plot = ggplot(df_stations) + 
      geom_ribbon(aes(x = Day, ymin = CI_l, ymax = CI_u), fill = "gray82") +
      geom_line(aes(x = Day, y = Values, group = Estimator, color = Estimator)) + 
      scale_colour_manual(values = c("Simulated" = "#a90076", "Empirical" = "#0f27ba")) +
      ggh4x::facet_grid2(pl_name~trans, scales = "free", independent = "all") +
      theme_classic() + 
      ylab("Transition Probability") + xlab("Day of Season") + 
      theme(strip.background = element_rect(fill = "#bbe0ed", linewidth = 0),
            theme(text=element_text(size=.1)), strip.text = element_text(size = 6),
            panel.border = element_rect(colour = "gray48", fill=NA),
            panel.spacing = unit(.5, "lines"))
  } else {
    plot = ggplot(df_stations) + 
      geom_ribbon(aes(x = Year, ymin = CI_l, ymax = CI_u), fill = "gray82") +
      geom_line(aes(x = Year, y = Values, group = Estimator, color = Estimator)) + 
      scale_colour_manual(values = c("Simulated" = "#a90076", "Empirical" = "#0f27ba")) +
      ggh4x::facet_grid2(pl_name~trans, scales = "free_y", independent = "y") +
      theme_classic() + 
      ylab("Transition Probability") + xlab("Year") + 
      theme(strip.background = element_rect(fill = "#bbe0ed", linewidth = 0),
            theme(text=element_text(size=.1)), strip.text = element_text(size = 6),
            panel.border = element_rect(colour = "gray48", fill=NA),
            panel.spacing = unit(.5, "lines"))
  }
  
  time_unit = ifelse(ind_daily, "-daily", "-yearly")
  filename = paste0("3stations_", "trans-prob-", state_prev, time_unit)
  pdf(paste0(results_path, filename, ".pdf"), width = 8, height = 10)
  print(plot)
  dev.off()
}

#--------------------------------------- Functions for trend metrics ------------------------------------------------

# Plot the trend metrics for Berkeley and Winslow. 
plot_metric_trend_BW = function(station) {
  df_intensity = make_df_stations(type = 1, 3)
  df_spell = make_df_stations(type = 1, 6)
  df_intensity = df_intensity[grep(station, df_intensity$pl_name), ]
  df_spell = df_spell[grep(station, df_spell$pl_name), ]
  
  plot_season = function(season) {
    pl_intensity = ggplot(df_intensity[df_intensity$season == season, ]) + 
      geom_ribbon(aes(x = Year, ymin = CI_l, ymax = CI_u), fill = "gray82") +
      geom_line(aes(x = Year, y = Values, group = Estimator, color = Estimator)) + 
      scale_colour_manual(values = c("Simulated" = "#a90076", "Empirical" = "#0f27ba")) +
      facet_wrap(~pl_name, nrow = 4, ncol = 2, scales = "free_y") +
      theme_classic() + 
      ylab("Precipitation Intensity (cm)") +
      theme(
        strip.background = element_rect(fill = "#bbe0ed", linewidth = 0),
        strip.text = element_text(size = 10))
    
    pl_spell = ggplot(df_spell[df_spell$season == season, ]) + 
      geom_ribbon(aes(x = Year, ymin = CI_l, ymax = CI_u), fill = "gray82") +
      geom_line(aes(x = Year, y = Values, group = Estimator, color = Estimator)) + 
      scale_colour_manual(values = c("Simulated" = "#a90076", "Empirical" = "#0f27ba")) +
      facet_wrap(~pl_name, nrow = 4, ncol = 2, scales = "free_y") +
      theme_classic() + 
      ylab("Mean Dry Spell Length") +
      theme(
        strip.background = element_rect(fill = "#bbe0ed", linewidth = 0),
        strip.text = element_text(size = 10))
    return(list(pl_intensity = pl_intensity, pl_spell = pl_spell))
  }
  
  # Make facet plots
  plots_DJF = plot_season("DJF")
  plots_MAM = plot_season("MAM")
  plots_SON = plot_season("SON")
  if (station == "Winslow") {
    plots_JJA = plot_season("JJA")
    plot = (plots_DJF$pl_spell + plots_MAM$pl_spell) / 
      (plots_JJA$pl_spell + plots_SON$pl_spell) /
      (plots_DJF$pl_intensity + plots_MAM$pl_intensity) /
      (plots_JJA$pl_intensity + plots_SON$pl_intensity) + plot_layout(guides = "collect")
  } else {
    plot = (plots_DJF$pl_spell + plots_MAM$pl_spell) / 
      (plots_SON$pl_spell + plots_DJF$pl_intensity) /
      (plots_MAM$pl_intensity + plots_SON$pl_intensity) + plot_layout(guides = "collect")
  }
  
  filename = paste0("trend_", station)
  pdf(paste0(results_path, filename, ".pdf"), width = 8, height = 10)
  print(plot)
  dev.off()
}

# Make the trend plots for Quincy
plot_metric_trend_Q = function() {
  df_Quincy = rbind(CA_Quincy_metrics[[1]]$"Number of Wet Spells"$df_seasons,
                    CA_Quincy_metrics[[1]]$"Mean Wet Spell Intensity (cm)"$df_seasons,
                    CA_Quincy_metrics[[1]]$"Maximum 40-day Precipitation (cm)"$df_seasons)
  df_Quincy$pl_name = rep(c("num_spells", "spell_amt", "max_prcp"),
                          each = nrow(df_Quincy)/3)
  df_Quincy$pl_name = fct_inorder(df_Quincy$pl_name)
  plot = ggplot(df_Quincy) + 
    geom_ribbon(aes(x = Year, ymin = CI_l, ymax = CI_u), fill = "gray82") +
    geom_line(aes(x = Year, y = Values, group = Estimator, color = Estimator)) + 
    scale_colour_manual(values = c("Simulated" = "#a90076", "Empirical" = "#0f27ba")) +
    facet_wrap(~pl_name, nrow = 3, ncol = 1, scales = "free_y", 
               strip.position = "left", 
               labeller = as_labeller(c(spell_amt = "Mean Wet Spell Precipitation (cm)",
                                        num_spells = "Number of Wet Spells",
                                        max_prcp = "Maximum 40-day Precipitation (cm)"))) +
    theme_classic() + 
    ylab("") +
    theme(strip.background = element_blank(), strip.placement = "outside",
          panel.border = element_rect(colour = "gray48", fill=NA), 
          panel.spacing = unit(0, "lines"),
          legend.position = "none")
  
  filename = "trend_Quincy"
  pdf(paste0(results_path, filename, ".pdf"))
  print(plot)
  dev.off()
}

# Make violin plots for Berkeley and Winslow
plot_violin_season_BW = function(season, trend_station, station) {
  if (season == "DJF") {
    ind_season = 1
  } else if (season == "MAM") {
    ind_season = 2
  } else if (season == "JJA") {
    ind_season = 3
  } else {
    ind_season = 4
  }
  
  n = length(trend_station[[ind_season]]$trend_1920$sensstat_spell_lens)
  name_pl = rep(c("Mean Dry Spell Len.", "Precipitation Intensity"), each = n)
  name_pl = paste0(name_pl, " (", season, ")")
  df_1920 = data.frame(sens = c(trend_station[[ind_season]]$trend_1920$sensstat_spell_lens,
                                trend_station[[ind_season]]$trend_1920$sensstat_mean_prcp),
                       name_pl)
  df_1950 = data.frame(sens = c(trend_station[[ind_season]]$trend_1950$sensstat_spell_lens,
                                trend_station[[ind_season]]$trend_1950$sensstat_mean_prcp),
                       name_pl)
  df_1980 = data.frame(sens = c(trend_station[[ind_season]]$trend_1980$sensstat_spell_lens,
                                trend_station[[ind_season]]$trend_1980$sensstat_mean_prcp),
                       name_pl)
  df = rbind(df_1920, df_1950, df_1980)
  df$Period = rep(c("1920 - present", "1950 - present", "1980 - present"), each = nrow(df_1920))
  
  
  pvals_1920 = data.frame(pvals = c(trend_station[[ind_season]]$trend_1920$pval_spell_lens,
                                    trend_station[[ind_season]]$trend_1920$pval_mean_prcp))
  pvals_1950 = data.frame(pvals = c(trend_station[[ind_season]]$trend_1950$pval_spell_lens,
                                    trend_station[[ind_season]]$trend_1950$pval_mean_prcp))
  pvals_1980 = data.frame(pvals = c(trend_station[[ind_season]]$trend_1980$pval_spell_lens,
                                    trend_station[[ind_season]]$trend_1980$pval_mean_prcp))
  df_pvals = rbind(pvals_1920, pvals_1950, pvals_1980)
  df_pvals$Period = rep(c("1920 - present", "1950 - present", "1980 - present"), each = 2)
  df_pvals$name_pl = rep(c("Mean Dry Spell Len.", "Precipitation Intensity"), 3)
  df_pvals$name_pl = paste0(df_pvals$name_pl, " (", season, ")")
  df_pvals$pvals = paste0("p = ", round(df_pvals$pvals, 4))
  
  if (station == "CA_Berkeley") {
    lim = -.3
    size_txt = 3
  } else {
    lim = -.35
    size_txt = 2.5
  }
  pl_spell = ggplot(df[grep("Spell", df$name_pl), ]) + aes(x = Period, y = sens, fill = Period) +
    geom_violin(colour = "gray35", alpha = .55) +
    geom_text(data = df_pvals[grep("Spell", df_pvals$name_pl), ], aes(Period, y =  lim, label = pvals), size = size_txt, color = palette$blue) +
    facet_wrap(~name_pl, scales = "free_y", nrow = 1, ncol = 2) +
    guides(fill="none") +
    ylab("Sen's Slope") +
    scale_fill_manual(values = c("1920 - present" = palette$purple,
                                 "1950 - present" = palette$red,
                                 "1980 - present" = palette$green)) +
    theme_classic() +
    theme(strip.background = element_rect(fill = "#bbe0ed", linewidth = 0),
          strip.text = element_text(size = 10),
          axis.text.x = element_text(size = 8))
  
  pl_mean = ggplot(df[grep("Intensity", df$name_pl), ]) + aes(x = Period, y = sens, fill = Period) +
    geom_violin(colour = "gray35", alpha = .55) +
    geom_text(data = df_pvals[grep("Intensity", df_pvals$name_pl), ], aes(Period, y =  -.02, label = pvals), size = size_txt, color = palette$blue) +
    facet_wrap(~name_pl, scales = "free_y", nrow = 1, ncol = 2) +
    guides(fill="none") +
    ylab("Sen's Slope") +
    scale_fill_manual(values = c("1920 - present" = palette$purple,
                                 "1950 - present" = palette$red,
                                 "1980 - present" = palette$green)) +
    theme_classic() +
    theme(strip.background = element_rect(fill = "#bbe0ed", linewidth = 0),
          strip.text = element_text(size = 10),
          axis.text.x = element_text(size = 8))
  
  return(list(pl_spell = pl_spell, pl_mean = pl_mean))
}

# Make violin plots for Winslow
plot_violin_Q = function() {
  trend_station = CA_Quincy_trend
  n = length(trend_station$Wet$trend_1920$sensstat_num_spells)
  name_pl = rep(c("Number of Wet Spells", "Mean Wet Spell Precipitation", "Maximum 40-day Precipitation"), each = n)
  df_1920 = data.frame(sens = c(trend_station$Wet$trend_1920$sensstat_num_spells,
                                trend_station$Wet$trend_1920$sensstat_spell_amt,
                                trend_station$Wet$trend_1920$sensstat_prcp_40),
                       name_pl)
  df_1950 = data.frame(sens = c(trend_station$Wet$trend_1950$sensstat_num_spells,
                                trend_station$Wet$trend_1950$sensstat_spell_amt,
                                trend_station$Wet$trend_1950$sensstat_prcp_40),
                       name_pl)
  df_1980 = data.frame(sens = c(trend_station$Wet$trend_1980$sensstat_num_spells,
                                trend_station$Wet$trend_1980$sensstat_spell_amt,
                                trend_station$Wet$trend_1980$sensstat_prcp_40),
                       name_pl)
  df = rbind(df_1920, df_1950, df_1980)
  df$Period = rep(c("1920 - present", "1950 - present", "1980 - present"), each = nrow(df_1920))
  
  
  pvals_1920 = data.frame(pvals = c(trend_station$Wet$trend_1920$pval_num_spells,
                                    trend_station$Wet$trend_1920$pval_spell_amt, 
                                    trend_station$Wet$trend_1920$pval_prcp_40))
  pvals_1950 = data.frame(pvals = c(trend_station$Wet$trend_1950$pval_num_spells,
                                    trend_station$Wet$trend_1950$pval_spell_amt, 
                                    trend_station$Wet$trend_1950$pval_prcp_40))
  pvals_1980 = data.frame(pvals = c(trend_station$Wet$trend_1980$pval_num_spells,
                                    trend_station$Wet$trend_1980$pval_spell_amt,
                                    trend_station$Wet$trend_1980$pval_prcp_40))
  df_pvals = rbind(pvals_1920, pvals_1950, pvals_1980)
  df_pvals$Period = rep(c("1920 - present", "1950 - present", "1980 - present"), each = 3)
  df_pvals$name_pl = rep(c("Number of Wet Spells", "Mean Wet Spell Precipitation", "Maximum 40-day Precipitation"), 3)
  df_pvals$pvals = paste0("p = ", round(df_pvals$pvals, 4))
  
  pl_num_spells = ggplot(df[grep("Num", df$name_pl), ]) + aes(x = Period, y = sens, fill = Period) +
    geom_violin(colour = "gray35", alpha = .55) +
    geom_text(data = df_pvals[grep("Num", df_pvals$name_pl), ], aes(Period, y =  -.18, label = pvals), size = 3, color = palette$blue) +
    facet_wrap(~name_pl, scales = "free_y", nrow = 1, ncol = 2) +
    guides(fill="none") +
    ylab("Sen's Slope") +
    scale_fill_manual(values = c("1920 - present" = palette$purple,
                                 "1950 - present" = palette$red,
                                 "1980 - present" = palette$green)) +
    theme_classic() +
    theme(strip.background = element_rect(fill = "#bbe0ed", linewidth = 0),
          strip.text = element_text(size = 10),
          axis.text.x = element_text(size = 10))
  
  pl_mean = ggplot(df[grep("Mean", df$name_pl), ]) + aes(x = Period, y = sens, fill = Period) +
    geom_violin(colour = "gray35", alpha = .55) +
    geom_text(data = df_pvals[grep("Mean", df_pvals$name_pl), ], aes(Period, y =  -.058, label = pvals), size = 3, color = palette$blue) +
    facet_wrap(~name_pl, scales = "free_y", nrow = 1, ncol = 2) +
    guides(fill="none") +
    ylab("Sen's Slope") + 
    scale_fill_manual(values = c("1920 - present" = palette$purple,
                                 "1950 - present" = palette$red,
                                 "1980 - present" = palette$green)) +
    theme_classic() +
    theme(strip.background = element_rect(fill = "#bbe0ed", linewidth = 0),
          strip.text = element_text(size = 10),
          axis.text.x = element_text(size = 10))
  
  pl_max = ggplot(df[grep("Max", df$name_pl), ]) + aes(x = Period, y = sens, fill = Period) +
    geom_violin(colour = "gray35", alpha = .55) +
    geom_text(data = df_pvals[grep("Max", df_pvals$name_pl), ], aes(Period, y =  -.63, label = pvals), size = 3, color = palette$blue) +
    facet_wrap(~name_pl, scales = "free_y", nrow = 1, ncol = 2) +
    guides(fill="none") +
    ylab("Sen's Slope") +
    scale_fill_manual(values = c("1920 - present" = palette$purple,
                                 "1950 - present" = palette$red,
                                 "1980 - present" = palette$green)) +
    theme_classic() +
    theme(strip.background = element_rect(fill = "#bbe0ed", linewidth = 0),
          strip.text = element_text(size = 10),
          axis.text.x = element_text(size = 10))
  
  pl = pl_num_spells / pl_mean / pl_max
  return(pl)
}

#------------------------------------------ General functions ------------------------------------------------------
# Save a plot to PDF file
pdfsave = function(filename, plot) {
  pdf(paste0(filename, ".pdf"), onefile = F)#, paper = "a4r", width = 20)
  print(plot)
  dev.off()
}

# Generate a color palette based on the requested number of colors and type 
get_cat_palette = function(num_colors, prcp) {
  if (num_colors == 3 & prcp == T) {
    palette = list(blue = "#0f27ba", red = "#FF6A7b", green = "#39ae86")
  } else if (num_colors == 3 & prcp == F) {
    palette = list(purple = "#a073e3", red = "#FF6A7b", green = "#39ae86")
  } else if (num_colors == 5 & prcp == T) {
    palette = list(blue = "#0f27ba", purple = "#a073e3", red = "#FF6A7b", green = "#39ae86",
                   tan = "#F9A14D")
  } else if (num_colors == 10 & prcp == F) {
    palette = list(red = "#c4213d", orange = "#f57733", yellow = "#f7a943", 
                   yellowgreen = "#dbe03d",lightgreen = "#47cc69", darkgreen = "#2a8265", cyan = "#00ebeb",
                   periwinkle = "#2987e6", blue = "#0f25b8", purple = "#a073e3", pink = "#db53d5")
  }
  return(palette)
}
