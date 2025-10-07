# Obtain and save the metrics for a particular station.
get_metrics_station = function(station_name) {
  # Read in original data
  truth = read.csv(paste0(data_path, station_name, ".csv"))
  
  # Read in files from model runs
  files = list.files(output_path, pattern = station_name)
  files_train = files[grep("train", files)]
  files_loss = files[grep("loss_", files)]
  files_gen = files[grep("gen_", files)]
  
  # Read in the training files 
  train = lapply(files_train, function(x) read.csv(paste0(output_path, x)))
  # Obtain alpha1, beta1, alpha2, beta2, mu1, mu2, var1, var2, and loss (on alpha, beta scale)
  train = lapply(train, function(dat) get_values(dat))
  
  # Subset truth according to the values actually used for training
  inds = train[[1]]$inds
  truth = truth[inds, ]
  n = nrow(truth)
  
  # Read in the loss file and all the generation files.
  loss = lapply(files_loss, function(x) read.csv(paste0(output_path, x)))
  gen = lapply(files_gen, function(x) data.frame(fread(paste0(output_path, x))))
  
  # Separate the generation files into the initial value and bootstrap replicates
  files_gen_boot = files_gen[grep("v1_m", files_gen)]
  files_gen_init = files_gen[grep("b1_v", files_gen)]
  files_gen_init = files_gen_init[!(files_gen_init %in% files_gen_boot)]
  names_boot = str_extract(files_gen_boot, "b[0-9]+_v[0-9]+_m[0-9]+")
  names_init = str_extract(files_gen_init, "b[0-9]+_v[0-9]+_m[0-9]+")
  gen_boot = gen[files_gen %in% files_gen_boot]
  gen_init = gen[files_gen %in% files_gen_init]
  names(gen_boot) = names_boot
  names(gen_init) = names_init
  
  # Check the files 
  check_successful_running(train, loss, gen_boot, gen_init)
  
  #-------------------------- Obtaining metrics  ---------------------------
  # Remove last several rows of generated values in order to make it the same length as the ground truth dataset. 
  # (They are different lengths, because for a batch_size of 32, 13 observations were trimmed off the end
  # of the training set, to make the number of its rows divisible by 32).
  gen_init = lapply(gen_init, function(x) x[1:n, ])
  gen_boot = lapply(gen_boot, function(x) x[1:n, ])
  metrics = call_metrics(station_name, gen_init, gen_boot, truth, units = "cm", .3)
  save(metrics, file = paste0(results_path, station_name, "_", "metrics", ".RData"))
}

# Obtain metrics for a given station based on the generated samples.
call_metrics = function(station_name, gen_init, gen_boot, truth, units = "cm", cutoff) { # name_rdata
  # Extract the generated values and convert any values to cm, if needed
  if (units == "in") {
    gens_init = lapply(gen_init, function(x) x$gen/2.54) # if desired units are inches, then divide the generated values (which are in cm) by 2.54.
    names(gens_init) = names(gen_init)
    gens_boot = lapply(gen_boot, function(x) x$gen/2.54)
    names(gens_boot) = names(gen_boot)
    true_y = truth$prcp/2.54
  } else {
    gens_init = lapply(gen_init, function(x) x$gen)
    names(gens_init) = names(gen_init)
    gens_boot = lapply(gen_boot, function(x) x$gen)
    names(gens_boot) = names(gen_boot)
    true_y = truth$prcp
  }
  
  # Convert the days with missing values BACK to NA (when training the LSTM, they were converted to 0s)
  true_y[truth$missing == 1] = NA
  
  # Each metricnostic will be calculated by season. 
  if (station_name == "CA_Quincy") {
    seasons1 = truth$season
    inds_season_w = which(truth$raw_month %in% c(11:12, 1:4))
    seasons1[inds_season_w] = "Wet" # Use the wet season for the area, November - April
    seasons = "Wet"
    truth$season = seasons1
    
    years = unique(truth$m_year)
    day_of_season = truth$day_of_year
    day_of_season[which(truth$day_of_year %in% 306:366)] = day_of_season[which(truth$day_of_year %in% 306:366)] - 305 
    day_of_season[which(truth$day_of_year %in% 1:121)] = day_of_season[which(truth$day_of_year %in% 1:121)] + 61 
    truth$day_of_season = day_of_season
  } else {
    seasons = unique(truth$season)
  }
  
  # Obtain the metrics for the empirical values and the generated values 
  metrics = lapply(seasons, function (season) process_metrics(station_name, true_y, gens_init, gens_boot, season, cutoff, truth))
  names(metrics) = seasons
  
  # Grab trend information for each season
  trend = lapply(metrics, function(x) x$trend)
  names(trend) = seasons
  
  # Plot each individual metricnostic per season 
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
  inds_daily = c(T, T,
                 F, F, F,
                 F, F, F,
                 T, T, T, 
                 F, F, F,
                 rep(T, 9),
                 rep(F, 9))
  if (station_name == "CA_Quincy") {
    names_metrics1 = c(names_metrics1, "Number of Wet Spells", "Maximum 40-day Precipitation (cm)")
    varnames_metrics1 = c(varnames_metrics1, "num-w-spells-yearly", "max-40day-yearly")
    inds_daily = c(inds_daily, F, F)
  }
  num_metrics1 = length(names_metrics1)
  
  plots1 = lapply(1:num_metrics1, function(i) plot_metric1(station_name, metrics, i, inds_daily[i], names_metrics1[i], truth))
  names(plots1) = names_metrics1
  lapply(1:num_metrics1, function(i) pdfsave(paste0(results_path, station_name, "_", varnames_metrics1[i]), plots1[[i]]$pl_arr))
  
  # Plot runs for Winslow as an example. 
  #lapply(1:6, function(i) plot_runs(station_name, metrics, i, inds_daily[i], names_metrics1[i], varnames_metrics1[i], truth))
  #plot_runs(station_name, metrics, 1, inds_daily[1], names_metrics1[1], varnames_metrics1[1], truth)
  # Grab variance components for prcp. intensity by day metric for SON, for day 79 which is November 18th
  # ind_stat = 79
  # metrics$SON$metrics_se[[1]][ind_stat]
  # metrics$SON$metrics_sigmahat_boot[[1]][ind_stat]
  # metrics$SON$metrics_sigmahat_init[[1]][ind_stat]
  # metrics$SON$metrics_sigmahat_err[[1]][ind_stat]
  
  names_metrics2 = c("Mean Dry Spell Length", "Mean Wet Spell Length", "1-day Precipitation (cm)", "3-day Precipitation (cm)", "10-day Precipitation (cm)")
  varnames_metrics2 = c("mean-d-spell-len-qq", "mean-w-spell-len-qq", "1-day-total-qq", "3-day-total-qq", "10-day-total-qq")
  num_metrics2 = length(names_metrics2)
  plots2 =  lapply(1:num_metrics2, function(i) plot_metric2(station_name, metrics, i, names_metrics2[i], truth))
  names(plots2) = names_metrics2
  lapply(1:num_metrics2, function(i) pdfsave(paste0(results_path, station_name, "_", varnames_metrics2[i]), plots2[[i]]$pl_arr))
  
  return(list(plots1, plots2, trend))
}


# This is a function that obtains alpha1, beta1, alpha2, beta2 (and the loss on that scale) for interpretation purposes. 
# It also rearranges the columns (if needed) in the outputs so that the first Gamma component is the "larger" one. 
get_values = function(dat) {
  inds_NA = which(dat$loss == 0)
  mu1 = dat$mu1
  mu2 = dat$mu2
  var1 = dat$var1
  var2 = dat$var2
  alpha1 = dat$mu1^2/dat$var1
  beta1 = dat$mu1/dat$var1
  alpha2 = dat$mu2^2/dat$var2
  beta2 = dat$mu2/dat$var2
  dat = cbind(dat, alpha1, beta1, alpha2, beta2)
  loss2 = unlist(lapply(1:nrow(dat), function(t) get_loss(dat$y_true[t], dat$p1[t], dat$p2[t], dat$alpha1[t], dat$alpha2[t], dat$beta1[t], dat$beta2[t])))
  loss2[inds_NA] = 0
  dat$loss = loss2
  # Rearrange the columns (if needed) so that the first Gamma component is the "larger" one
  if (median(mu1) < median(mu2)) {
    alpha1_start = dat$alpha1
    alpha2_start = dat$alpha2
    beta1_start = dat$beta1
    beta2_start = dat$beta2
    p2_start = dat$p2
    mu1_start = mu1
    mu2_start = mu2
    var1_start = var1
    var2_start = var2
    dat$alpha1 = alpha2_start
    dat$alpha2 = alpha1_start
    dat$beta1 = beta2_start
    dat$beta2 = beta1_start
    dat$p2 = 1 - p2_start
    dat$mu1 = mu2_start
    dat$mu2 = mu1_start
    dat$var1 = var2_start
    dat$var2 = var1_start
  }
  return(dat)
}


# Computes the loss on the alpha, beta scale
get_loss = function(y, p1, p2, alpha1, alpha2, beta1, beta2) {
  if (y == 0) {
    loss = -log(p1)
  } else {
    loss = -log((1 - p1) * p2 * beta1^alpha1 * y^(alpha1 - 1) * exp(-beta1 * y)/gamma(alpha1) + 
                  (1 - p1) * (1 - p2) * beta2^alpha2 * y^(alpha2 - 1) * exp(-beta2 * y)/gamma(alpha2))
  }
  
  return(loss)
}

# For a dataframe, check if all values in it are valid/finite (not NA, NaN, Inf, or -Inf)
check_valid = function(df) {
  finite = all(apply(df, 1, function(x) all(is.finite(x))))
  return(finite) # returns T if all are valid (finite)
}

# Check that all the files are there. 
check_successful_running = function(train, loss, gen_boot, gen_init) {
  # Check the lengths of all the files
  lens = c(length(train), length(loss), length(gen_boot), length(gen_init))
  lens_check = all(lens %in% c(V + B, V + B, M * B, M * V))
  if (!lens_check) {
    return("One of the lengths in the files is incorrect")
  } 
  
  # Check if all the files have finite values
  finite = all(all(sapply(train, check_valid)), all(sapply(gen_init, check_valid)), 
             all(sapply(gen_boot, check_valid)), all(sapply(loss, check_valid)))
  if (!finite) {
    return("Not all of the outputted files contain finite values")
  }
  
  # Check the number of epochs from training. 
  num_epochs = sapply(loss, nrow)
  return(num_epochs)
}


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


# Obtain estimates of the variance of the estimator and the individual variance components within it,
# based on our method for combining multiple sources of uncertainty
get_uq = function(S_boot, S_init) {
  # For a specific statistic, takes the values from bootstrapping and 
  # the values from initial values as input.
  S_boot = unlist(S_boot)
  S_init = unlist(S_init)
  
  calc_mu_i.. = function(i) {
    vals = S_boot[grep(paste0("b", i, "_"), names(S_boot))]
    mean(vals, na.rm = T) # if all the values were NA, then this will return NA
  }
  
  calc_mu_.j. = function(j) {
    vals = S_init[grep(paste0("v", j, "_"), names(S_init))]
    mean(vals, na.rm = T) # if all the values were NA, then this will return NA
  }
  
  # Grab first bootstrap sample
  b_start = 1
  v_start = 1
  mu_1.. = calc_mu_i..(b_start) # initial value mean for bootstrap index b_start, b' (original data)
  mu_.1. = mu_1.. # In this case, due to lack of replication, these means are equal
  mu_... = mean(c(S_boot, S_init), na.rm = T)
  vals_j = 1:V  #initial value indices
  vals_i = 1:B  # bootstrap indices
  # Use formula for estimating the means of non-missing values:
  # mu_ij = mu_i. + mu_.j - mu..
  # Estimates:
  mu_1js = sapply(vals_j, function(j) mu_1.. + calc_mu_.j.(j) - mu_...)
  mu_is1 = sapply(vals_i, function(i) calc_mu_i..(i) + mu_.1. - mu_...)
  
  trt_means = matrix(nrow = B, ncol = V)
  trt_means[1, 2:V] = mu_1js[-1]
  trt_means[, 1] = mu_is1
  
  # Use formula for missing values:
  # mu_ij = mu_ij' + mu_i'j - mu_i'j', with i' = j' = 1
  pairs = expand.grid(vals_i[-1], vals_j[-1])
  for (r in 1:nrow(pairs)) {
    i = pairs[r, 1]
    j = pairs[r, 2]
    mu_ij = trt_means[i, 1] + trt_means[1, j] - trt_means[1, 1]
    trt_means[i, j] = mu_ij
  }
  
  mu_b..s = apply(trt_means, 1, mean)
  mu_.v.s = apply(trt_means, 2, mean)
  SS_boot = V * M * sum((mu_b..s - mu_...)^2)
  SS_init = M * sum(unlist(lapply(1:B, function(b) (trt_means[b, ] - mu_b..s[b])^2 )))
  # For SS_err, use what would be the interaction sum-of-squares, SS_{boot * init}
  SS_err = M * sum(unlist(lapply(1:B, function(b) lapply(1:V, function(v) trt_means[b, v] - mu_b..s[b] - mu_.v.s[v] + mu_...)))^2)
  MS_boot = SS_boot/(B - 1)
  MS_init = SS_init/(B * (V - 1))
  MS_err = SS_err/(B * V * (M - 1))
  sigmahat_boot = sqrt((MS_boot - MS_init)/(V*M))
  sigmahat_init = sqrt((MS_init - MS_err)/M)
  sigmahat_err = sqrt(MS_err)
  
  est_var = sum((mu_b..s - mu_...)^2)/(B - 1) 
  est_se = sqrt(est_var)
  
  return(list(est_se = est_se, sigmahat_boot = sigmahat_boot, sigmahat_init = sigmahat_init, sigmahat_err = sigmahat_err))
}

# Calculate the Sen's slope for a given time series, vec
calc_sens_slope = function(vec) {
  if (anyNA(vec)) {
    vec = vec[-which(is.na(vec))]
  }
  if (length(vec) < 2) {
    return(NA)
  } else {
    sens_slope = sens.slope(vec)$estimates
    return(sens_slope)
  }
}

# Calculate the Mann-Kendall statistic for test of trend, with a continuity correction, 
# based on a time series vec.
calc_mk_stat = function(vec) {
  if (anyNA(vec)) {
    vec = vec[-which(is.na(vec))]
  }
  if (length(vec) < 3) {
    return(NA)
  } else {
    mk_stat = mkttest(vec)[[3]]
    # Apply a continuity correction to the statistic.
    if (mk_stat > 0) {
      stat = mk_stat - 1
    } else if (mk_stat < 0) {
      stat = mk_stat + 1
    } else {
      stat = mk_stat
    }
    return(stat)
  }
}


# Obtain metrics for the empirical time series and generated samples
process_metrics = function(station_name, true_y, gens_init, gens_boot, season, cutoff, truth) {
  # Metrics for the empirical prcp time series
  metrics_emp = calculate_metrics(station_name, true_y, season, cutoff, truth, T)
  metrics_emp1 = metrics_emp$type1
  metrics_emp2 = metrics_emp$type2
  metrics_emp1 = lapply(metrics_emp1, remove_invalid) # Remove invalid values, e.g., if there were no days with > 1 cm of prcp in JJA, 1981
  num_invalid_emp = lapply(metrics_emp1, count_invalid)
  num_metrics1 = length(metrics_emp1)
  num_metrics2 = length(metrics_emp2)
  
  # Metrics for each of the synthetic time series (initial value replicates)
  metrics_init = lapply(gens_init, function(x) calculate_metrics(station_name, x, season, cutoff, truth, F))
  metrics_init1 = lapply(metrics_init, function(x) x$type1)
  metrics_init2 = lapply(metrics_init, function(x) x$type2)
  
  # Process metrics of type 1 (ones that will be in line plots) for initial value replicates
  metrics_init1 = lapply(metrics_init1, function(x) lapply(x, remove_invalid))
  num_invalid_init = lapply(metrics_init1, function(x) lapply(x, count_invalid))
  metrics_init_df1 = lapply(1:num_metrics1, function(i) {
    df = sapply(metrics_init1, "[[", i) %>% cbind.data.frame()
    names(df) = names(gens_init)  
    return(df)
  }) 
  metrics_init_means = lapply(metrics_init_df1, function(metric_df) apply(metric_df, 1, function(r) mean(r, na.rm = T))) #NAs only removed from invalid values from calculatin of metrics (NOT NAs in the generated ts)
  names(metrics_init_df1) = names(metrics_emp1)
  names(metrics_init_means) = names(metrics_emp1) 
  
  # Process metrics of type 2 (ones that will be in qqplots) for initial value replicates
  metrics_init_df2 = lapply(1:num_metrics2, function(i) {
    df = sapply(metrics_init2, "[[", i) %>% cbind.data.frame()
    names(df) = names(gens_init)  
    return(df)
  }) 
  names(metrics_init_df2) = names(metrics_emp2)
  spell_len_quantiles_med = lapply(metrics_init_df2, function(metric_df) apply(metric_df, 1, function(r) median(r)))
  spell_len_quantiles_qu = lapply(metrics_init_df2, function(metric_df) apply(metric_df, 1, function(r) quantile(r, .975)))
  spell_len_quantiles_ql = lapply(metrics_init_df2, function(metric_df) apply(metric_df, 1, function(r) quantile(r, .025)))
  
  # Process metrics for bootstrap replicates
  metrics_boot = lapply(gens_boot, function(x) calculate_metrics(station_name, x, season, cutoff, truth, F))
  metrics_boot1 = lapply(metrics_boot, function(x) x$type1)
  metrics_boot2 = lapply(metrics_boot, function(x) x$type2) # Currently we don't use type 2 metrics for bootstrapping
  # Process metrics of type 1
  metrics_boot1 = lapply(metrics_boot1, function(x) lapply(x, remove_invalid))
  num_invalid_boot = lapply(metrics_boot1, function(x) lapply(x, count_invalid))
  metrics_boot_df1 = lapply(1:num_metrics1, function(i) {
    df = sapply(metrics_boot1, "[[", i) %>% cbind.data.frame()
    names(df) = names(gens_boot) 
    return(df)
  }) 
  metrics_boot_means = lapply(metrics_boot_df1, function(metric_df) apply(metric_df, 1, function(r) mean(r, na.rm = T))) #NAs only removed from invalid values from calculatin of metrics (NOT NAs in the generated ts)
  names(metrics_boot_df1) = names(metrics_emp1)
  names(metrics_boot_means) = names(metrics_emp1)
  
  names_boot = names(metrics_boot_df1[[1]])
  names_init = names(metrics_init_df1[[1]])
  
  # Obtain estimates of the variance and variance components based on our method for combining
  # multiple sources of uncertainty.
  metrics_uq = lapply(1:num_metrics1, function(d) sapply(
    1:nrow(metrics_boot_df1[[d]]), function(r) get_uq(metrics_boot_df1[[d]][r, ], metrics_init_df1[[d]][r, ])))
  names(metrics_uq) = metrics_emp1
  metrics_se = lapply(1:num_metrics1, function(d) unlist(metrics_uq[[d]][1, ]))
  metrics_sigmahat_boot = lapply(1:num_metrics1, function(d) unlist(metrics_uq[[d]][2, ]))
  metrics_sigmahat_init = lapply(1:num_metrics1, function(d) unlist(metrics_uq[[d]][3, ]))
  metrics_sigmahat_err = lapply(1:num_metrics1, function(d) unlist(metrics_uq[[d]][4, ]))
  
  # Get trend information
  years = unique(truth$m_year)
  # Calculate Sen's slope for a given period with year "start"
  get_trend = function(start) {
    len = length(years)
    inds = which(years == start):len
    
    if (station_name != "CA_Quincy") {
      # Obtain estimators of Sen's slope for the mean dry spell length and prcp. intensity metrics for each simulated ts
      sensstat_spell_lens = apply(metrics_init_df1$mean_spell_lens_D[inds, ], 2, calc_sens_slope)
      sensstat_mean_prcp = apply(metrics_init_df1$means[inds, ], 2, calc_sens_slope)
      
      # Obtain Mann-Kendall S statistic for each metric and simulated ts.
      mkstat_spell_lens = apply(metrics_init_df1$mean_spell_lens_D[inds, ], 2, calc_mk_stat)
      mkstat_mean_prcp = apply(metrics_init_df1$means[inds, ], 2, calc_mk_stat)
      
      # Obtain Mann-Kendall S statistic for each metric and simulated bootstrapped ts.
      mkstat_spell_lens_boot = apply(metrics_boot_df1$mean_spell_lens_D[inds, ], 2, calc_mk_stat)
      mkstat_mean_prcp_boot = apply(metrics_boot_df1$means[inds, ], 2, calc_mk_stat)
      
      # Average of the statistics
      meanstat_spell_lens = mean(mkstat_spell_lens, na.rm = T)
      meanstat_mean_prcp = mean(mkstat_mean_prcp, na.rm = T)
      
      # Compute standard errors
      uq_spell_lens = get_uq(mkstat_spell_lens_boot, mkstat_spell_lens)
      uq_mean_prcp = get_uq(mkstat_mean_prcp_boot, mkstat_mean_prcp)
      
      # Calculate test statistics
      stat_spell_lens = abs(meanstat_spell_lens)/uq_spell_lens$est_se
      stat_mean_prcp = abs(meanstat_mean_prcp)/uq_mean_prcp$est_se
      
      # Calculate p-values
      pval_spell_lens = 2 * pnorm(-stat_spell_lens)
      pval_mean_prcp = 2 * pnorm(-stat_mean_prcp)
      
      return(list(sensstat_spell_lens = sensstat_spell_lens, sensstat_mean_prcp = sensstat_mean_prcp,
                  pval_spell_lens = pval_spell_lens, pval_mean_prcp = pval_mean_prcp))
    } else {
      # Obtain estimators of Sen's slope for the number of wet spells, the mean wet spell prcp., and max 40-day prcp
      sensstat_num_spells = apply(metrics_init_df1$num_spells[inds, ], 2, calc_sens_slope)
      sensstat_spell_amt = apply(metrics_init_df1$mean_spell_amt[inds, ], 2, calc_sens_slope)
      sensstat_prcp_40 = apply(metrics_init_df1$prcp_40[inds, ], 2, calc_sens_slope)
      
      # Obtain Mann-Kendall S statistic for each metric and simulated ts.
      mkstat_num_spells = apply(metrics_init_df1$num_spells[inds, ], 2, calc_mk_stat)
      mkstat_spell_amt = apply(metrics_init_df1$mean_spell_amt[inds, ], 2, calc_mk_stat)
      mkstat_prcp_40 = apply(metrics_init_df1$prcp_40[inds, ], 2, calc_mk_stat)
      
      # Obtain Mann-Kendall S statistic for each metric and simulated bootstrapped ts.
      mkstat_num_spells_boot = apply(metrics_boot_df1$num_spells[inds, ], 2, calc_mk_stat)
      mkstat_spell_amt_boot = apply(metrics_boot_df1$mean_spell_amt[inds, ], 2, calc_mk_stat)
      mkstat_prcp_40_boot = apply(metrics_boot_df1$prcp_40[inds, ], 2, calc_mk_stat)
      
      # Average of the statistics
      meanstat_num_spells = mean(mkstat_num_spells, na.rm = T)
      meanstat_spell_amt = mean(mkstat_spell_amt, na.rm = T)
      meanstat_prcp_40 = mean(mkstat_prcp_40, na.rm = T)
      
      # Compute standard errors
      uq_num_spells = get_uq(mkstat_num_spells_boot, mkstat_num_spells)
      uq_spell_amt = get_uq(mkstat_spell_amt_boot, mkstat_spell_amt)
      uq_prcp_40 = get_uq(mkstat_prcp_40_boot, mkstat_prcp_40)
      
      # Calculate test statistics
      stat_num_spells = abs(meanstat_num_spells)/uq_num_spells$est_se
      stat_spell_amt = abs(meanstat_spell_amt)/uq_spell_amt$est_se
      stat_prcp_40 = abs(meanstat_prcp_40)/uq_prcp_40$est_se
      
      # Calculate p-values
      pval_num_spells = 2 * pnorm(-stat_num_spells)
      pval_spell_amt = 2 * pnorm(-stat_spell_amt)
      pval_prcp_40 = 2 * pnorm(-stat_prcp_40)
      
      return(list(sensstat_num_spells = sensstat_num_spells, sensstat_spell_amt = sensstat_spell_amt, sensstat_prcp_40 = sensstat_prcp_40,
                  pval_num_spells = pval_num_spells, pval_spell_amt = pval_spell_amt, pval_prcp_40 = pval_prcp_40))
    }
  }
  
  # Obtain trend information for each time period under consideration
  trend_1920 = get_trend(1920) # 1920 - present
  trend_1950 = get_trend(1950) # 1950 - present
  trend_1980 = get_trend(1980) # 1980 - present
  
  trend = list(trend_1920 = trend_1920, trend_1950 = trend_1950, trend_1980 = trend_1980)  
  
  return(list(metrics_emp1 = metrics_emp1, metrics_init_means = metrics_init_means, metrics_boot_means = metrics_boot_means, 
              metrics_se = metrics_se, metrics_sigmahat_boot = metrics_sigmahat_boot, metrics_sigmahat_init = metrics_sigmahat_init, metrics_sigmahat_err = metrics_sigmahat_err,
              metrics_init_df = metrics_init_df1, metrics_boot_df = metrics_boot_df1, 
              metrics_emp2 = metrics_emp2, metrics_init_meds = spell_len_quantiles_med, 
              metrics_ql = spell_len_quantiles_ql, metrics_qu = spell_len_quantiles_qu,
              trend = trend))
}


# Calculate the metrics for a given series (whether simulated or empirical)
calculate_metrics = function(station_name, series, season, cutoff, truth, emp = F) {
  if (!is.null(season)) {
    inds = which(truth$season == season)
  } else {
    # If no season is provided as a function argument, then use all of the data
    inds = 1:nrow(truth)
  }
  season_truth = truth[inds, ]
  season_ts = series[inds]
  days = sort(unique(season_truth$day_of_season))
  years = sort(unique(season_truth$m_year))
  
  # Get daily prcp probability and intensity, by day of the season (i.e., aggregation is over the years)
  means_daily = sapply(days, function(d) est_mean(season_ts[which(season_truth$day_of_season == d)], cutoff, emp))
  probs_daily = sapply(days, function(d) est_p(season_ts[which(season_truth$day_of_season == d)], cutoff, emp))
  
  # Get daily prcp probability, intensity, and total, by year (aggregation is over all the days of the season)
  means = sapply(years, function(year) est_mean(season_ts[which(season_truth$m_year == year)], cutoff, emp))
  probs = sapply(years, function(year) est_p(season_ts[which(season_truth$m_year == year)], cutoff, emp))
  sums = sapply(years, function(year) est_sum(season_ts[which(season_truth$m_year == year)], emp))
  
  # Get dry and wet spell statistics, by year (aggregation is over all the days of the season)
  spell_stats_D =  lapply(years, function(year) calc_spell_stats_year(station_name, season_ts, season_truth, year, dry = T, cutoff))
  spell_stats_W =  lapply(years, function(year) calc_spell_stats_year(station_name, season_ts, season_truth, year, dry = F, cutoff))
  mean_spell_lens_D = sapply(spell_stats_D, function(x) x$mean_spell_len)
  mean_spell_lens_W = sapply(spell_stats_W, function(x) x$mean_spell_len)
  mean_spell_amt_W = sapply(spell_stats_W, function(x) x$mean_spell_amt)
  if (station_name == "CA_Quincy") {
    num_spells_W = sapply(spell_stats_W, function(x) x$num_spells)
  }
  
  # Spell length quantiles
  spell_len_quantiles_D = calc_spell_len_quantiles(season_ts, season_truth, years, dry = T, cutoff)
  spell_len_quantiles_W = calc_spell_len_quantiles(season_ts, season_truth, years, dry = F, cutoff)
  
  # Get proportion of prcp values exceeding .3, 1, 2 cm, by day of the season (1 - ecdf)
  cdf_.3_daily = sapply(days, function(d) est_cdf(season_ts[which(season_truth$day_of_season == d)], .3, emp))
  cdf_1_daily = sapply(days, function(d) est_cdf(season_ts[which(season_truth$day_of_season == d)], 1, emp))
  cdf_2_daily = sapply(days, function(d) est_cdf(season_ts[which(season_truth$day_of_season == d)], 2, emp))
  
  # Get proportion of prcp values exceeding .3, 1, 2 cm, by year 
  cdf_.3 = sapply(years, function(year) est_cdf(season_ts[which(season_truth$m_year == year)], .3, emp))
  cdf_1 = sapply(years, function(year) est_cdf(season_ts[which(season_truth$m_year == year)], 1, emp))
  cdf_2 = sapply(years, function(year) est_cdf(season_ts[which(season_truth$m_year == year)], 2, emp))
  
  # Get transition probabilities between dry (0-.3 cm), moist (.3-1 cm), and wet (> 1 cm) days, by day of season
  dry = c(0, .3)
  moist = c(.30000001, 1)
  wet = c(1.00000001, Inf)
  prob_DD_daily = sapply(days, function(d) est_transition(season_ts[which(season_truth$day_of_season == d)], dry, dry))
  prob_DM_daily = sapply(days, function(d) est_transition(season_ts[which(season_truth$day_of_season == d)], dry, moist))
  prob_DW_daily = sapply(days, function(d) est_transition(season_ts[which(season_truth$day_of_season == d)], dry, wet))
  prob_MD_daily = sapply(days, function(d) est_transition(season_ts[which(season_truth$day_of_season == d)], moist, dry))
  prob_MM_daily = sapply(days, function(d) est_transition(season_ts[which(season_truth$day_of_season == d)], moist, moist))
  prob_MW_daily = sapply(days, function(d) est_transition(season_ts[which(season_truth$day_of_season == d)], moist, wet))
  prob_WD_daily = sapply(days, function(d) est_transition(season_ts[which(season_truth$day_of_season == d)], wet, dry))
  prob_WM_daily = sapply(days, function(d) est_transition(season_ts[which(season_truth$day_of_season == d)], wet, moist))
  prob_WW_daily = sapply(days, function(d) est_transition(season_ts[which(season_truth$day_of_season == d)], wet, wet))
  # NOTE: prob_DD, prob_DM, prob_DW, e.g., may not sum to 1 for the empirical ts, if there are missing values
  
  # Calculate by year
  prob_DD = sapply(years, function(year) est_transition(season_ts[which(season_truth$m_year == year)], dry, dry))
  prob_DM = sapply(years, function(year) est_transition(season_ts[which(season_truth$m_year == year)], dry, moist))
  prob_DW = sapply(years, function(year) est_transition(season_ts[which(season_truth$m_year == year)], dry, wet))
  prob_MD = sapply(years, function(year) est_transition(season_ts[which(season_truth$m_year == year)], moist, dry))
  prob_MM = sapply(years, function(year) est_transition(season_ts[which(season_truth$m_year == year)], moist, moist))
  prob_MW = sapply(years, function(year) est_transition(season_ts[which(season_truth$m_year == year)], moist, wet))
  prob_WD = sapply(years, function(year) est_transition(season_ts[which(season_truth$m_year == year)], wet, dry))
  prob_WM = sapply(years, function(year) est_transition(season_ts[which(season_truth$m_year == year)], wet, moist))
  prob_WW = sapply(years, function(year) est_transition(season_ts[which(season_truth$m_year == year)], wet, wet))
  
  # # Get total prcp for 1-day, 3-day, and 10-day blocks
  # 1 day
  prcp_1 = season_ts
  prcp_3 = calc_prcp_block(season_ts, len_block = 3, emp)
  prcp_10 = calc_prcp_block(season_ts, len_block = 10, emp)
  
  # Get quantiles for total 1-day, 3-day, and 10-day prcp
  prcp_1_quantiles = calc_prcp_block_quantiles(prcp_1, emp)
  prcp_3_quantiles = calc_prcp_block_quantiles(prcp_3, emp)
  prcp_10_quantiles = calc_prcp_block_quantiles(prcp_10, emp)
  
  if (station_name == "CA_Quincy") {
    # Get maximum 40-day prcp
    prcp_40 = sapply(years, function(year) calc_prcp_block(season_ts[which(season_truth$m_year == year)], len_block = 40, emp, calc_max = T))
  }
  
  if (station_name == "CA_Quincy") {
    output = list(
      type1 = list(means_daily = means_daily, probs_daily = probs_daily, 
                   means = means, probs = probs, sums = sums,
                   mean_spell_lens_D = mean_spell_lens_D, mean_spell_lens_W = mean_spell_lens_W, mean_spell_amt_W = mean_spell_amt_W, 
                   cdf_.3_daily = cdf_.3_daily, cdf_1_daily = cdf_1_daily, cdf_2_daily = cdf_2_daily,
                   cdf_.3 = cdf_.3, cdf_1 = cdf_1, cdf_2 = cdf_2,
                   prob_DD_daily = prob_DD_daily, prob_DM_daily = prob_DM_daily, prob_DW_daily = prob_DW_daily,
                   prob_MD_daily = prob_MD_daily, prob_MM_daily = prob_MM_daily, prob_MW_daily = prob_MW_daily,
                   prob_WD_daily = prob_WD_daily, prob_WM_daily = prob_WM_daily, prob_WW_daily = prob_WW_daily,
                   prob_DD = prob_DD, prob_DM = prob_DM, prob_DW = prob_DW,
                   prob_MD = prob_MD, prob_MM = prob_MM, prob_MW = prob_MW,
                   prob_WD = prob_WD, prob_WM = prob_WM, prob_WW = prob_WW,
                   num_spells_W = num_spells_W, prcp_40 = prcp_40),
      type2 = list(spell_len_quantiles_D = spell_len_quantiles_D, spell_len_quantiles_W = spell_len_quantiles_W,
                   prcp_1_quantiles = prcp_1_quantiles, prcp_3_quantiles = prcp_3_quantiles, prcp_10_quantiles = prcp_10_quantiles))
  } else {
    output = list(
      type1 = list(means_daily = means_daily, probs_daily = probs_daily, 
                   means = means, probs = probs, sums = sums,
                   mean_spell_lens_D = mean_spell_lens_D, mean_spell_lens_W = mean_spell_lens_W, mean_spell_amt_W = mean_spell_amt_W, 
                   cdf_.3_daily = cdf_.3_daily, cdf_1_daily = cdf_1_daily, cdf_2_daily = cdf_2_daily,
                   cdf_.3 = cdf_.3, cdf_1 = cdf_1, cdf_2 = cdf_2,
                   prob_DD_daily = prob_DD_daily, prob_DM_daily = prob_DM_daily, prob_DW_daily = prob_DW_daily,
                   prob_MD_daily = prob_MD_daily, prob_MM_daily = prob_MM_daily, prob_MW_daily = prob_MW_daily,
                   prob_WD_daily = prob_WD_daily, prob_WM_daily = prob_WM_daily, prob_WW_daily = prob_WW_daily,
                   prob_DD = prob_DD, prob_DM = prob_DM, prob_DW = prob_DW,
                   prob_MD = prob_MD, prob_MM = prob_MM, prob_MW = prob_MW,
                   prob_WD = prob_WD, prob_WM = prob_WM, prob_WW = prob_WW),
      type2 = list(spell_len_quantiles_D = spell_len_quantiles_D, spell_len_quantiles_W = spell_len_quantiles_W,
                   prcp_1_quantiles = prcp_1_quantiles, prcp_3_quantiles = prcp_3_quantiles, prcp_10_quantiles = prcp_10_quantiles))
  }
  
  return(output)
  
}


# Converts invalid values to NAs, so that they can easily be omitted in later calculatios.
# (An invalid value occurs if, e.g., there are no days with prcp > 3 cm in SON 1985)
remove_invalid = function(vec) {
  num_invalid = length(which(vec == "none"))
  if (num_invalid > 0) {
    vec[vec == "none"] = NA
  }
  return(as.numeric(vec))
}


# Counts number of invalid values
count_invalid = function(vec) {
  num_invalid = length(which(vec == "none"))
  return(num_invalid)
}

# Make a name for a plot
name_plot = function(season, station_name) {
  splitted = strsplit(station_name, "_")[[1]]
  station_ = paste(splitted[2], splitted[1], sep = ", ")
  season_ = paste0("(", season, ")")
  pl_name = paste(station_, season_)
  return(pl_name)
}


# Plot metrics of type 2
plot_metric2 = function(station_name, metrics, ind_metric, name_metric, truth) {
  # Function for making dataframes for plotting
  make_df_season = function(season, metric_emp_season, metric_init_season, metric_lq_season, metric_uq_season) {
    # Create a name for the plot
    pl_name = name_plot(season, station_name)
    
    df = data.frame(Emp = metric_emp_season, Sim = metric_init_season, 
                    CI_l = metric_lq_season, CI_u = metric_uq_season, 
                    season = rep(season, length(metric_emp_season)),
                    pl_name = rep(pl_name, length(metric_emp_season)))
    return(df)
  }
  
  if (station_name != "CA_Quincy") {
    metric_emp_DJF = metrics$DJF$metrics_emp2[[ind_metric]]
    metric_emp_MAM = metrics$MAM$metrics_emp2[[ind_metric]]
    metric_emp_JJA = metrics$JJA$metrics_emp2[[ind_metric]]
    metric_emp_SON = metrics$SON$metrics_emp2[[ind_metric]]
    
    metric_init_DJF = metrics$DJF$metrics_init_meds[[ind_metric]]
    metric_init_MAM = metrics$MAM$metrics_init_meds[[ind_metric]]
    metric_init_JJA = metrics$JJA$metrics_init_meds[[ind_metric]]
    metric_init_SON = metrics$SON$metrics_init_meds[[ind_metric]]
    
    # Lower CI bound
    metric_ql_DJF = metrics$DJF$metrics_ql[[ind_metric]]
    metric_ql_MAM = metrics$MAM$metrics_ql[[ind_metric]]
    metric_ql_JJA = metrics$JJA$metrics_ql[[ind_metric]]
    metric_ql_SON = metrics$SON$metrics_ql[[ind_metric]]
    
    # Upper CI bound
    metric_qu_DJF = metrics$DJF$metrics_qu[[ind_metric]]
    metric_qu_MAM = metrics$MAM$metrics_qu[[ind_metric]]
    metric_qu_JJA = metrics$JJA$metrics_qu[[ind_metric]]
    metric_qu_SON = metrics$SON$metrics_qu[[ind_metric]]
    
    # Make dataframes for plotting
    df_DJF = make_df_season("DJF", metric_emp_DJF, metric_init_DJF, metric_ql_DJF, metric_qu_DJF)
    df_MAM = make_df_season("MAM", metric_emp_MAM, metric_init_MAM, metric_ql_MAM, metric_qu_MAM)
    df_JJA = make_df_season("JJA", metric_emp_JJA, metric_init_JJA, metric_ql_JJA, metric_qu_JJA)
    df_SON = make_df_season("SON", metric_emp_SON, metric_init_SON, metric_ql_SON, metric_qu_SON)
    
    if (station_name == "CA_Berkeley") {
      df_seasons = rbind(df_DJF, df_MAM, df_SON)
    } else {
      df_seasons = rbind(df_DJF, df_MAM, df_JJA, df_SON)
    }
    
    df_seasons$season = fct_inorder(as.factor(df_seasons$season))
  } else {
    metric_emp_wet = metrics$Wet$metrics_emp2[[ind_metric]]
    
    metric_init_wet = metrics$Wet$metrics_init_meds[[ind_metric]]
    
    # Lower CI bound
    metric_ql_wet = metrics$Wet$metrics_ql[[ind_metric]]
    
    # Upper CI bound
    metric_qu_wet = metrics$Wet$metrics_qu[[ind_metric]]
    
    # Make dataframes for plotting
    df_wet = make_df_season("Wet", metric_emp_wet, metric_init_wet, metric_ql_wet, metric_qu_wet)
    
    df_seasons = df_wet
  }
  
  # Make facet plots
  xy_lims = range(c(df_seasons$Emp, df_seasons$CI_l, df_seasons$CI_u))
  plot = ggplot(df_seasons) + 
    geom_ribbon(aes(x = Emp, ymin = CI_l, ymax = CI_u), fill = "gray86") + #gray82
    geom_point(aes(x = Emp, y = Sim), colour = "#441074", fill = "blueviolet", alpha = .65, shape = 21) +
    geom_abline(aes(slope = 1, intercept = 0), linetype = 2) + 
    coord_fixed(ratio = 1) + 
    scale_x_continuous(limits = xy_lims) + 
    scale_y_continuous(limits = xy_lims) +
    facet_wrap(~season, nrow = 2, ncol = 2) +
    xlab("Simulated (cm)") + ylab("Empirical (cm)") +
    theme_classic() + 
    theme(#panel.spacing.x = unit(-1, "lines"), 
      #panel.spacing.y = unit(0, "lines"), 
      #panel.background = element_rect(linewidth = 1, fill = NA)
      strip.background = element_rect(fill = "#bbe0ed", linewidth = 0),
      strip.text = element_text(size = 10)) 
  
  return(list(pl_arr = plot, df_seasons = df_seasons))
  
}

# Plot metrics of type 1
plot_metric1 = function(station_name, metrics, ind_metric, ind_daily, name_metric, truth) {
  # Function for making dataframes for plotting
  make_df_season = function(season, metric_emp_season, metric_init_season, metric_se_season) {
    # Lower and upper limits for a 95% CI
    CI_l = metric_init_season - qnorm(.975) * metric_se_season
    CI_u = metric_init_season + qnorm(.975) * metric_se_season
    
    # Create a name for the plot
    pl_name = name_plot(season, station_name)
    
    vals = which(!is.na(metric_emp_season))
  
    if (ind_daily) {
      len = length(metric_emp_season)
      df = data.frame(Values = c(metric_emp_season, metric_init_season),
                      Day = rep(1:len, 2), 
                      Estimator = rep(c("Empirical", "Simulated"), each = len),
                      CI_l = rep(CI_l, 2),
                      CI_u = rep(CI_u, 2), 
                      season = rep(season, len * 2), 
                      pl_name = rep(pl_name, len * 2))
    } else {
      years_season = sort(unique(truth$m_year[which(truth$season == season)]))
      df = data.frame(Values = c(metric_emp_season, metric_init_season),
                      Year = rep(years_season, 2),
                      Estimator = rep(c("Empirical", "Simulated"), each = length(years_season)),
                      CI_l = rep(CI_l, 2),
                      CI_u = rep(CI_u, 2),
                      season = season,
                      pl_name = rep(pl_name, length(years_season)))
    }
    return(list(df = df))
  }
  
  if (station_name != "CA_Quincy") {
    metric_emp_DJF = metrics$DJF$metrics_emp1[[ind_metric]]
    metric_emp_MAM = metrics$MAM$metrics_emp1[[ind_metric]]
    metric_emp_JJA = metrics$JJA$metrics_emp1[[ind_metric]]
    metric_emp_SON = metrics$SON$metrics_emp1[[ind_metric]]
    
    metric_init_DJF = metrics$DJF$metrics_init_means[[ind_metric]]
    metric_init_MAM = metrics$MAM$metrics_init_means[[ind_metric]]
    metric_init_JJA = metrics$JJA$metrics_init_means[[ind_metric]]
    metric_init_SON = metrics$SON$metrics_init_means[[ind_metric]]
    
    metric_se_DJF = metrics$DJF$metrics_se[[ind_metric]]
    metric_se_MAM = metrics$MAM$metrics_se[[ind_metric]]
    metric_se_JJA = metrics$JJA$metrics_se[[ind_metric]]
    metric_se_SON = metrics$SON$metrics_se[[ind_metric]]
    
    # Make dataframes for plotting
    list_DJF = make_df_season("DJF", metric_emp_DJF, metric_init_DJF, metric_se_DJF)
    list_MAM = make_df_season("MAM", metric_emp_MAM, metric_init_MAM, metric_se_MAM)
    list_JJA = make_df_season("JJA", metric_emp_JJA, metric_init_JJA, metric_se_JJA)
    list_SON = make_df_season("SON", metric_emp_SON, metric_init_SON, metric_se_SON)
    
    df_DJF = list_DJF$df
    df_MAM = list_MAM$df
    df_JJA = list_JJA$df
    df_SON = list_SON$df
    
    if (station_name == "CA_Berkeley") {
      df_seasons = rbind(df_DJF, df_MAM, df_SON)
    } else {
      df_seasons = rbind(df_DJF, df_MAM, df_JJA, df_SON)
    }
    
    df_seasons$season = fct_inorder(as.factor(df_seasons$season))
  } else {
    metric_emp_wet = metrics$Wet$metrics_emp1[[ind_metric]]
    
    metric_init_wet = metrics$Wet$metrics_init_means[[ind_metric]]
    
    metric_se_wet = metrics$Wet$metrics_se[[ind_metric]]
    
    # Make dataframes for plotting
    list_wet = make_df_season("Wet", metric_emp_wet, metric_init_wet, metric_se_wet)
    
    df_wet = list_wet$df
    df_seasons = df_wet
  }
  
  # Make facet plots
  if (ind_daily) {
    plot = ggplot(df_seasons) + 
      geom_ribbon(aes(x = Day, ymin = CI_l, ymax = CI_u), fill = "gray82") +
      geom_line(aes(x = Day, y = Values, group = Estimator, color = Estimator)) + 
      scale_colour_manual(values = c("Simulated" = "#a90076", "Empirical" = "#0f27ba")) +
      facet_wrap(~season, scales = "free_y") +
      theme_classic() + 
      ylab(name_metric) + xlab("Day of Season") +
      theme( 
        strip.background = element_rect(fill = "#bbe0ed", linewidth = 0),
        strip.text = element_text(size = 10))
  } else {
    plot = ggplot(df_seasons) + 
      geom_ribbon(aes(x = Year, ymin = CI_l, ymax = CI_u), fill = "gray82") +
      geom_line(aes(x = Year, y = Values, group = Estimator, color = Estimator)) + 
      scale_colour_manual(values = c("Simulated" = "#a90076", "Empirical" = "#0f27ba")) +
      facet_wrap(~season, nrow = 2, ncol = 2, scales = "free_y") +
      theme_classic() + 
      ylab(name_metric) +
      theme(
        strip.background = element_rect(fill = "#bbe0ed", linewidth = 0),
        strip.text = element_text(size = 10))
  }      
  
  return(list(pl_arr = plot, df_seasons = df_seasons))
}    


# Plot the individual runs for visualizing the variance
plot_runs = function(station_name, metrics, ind_metric, ind_daily, name_metric, varname_metric, truth) {
  df_init_DJF = metrics$DJF$metrics_init_df[[ind_metric]]
  df_init_MAM = metrics$MAM$metrics_init_df[[ind_metric]]
  df_init_JJA = metrics$JJA$metrics_init_df[[ind_metric]]
  df_init_SON = metrics$SON$metrics_init_df[[ind_metric]]
  
  df_boot_DJF = metrics$DJF$metrics_boot_df[[ind_metric]]
  df_boot_MAM = metrics$MAM$metrics_boot_df[[ind_metric]]
  df_boot_JJA = metrics$JJA$metrics_boot_df[[ind_metric]]
  df_boot_SON = metrics$SON$metrics_boot_df[[ind_metric]]
  
  mean_init_DJF = metrics$DJF$metrics_init_mean[[ind_metric]]
  mean_init_MAM = metrics$MAM$metrics_init_mean[[ind_metric]]
  mean_init_JJA = metrics$JJA$metrics_init_mean[[ind_metric]]
  mean_init_SON = metrics$SON$metrics_init_mean[[ind_metric]]
  
  mean_boot_DJF = apply(df_boot_DJF, 1, function(x) mean(x, na.rm = T))
  mean_boot_MAM = apply(df_boot_MAM, 1, function(x) mean(x, na.rm = T))
  mean_boot_JJA = apply(df_boot_JJA, 1, function(x) mean(x, na.rm = T))
  mean_boot_SON = apply(df_boot_SON, 1, function(x) mean(x, na.rm = T))
  
  names_boot = names(df_boot_DJF)
  names_init = names(df_init_DJF)
  
  plot_runs_season = function(season, df_init_season, df_boot_season, mean_init_season, mean_boot_season) {
    len = nrow(df_init_season)
    
    set.seed(5)
    ind_stat = sample(1:len, 1)
    
    vals_v = as.numeric(sapply(names_init, function(name) str_remove(str_extract(name, "v[0-9]+"), "v")))
    vals_b = as.numeric(sapply(names_boot, function(name) str_remove(str_extract(name, "b[0-9]+"), "b")))
    vals_m_boot = as.numeric(sapply(names_boot, function(name) str_remove(str_extract(name, "m[0-9]+"), "m")))
    vals_m_init = as.numeric(sapply(names_init, function(name) str_remove(str_extract(name, "m[0-9]+"), "m")))
    
    # Mean for each bootstrap index b
    calc_mu_i.. = function(i) {
      vals = df_boot_season[, grep(paste0("b", i, "_"), names_boot)]
      means = apply(vals, 1, function(x) mean(x, na.rm = T))
      return(means)
    } 
    # Mean for each bootstrap index v
    calc_mu_.j. = function(j) {
      vals = df_init_season[, grep(paste0("v", j, "_"), names_init)]
      means = apply(vals, 1, function(x) mean(x, na.rm = T))
      return(means)
    } 
    
    mu_i.. = lapply(1:B, calc_mu_i..) 
    mu_.j. = lapply(2:(V + 1), calc_mu_.j.) 
    
    palette = get_cat_palette(10, F)
    colors = c("1" = palette$red, "2" = palette$orange, "3" = palette$yellow, "4" = palette$yellowgreen,
               "5" = palette$lightgreen, "6" = palette$darkgreen, "7" = palette$cyan,
               "8" = palette$periwinkle, "9" = palette$purple, "10" = palette$pink)
    
    if (ind_daily) {
      df_boot = data.frame(Value = unlist(mu_i..), Day = rep(1:len, B), 
                           Index = rep(1:B, each = len), Mean = rep(mean_boot_season, B))
      
      df_init = data.frame(Value = unlist(mu_.j.), Day = rep(1:len, V), 
                           Index = rep(2:(V + 1), each = len), Mean = rep(mean_init_season, V))

      df = rbind(df_init, df_boot)
      df$Run = c(rep("Init.", nrow(df_init)), rep("Boot.", nrow(df_boot)))
      df$Index = as.factor(df$Index)
      pl_run = ggplot(df) + geom_line(aes(x = Day, y = Value, group = Index, colour = Index)) +
        geom_line(aes(x = Day, y = Mean), linewidth = 1, colour = "#0f27ba") +
        facet_wrap(~Run) +
        xlab("Day of the Season") + ylab(name_metric) + 
        scale_colour_manual(values = colors) +
        theme_classic() +
        theme(panel.border = element_rect(colour = "black", fill=NA),
              strip.background = element_rect(fill = "#bbe0ed", linewidth = 0))
    } else {
      years_season = sort(unique(truth$m_year[which(truth$season == season)]))
      df_boot = data.frame(Value = unlist(mu_i..), Year = rep(years_season, B), 
                           Index = rep(1:B, each = len), Mean = rep(mean_boot_season, B))
      
      df_init = data.frame(Value = unlist(mu_.j.), Year = rep(years_season, V), 
                           Index = rep(2:(V + 1), each = len), Mean = rep(mean_init_season, V))
      
      df = rbind(df_init, df_boot)
      df$Index = as.factor(df$Index)
      df$Run = rep(c("Init.", "Boot."), each = nrow(df_init))
      pl_run = ggplot(df) + geom_line(aes(x = Year, y = Value, group = Index, colour = Index)) +
        geom_line(aes(x = Year, y = Mean), linewidth = 1, colour = "#0f27ba") +
        facet_wrap(~Run) +
        scale_colour_manual(values = colors) +
        ylab(name_metric) +
        theme_classic() +
        theme(panel.border = element_rect(colour = "black", fill=NA),
              strip.background = element_rect(fill = "#bbe0ed", linewidth = 0))
      
    }
    
    # Make plot of replicates for an individual sample
    #ind_stat = sample(1:len, 1) # 79
    ind_stat = 79
    group_means_b = sapply(mu_i.., function(x) x[ind_stat])
    group_means_v = sapply(mu_.j., function(x) x[ind_stat])
    df_boot_stat = data.frame(Values = as.numeric(df_boot_season[ind_stat, ]),
                              Index = vals_b, m = vals_m_boot,
                              Group_Means = rep(group_means_b, each = M))
    df_init_stat = data.frame(Values = as.numeric(df_init_season[ind_stat, ]),
                              Index = vals_v, m = vals_m_init,
                              Group_Means = rep(group_means_v, each = M))
    df_stat = rbind(df_boot_stat, df_init_stat)
    df_stat$Index = as.factor(df_stat$Index)
    df_stat$Run = c(rep("Boot", nrow(df_boot_stat)), rep("Init", nrow(df_init_stat)))
    df_stat$Run = fct_inorder(df_stat$Run)
    pl_stat = ggplot(df_stat) +
      geom_point(aes(x = Index, y = Values), alpha = .6) +
      geom_point(aes(x = Index, y = Group_Means, colour = Index), size = 2.75) + #or shape = 1
      facet_wrap(~Run) +
      scale_colour_manual(values = colors) +
      theme_classic() +
      ylab(name_metric) + ggtitle("") +
      theme(panel.border = element_rect(colour = "black", fill=NA),
            strip.background = element_blank(),
            strip.text.x = element_blank(),
            legend.position = "none")
    pl_stat
    pl_arr = pl_run / pl_stat
    
    pdf(paste0(results_path, station_name, "_", varname_metric, "-", season, "_runs.pdf"))
    print(pl_run)
    dev.off()
    
    pdf(paste0(results_path, station_name, "_", varname_metric, "-", season, "_individual_run.pdf"))
    print(pl_stat)
    dev.off()
    
    return(pl_run)
  }
  # Make dataframes for plotting
  plot_DJF = plot_runs_season("DJF", df_init_DJF, df_boot_DJF, mean_init_DJF, mean_boot_DJF)
  plot_MAM = plot_runs_season("MAM", df_init_MAM, df_boot_MAM, mean_init_MAM, mean_boot_MAM)
  plot_JJA = plot_runs_season("JJA", df_init_JJA, df_boot_JJA, mean_init_JJA, mean_boot_JJA)
  plot_SON = plot_runs_season("SON", df_init_SON, df_boot_SON, mean_init_SON, mean_boot_SON)
  
  # pdfsave(paste0(results_path, station_name, "_", varname_metric, "-DJF"), plot_DJF)
  # pdfsave(paste0(results_path, station_name, "_", varname_metric, "-MAM"), plot_MAM)
  # pdfsave(paste0(results_path, station_name, "_", varname_metric, "-JJA"), plot_JJA)
  # pdfsave(paste0(results_path, station_name, "_", varname_metric, "-SON"), plot_SON)
}


#-------------------------- Functions used in calculate_metrics ---------------------------
# Calculate probability of non-negligible precipitation
est_p = function(vec, cutoff = 0, emp = F) {
  if (emp) {
    if (anyNA(vec)) {
      vec = vec[!is.na(vec)]
    }
  }
  if (length(vec[vec > cutoff]) == 0) {
    return("none")
  } else {
    return(length(vec[vec > cutoff])/length(vec))
  }
}

# Calculate precipitation intensity
est_mean = function(vec, cutoff = 0, emp = F) {
  if (emp) {
    if (anyNA(vec)) {
      vec = vec[!is.na(vec)]
    }
  }
  if (length(vec[vec > cutoff]) == 0) {
    return("none")
  } else {
    return(mean(vec[vec > cutoff]))
  }
}

# Calculate total precipitation
est_sum = function(vec, emp) {
  if (emp) {
    if (anyNA(vec)) {
      vec = vec[!is.na(vec)]
    }
  }
  return(sum(vec))
}

# Calculate 1 - ecdf 
est_cdf = function(vec, value, emp) {
  if (emp) {
    if (anyNA(vec)) {
      vec = vec[!is.na(vec)]
    }
  }
  return(length(which(vec > value))/length(vec))
}

# Get spell statistics
get_spells = function(vec, year, year_min, year_max, dry = T, cutoff) {
  if (dry) {
    bin = c(0, cutoff) 
  } else {
    bin = c(cutoff + .1^(10), Inf)
  }
  
  # Obtain spell lengths
  spells = lapply(1:length(vec), function(i) NA)
  spell = c()
  for (i in 1:length(vec)) {
    amt_today = vec[i]
    if (is.na(amt_today)) {
      if (length(spell) > 0) {
        spells[[i - 1]] = spell # Save previous bin spell
      }
      spell = amt_today
      spells[[i]] = amt_today   # Save NA spell
      spell = c()               # Reset spell
    } else if (between(amt_today, bin[1], bin[2])) {
      spell = c(spell, amt_today)  # Add current day to the bin spell
      if (i == length(vec)) {
        # If it's the last day in vec, save the bin spell
        spells[[i]] = spell
      }
    } else {
      if (length(spell) > 0) {
        spells[[i]] = spell # Save the bin spell when it ends (i.e., when a non-bin spell begins)
        spell = c()         # Reset spell
      }
    }
  }
  spells = spells[!is.na(spells)]
  return(spells)
}

# Calculate spell lengths
calc_spell_lens = function(season_ts, season_truth, year, dry = T, cutoff) {
  year_min = min(season_truth$m_year)
  year_max = max(season_truth$m_year)
  inds_season = which(season_truth$m_year == year)
  vec = season_ts[inds_season]
  spells = get_spells(vec, year, year_min, year_max, dry, cutoff)
  lens = sapply(spells, length)
  head(lens)
  if (length(lens) == 0) {
    lens = 0
  }
  return(lens)
}

# Calculate quantiles for spell lengths 
calc_spell_len_quantiles = function(season_ts, season_truth, years, dry = T, cutoff) {
  lens = sapply(years, function(year) calc_spell_lens(season_ts, season_truth, year, dry, cutoff))
  quantiles = quantile(unlist(lens), seq(0, 1, .01), na.rm = T)
  return(quantiles)
}

# Calculate spell statistics for each year
calc_spell_stats_year = function(station_name, season_ts, season_truth, year, dry = T, cutoff) {
  year_min = min(season_truth$m_year)
  year_max = max(season_truth$m_year)
  inds_season = which(season_truth$m_year == year)
  vec = season_ts[inds_season]
  spells = get_spells(vec, year, year_min, year_max, dry, cutoff)
  if (length(spells) == 0) {
    # If length of the spells vector is 0, this means that every day was in the opposite state or missing.
    lens = 0
    mean_len = "none"
    if (!dry) {
      mean_amt = "none"
    }
  } else {
    lens = unlist(lapply(spells, length))
    mean_len = mean(lens)
    if (!dry) {
      mean_amt = mean(unlist(lapply(spells, sum)))
    }
  }
  
  if (station_name == "CA_Quincy") {
    # Calculate number of spells 
    num_spells = length(spells)
    if (dry) {
      output = list(mean_spell_len = mean_len, num_spells = num_spells)
    } else {
      output = list(mean_spell_len = mean_len, mean_spell_amt = mean_amt, num_spells = num_spells)
    }
  } else {
    if (dry) {
      output = list(mean_spell_len = mean_len)
    } else {
      output = list(mean_spell_len = mean_len, mean_spell_amt = mean_amt)
    }
  }
  
  return(output)
}

# Calculate transition probabilities from one state to the next 
est_transition = function(vec, bin_yesterday, bin_today) {
  vec_today = vec[2:length(vec)]
  vec_yesterday = vec[1:(length(vec) - 1)]
  inbin_today = which(between(vec_today, bin_today[1], bin_today[2])) # these points t are s.t. Y_t* is in bin_today
  inbin_yesterday = which(between(vec_yesterday, bin_yesterday[1], bin_yesterday[2]))  #these points t are s.t. Y(t - 1)* is in bin_yesterday 
  if (length(inbin_yesterday) == 0) {
    return("none")
  }
  inters = intersect(inbin_today + 1, inbin_yesterday + 1)
  trans_prob = length(intersect(inbin_today, inbin_yesterday))/length(inbin_yesterday)
  return(trans_prob)
}


# Calculate prcp total over blocks of length len_lock
calc_prcp_block = function(vec, len_block, emp, calc_max = F) {
  if (length(vec) < len_block) {
    return(NA)
  }
  prcp_sum = c()
  for (t in len_block:length(vec)) {
    if (emp) {
      sum_t = sum(vec[(t - (len_block - 1)):t], na.rm = T)
    } else {
      sum_t = sum(vec[(t - (len_block - 1)):t])
    }
    prcp_sum = c(prcp_sum, sum_t)
  }
  if (!calc_max) {
    return(prcp_sum)
  } else {
    return(max(prcp_sum))
  }
}


# Calculate quantiles for the preciptation amounts
calc_prcp_block_quantiles = function(prcp_sum, emp) {
  if (emp) {
    quantiles = quantile(prcp_sum, seq(0, 1, .01), na.rm = T)
  } else {
    quantiles = quantile(prcp_sum, seq(0, 1, .01))
  }
  return(quantiles)
}
