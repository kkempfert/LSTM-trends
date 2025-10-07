construct_filename = function(h, K, opt, batch_size) {
  h = as.numeric(h)
  K = as.numeric(K)
  batch_size = as.numeric(batch_size)
  file_end = paste0("_steps365_h", h, "_batch", batch_size, "_epoch300_stateful0_LR0.001_opt", opt, "_cov1_K", K, "_dropout0_reparam_b1_v10.csv")
  return(file_end)
}

parse_filename = function(filename) {
  h = as.numeric(str_extract(str_extract(filename, "h[0-9]+_"), "[0-9]+"))
  K = as.numeric(str_extract(str_extract(filename, "K[0-9]+_"), "[0-9]+"))
  opt = str_remove(str_extract(filename, "opt[a-z]+"), "opt")
  batch_size = as.numeric(str_extract(str_extract(filename, "batch[0-9]+_"), "[0-9]+"))
  return(c(h, K, opt, batch_size))
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
                   lightgreen = "#47cc69", darkgreen = "#2a8265", cyan = "#00ebeb",
                   periwinkle = "#2987e6", blue = "#0f25b8", purple = "#a073e3", pink = "#db53d5")
  }
  return(palette)
}


# ----------------------  Functions for analyzing loss based on the hyperparameters  --------------------
plot_loss = function(params) {
  params2 = params
  params2$station_name = sapply(1:nrow(params2), function(i) paste0(params2$station_name[i], " (K = ", params2$K[i], ")"))
  params2$Optimizer = as.character(params2$Optimizer)
  params2$Optimizer[params2$Optimizer == "adam"] = "Adam"
  params2$Optimizer[params2$Optimizer == "sgd"] = "SGD"
  params2$h = as.numeric(as.character(params2$h))
  params2$K = as.numeric(params2$K)
  
  # Manually create jitter for sgd vs adam optimizers
  params2$h[params2$Optimizer == "Adam"] = params2$h[params2$Optimizer == "Adam"] - 2
  params2$h[params2$Optimizer == "SGD"] = params2$h[params2$Optimizer == "SGD"] + 2
  
  params2$batch_size = as.factor(params2$batch_size)
  
  palette10 = get_cat_palette(10, prcp = F)
  pl_values = c("16" = palette10$red, "32" = palette10$orange, "64" = palette10$lightgreen, "128" = palette10$blue, "1024" = palette10$purple)
  
  pl_Berkeley = ggplot(params2[grep("Berkeley", params2$station_name), ]) + aes(x = h, y = loss, colour = batch_size, shape = Optimizer) + 
    geom_point(alpha = .65) +
    facet_wrap(~station_name, scales = "fixed", nrow = 1, ncol = 2) +
    theme_classic() + 
    theme(
      strip.background = element_rect(fill = "#bbe0ed", linewidth = 0)) + 
    scale_colour_manual(values = pl_values) +
    scale_x_continuous(breaks = c(8, 16, 50, 100)) +
    ylab("Testing Risk") + xlab("h") + labs(colour = "Batch Size")
  pl_Winslow = ggplot(params2[grep("Winslow", params2$station_name), ]) + aes(x = h, y = loss, colour = batch_size, shape = Optimizer) + 
    geom_point(alpha = .65) +
    facet_wrap(~station_name, scales = "fixed", nrow = 1, ncol = 2) +
    theme_classic() + 
    theme(
      strip.background = element_rect(fill = "#bbe0ed", linewidth = 0)) + 
    scale_colour_manual(values = pl_values) +
    scale_x_continuous(breaks = c(8, 16, 50, 100)) +
    ylab("Testing Risk") + xlab("h") + labs(colour = "Batch Size")
  pl_Quincy = ggplot(params2[grep("Quincy", params2$station_name), ]) + aes(x = h, y = loss, colour = batch_size, shape = Optimizer) + 
    geom_point(alpha = .65) +
    facet_wrap(~station_name, scales = "fixed", nrow = 1, ncol = 2) +
    theme_classic() + 
    theme(
      strip.background = element_rect(fill = "#bbe0ed", linewidth = 0)) + 
    scale_colour_manual(values = pl_values) +
    scale_x_continuous(breaks = c(8, 16, 50, 100)) +
    ylab("Testing Risk") + xlab("h") + labs(colour = "Batch Size")
  
  return(list(pl_Berkeley = pl_Berkeley, pl_Winslow = pl_Winslow, pl_Quincy = pl_Quincy))
}

# ---------------------------- Functions for analyzing K = 1 vs. K = 2 ----------------------------
# This is a function that obtains alpha1, beta1, alpha2, beta2 (and the loss on that scale) for interpretation purposes. 
# It also rearranges the columns (if needed) in the outputs so that the first Gamma component is the "larger" one. 
get_values = function(dat, K = 2) {
  inds_NA = which(dat$loss == 0)
  if (K == 1) {
    mu = dat$mu
    var = dat$var
    alpha = dat$mu^2/dat$var
    beta = dat$mu/dat$var
    dat = cbind(dat, alpha, beta)
    loss2 = dat$loss
    loss2[inds_NA] = 0
    dat$loss = loss2
  } else {
    mu1 = dat$mu1
    mu2 = dat$mu2
    var1 = dat$var1
    var2 = dat$var2
    alpha1 = dat$mu1^2/dat$var1
    beta1 = dat$mu1/dat$var1
    alpha2 = dat$mu2^2/dat$var2
    beta2 = dat$mu2/dat$var2
    dat = cbind(dat, alpha1, beta1, alpha2, beta2)
    loss2 = dat$loss
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
  }
  return(dat)
}

# Calculate the density for "typical" parameters for K = 1 vs. K = 2 from the testing set
# That is, calculate the density for the day with median nonzero prcp amount.
calc_dens = function(test_K1, test_K2, station_name, next_dry = F) {
  y_true = test_K1$y_true
  vals = seq(.000000001, median(y_true[y_true > 0]) * 10, .0001) # 1.5
  #vals = seq(.000000001, 1.5, .0001) # 1.5
  med = median(y_true[y_true > 0], type = 1)
  ind = which(y_true == med)[1]
  if (next_dry) {
    if (grepl("Berkeley", station_name)) {
      ind = ind - 1
    } else {
      ind = ind + 1
    }
  }
  
  med_alpha_K1 = test_K1$alpha[ind]
  med_beta_K1 = test_K1$beta[ind]
  
  med_alpha_K2_1 = test_K2$alpha1[ind]
  med_beta_K2_1 = test_K2$beta1[ind]
  med_alpha_K2_2 = test_K2$alpha2[ind]
  med_beta_K2_2 = test_K2$beta2[ind]
  
  dens_K1 = dgamma(vals, shape = med_alpha_K1, rate = med_beta_K1)
  dens_K2_1 = dgamma(vals, shape = med_alpha_K2_1, rate = med_beta_K2_1)
  dens_K2_2 = dgamma(vals, shape = med_alpha_K2_2, rate = med_beta_K2_2)
  df = data.frame(Density = c(dens_K1, dens_K2_1, dens_K2_2), 
                  Values = rep(vals, 3), 
                  Distribution = rep(c("K = 1", "K = 2, Comp. 1", "K = 2, Comp. 2"), each = length(vals)),
                  station = rep(station_name, length(vals) * 3))
  # Restrict values of the density for plotting
  cutoff = 10
  df = df[-which(df$Density > cutoff), ]
  return(df)
}

# Make a dataframe that will be used for plotting the predicted parameters.
make_df_preds = function(test_K1, test_K2, station_name) {
  n = nrow(test_K1)
  df_preds_mean = data.frame(Values = c(test_K1$mu, test_K2$mu1, test_K2$mu2), 
                             Distribution = rep(c("K = 1", "K = 2, Comp. 1", "K = 2, Comp. 2"), each = n),
                             station = rep(station_name, n * 3))
  df_preds_var = data.frame(Values = c(test_K1$var, test_K2$var1, test_K2$var2), 
                            Distribution = rep(c("K = 1", "K = 2, Comp. 1", "K = 2, Comp. 2"), each = n),
                            station = rep(station_name, n * 3))
  return(list(df_preds_mean = df_preds_mean, df_preds_var = df_preds_var))
}

# Make density plots and histograms of the predicted parameters for comparing the Gamma distributions
# for K = 1 vs. K = 2
plot_dist_K = function(next_day = F) {
  dens_Berkeley = calc_dens(test_K1_Berkeley, test_K2_Berkeley, "Berkeley, CA", next_day)
  dens_Winslow = calc_dens(test_K1_Winslow, test_K2_Winslow, "Winslow, AZ", next_day)
  dens_Quincy = calc_dens(test_K1_Quincy, test_K2_Quincy, "Quincy, CA", next_day)
  
  df_dens = rbind(dens_Berkeley, dens_Winslow, dens_Quincy)
  df_dens$station = fct_inorder(df_dens$station)
  
  
  preds_Berkeley = make_df_preds(test_K1_Berkeley, test_K2_Berkeley, "Berkeley, CA")
  preds_Winslow = make_df_preds(test_K1_Winslow, test_K2_Winslow, "Winslow, AZ")
  preds_Quincy = make_df_preds(test_K1_Quincy, test_K2_Quincy, "Quincy, CA")
  
  preds_mean = rbind(preds_Berkeley$df_preds_mean, preds_Winslow$df_preds_mean, preds_Quincy$df_preds_mean)
  preds_var = rbind(preds_Berkeley$df_preds_var, preds_Winslow$df_preds_var, preds_Quincy$df_preds_var)
  
  palette = get_cat_palette(3, prcp = F)
  labs = unique(preds_mean$Distribution)
  
  pl_mean = ggplot(preds_mean) + 
    aes(x = Values, fill = Distribution, group = Distribution) + 
    geom_histogram(position = "identity", alpha = .55, colour = "gray40") + 
    xlab("Mean") + ylab("Count") +
    facet_wrap(~station, nrow = 3, ncol = 1, scales = "free") +
    theme_classic() + 
    theme(
      strip.background = element_rect(fill = "#bbe0ed", linewidth = 0),
      strip.text = element_text(size = 10)) + 
    scale_fill_manual(values = c("K = 1" = palette$purple, 
                                 "K = 2, Comp. 1" = palette$red,
                                 "K = 2, Comp. 2" = palette$green))
  pl_var = ggplot(preds_var) + 
    aes(x = Values, fill = Distribution, group = Distribution) + 
    geom_histogram(position = "identity", alpha = .55, colour = "gray40") + 
    xlab("Variance") + ylab("Count") +
    facet_wrap(~station, nrow = 3, ncol = 1, scales = "free") +
    theme_classic() + 
    theme(
      strip.background = element_rect(fill = "#bbe0ed", linewidth = 0),
      strip.text = element_text(size = 10)) +
    scale_fill_manual(values = c("K = 1" = palette$purple, 
                                 "K = 2, Comp. 1" = palette$red,
                                 "K = 2, Comp. 2" = palette$green))
  
  pl_dens = ggplot(df_dens) + 
    aes(x = Values, y = Density, colour = Distribution, group = Distribution) + 
    geom_line() + 
    facet_wrap(~station, nrow = 3, ncol = 1, scales = "free") +
    theme_classic() + 
    theme( 
      strip.background = element_rect(fill = "#bbe0ed", linewidth = 0),
      strip.text = element_text(size = 10)) +
    scale_colour_manual(values = c("K = 1" = palette$purple, 
                                   "K = 2, Comp. 1" = palette$red,
                                   "K = 2, Comp. 2" = palette$green))
  
  pl_preds = (pl_mean + pl_var) + plot_layout(guides = "collect")
  
  return(list(pl_preds = pl_preds, pl_dens = pl_dens))
}
