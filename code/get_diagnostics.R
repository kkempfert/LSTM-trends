# This script reads in the data for a weather station and gets the diagnostics. 
library(ggplot2)
library(ggpubr)
library(stringr)
library(dplyr)
library(data.table)
#library(trend)

station_name = "CA_Quincy"
num_K = 2

dir_path = c("/Users/kempfert/Documents/rain2025/")
code_path = paste0(dir_path, "code/")
data_path = paste0(dir_path, "data/")
output_path = paste0(dir_path, "output/hpc/")
results_path = paste0(dir_path, "output/results/")
source(paste0(code_path, "functions_diagnostics.R"))

# Read in some extra files
inds = read.csv(paste0(output_path, station_name, "_inds_b0.csv"))$indices
truth = read.csv(paste0(data_path, station_name, ".csv"))
truth = truth[inds, ]
n = nrow(truth)

# Read in files from model runs
files = list.files(output_path, pattern = station_name)
files_train = files[grep("train", files)]
files_loss = files[grep("loss_", files)]
files_gen = files[grep("gen_", files)]
#files_num_train = length(files_train) # don't need
#files_num_gen = length(files_gen)


# ---------------------------------------------------------------------------------------------------
#                                         A few functions 
# ---------------------------------------------------------------------------------------------------

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


# Read in the training files 
train = lapply(files_train, function(x) read.csv(paste0(output_path, x)))
# Obtain alpha1, beta1, alpha2, beta2, mu1, mu2, var1, var2, and loss (on alpha, beta scale)
train = lapply(train, function(dat) get_values(dat))

# Read in the loss file and all the generation files.
loss = lapply(files_loss, function(x) read.csv(paste0(output_path, x)))
gen = lapply(files_gen, function(x) data.frame(fread(paste0(output_path, x))))

# Separate the generation files into the initial value and bootstrap replicates
gen_boot = gen[grep("i0", files_gen)]
gen_init = gen[grep("b0", files_gen)]

# Check the lengths of all the files
length(train)
length(loss)
length(gen)
length(gen_boot)
length(gen_init)

# Check for NAs/NaNs, Infs, or negative Infs
check_valid = function(df) {
  finite = all(apply(df, 1, function(x) all(is.finite(x))))
  return(finite) # returns T if all are valid (finite)
}

# TRUE means that all values are valid in all the files
all(sapply(train, check_valid))
all(sapply(gen_init, check_valid))
all(sapply(gen_boot, check_valid))
all(sapply(loss, check_valid))

# Check the number of epochs from training. 
num_epochs = sapply(loss, nrow)
num_epochs

#-------------------------- Obtaining diagnostics  ---------------------------
inds = read.csv(paste0(output_path, station_name, "_inds_b0.csv"))$indices
truth = read.csv(paste0(data_path, station_name, ".csv"))
truth = truth[inds, ]
n = nrow(truth)

# Remove last several rows of generated values (init) in order to make it the same length as the ground truth dataset. 
# (They are different lengths, because for a batch_size of 32, 13 observations were trimmed off the end
# of the training set, to make the number of its rows divisible by 32).
gen_init = lapply(gen_init, function(x) x[1:n, ])
diagnostics = call_diagnostics(gen_init, NULL, truth, units = "cm", .3)
save(diagnostics, file = paste0(results_path, station_name, "_", "diagnostics", ".RData"))
#dats_init = gen_init
#dats_boot = NULL






