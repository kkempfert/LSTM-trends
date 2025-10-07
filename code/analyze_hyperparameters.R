# Load required packages
library(ggplot2)
library(patchwork)
library(stringr)
library(forcats)

# Set file paths
dir_path = ""
results_path = paste0(dir_path, "output/results/")
code_path = paste0(dir_path, "code/")
output_path = paste0(dir_path, "output/hpc/CV/")

source(paste0(code_path, "analyze_hyperparameters_functions.R"))

station_names = c("CA_Berkeley", "CA_Quincy", "AZ_Winslow")

# Construct filenames based on the hyperparameters considered.
params = read.csv(paste0(code_path, "hyperparams_CV.csv"), header = F)
names(params) = c("h", "station_name", "K", "opt", "b", "v", "batch_size")
file_ends = apply(params[1:(nrow(params)/3),], 1, function(x) construct_filename(x[1], x[3], x[4], x[7]))
files_full = sapply(station_names, function(x) paste0(x, "_loss", file_ends))
files_train = sapply(station_names, function(x) paste0(x, "_train-v", file_ends))


# Read in available files.
files = list.files(output_path)
files_loss = files[grep("loss", files)]
losses = lapply(files_loss, function(file) read.csv(paste0(output_path, file)))
#min_loss_v = sapply(losses, function(x) min(x$val))
min_loss_te = sapply(losses, function(x) min(x$test))
epochs = sapply(losses, nrow)


files_train = files[grep("train-t", files)]
files_val = files[grep("val-t", files)]
files_test = files[grep("test-t", files)]
train = lapply(files_train, function(file) read.csv(paste0(output_path, file)))
val = lapply(files_val, function(file) read.csv(paste0(output_path, file)))
test = lapply(files_test, function(file) read.csv(paste0(output_path, file)))
dims_train = sapply(train, function(x) nrow(x))
dims_val = sapply(val, function(x) nrow(x))
dims_test = sapply(test, function(x) nrow(x))
# Note that the number of rows of data differ because we have to adjust it in Python based on the batch size.
# summary(dims_train)
# summary(dims_val)
# summary(dims_test)


# Identify if any configurations are missing by comparing the constructed filenames to the available filenames.
if (length(files_loss) != length(files_full)) {
  print("Some configurations resulted in NaNs.")
  inds_available = sapply(files_full, function(x) grep(x, files_loss))
  inds_unavailable = (1:length(files_full))[!(1:length(files_full) %in% inds_available)]
  condition_nan1 = !is.finite(min_loss_te)
  condition_nan2 = sapply(files_full, function(x) {
    condition_nan1[grep(x, files_loss)]
  })
  inds_nan = which(condition_nan2 == T)
  files_full[c(inds_unavailable, inds_nan)]
} else {
  print("No configurations are missing.")
}


# Prepare hyperparameters dataframe for analysis. 
params = t(sapply(files_loss, parse_filename))
params = data.frame(params)
names(params) = c("h", "K", "Optimizer", "batch_size")
params = cbind(params, loss = min_loss_te)
params$station_name = sapply(files_loss, function(file) paste(strsplit(file, split = "_")[[1]][2:1], collapse = ", "))
row.names(params) = NULL
if (mean(is.finite(params$loss) != 1)) {
  params = params[is.finite(params$loss), ]
}

# Identify best configurations
sapply(unique(params$station_name), function(name) {
  loss = params$loss[params$station_name == name]
  inds_best = which(params$loss == min(loss, na.rm = T))
  params_best = params[inds_best, ]
})


# --------------------------------- Analyzing loss based on the hyperparameters  --------------------
# First, plot the testing risk based on the hyperparameters.
plots_loss = plot_loss(params)
pl_Berkeley = plots_loss$pl_Berkeley
pl_Winslow = plots_loss$pl_Winslow
pl_Quincy = plots_loss$pl_Quincy

pl_loss = (pl_Berkeley / pl_Winslow / pl_Quincy) + plot_layout(guides = "collect")
pdf(paste0(results_path, "hyperparams_loss.pdf"))
print(pl_loss)
dev.off()


# Second, conduct ANOVA
params$K = as.factor(params$K)
params$batch_size = as.factor(params$batch_size)
params$h = as.factor(params$h)
params$Optimizer = as.factor(params$Optimizer)

fit_Berkeley = aov(loss ~ h + K + Optimizer + batch_size, data = params[params$station_name == "Berkeley, CA", ])
summary(fit_Berkeley)
fit_Berkeley$coefficients

fit_Winslow = aov(loss ~ h + K + Optimizer + batch_size, data = params[params$station_name == "Winslow, AZ", ])
summary(fit_Winslow)
fit_Winslow$coefficients

fit_Quincy = aov(loss ~ h + K + Optimizer + batch_size, data = params[params$station_name == "Quincy, CA", ])
summary(fit_Quincy)
fit_Quincy$coefficients


# --------------------------------- Analysis of K = 1 vs. K = 2 -------------------------------------
file_K1_Berkeley = paste0("CA_Berkeley_test-v", construct_filename(h = 8, K = 1, opt = "adam", batch_size = 64))
file_K2_Berkeley = paste0("CA_Berkeley_test-v", construct_filename(h = 8, K = 2, opt = "adam", batch_size = 64))
file_K1_Winslow = paste0("AZ_Winslow_test-v", construct_filename(h = 8, K = 1, opt = "sgd", batch_size = 1024))
file_K2_Winslow = paste0("AZ_Winslow_test-v", construct_filename(h = 8, K = 2, opt = "sgd", batch_size = 1024))
file_K1_Quincy = paste0("CA_Quincy_test-v", construct_filename(h = 16, K = 1, opt = "adam", batch_size = 16))
file_K2_Quincy = paste0("CA_Quincy_test-v", construct_filename(h = 16, K = 2, opt = "adam", batch_size = 16))

test_K1_Berkeley = get_values(read.csv(paste0(output_path, file_K1_Berkeley)), K = 1)
test_K2_Berkeley = get_values(read.csv(paste0(output_path, file_K2_Berkeley)), K = 2)
test_K1_Winslow = get_values(read.csv(paste0(output_path, file_K1_Winslow)), K = 1)
test_K2_Winslow = get_values(read.csv(paste0(output_path, file_K2_Winslow)), K = 2)
test_K1_Quincy = get_values(read.csv(paste0(output_path, file_K1_Quincy)), K = 1)
test_K2_Quincy = get_values(read.csv(paste0(output_path, file_K2_Quincy)), K = 2)

plots_wet = plot_dist_K()
plots_dry = plot_dist_K(T)
plots_wet$pl_dens
plots_dry$pl_dens
plots_wet$pl_preds

pdf(paste0(results_path, "K1-2_preds.pdf"))
print(plots_wet$pl_preds)
dev.off()

pdf(paste0(results_path, "K1-2_dens.pdf"))
print(plots_wet$pl_dens)
dev.off()

# Count number of configurations for which K = 2 loss was less than K = 1 loss, by station
comparison = lapply(c("Berkeley", "Quincy", "Winslow"), function(x) {
  params_station = params[grep(x, files_loss), ]
  params_station = params_station[order(params_station$K),]
  params_paste_station = apply(params_station, 1, function(y) paste(y[c(1, 3, 4)], collapse = ", "))
  compare_station = t(sapply(unique(params_paste_station), function(y) {
    inds = which(params_paste_station == y)
    ind_K1 = inds[1]
    ind_K2 = inds[2]
    losses = c(params_station$loss[ind_K1], params_station$loss[ind_K2])
  }))
  inds_finite = apply(compare_station, 1, function(y) ifelse(mean(is.finite(y)) == 1, T, F))
  compare_station = compare_station[inds_finite, ]
  condition = compare_station[, 2] < compare_station[, 1]
  mean_condition = mean(condition, na.rm = T)
  diff = summary(compare_station[, 2] - compare_station[, 1], na.rm = T)
  return(list(table(condition), diff))
})
comparison

