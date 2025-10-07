# -*- coding: utf-8 -*-

# Import modules
import pandas as pd
import numpy as np
import tensorflow as tf
import tensorflow.keras as keras
from tensorflow.keras.models import Sequential, Model, load_model 
from tensorflow.keras.layers import Input, Dense, Concatenate, Lambda, LSTM, Dropout
from tensorflow.keras import optimizers, metrics
#from keras.activations import softplus, sigmoid, softmax, tanh
import tensorflow.keras.activations
import tensorflow.keras.backend as K
from tensorflow.keras.callbacks import EarlyStopping
from tensorflow.keras.optimizers import Adam, SGD
import tensorflow_probability as tfp
import datetime
import math
import time
import random as rn
import os
import itertools
from scipy.stats import binom, gamma
#from sklearn.preprocessing import MinMaxScaler

# Set number of threads to 1. Otherwise, by default, Python will detect if multiple threads are running, 
# which can thwart reproducibility of results in Keras/Tensorflow. 
tf.config.threading.set_intra_op_parallelism_threads(1)
tf.config.threading.set_inter_op_parallelism_threads(1)

# Set default for floats to be float64 (doubles). For float32, numerical issues of NaNs have been known to occur, either in the training or generation stage.
float_type = "float64"  
if float_type == "float64":
    data_type = tf.float64 
else:
    data_type = tf.float32
    
tf.keras.backend.set_floatx(float_type)


# Other settings 
variant = "reparam" # one of "orig", "reparam", "orig-round", "reparam-round" for different Gamma parametrizations and whether to account for rounding or not in the loss function.
# "orig" uses the alpha-beta parametrization and the ordinary loss function, "reparam" uses the mean-variance parametrization and the ordinary loss function, 
# "orig-round" uses the alpha-beta parametrization with the modified loss function, and "reparam-round" uses the mean-variance parametrization with the modified loss function.

# Set file paths
dir_path = ""
    
code_path = dir_path + "code/"
data_path = dir_path + "data/" 
output_path = dir_path + "output/hpc/" + float_type + "/"
source_name = code_path + "source.py"


# Read in source file
exec(compile(open(source_name, "rb").read(), source_name, 'exec'))

# Define parameters/hyperparameters that are intended to be automatically changed with sed
param_timesteps = 365
param_batch_size = 32
param_epochs = 300
param_neurons = 8
param_learning_rate = .001
param_opt = """adam"""
param_seed_v = 10
param_seed_b = 0
param_dropout = 0
param_num_K = 2
param_station_name = """CA_Berkeley"""

param_seed_m = [m for m in range(20)]

# Additional parameters/hyperparameters
#dropout = .25 
overlapping = True
stateful = False
covariates_year = True
covariates_season = True
covariates_NA = True
covariates_norm = True
write_out = True
write_every = False
return_states = False
#epsilon = .0127 # for the rounding approaches, the data read in is in cm, so .005 in = .0127 cm is the amount we use to account for rounding.


# Set random seeds correctly for reproducible results in Keras
# Instructions were obtained from https://keras.io/getting_started/faq/#how-can-i-obtain-reproducible-results-using-keras-during-development. 

# Start Numpy generated random number in a well-defined initial state
np.random.seed(param_seed_b) 

# Start core Python generated random number in a well-defined state
rn.seed(param_seed_v)

# Start random number generator in Tensorflow backend have a well-defined initial state. 
tf.random.set_seed(param_seed_v)

# Check by printing hash
print(hash("keras"))

# Read in data
station = pd.read_csv(data_path + param_station_name + ".csv", delimiter = ",") # simulated time series 

    
series = np.array(station["rain"])
names = ["rain"] # (can be multi-dimensional)
# Different ways of setting up the data, depending on which covariates are wanted for inclusion
if (covariates_season == True):
    series = np.column_stack([series, station["day_of_year"]])
    names.append("day_of_year")
if (covariates_year == True):
    series = np.column_stack([series, station["raw.year"]])
    names.append("year")
if (covariates_NA == True):
    series = np.column_stack([series, station["missing"]])
    names.append("missing")
    
series = pd.DataFrame(series, columns = names)  
covariates_num = len(names) - 1


gfile_end = construct_filename(param_timesteps, param_batch_size, param_epochs, param_neurons, stateful, param_learning_rate, param_opt, param_dropout, param_num_K)   
 # set other file names that correspond to thfe specific trial r 
file_end = gfile_end + "b%d_i%d" % (param_seed_b, param_seed_v)  
file_begin = output_path + param_station_name + "_"
# To check that the correct model is loaded, one could check that the forecasts from it are the same as previously obtained, using forecast_call(model, ...)


# Generate samples from the fitted LSTM.
for m in param_seed_m:
    model_bestt = load_model(file_begin + "model" + file_end, 
                             custom_objects={"keras":keras, "crop":crop, "num_K":param_num_K,
                                             "gamma_likelihood2":gamma_likelihood2,
                                             "custom_loss":custom_loss})
    mfile_end = gfile_end + "b%d_i%d_m%d" % (param_seed_b, param_seed_v, m)   # specific file end for generation index m
    mfilename = file_begin + "gen" + mfile_end
    generate_lstm(m, model_bestt, mfilename, series, param_timesteps, param_batch_size, param_epochs, param_neurons, 
              stateful = stateful, learning_rate = param_learning_rate, name = param_station_name, 
              opt = param_opt, dropout = param_dropout, num_K = param_num_K)  
    tf.keras.backend.clear_session()


