# -*- coding: utf-8 -*-

# Import modules
import pandas as pd
import numpy as np
import datetime
import math
import time
import random as rn
import os
import itertools
from scipy.stats import binom, gamma
import resource

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

# Set number of threads to 1. Otherwise, by default, Python will detect if multiple threads are running, 
# which can thwart reproducibility of results in Keras/Tensorflow. 
tf.config.threading.set_intra_op_parallelism_threads(1)
tf.config.threading.set_inter_op_parallelism_threads(1)

# Set default for floats to be float64 (doubles). For float32, numerical issues of NaNs have been known to occur, either in the training or generation stage.
float_type = "float64" # or use "float32" 
if float_type == "float64":
    data_type = tf.float64 
else:
    data_type = tf.float32
    
tf.keras.backend.set_floatx(float_type)

# Other settings 
variant = "reparam"

# Define file paths
dir_path = ""
    
code_path = dir_path + "code/"
data_path = dir_path + "data/" 
output_path = dir_path + "output/hpc/CV/"
source_name = code_path + "source_CV.py"


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
param_station_name = """AZ_Winslow"""

# Additional parameters/hyperparameters
overlapping = True
stateful = False
covariates_year = True
covariates_season = True
covariates_NA = True
covariates_norm = True
write_out = True
write_every = False
return_states = False
early_stopping = True
patience = 50 # for early stopping


# Set random seeds correctly for reproducible results in Keras
# Instructions were obtained from https://keras.io/getting_started/faq/#how-can-i-obtain-reproducible-results-using-keras-during-development. 

# Generate random seeds.
np.random.seed(5) 

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

    
series = np.array(station["prcp"]) #(can be multi-dimensional)
names = ["prcp"] # 
# Different ways of setting up the data, depending on which covariates are wanted for inclusion
if (covariates_season == True):
    series = np.column_stack([series, station["day_of_year"]])
    names.append("day_of_year")
if (covariates_year == True):
    series = np.column_stack([series, station["raw_year"]])
    names.append("year")
if (covariates_NA == True):
    series = np.column_stack([series, station["missing"]])
    names.append("missing")
    
series = pd.DataFrame(series, columns = names)  
covariates_num = len(names) - 1


# Run
run_lstm_CV(series, param_timesteps, param_batch_size, param_epochs, param_neurons,
    stateful = stateful, learning_rate = param_learning_rate, name = param_station_name, 
    opt = param_opt, dropout = param_dropout, num_K = param_num_K)           