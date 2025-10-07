# -*- coding: utf-8 -*-

#____________________________________________________________________________________________________________________________
#______________________________________________ Functions __________________________________________________________________
#____________________________________________________________________________________________________________________________

# Convert a sequence to a supervised learning problem 
def timeseries_to_supervised(data, lag = 1, overlapping = True):
    df = pd.DataFrame(data)
    #columns = [df.shift(i) for i in range(1, lag + 1)]
    columns = [df.shift(i) for i in range(lag, 0, -1)]
    columns.append(df)
    df = pd.concat(columns, axis = 1)
    df = df[lag:]
    
    if (not overlapping):
        df = df.iloc[0::lag, :]
    
    df_values = df.values
    return df_values


# Crop a tensor on a given dimension from start to end
def crop(dim, start, end):
    # Crops (or slices) a Tensor on a given dimension from start to end
        def func(x):
            dimension = dim
            if dimension == -1:
                dimension = len(x.shape) - 1
            if dimension == 0:
                return x[start:end]
            if dimension == 1:
                return x[:, start:end]
            if dimension == 2:
                return x[:, :, start:end]
            if dimension == 3:
                return x[:, :, :, start:end]
            if dimension == 4:
                return x[:, :, :, :, start:end]
        return Lambda(func)


# Calculate the loss for the "reparam" variant
def calc_loss_reparam(y_true, y_pred):
    num_K = int(K.int_shape(y_pred)[1]/3)
    print(num_K)  
    if (num_K == 1):
        pred_p = crop(1, 0, num_K)(y_pred) # first num_K column(s) of output
        pred_mu = crop(1, num_K, 2*num_K)(y_pred) # second num_K column(s) of output
        pred_var = crop(1, 2*num_K, 3*num_K)(y_pred) # third num_K column(s) of output
        
        y = tf.keras.backend.cast(y_true, dtype = data_type)
        p = pred_p
        mu = pred_mu
        var = pred_var
    
        mask = tf.greater(y, 0)
        nonzero_y = tf.boolean_mask(y, mask)
        nonzero_p = tf.boolean_mask(p, mask)
        nonzero_mu = tf.boolean_mask(mu, mask)
        nonzero_var = tf.boolean_mask(var, mask)
        
        nonzero_alpha = tf.divide(K.pow(nonzero_mu, 2), nonzero_var)
        nonzero_beta = tf.divide(nonzero_mu, nonzero_var)
        
        gamma = tfp.distributions.Gamma(concentration = nonzero_alpha, rate = nonzero_beta)
        
        mask2 = K.equal(y, 0)
        zero_p = tf.boolean_mask(p, mask2)
        nonzero_loss = -K.log(nonzero_p) - gamma.log_prob(nonzero_y) # negative log-likelihood using Keras 
        zero_loss = -K.log((1 - zero_p))
    else:
        p1 = crop(1, 0, 1)(y_pred)
        y = tf.keras.backend.cast(y_true, dtype = data_type)
        
        mask = tf.greater(y, 0)
        nonzero_y = tf.boolean_mask(y, mask)
        nonzero_p1 = tf.boolean_mask(p1, mask)
        
        mask2 = K.equal(y, 0)
        zero_p1 = tf.boolean_mask(p1, mask2)
        zero_loss = -K.log(zero_p1)
        
        nonzero_like = 0
        nonzero_p_remain = 1 - nonzero_p1
        for i in range(num_K):
            if (i == (num_K - 1)):
                pk = 1        # for Gamma distribution K, the probability of drawing from it should be nonzero_remain * 1 
            else: 
                pk = crop(1, i + 1, i + 2)(y_pred)
                nonzero_pk = tf.boolean_mask(pk, mask)
            
            muk = crop(1, num_K + i, num_K + i + 1)(y_pred)
            vark = crop(1, num_K*2 + i, num_K*2 + i + 1)(y_pred)
            
            nonzero_muk = tf.boolean_mask(muk, mask)
            nonzero_vark = tf.boolean_mask(vark, mask)
            
            nonzero_alphak = tf.divide(K.pow(nonzero_muk, 2), nonzero_vark)
            nonzero_betak = tf.divide(nonzero_muk, nonzero_vark)
            
            gamma = tfp.distributions.Gamma(concentration = nonzero_alphak, rate = nonzero_betak)
            
            if (i == (num_K - 1)):
                nonzero_like = nonzero_like + nonzero_p_remain * gamma.prob(nonzero_y) # likelihood using Keras density function prob
            else:
                nonzero_like = nonzero_like + nonzero_p_remain * nonzero_pk * gamma.prob(nonzero_y) # likelihood using Keras density function prob
  
            nonzero_p_remain = nonzero_p_remain * (1 - nonzero_pk)
            
        nonzero_loss = -K.log(nonzero_like)
        

    loss = K.sum(nonzero_loss) + K.sum(zero_loss)
    return loss
    

    
# Custom loss function to be used for training the network
def custom_loss(y_true, y_pred):
    if variant == "orig":
        loss = calc_loss_orig(y_true, y_pred) 
    elif variant == "reparam":
        loss = calc_loss_reparam(y_true, y_pred) 
    elif variant == "orig-round":
        loss = calc_loss_orig_round(y_true, y_pred)
    else:
        loss = calc_loss_reparam_round(y_true, y_pred)
    return loss


# Calling the loss function for training the network
def gamma_likelihood2(output):
    return custom_loss


# Assigning a counter value for how frequently to write out the results during training
def get_counter_writeout(batch_size):
    if (write_every):
        counter_writeout = 1
    elif (write_out):
        if (batch_size < 50):
             counter_writeout = 10
        else:
            counter_writeout = 20
    else:
        counter_writeout = False
    return counter_writeout


# Main function for fitting the LSTM with CV
def fit_lstm_CV(train, val, test, batch_size, epoch, neurons, timesteps, stateful, learning_rate, file_begin, file_end, opt, dropout, num_K):
    # Create filenames
    filename_loss = file_begin + "loss" + file_end  + ".csv"
    filename_train_t = file_begin + "train-t" + file_end + ".csv"
    filename_val_t = file_begin + "val-t" + file_end + ".csv"
    filename_test_t = file_begin + "test-t" + file_end + ".csv"
    filename_train_v = file_begin + "train-v" + file_end + ".csv"
    filename_val_v = file_begin + "val-v" + file_end + ".csv"
    filename_test_v = file_begin + "test-v" + file_end + ".csv"
    
    dim_output = num_K * 3
    
    # Grab missing value indicators from first column, which will be used for loss calculations 
    # The missing value information is still inputted to the LSTM, it's just stored as lagged variables.
    missing_train = train[:,0]
    missing_val = val[:,0]
    missing_test = test[:, 0]
    train = np.delete(train, 0, 1)
    val = np.delete(val, 0, 1)
    test = np.delete(test, 0, 1)
    
    # Get data indices from last column, which will be used to keep track of the observations.
    inds_train = train[:, train.shape[1] - 1]
    inds_val = val[:, val.shape[1] - 1]
    inds_test = test[:, test.shape[1] - 1]
    train = np.delete(train, train.shape[1] - 1, 1)
    val = np.delete(val, val.shape[1] - 1, 1)
    test = np.delete(test, test.shape[1] - 1, 1)
    
    # Set up data for training 
    X = train[:, 0:-1]# the first timesteps columns (lagged) will be used to predict the last column
    y = train[:, -1]
    X = X.reshape(X.shape[0], timesteps, covariates_num + 1, order = "F") # nrow in training series, number of columns of features (timesteps), 1
    
    weight_train = np.ones(shape=(len(y),))
    if (covariates_NA):
        # Creating "weights" for the loss function (0 if an obs. is missing, 1 if not)
        weight_train[missing_train == 1] = 0
        # Creating indicators for missing values for the forecasting stage
        indicator_train = missing_train
        indicator_val = missing_val
        indicator_test = missing_test
        
    else:
        indicator_train = None
        indicator_val = None
        indicator_test = None
    
    # Set up architecture 
    input1 = Input(shape = (X.shape[1], X.shape[2]))
    if (return_states):
        lstm, state_ct, state_ht = LSTM(neurons, stateful = stateful, return_state = True)(inputs = input1)
    else:
        lstm = LSTM(neurons, stateful = stateful)(inputs = input1)
    dense = Dense(dim_output)(Dropout(dropout)(lstm))

    #dense = Dropout(dropout)(dense)
    # Note: The if-else statement is written for clarity, even though wlog we could use just one of the code blocks since the activations are the same, regardless of variant.
    if variant in "orig":
        lambda_p = Lambda(lambda x: tf.keras.activations.sigmoid(crop(1, 0, num_K)(x)))
        lambda_alpha = Lambda(lambda x: tf.keras.activations.softplus(crop(1, num_K, 2*num_K)(x)))
        lambda_beta = Lambda(lambda x: tf.keras.activations.softplus(crop(1, 2*num_K, 3*num_K)(x)))
        lambda_1 = lambda_p
        lambda_2 = lambda_alpha
        lambda_3 = lambda_beta
    else:
        lambda_p = Lambda(lambda x: tf.keras.activations.sigmoid(crop(1, 0, num_K)(x)))
        lambda_mu = Lambda(lambda x: tf.keras.activations.softplus(crop(1, num_K, 2*num_K)(x)))
        lambda_var = Lambda(lambda x: tf.keras.activations.softplus(crop(1, 2*num_K, 3*num_K)(x)))
        lambda_1 = lambda_p 
        lambda_2 = lambda_mu 
        lambda_3 = lambda_var
   
    output_1 = lambda_1(dense)
    output_2 = lambda_2(dense)
    output_3 =  lambda_3(dense)
    
    if (return_states):
        output = Concatenate()([output_1, output_2, output_3, state_ht, state_ct])
    else:
        output = Concatenate()([output_1, output_2, output_3])
    
    # Create model
    model = Model(inputs=input1, outputs=output)
    
    # Set up optimizer
    if (opt == "adam"):
        opt = Adam(learning_rate = learning_rate)
    else:
        opt = SGD(learning_rate = learning_rate)
        
    model.compile(optimizer = opt, loss = gamma_likelihood2(output), metrics = [gamma_likelihood2(output)])
    
    
    # Fit LSTM over the training epochs
    counter_writeout = get_counter_writeout(batch_size)
    counter_ES = 0
    train_loss_vec = list()
    val_loss_vec = list()
    test_loss_vec = list()
    
    for e in range(epoch):
        if (stateful):
            model.layers[1].reset_states(states)
        
        shuffle = not stateful
        
      
        fit = model.fit(X, y, sample_weight = weight_train, epochs = 1, batch_size = batch_size, verbose = 2, shuffle = shuffle)
           
            
        train_preds = forecast_call(model, train, batch_size, neurons, timesteps, stateful, indicator_train, inds_train, num_K)
        val_preds = forecast_call(model, val, batch_size, neurons, timesteps, stateful, indicator_val, inds_val, num_K)
        test_preds = forecast_call(model, test, batch_size, neurons, timesteps, stateful, indicator_test, inds_test, num_K)
        
        train_loss = sum(train_preds["loss"])
        val_loss = sum(val_preds["loss"])
        test_loss = sum(test_preds["loss"])
        
        print(train_loss)
        print(val_loss)
        print(test_loss)
        
        
        # Get weights for this epoch
        #weights_e = get_weights_e(model, neurons)
        #weights = pd.concat([weights, weights_e], axis = 0)
        
        # Variable description:
        #train_preds_t : training set predictions for best training model (model that achieves lowest loss in the training set)
        #val_preds_t   : validation set predictions for best training model (model that achieves lowest loss in the training set)
        #test_preds_t  : tessting set predictions for best training model (model that achieves lowest loss in the validation set)
        
        #train_preds_v : training set predictions for best validation model (model that achieves lowest loss in the validation set)
        #val_preds_v   : validation set predictions for best validation model (model that achieves lowest loss in the validation set)
        #test_preds_v  : testing set predictions for best validation model (model that achieves lowest loss in the validation set)
        
        
        if np.isfinite(train_loss):
            train_loss_vec.append(train_loss)
            val_loss_vec.append(val_loss)
            test_loss_vec.append(test_loss)
            loss_df = pd.DataFrame(list(zip(train_loss_vec, val_loss_vec, test_loss_vec)), columns = ['train','val', 'test'])
            if train_loss <= min(train_loss_vec):
                tf.keras.models.save_model(model, file_begin + "model-t" + file_end)
                train_preds_t = train_preds
                val_preds_t = val_preds
                test_preds_t = test_preds
             # If enabled, write out the results every so often. 
            if write_every:
                train_preds_t.to_csv(filename_train_t, index = False)
                val_preds_t.to_csv(filename_val_t, index = False)
                test_preds_t.to_csv(filename_test_t, index = False)
                loss_df.to_csv(filename_loss, index = False)
            elif (write_out and e!= 0):
                if (e % counter_writeout == 0):
                    train_preds_t.to_csv(filename_train_t, index = False)
                    val_preds_t.to_csv(filename_val_t, index = False)
                    test_preds_t.to_csv(filename_test_t, index = False)
                    loss_df.to_csv(filename_loss, index = False)
        else:
            if e == 0:
                flag_str = "Issue in first epoch: loss = " + str(train_loss)
                flag_df = pd.DataFrame([flag_str])
                return flag_df, flag_df
            else: 
                # If the training loss is NaN, it will stay NaN in future iterations, so return 
                # the most recent valid model. 
                return train_preds_t, val_preds_t, test_preds_t, train_preds_v, val_preds_v, test_preds_v, loss_df
        if val_loss > min(val_loss_vec):
            if early_stopping:
                # For early stopping, if the training loss exceeds the best achived loss, add to the counter.
                # When the counter reaches the specified patience level, training is stopped. 
                counter_ES = counter_ES + 1 
                if counter_ES >= patience:
                    return train_preds_t, val_preds_t, test_preds_t, train_preds_v, val_preds_v, test_preds_v, loss_df
        else:
            tf.keras.models.save_model(model, file_begin + "model-v" + file_end)
            train_preds_v = train_preds
            val_preds_v = val_preds
            test_preds_v = test_preds
            if write_every:
                train_preds_v.to_csv(filename_train_v, index = False)
                val_preds_v.to_csv(filename_val_v, index = False)
                test_preds_v.to_csv(filename_test_v, index = False)
            elif (write_out and e!= 0):
                if (e % counter_writeout == 0):
                    #write_loss(train_loss_vec, filename_loss)
                    train_preds_v.to_csv(filename_train_v, index = False)
                    val_preds_v.to_csv(filename_val_v, index = False)
                    test_preds_v.to_csv(filename_test_v, index = False)
                

    return train_preds_t, val_preds_t, test_preds_t, train_preds_v, val_preds_v, test_preds_v, loss_df


# Helper function for get_weights_e
def make_weight_name(weight_name, gate_name, neurons, U = False):
    ranges = range(1, neurons + 1)
    if (not U):
        names = [weight_name + gate_name + "_" + str(i) for i in ranges]
    else:
        names = [weight_name + gate_name + "_" + str(i) + str(j) for i,j in itertools.product(ranges, ranges)]
    return names


# Function to obtain weights and biases from the fitted LSTM
def get_weights_e(model, neurons):
    W = model.layers[1].get_weights()[0]
    U = model.layers[1].get_weights()[1]
    b = model.layers[1].get_weights()[2] 
    
    W_i = pd.DataFrame(W[:, :neurons], 
                       columns = make_weight_name("W", "i", neurons))
    W_f = pd.DataFrame(W[:, neurons: neurons * 2], 
                       columns = make_weight_name("W", "f", neurons))
    W_c = pd.DataFrame(W[:, neurons * 2: neurons * 3], 
                       columns = make_weight_name("W", "c", neurons))
    W_o = pd.DataFrame(W[:, neurons * 3:], 
                       columns = make_weight_name("W", "o", neurons))

    U_i = pd.DataFrame(U[:, :neurons].reshape(1, neurons**2), 
                       columns = make_weight_name("U", "i", neurons, True))
    U_f = pd.DataFrame(U[:, neurons: neurons * 2].reshape(1, neurons**2), 
                       columns = make_weight_name("U", "f", neurons, True))
    U_c = pd.DataFrame(U[:, neurons * 2: neurons * 3].reshape(1, neurons**2), 
                       columns = make_weight_name("U", "c", neurons, True))
    U_o = pd.DataFrame(U[:, neurons * 3:].reshape(1, neurons**2), 
                       columns = make_weight_name("U", "o", neurons, True))

    b_i = pd.DataFrame(b[:neurons].reshape(1, neurons), 
                       columns = make_weight_name("b", "i", neurons))
    b_f = pd.DataFrame(b[neurons: neurons * 2].reshape(1, neurons), 
                       columns = make_weight_name("b", "f", neurons))
    b_c = pd.DataFrame(b[neurons * 2: neurons * 3].reshape(1, neurons), 
                       columns = make_weight_name("b", "c", neurons))
    b_o = pd.DataFrame(b[neurons * 3:].reshape(1, neurons), 
                       columns = make_weight_name("b", "o", neurons))
    
    df = pd.concat([W_i, W_f, W_c, W_o, b_i, b_f, b_c, b_o, U_i, U_f, U_c, U_o], axis = 1)
    
    return df


# Helper function for get_weights_e function
def make_weight_colnames(weight_name, gate_name, neurons):
    gates = ["i", "f", "c", "o"] # We have input, forget, cell state, and output gates in a basic LSTM.
    names_W = [make_weight_name("W", str(i), neurons) for i in gates]
    names_b = [make_weight_name("b", str(i), neurons) for i in gates]
    names_U = [[make_weight_name("W", str(i), neurons, str(j)) for i in gates] for j in range(1, neurons + 1)]
    
    
    merged = [names_W, names_b]
    W = sum(names_W, [])
    U = sum(names_U, [])
    b = sum(names_b, [])
    
    return(np.concatenate([W, b]))


# Function to name the states 
def get_colnames_states(list1, neurons):
    h_names = list()
    c_names = list()
    for i in range(neurons):
        h_names.append("h_%d" % (i + 1))
        c_names.append("c_%d" % (i + 1))
    names = list1 + h_names + c_names
    
    return(names)


# Function to name the dataframe of predictions
def get_colnames(variant, num_K):
    if "orig" in variant:
        if (num_K == 1):
            colnames=["p", "alpha", "beta"]
        else:
            name_p = list(map(lambda x: "p" + str(x), range(1, num_K + 1)))
            name_alpha = list(map(lambda x: "alpha" + str(x), range(1, num_K + 1)))
            name_beta = list(map(lambda x: "beta" + str(x), range(1, num_K + 1)))
            colnames = name_p + name_alpha + name_beta
            #colnames=["p1", "p2", "alpha1", "alpha2", "beta1", "beta2"] # e.g., for K = 2
    else:
        if (num_K == 1):
            colnames=["p", "mu", "var"]
        else:
            name_p = list(map(lambda x: "p" + str(x), range(1, num_K + 1)))
            name_mu = list(map(lambda x: "mu" + str(x), range(1, num_K + 1)))
            name_var = list(map(lambda x: "var" + str(x), range(1, num_K + 1)))
            colnames = name_p + name_mu + name_var
            #colnames=["p1", "p2", "mu1", "mu2", "var1", "var2"] # e.g., for K = 2
    return colnames


# Function to make predictions from the LSTM, used by forecast_call
def forecast_lstm(model, X, timesteps, neurons = None):
    X = X.reshape(X.shape[0], timesteps, covariates_num + 1, order = "F")
    yhat = model.predict(X, verbose = 0)
  
    return yhat  


# Main function for obtaining the LSTM predictions
def forecast_call(model, data, batch_size, neurons, timesteps, stateful = True, indicator = None, inds_data = None, num_K = 2):
    dim_output = num_K * 3 
    if (stateful):
        model.layers[1].reset_states(states)
        
    predictions = forecast_lstm(model, data[:, 0:-1], timesteps, neurons)
    # Get names for the columns that will be used later
    colnames = get_colnames(variant, num_K)
    
    if (return_states):
        num_vars = neurons * 2 + dim_output
        colnames = get_colnames_states(colnames, neurons)
    else:
        num_vars = dim_output
        
    predictions_arr = np.reshape(predictions, (len(data), num_vars)) 
    predictions_df = pd.DataFrame(data=predictions_arr, columns=colnames)
    loss_vec = list()
    if (indicator is None):
        for i in range(len(data)):
            pred_i = predictions_df.iloc[i]
            if (num_K == 1):
                loss = log_likelihood(data[i, -1], pred_i[0], pred_i[1], pred_i[2], num_K)
            else:
                loss = log_likelihood(data[i, -1], pred_i[0:num_K], pred_i[num_K:(2*num_K)], pred_i[(2*num_K):(3*num_K)], num_K)
            loss_vec.append(loss)
    else:
        for i in range(len(data)):
            pred_i = predictions_df.iloc[i]
            if (indicator[i] == 1):
                loss = 0
            else:
                if (num_K == 1):
                    loss = log_likelihood(data[i, -1], pred_i[0], pred_i[1], pred_i[2], num_K)
                else:
                    loss = log_likelihood(data[i, -1], pred_i[0:num_K], pred_i[num_K:(2*num_K)], pred_i[(2*num_K):(3*num_K)], num_K)
            loss_vec.append(loss)
      
    
    loss_df = pd.DataFrame(data = loss_vec, columns=["loss"])
    truth_df = pd.DataFrame(data[:, -1], columns=["y_true"])
    predictions_df = pd.concat([predictions_df, truth_df, loss_df], axis = 1)
    
    if inds_data is not None:
        inds_df = pd.DataFrame(data = inds_data, columns=["inds"])
        predictions_df = pd.concat([predictions_df, inds_df], axis = 1)
    
    return predictions_df


# Write out loss dataframe
def write_loss(loss_history, filename):
    loss_df = pd.DataFrame(data = loss_history, columns = ["loss"])
    loss_df.to_csv(filename, index = False) 
    

# Write out weights dataframe
def write_weights(weights, filename):
    weights.to_csv(filename, index = False) 
    

# Write out dataframe of runtime
def write_report(time_vec, mem_vec, filename):
    info = [[time_vec, mem_vec]]
    info_df = pd.DataFrame(data = info, columns = ["total_time", "peak_mem"])
    info_df.to_csv(filename, index = False)
    

# Calculate loss after training
def log_likelihood(y, output1, output2, output3, num_K):
    if "orig" in variant:
        p = output1
        alpha = output2
        beta = output3
    else:
        p = output1
        mu = output2
        var = output3
        if num_K == 1:
            alpha = mu**2/var 
            beta = mu/var
        else:
            alpha = [mu[i]**2/var[i] for i in range(len(mu))]
            beta = [mu[i]/var[i] for i in range(len(mu))]
    
    if (num_K == 1):
        if (y > 0):
            if (alpha == 0.0):
                return(float("inf"))
            else:    
                loss = -np.log(p) - alpha * np.log(beta) + np.log(math.gamma(alpha)) + (1 - alpha)*np.log(y) + beta*y
        else:
            loss = -np.log(1 - p)
    else:
        if (y > 0):
            if (alpha[0] == 0.0 and alpha[1] == 0.0):
                return(np.array([float("inf"), float("inf")]))
            else:
                loss = -np.log((1 - p[0]) * p[1]*beta[0]**alpha[0]*y**(alpha[0] - 1)*np.e**(-beta[0]*y)/math.gamma(alpha[0]) +
                               (1 - p[0]) * (1 - p[1])*beta[1]**alpha[1]*y**(alpha[1] - 1)*np.e**(-beta[1]*y)/math.gamma(alpha[1])) 
        else:
            loss = -np.log(p[0])
    return loss


# Construct a filename corresponding to all parameter values
def construct_filename(timesteps, batch_size, epochs, neurons, stateful, learning_rate, opt, dropout, num_K):     
    covariates = covariates_year or covariates_season
    # save filenames corresponding to all parameter values (except r, the number of trials)
    gfile_end = "_steps%d_h%d_batch%d_epoch%d_stateful%d_LR%s_opt%s_cov%d_K%d_dropout%s_%s_" % (timesteps, neurons, batch_size, epochs, stateful, str(learning_rate), opt, covariates, num_K, str(dropout), variant)    
    return gfile_end


# Structure the data for fitting the LSTM
def setup_data(series, timesteps, gen = False):
    # series is a (possibly multivariate) time series
    if (series.shape[1] > 1):
        if (covariates_season and covariates_year):
            # Covariates (lagged) are to be included 
            season = series["day_of_year"].values
            year = series["year"].values
            if (covariates_norm):
                season = season/len(np.unique(season))
                year = (year - min(year) + 1)/len(np.unique(year))
            cov1 = timeseries_to_supervised(season, timesteps, overlapping)[:, :(timesteps)]
            cov2 = timeseries_to_supervised(year, timesteps, overlapping)[:, :(timesteps)]
            cov_supervised = np.column_stack([cov1, cov2])
            if (covariates_NA):
                    # Lag the missing indicator, which will be used to mark missing values that are in the input sequences. 
                    missing_lag = timeseries_to_supervised(series["missing"], timesteps, overlapping)[:, :(timesteps)]
                    if gen:
                        # In the generation stage, we don't want the missing values to be regarded at all. 
                        missing_gen = np.full(shape = missing_lag.shape, fill_value = 0)
                        cov_supervised = np.column_stack([cov_supervised, missing_gen])     
                    else:
                        # In the fitting state, we want a missing value indicator to mark which outputs 
                        # should be disregarded for calculating the loss. 
                        missing_ind = series["missing"][timesteps:,]
                        cov_supervised = np.column_stack([missing_ind, cov_supervised, missing_lag])     
        elif (covariates_year):
            year = series["year"].values
             # Covariates (lagged) are to be included 
            if (covariates_norm):
                year = (year - min(year) + 1)/len(np.unique(year))
            cov1 = timeseries_to_supervised(year, timesteps, overlapping)[:, :(timesteps)]
            cov_supervised = cov1
     
    y = series.values[:, 0]
    y_supervised = timeseries_to_supervised(y, timesteps, overlapping) #lags training data by 1, ..., timesteps (1st timesteps columns predict last column)
    
    inds_b0 = np.array(range(timesteps + 1, len(y) + 1)) # This will be used to keep track of the data, if we go on to bootstrap.
    
    if (series.shape[1] > 1):
        data_supervised = np.column_stack([cov_supervised, y_supervised, inds_b0])
        return data_supervised
        

# Structure the data for using the training, validation, and testing sets and to fit the LSTM
def setup_data_CV(series, timesteps, gen = False):
    # series is a (possibly multivariate) time series
    if (series.shape[1] > 1):
        if (covariates_season and covariates_year):
            # Covariates (lagged) are to be included 
            season = series["day_of_year"].values
            year = series["year"].values
            year_u = series["year"]  # year unscaled
            if (covariates_norm):
                season = season/len(np.unique(season))
                year = (year - min(year) + 1)/len(np.unique(year))
            cov1 = timeseries_to_supervised(season, timesteps, overlapping)[:, :(timesteps)]
            cov2 = timeseries_to_supervised(year, timesteps, overlapping)[:, :(timesteps)]
            cov_supervised = np.column_stack([cov1, cov2])
            if (covariates_NA):
                    # Lag the missing indicator, which will be used to mark missing values that are in the input sequences. 
                    missing_lag = timeseries_to_supervised(series["missing"], timesteps, overlapping)[:, :(timesteps)]
                    if gen:
                        # In the generation stage, we don't want the missing values to be regarded at all. 
                        missing_gen = np.full(shape = missing_lag.shape, fill_value = 0)
                        cov_supervised = np.column_stack([cov_supervised, missing_gen])     
                    else:
                        # In the fitting state, we want a missing value indicator to mark which outputs 
                        # should be disregarded for calculating the loss. 
                        missing_ind = series["missing"][timesteps:,]
                        cov_supervised = np.column_stack([missing_ind, cov_supervised, missing_lag])     
        elif (covariates_year):
            year = series["year"].values
             # Covariates (lagged) are to be included 
            if (covariates_norm):
                year = (year - min(year) + 1)/len(np.unique(year))
            cov1 = timeseries_to_supervised(year, timesteps, overlapping)[:, :(timesteps)]
            cov_supervised = cov1
     
    y = series.values[:, 0]
    y_supervised = timeseries_to_supervised(y, timesteps, overlapping) #lags training data by 1, ..., timesteps (1st timesteps columns predict last column)
    
    inds_b0 = np.array(range(timesteps + 1, len(y) + 1)) # Indices of data after lagging, which will be used to keep track of data.
     
    if (series.shape[1] > 1):
        data_supervised = np.column_stack([cov_supervised, y_supervised, inds_b0])
        if (covariates_year):
            year_u = year_u[inds_b0 - 1]
            year_u.reset_index(drop = True, inplace = True)
            year_u = year_u.astype(int)
            
            num_yrs = len(np.unique(series["year"]))
                
             # Split data into training, validation, and testing sets 
            num_yrs_train = 4
            num_yrs_val = 3
            num_yrs_test = 3
            #num_yrs_train = 4
            #num_yrs_val = 4
            #num_yrs_test = 2
            start_yr = year_u[0]
            
            # Code that grabs the exact years
            val_l = list(range(num_yrs_train + start_yr, num_yrs + start_yr, 10))
            test_l = list(range(num_yrs_train + num_yrs_val + start_yr, num_yrs + start_yr, 10))
            val_yrs = [list(range(val_l[i], val_l[i] + num_yrs_val))  for i in range(len(val_l))]
            test_yrs = [list(range(test_l[i], test_l[i] + num_yrs_test))  for i in range(len(test_l))]
            val_yrs = list(itertools.chain(*val_yrs))
            test_yrs = list(itertools.chain(*test_yrs))
            
            inds = pd.Series(range(len(data_supervised)))
            inds_val = inds[year_u.isin(val_yrs)].tolist()
            inds_test = inds[year_u.isin(test_yrs)].tolist()
            
            if (max(inds_val) > len(data_supervised)):
                inds_val = [i for i in inds_val if i < len(data_supervised)]
            
            val = data_supervised[inds_val, :]
            test = data_supervised[inds_test, :]
            train = np.delete(data_supervised, list(np.sort(inds_val + inds_test)), 0)  
            print(train.shape)
            print(val.shape)
            print(test.shape)
            
            return train, val, test
        else:
            # Case not used for CV
            return data_supervised
    else:
        # Case not used for CV
        return y_supervised
        
    
# Main functon for running the LSTM with the scheme for out-of-sample evaluation
def run_lstm_CV(series, timesteps, batch_size, epochs, neurons, stateful, learning_rate, name, opt, dropout, num_K):
    # Form lagged data that will be used as input to the LSTM
    train, val, test = setup_data_CV(series, timesteps)           

    if (batch_size == "full"):
        batch_size = train.shape[0] 
    
    # Trim the training set so that the batchsize divides the sample size.
    remainder = train.shape[0] % batch_size
    if remainder != 0:
        train = train[:-remainder, :] # remove the observation(s) from the end of the time series
        
    if stateful:
        # The validation and testing set only have to be trimmed to ensure batch_size divides the sample size.  
        # if we are training with stateful = True.)
         remainder = val.shape[0] % batch_size
         if remainder != 0:
             val = val[:-remainder, :] # remove the observation(s) from the end of the time series
         remainder = test.shape[0] % batch_size
         if remainder != 0:
             test = test[:-remainder, :] # remove the observation(s) from the end of the time series
    
            
    gfile_end = construct_filename(timesteps, batch_size, epochs, neurons, stateful, learning_rate, opt, dropout, num_K)     
    
    
    # set other file names that correspond to thfe specific trial r 
    file_end = gfile_end + "b%d_v%d" % (param_seed_b, param_seed_v)  
    file_begin = output_path + name + "_"
    
    filename_loss = file_begin + "loss" + file_end  + ".csv"
    filename_train_t = file_begin + "train-t" + file_end + ".csv"
    filename_val_t = file_begin + "val-t" + file_end + ".csv"
    filename_test_t = file_begin + "test-t" + file_end + ".csv"
    filename_train_v = file_begin + "train-v" + file_end + ".csv"
    filename_val_v = file_begin + "val-v" + file_end + ".csv"
    filename_test_v = file_begin + "test-v" + file_end + ".csv"
    filename_report = file_begin + "report" + file_end + ".csv"
    
    
    print(filename_train_t)
    
    # Fit model 
    begin = time.time()
    lstm_model = fit_lstm_CV(train, val, test, batch_size, epochs, neurons, timesteps, stateful, learning_rate, file_begin, file_end, opt, dropout, num_K)
    end = time.time()
    
    total_time = end - begin
    peak_mem = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss # in kb on Linux, in bytes on MacOS
    peak_mem = peak_mem/(10**6)
    
    
    write_report(total_time, peak_mem, filename_report)
    
    
    train_preds_t = lstm_model[0]
    val_preds_t = lstm_model[1]
    test_preds_t = lstm_model[2]
    
    train_preds_v = lstm_model[3]
    val_preds_v = lstm_model[4]
    test_preds_v = lstm_model[5]
    
    loss_df = lstm_model[6]

    train_preds_t.to_csv(filename_train_t, index = False)
    val_preds_t.to_csv(filename_val_t, index = False)
    test_preds_t.to_csv(filename_test_t, index = False)
    
    train_preds_v.to_csv(filename_train_v, index = False)
    val_preds_v.to_csv(filename_val_v, index = False)
    test_preds_v.to_csv(filename_test_v, index = False)
    
    loss_df.to_csv(filename_loss, index = False)
    
           