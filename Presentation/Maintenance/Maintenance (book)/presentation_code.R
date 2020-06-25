require('data.table')
require('stringr')
require('dplyr')
require('keras')



data = fread('D:/Rstudio/car_engine.csv', header = F)
data = data %>% rename(time = V1, amp = V2)

ts.plot(data$amp)



data = data %>% mutate(amp_scaled = (amp - max(amp)) / (max(amp) - min(amp)))
ts.plot(data$amp_scaled)
time_lag = 20
batch_size = 256
data = data %>% mutate(amp_lag = lag(amp_scaled, time_lag))


data_input = data %>% filter(!is.na(amp_lag)) %>% 
  as.matrix() %>% 
  array(dim = c(1200048, time_lag, 1))
data_input = data_input[21:1200048, , 1]
data_input = data_input %>% array_reshape(dim = c(1200028, 20, 1))


data_target = data %>% select(amp_lag) %>% 
  filter(!is.na(amp_lag)) %>% 
  as.matrix() %>% 
  array(dim = c(nrow(.), 1))



model_lstm = keras_model_sequential() %>% 
  layer_lstm(units = 256,
             input_shape = c(time_lag, 1),
             batch_size = batch_size,
             return_sequences = T,
             stateful = T) %>%
  layer_dropout(rate = 0.5) %>%
  layer_lstm(units = 128,
             return_sequences = T,
             stateful = T) %>%
  layer_dropout(rate = 0.5) %>%
  layer_lstm(units = 64,
             return_sequences = F,
             stateful = T) %>%
  layer_dropout(rate = 0.5) %>% 
  layer_dense(units = 1) %>% 
  compile(loss = 'mae', 
          optimizer = 'Nadam')


for(i in 1:1000){
  model_lstm %>% 
    fit(x = data_input,
        y = data_target,
        batch_size = 256,
        epochs = 1,
        verbose = 1,
        shuffle = FALSE)
  model_lstm %>% reset_states()
}




save_model_hdf5(model_lstm, 'D:/Rstudio/lstm_model_h5.h5')
save_model_tf(model_lstm, 'D:/Rstudio/lstm_model_tf.h5')
model_to_saved_model(model_lstm, 'D:/Rstudio/lstm_model.h5')