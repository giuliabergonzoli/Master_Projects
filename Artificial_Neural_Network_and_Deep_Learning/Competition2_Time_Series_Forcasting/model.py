import os
import tensorflow as tf
import numpy as np

def regression(model, x_temp, telescope):
    reg_predictions = np.array([])
    for reg in range(1, 865, telescope):
        pred_temp = model.predict(x_temp) 
        if (len(reg_predictions)==0):
            reg_predictions = pred_temp
        else:
            reg_predictions = np.concatenate((reg_predictions, pred_temp),axis=1)
        x_temp = np.concatenate((x_temp[:,telescope:,:],pred_temp), axis=1)	
    return reg_predictions

class model:
    def __init__(self, path):
        self.model_0 = tf.keras.models.load_model(os.path.join(path, 'SubmissionModel_0'))
        self.model_1 = tf.keras.models.load_model(os.path.join(path, 'SubmissionModel_1'))
        self.model_2 = tf.keras.models.load_model(os.path.join(path, 'SubmissionModel_2'))
        self.model_3 = tf.keras.models.load_model(os.path.join(path, 'SubmissionModel_3'))
        self.model_4 = tf.keras.models.load_model(os.path.join(path, 'SubmissionModel_4'))
        self.model_5 = tf.keras.models.load_model(os.path.join(path, 'SubmissionModel_5'))
        self.model_6 = tf.keras.models.load_model(os.path.join(path, 'SubmissionModel_6'))

    def predict(self, X):
        
        # Insert your preprocessing here
        X = X.numpy()
        Xmin = X.min(axis=0)
        Xmax = X.max(axis=0)
        
        #for each model: 0
        window_0=200
        telescope_0=8
        
        x_temp = X[-window_0:] 
        x_temp = (x_temp - Xmin) / (Xmax - Xmin) #normalization
        x_temp_0 = np.expand_dims(x_temp, axis=0)
        out_0 = np.array([])
        out_0 = regression(self.model_0, x_temp_0, telescope_0)
        
        #for each model: 1
        window_1=200
        telescope_1=8
        
        x_temp = X[-window_1:] 
        x_temp = (x_temp - Xmin) / (Xmax - Xmin) #normalization
        x_temp_1 = np.expand_dims(x_temp, axis=0)
        out_1 = np.array([])
        out_1 = regression(self.model_1, x_temp_1, telescope_1)
        
        #for each model: 2
        window_2=200
        telescope_2=8
        
        x_temp = X[-window_2:] 
        x_temp = (x_temp - Xmin) / (Xmax - Xmin) #normalization
        x_temp_2 = np.expand_dims(x_temp, axis=0)
        out_2 = np.array([])
        out_2 = regression(self.model_2, x_temp_2, telescope_2)
        
        #for each model: 3
        window_3=200
        telescope_3=8
        
        x_temp = X[-window_3:] 
        x_temp = (x_temp - Xmin) / (Xmax - Xmin) #normalization
        x_temp_3 = np.expand_dims(x_temp, axis=0)
        out_3 = np.array([])
        out_3 = regression(self.model_3, x_temp_3, telescope_3)
        
        #for each model: 4
        window_4=200
        telescope_4=8
        
        x_temp = X[-window_4:] 
        x_temp = (x_temp - Xmin) / (Xmax - Xmin) #normalization
        x_temp_4 = np.expand_dims(x_temp, axis=0)
        out_4 = np.array([])
        out_4 = regression(self.model_4, x_temp_4, telescope_4)
        
        #for each model: 5
        window_5=150
        telescope_5=1
        
        x_temp = X[-window_5:] 
        x_temp = (x_temp - Xmin) / (Xmax - Xmin) #normalization
        x_temp_5 = np.expand_dims(x_temp, axis=0)
        out_5 = np.array([])
        out_5 = regression(self.model_5, x_temp_5, telescope_5)
        
        #for each model: 6
        window_6=200
        telescope_6=8
        
        x_temp = X[-window_6:] 
        x_temp = (x_temp - Xmin) / (Xmax - Xmin) #normalization
        x_temp_6 = np.expand_dims(x_temp, axis=0)
        out_6 = np.array([])
        out_6 = regression(self.model_6, x_temp_6, telescope_6)       
        	
        # Insert your postprocessing here
              
        out_0 = out_0*(Xmax - Xmin) + Xmin
        out_1 = out_1*(Xmax - Xmin) + Xmin
        out_2 = out_2*(Xmax - Xmin) + Xmin
        out_3 = out_3*(Xmax - Xmin) + Xmin
        out_4 = out_4*(Xmax - Xmin) + Xmin
        out_5 = out_5*(Xmax - Xmin) + Xmin
        out_6 = out_6*(Xmax - Xmin) + Xmin
        
        out_0 = np.reshape(out_0, (864, 7))
        out_1 = np.reshape(out_1, (864, 7))
        out_2 = np.reshape(out_2, (864, 7))
        out_3 = np.reshape(out_3, (864, 7))
        out_4 = np.reshape(out_4, (864, 7))
        out_5 = np.reshape(out_5, (864, 7))
        out_6 = np.reshape(out_6, (864, 7))
        
        out = np.concatenate((out_0[:, [0]], out_1[:, [1]], out_2[:, [2]], out_3[:, [3]], out_4[:, [4]], out_5[:, [5]], out_6[:, [6]]), axis=1) 
        
        out = tf.convert_to_tensor(out)

        return out
