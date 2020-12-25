#Part 4. Forecasting the treatment result
#Loading package
import csv
import pandas as pd
import numpy as np
import tensorflow as tf
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import Dense, LSTM, Bidirectional
import itertools
from sklearn.model_selection import train_test_split
from keras.utils import to_categorical
from keras.utils import to_categorical
from sklearn.preprocessing import LabelEncoder
from sklearn import preprocessing
from numpy import mean,sqrt,square
from tensorflow.keras.metrics import RootMeanSquaredError
import matplotlib.pyplot as plt

#Split the dataset and apply one-hot-encoding
def init_data_split(train_df, applied_df):
    train_df = train_df.drop_duplicates(subset=["PatientID", "VisitDay"], keep='first')
    applied_df = applied_df.drop_duplicates(subset=["PatientID", "VisitDay"], keep='first')
    cut_index = len(train_df)
    df_combined = pd.concat([train_df, applied_df])

def init_data_split(train_df, applied_df):
    train_df = train_df.drop_duplicates(subset=["PatientID", "VisitDay"], keep='first')
    applied_df = applied_df.drop_duplicates(subset=["PatientID", "VisitDay"], keep='first')
    cut_index = len(train_df)
    df_combined = pd.concat([train_df, applied_df])
    #Perform One-Hot encoding on Country and TxGroup
    label_encoder = LabelEncoder()
    country_encoded = label_encoder.fit_transform(df_combined['Country'].to_numpy())
    txgroup_encoded = label_encoder.fit_transform(df_combined['TxGroup'].to_numpy())
    country_ohc = to_categorical(country_encoded)
    txgroup_ohc = to_categorical(txgroup_encoded)
    merged_array = np.hstack((country_ohc,txgroup_ohc))
    df_ohc = pd.DataFrame(data=merged_array)
    df_combined = df_combined.drop(['Study','SiteID','RaterID','AssessmentID','PANSS_Total','Country','TxGroup'],axis=1)
    df_combined = pd.concat([df_combined.reset_index(drop=True),df_ohc.reset_index(drop=True)],axis=1)
    return(df_combined,cut_index)

def count_max_occur(df):
    cur_patient_id = df.iloc[0,2]
    cur_count = 0
    max_count = 0
    max_id = cur_patient_id
    for i in range(0,len(df)):
        if df.iloc[i,0]==cur_patient_id:
            cur_count += 1
        else:
            if max_count < cur_count:
                max_count = cur_count
                max_id = cur_patient_id
            cur_patient_id = df.iloc[i,0]
            cur_count = 1
        return max_count,max_id

def extract_y(df):
    y_mtx = []
    cur_patient_id = df.iloc[0,0]
    for i in range(0,len(df)):
        print("preprocessing iteration",i,"/",len(df)," [1/2")
        if i != len(df)-1:
            if df.iloc[i+1,0] != cur_patient_id:
                y_mtx = np.append(y_mtx, df.iloc[i,2:32].tolist())
                cur_patient_id = df.iloc[i+1,0]
            else:
                y_mtx = np.append(y_mtx,df.iloc[i,2:32].tolist())
    y_mtx = np.array(y_mtx)
    return(y_mtx)

#Obtain the input (feature) of the training set
def extract_x(inp_df,max_count,cut_index):
    patient_id = inp_df['PatientID']
    #Minmax normalization
    inp_df = inp_df.drop(['PatientID'],axis=1)
    df_scaled = (inp_df-inp_df.min())/(inp_df.max()-inp_df.min())
    inp_df = pd.concat([patient_id, df_scaled],axis=1)
    x_train_df = inp_df[:cut_index]
    x_applied_df = inp_df[cut_index:]
    print(x_train_df.to_numpy().shape)
    print(x_applied_df.to_numpy().shape)
    x_combined = [x_train_df,x_applied_df]
    x_result=[]
    for df in x_combined:
        x_mtx = []
        cur_x_mtx = []
        cur_patient_id = df.iloc[0,0]
        cur_iter = 0
        row_length = len(df.drop(['PatientID'],axis=1).iloc[0, ].tolist())
        for i in range(0,len(df)):
            print("preprocessing iteration",i,"/",len(df),"[2/2]")
            cur_iter += 1
            if i != len(df)-1:
                #check if next patient in list is still the same patient
                if df.iloc[i+1,0]!= cur_patient_id:
                    #Pad with 0 if there are empty row
                    while (cur_iter <= max_count):
                        masked_row = [0]*row_length
                        cur_x_mtx.insert(0,masked_row)
                        cur_iter += 1
                    cur_iter = 0
                    # todo fix append
                    x_mtx = np.append(x_mtx,cur_x_mtx)
                    cur_x_mtx = []
                    cur_patient_id = df.iloc[i+1,0]
                else:
                    if(cur_iter <= max_count):
                        cur_x_mtx = np.append(cur_x_mtx, df.drop(['PatientID'],axis=1).iloc[i,].tolist())
            else:
                #Pad with 0 if there are empty row
                while(cur_iter <= max_count):
                    masked_row = [0]*row_lengthe
                    cur_x_mtx.insert(0,masked_row)
                    cur_iter += 1
                    cur_iter = 0
                    x_mtx = np.append(x_mtx,cur_x_mtx)
            #convert to numpy array
            x_mtx = np.array(x_mtx)
            x_result = np.append(x_result,x_mtx)
        #return x and y matrices
        return (x_result[0],x_result[1])

#Create RNN with one Bidirectional Layer and one dense layer
def createRNN(x_train,y_train,epoch):
    model = Sequential()
    model.add(Bidirectional(LSTM(30,activation='tanh'),input_shape=(x_train.shape[1:])))
    #30 PANSS symptom score and no activation function
    model.add(Dense(y_train.shape[1]))
    model.compile(loss="mse",optimizer="adam",metrics=[
        tf.keras.metrics.RootMeanSquareError()])
    model.fit(x_train,y_train,epochs=epoch,batch_size=5,verbose=1,validation_split=0.1)
    return model

def main():
    df_A = pd.read_csv('Study_A.csv')
    df_B = pd.read_csv('Study_B.csv')
    df_C = pd.read_csv('Study_C.csv')
    df_D = pd.read_csv('Study_D.csv')
    df_E = pd.read_csv('Study_E.csv')

    df = pd.concat([df_A,df_B,df_C,df_D])
    df.to_csv(r'combinedstudy.csv',index=False)
    #Filter the training model to include patients who finished 18 weeks of study
    train_df = df[df.LeadStatus == 'Passed']
    train_df = train_df[train_df.PatientID.isin(train_df[train_df["VisitDay"]>120]["PatientID"].values.tolist())]
    train_df = train_df.drop(['LeadStatus'],axis=1)
    df_combined,cut_index = init_data_split(train_df,df_E)
    max_count,max_id = count_max_occur(df_combined)
    try:
        #Attempt to load the numpy array
        print("Attempting to open train numpy file...")
        with open('preproc_data.npy','rb') as f:
            x_applied_mtx = np.load(f)
            x_mtx = np.load(f)
            y_mtx = np.load(f)
    except:
        #Preprocess data and save it
        print("No train numpy file found, preprocessing data...")
        y_mtx = extract_y(df_combined[:cut_index])
        x_mtx , x_applied_mtx = extract_x(df_combined,max_count,cut_index)
        with open('preproc_data.npy','wb') as f:
            np.save(f,x_applied_mtx)
            np.save(f,x_mtx)
            np.save(f,y_mtx)
        x_train, x_test, y_train, y_test = train_test_split(x_mtx,y_mtx,test_size =0.2,random_state=21314)

        #Obtain test MSE from study A-D
        y_test = np.sum(y_test,axis = 1)
        model = createRNN(x_train,y_train,80)
        ypred = model.predict(x_test)
        ypred[ypred < 1] = 1
        ypred[ypred > 7] = 7
        ypred = np.sum(ypred, axis=1)
        ypred = np.round(ypred,0)
        print("RMSE error:",np.sqrt(np.mean((y_test-ypred)**2)))

        #Obtain the prediction on study E
        model = createRNN(x_mtx,y_mtx,80)
        ypred = model.predict(x_applied_mtx)
        ypred[ypred < 1] = 1
        ypred[ypred > 7] = 7
        ypred = np.sum(ypred,axis=1)
        ypred = np.round(ypred,0)
        print(ypred)
        print(ypred.shape)
        y_result = pd.DataFrame(data=ypred,columns=["PANSS_Total"])
        y_result.to_csv(r'result.csv',index = False)

if __name__ == "__main__":
    main()


