import tensorflow as tf
print(tf.__version__)
print(tf.config.list_physical_devices('GPU'))

from math import cos
from tqdm import tqdm
import os
import xarray as xr
import pandas as pd
import numpy as np

import matplotlib.pyplot as plt
##


# Define paths to your data
gps_file = 'data/Serengeti_all.csv'

# Load GPS data
gps_data = pd.read_csv(gps_file)
gps_data = gps_data[gps_data['case_'] == True]
gps_data['sp_m'] = gps_data['species'] + '_' + gps_data['migrant'].astype(str)
gps_data = gps_data[gps_data['sp_m'] == 'WB_migrant']
gps_data['date'] = pd.to_datetime(gps_data['date'])
gps_data['date'] = gps_data['date'].dt.date
gps_data['time1'] = pd.to_datetime(gps_data['t1_'])
gps_data['date1'] = gps_data['time1'].dt.date  # Extract only the date

gps_data = gps_data.sort_values(by=['ID','date'])

## 

ds = xr.open_dataset('data/serengeti_chirps.nc')
##

image_size = 32
step = 7
lag = 7

# Create an empty array to store image-GPS pairs
xdata = np.zeros((len(gps_data),image_size,image_size,lag))
ydata = np.zeros((len(gps_data),2))
years = np.zeros(len(gps_data))

i=0
for uid in tqdm(gps_data['ID'].unique()):
    current_data = gps_data[gps_data['ID'] == uid]
    # iterate through every other row
    for index, row in current_data.iterrows():

        # if index % step != 0:
        #     continue
        
        # Extract GPS coordinates and time from the DataFrame
        center_x = row['x1_']
        center_y = row['y1_']
        center_date = row['date1']
        # find the row that is step days later
        next_date = center_date + pd.Timedelta(days=step)
        next_data = current_data[current_data['date1'] == next_date]
        if len(next_data) == 0:
            continue
        next_x = next_data['x1_'].values[0]
        next_y = next_data['y1_'].values[0]
        ydata[i] = [next_x - center_x, next_y - center_y]

        # Convert latitude and longitude to indices in the xarray dataset
        x_index = np.abs(ds.x - center_x).argmin().values
        y_index = np.abs(ds.y - center_y).argmin().values

        # Find the index corresponding to the center_time in the time dimension
        time_index = np.abs(ds.time - np.datetime64(center_date)).argmin().values 
        if time_index - lag < 0:
            continue

        # Extract a subset of the dataset centered around the specified location for 1 week starting at the current time
        subset = ds.isel(x=slice(x_index - image_size//2, x_index + image_size//2),
                            y=slice(y_index - image_size//2, y_index + image_size//2),
                            time=slice(time_index - lag + 1, time_index+1))

        # Extract the 2D array of the variable you want to visualize
        data_array = subset.rainfall.values

        # Check for NaN and infinite values
        if np.isnan(data_array).any() or np.isinf(data_array).any():
            print(f"Warning: NaN or infinite values found in data_array for ID {uid}, index {index}, time_index {time_index}")
            print(f"NaN values: {np.sum(np.isnan(data_array))}")
            print(f"Infinite values: {np.sum(np.isinf(data_array))}")

        # Handle invalid values by replacing NaN and infinite values with 0
        # normalized_data = np.nan_to_num((data_array - np.min(data_array)) / (np.max(data_array) - np.min(data_array)))
        data_array = np.nan_to_num(data_array)
        
        # data_array = np.mean(data_array,axis=0)
        data_array = np.transpose(data_array,[1,2,0])
        xdata[i] = data_array #np.reshape(data_array, (image_size, image_size, 1))
        years[i] = center_date.year
        i += 1
        

##
print(i)
xdata = xdata[:i]
ydata = ydata[:i]
years = years[:i]
xdata = xdata.astype('float32')
xdata = xdata / np.percentile(xdata,95)
ydata = ydata.astype('float32')
ydata = ydata / np.linalg.norm(ydata,axis=1,keepdims=True)
##

# count the number of images for each year
unique_years, counts = np.unique(years, return_counts=True)
plt.bar(unique_years, counts)
plt.xlabel('Year')
plt.ylabel('Number of Images')
plt.show()
##

# use 2018 for testing and the rest for training
xtrain = xdata[years != 2018]
ytrain = ydata[years != 2018]
xtest = xdata[years == 2018]
ytest = ydata[years == 2018]



##
# Step 2: Preprocess Images and Combine with GPS Data

# Split the data into training and testing sets
# from sklearn.model_selection import train_test_split


# xtrain, xtest, ytrain, ytest = train_test_split(xdata, ydata, test_size=0.2, random_state=42)



##

# Step 3: Model-Specific Preprocessing
from tensorflow.keras import layers, models

# Define your model
image_input = layers.Input(shape=(image_size, image_size, lag), name='image_input')

# Image processing branch
x = layers.Conv2D(8, (3, 3), activation='relu', padding='same')(image_input)
x = layers.MaxPooling2D((2, 2))(x)
x = layers.Conv2D(8, (3, 3), activation='relu', padding='same')(x)
x = layers.MaxPooling2D((2, 2))(x)
x = layers.Conv2D(8, (3, 3), activation='relu', padding='same')(x)
x = layers.MaxPooling2D((2, 2))(x)
x = layers.Flatten()(x)
x = layers.Dense(16, activation='relu')(x)

# GPS information processing branch

# Combine the branches

# Output layer for turning angle prediction
output = layers.Dense(2, activation='linear')(x)

# keras layer for normalizing the output vector to have unit norm
output = layers.Lambda(lambda x: tf.keras.backend.l2_normalize(x, axis=1))(output)


# Define the model with multiple inputs
#model = models.Model(inputs=[image_input, gps_input], outputs=output)
model = models.Model(inputs=image_input, outputs=output)

##

from tensorflow.keras.optimizers import Adam

opt = Adam(learning_rate=1e-2) 
model.compile(optimizer=opt,loss=tf.keras.losses.CosineSimilarity(axis=-1)) 

# Display the model summary
model.summary()
##


# Step 4: Model Training

# fit with early stopping on the validation loss
from tensorflow.keras.callbacks import EarlyStopping

es = EarlyStopping(monitor='val_loss', mode='min', verbose=1, patience=5, restore_best_weights=True)

# Train the model with learning rate decay
model.fit(xtrain, ytrain, epochs=100, validation_data=(xtest, ytest), callbacks=[es], batch_size=512) 


##

# Step 5: Model Evaluation

# predict on the train set in batches of 512

for i in range(0,len(xtrain),512):
    preds = model.predict(xtrain[i:i+512],verbose=0)
    if i == 0:
        all_preds = preds
    else:
        all_preds = np.concatenate((all_preds,preds),axis=0)


a = np.arctan2(ytrain[:,1],ytrain[:,0]) - np.arctan2(all_preds[:,1],all_preds[:,0])

a[a > np.pi] = a[a > np.pi] - 2*np.pi
a[a < -np.pi] = a[a < -np.pi] + 2*np.pi

plt.hist(a,bins=50)
plt.title('Training Set (all years except 2018)')
plt.xlabel('Heading Error (radians)')
plt.ylabel('Number of Images')
plt.show()

##

preds = model.predict(xtest)


# plot a histogram of the difference between true and predicted angles 
a = np.arctan2(ytest[:,1],ytest[:,0]) - np.arctan2(preds[:,1],preds[:,0])

a[a > np.pi] = a[a > np.pi] - 2*np.pi
a[a < -np.pi] = a[a < -np.pi] + 2*np.pi

plt.hist(a,bins=50)
plt.title('Test Set (2018)')
plt.xlabel('Heading Error (radians)')
plt.ylabel('Number of Images')
plt.show()
##