import tensorflow as tf

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from scipy import ndimage
from tensorflow.keras import layers, models
from tensorflow.keras.optimizers import Adam
from tensorflow.keras.callbacks import EarlyStopping
from tqdm import tqdm

# Define function to rotate images and headings
def rotate_image_and_heading(image, heading):
    height, width, channels = image.shape
    angle = np.random.randint(0, 360)  # Random angle in the range of 0 to 359 degrees
    rotated_images = np.zeros_like(image)
    for i in range(channels):
        rotated_images[:, :, i] = ndimage.rotate(image[:, :, i], angle, reshape=False)
    rotated_heading = np.dot([[np.cos(np.deg2rad(angle)), -np.sin(np.deg2rad(angle))],
                              [np.sin(np.deg2rad(angle)), np.cos(np.deg2rad(angle))]], heading)
    return rotated_images, rotated_heading

# Define function to train and visualize results
def train_and_visualize(gps_data, ds, year, image_size, step, lag):
    # Define your model
    image_input = layers.Input(shape=(image_size, image_size, lag), name='image_input')

    # Image processing branch
    x = layers.Conv2D(8, (3, 3), activation='relu', padding='same')(image_input)
    activation_map1 = layers.Activation('relu')(x)  # Save activation map for the first convolutional layer
    x = layers.MaxPooling2D((2, 2))(x)
    x = layers.Conv2D(8, (3, 3), activation='relu', padding='same')(x)
    activation_map2 = layers.Activation('relu')(x)  # Save activation map for the second convolutional layer
    x = layers.MaxPooling2D((2, 2))(x)
    x = layers.Conv2D(8, (3, 3), activation='relu', padding='same')(x)
    activation_map3 = layers.Activation('relu')(x)  # Save activation map for the third convolutional layer
    x = layers.MaxPooling2D((2, 2))(x)

    # Output layer for heading prediction
    x = layers.Flatten()(x)
    x = layers.Dense(16, activation='relu')(x)
    output = layers.Dense(2, activation='linear')(x)

    # Keras layer for normalizing the heading output vector to have unit norm
    output = layers.Lambda(lambda x: tf.keras.backend.l2_normalize(x, axis=1))(output)

    # Define the model with multiple outputs
    model = models.Model(inputs=image_input, outputs=output)

    # Compile the model with losses
    opt = Adam(learning_rate=1e-3)
    model.compile(optimizer=opt,loss=tf.keras.losses.CosineSimilarity(axis=-1))

    # Create an empty array to store image-GPS pairs
    xdata = np.zeros((len(gps_data), image_size, image_size, lag))
    ydata = np.zeros((len(gps_data), 2))
    years = np.zeros(len(gps_data))
    months = np.zeros(len(gps_data))
    id = np.empty(len(gps_data), dtype='U10')  # Initialize array to store IDs as strings

    ## create the datasets
    i = 0
    for uid in tqdm(gps_data['ID'].unique()):
        current_data = gps_data[gps_data['ID'] == uid]
        # iterate through every other row
        for index, row in current_data.iterrows():

            # Extract GPS coordinates and time from the DataFrame
            center_x = row['x1_']
            center_y = row['y1_']
            center_date = row['date']

            # find the row that is step days later
            next_date = center_date + pd.Timedelta(days=step)
            next_data = current_data[current_data['date'] == next_date]

            if len(next_data) == 0:
                continue

            next_x = next_data['x1_'].values[0]
            next_y = next_data['y1_'].values[0]

            # Store heading parameters in ydata arrays
            ydata[i] = [next_x - center_x,next_y - center_y]

            # Convert latitude and longitude to indices in the xarray dataset
            x_index = np.abs(ds.x - center_x).argmin().values
            y_index = np.abs(ds.y - center_y).argmin().values

            # Find the index corresponding to the center_time in the time dimension
            time_index = np.abs(ds.time - np.datetime64(center_date)).argmin().values
            if time_index - lag < 0:
                continue

            # Extract a subset of the dataset centered around the specified location based on lag value
            # Extract a subset of the dataset centered around the specified location for 1 week starting at the current time
            subset = ds.isel(x=slice(x_index - image_size//2, x_index + image_size//2),
                            y=slice(y_index - image_size//2, y_index + image_size//2),
                            time=slice(time_index - lag + 1, time_index+1))

            # Extract the 2D array of the variable you want to visualize
            #data_array = subset.rainfall.values
            data_array = subset.NDVI.values

            # Handle invalid values by replacing NaN and infinite values with 0
            data_array = np.nan_to_num(data_array)

            # Transpose the array to the required shape
            data_array = np.transpose(data_array, [1, 2, 0])

            # Store data_array in xdata
            xdata[i] = data_array
            years[i] = center_date.year
            months[i] = center_date.month
            id[i] = uid
            i += 1

    # Trim arrays to the actual number of valid samples
    xdata = xdata[:i]
    ydata = ydata[:i]
    years = years[:i]
    months = months[:i]
    id = id[:i]

    # Normalize the data
    xdata = xdata.astype('float32') / np.percentile(xdata, 95)
    ydata = ydata.astype('float32') / np.linalg.norm(ydata, axis=1, keepdims=True)

    # Initialize lists to store results
    results = []

    # Train and visualize results for non-rotated images
    xtrain = xdata[years != year]
    ytrain = ydata[years != year]

    xtest = xdata[years == year]
    ytest = ydata[years == year]

    # Your model training code here
    # fit with early stopping on the validation loss
    es = EarlyStopping(monitor='val_loss', mode='min', verbose=1, patience=10, restore_best_weights=True)

    model.fit(xtrain, ytrain, batch_size=512,  # Specify steps per epoch based on batch size
              epochs=100,
              shuffle=True,
              validation_data=(xtest, ytest),
              callbacks=[es])

    preds = model.predict(xtrain)
    true_angles = np.arctan2(ytrain[:, 0], ytrain[:, 1])
    predicted_angles = np.arctan2(preds[:, 0], preds[:, 1])
    heading_errors = predicted_angles - true_angles

    # Your visualization code here
    # Handle wrapping of angles
    heading_errors[heading_errors > np.pi] -= 2 * np.pi
    heading_errors[heading_errors < -np.pi] += 2 * np.pi
    
    # Append results to the list
    results.append(('Non-Rotated', true_angles, predicted_angles, heading_errors))

    # Train and visualize results for rotated images
    rotated_xdata = np.zeros_like(xdata)
    rotated_ydata = np.zeros_like(ydata)

    for i in range(len(xdata)):
        rotated_xdata[i], rotated_ydata[i] = rotate_image_and_heading(xdata[i], ydata[i])

    xtrain_rotated = rotated_xdata[years != year]
    ytrain_rotated = rotated_ydata[years != year]

    xtest_rotated = rotated_xdata[years == year]
    ytest_rotated = rotated_ydata[years == year]

    # Your model training code here for rotated images
    model.fit(xtrain_rotated, ytrain_rotated, batch_size=512,  # Specify steps per epoch based on batch size
              epochs=100,
              shuffle=True,
              validation_data=(xtest_rotated, ytest_rotated),
              callbacks=[es])

    preds_rotated = model.predict(xtrain_rotated)
    true_angles_rotated = np.arctan2(ytrain_rotated[:, 0], ytrain_rotated[:, 1])
    predicted_angles_rotated = np.arctan2(preds_rotated[:, 0], preds_rotated[:, 1])
    heading_errors_rotated = predicted_angles_rotated - true_angles_rotated

    # Handle wrapping of angles
    heading_errors_rotated[heading_errors_rotated > np.pi] -= 2 * np.pi
    heading_errors_rotated[heading_errors_rotated < -np.pi] += 2 * np.pi
    
    # Append results to the list
    results.append(('Rotated', true_angles_rotated, predicted_angles_rotated, heading_errors_rotated))

    return results