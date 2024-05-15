import os
import pandas as pd
from tqdm import tqdm
import numpy as np
from PIL import Image

# Define paths to folders containing images of each person
person_folders = ['ABHYUSH', 'AZAD', 'HARSHIT', 'SIDDHANT', 'SUMAIR', 'TANVEER']

# Define path where you want to save the CSV file
csv_path = 'Datasets/'
filename = 'Dataset.csv'

# Initialize DataFrame columns
image_size = (100, 100)
columns = [f'pix-{i}' for i in range(1, 1 + (image_size[0] * image_size[1]))] + ['class']
df = pd.DataFrame(columns=columns)

# Iterate through each person's folder
for class_value, folder in enumerate(person_folders, start=1):
    # Get list of files in the current folder
    files = os.listdir(os.path.join(csv_path, folder))

    # Iterate through each image in the folder
    for i, file in tqdm(enumerate(files, start=len(df) + 1)):
        img = Image.open(os.path.join(csv_path, folder, file))  # Load image
        pixel_data = np.array(img.getdata()).reshape(-1)  # Get pixel data

        # Check if pixel data length matches expected length
        if len(pixel_data) != len(columns) - 1:
            print(f"Skipping {file}. Length of pixel data: {len(pixel_data)}, Expected length: {len(columns) - 1}")
            continue  # Skip this iteration if lengths don't match

        # Concatenate pixel data with class value and add to DataFrame
        df.loc[i] = np.concatenate((pixel_data, [class_value]), axis=None)

# Save DataFrame to CSV
df.to_csv(os.path.join(csv_path, filename), index=False)
print('Task Completed')
