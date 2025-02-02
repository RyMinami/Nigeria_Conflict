
"""
SCRIPT: Conflict Event Classification and Visualization
DESCRIPTION:
This script processes and visualizes **conflict-related event data** 
by performing the following steps:
""""

# Import necessary libraries
import pandas as pd
import re
import matplotlib.pyplot as plt
import ace_tools as tools

# Step 1: Load the dataset
file_path = '/mnt/data/Conflict_evemt_text.csv'

try:
    data = pd.read_csv(file_path)
    print("Dataset successfully loaded.")
except FileNotFoundError:
    raise FileNotFoundError(f"Error: The file '{file_path}' was not found.")

# Check dataset structure
if 'NOTES' not in data.columns or 'YEAR' not in data.columns:
    raise ValueError("Dataset must contain 'NOTES' and 'YEAR' columns.")

# Display first few rows to verify data structure
print(data.head())

### **Step 2: Text Cleaning and Event Categorization**

# Create a dummy variable for Boko Haram mentions
data['Boko_Haram'] = data['NOTES'].apply(lambda x: 1 if 'Boko Haram' in str(x) else 0)

# Function to categorize events based on keywords
def categorize_event(notes):
    """
    Classifies events based on specific keywords in the event description.
    Returns a predefined category or 'Other' if no match is found.
    """
    notes = str(notes).lower()  # Convert text to lowercase
    
    # Define category keywords
    categories = {
        'Teacher or School': ['teacher', 'school', 'education', 'academic'],
        'Children and Student': ['children', 'student', 'kid', 'pupil'],
        'Kidnapping': ['kidnapping', 'abduct', 'hostage'],
        'Bombing': ['bombing', 'bomb', 'explosive', 'landmine', 'ied'],
        'Strike and Clash': ['strike', 'clash', 'conflict', 'fight', 'attack']
    }
    
    # Match keywords to categorize events
    for category, keywords in categories.items():
        if any(keyword in notes for keyword in keywords):
            return category
    return 'Other'

# Apply the categorization function
data['Event_Category'] = data['NOTES'].apply(categorize_event)

### **Step 3: Filter Data for Specific Years**
years_to_include = [2010, 2011, 2013, 2014, 2015, 2016, 2018, 2019]
filtered_data = data[data['YEAR'].isin(years_to_include)]

# Display the filtered dataset
tools.display_dataframe_to_user(name="Filtered Conflict Event Data", dataframe=filtered_data)

# Display first few rows of processed data
print(filtered_data.head())

### **Step 4: Generate a Bar Chart for Event Categories**
# Count the number of events in each category
event_counts = filtered_data['Event_Category'].value_counts()

# Check if data is available before plotting
if event_counts.empty:
    raise ValueError("No events found for the selected years. Please check the dataset.")

# Create a bar chart
plt.figure(figsize=(10, 6))
plt.bar(event_counts.index, event_counts.values, color=['#1f77b4', '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b'])
plt.xlabel('Event Category', fontsize=12)
plt.ylabel('Number of Events', fontsize=12)
plt.title('Number of Conflict Events by Category', fontsize=14, fontweight='bold')
plt.xticks(rotation=45, ha='right')
plt.grid(axis='y', linestyle='--', alpha=0.7)
plt.tight_layout()

# Display the chart
plt.show()

### **Step 5: Export Processed Data**
export_path = '/mnt/data/Filtered_Conflict_Event_Data.csv'
filtered_data.to_csv(export_path, index=False)

print(f"Filtered dataset successfully saved to: {export_path}")
