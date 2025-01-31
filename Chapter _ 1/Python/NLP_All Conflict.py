
"""
SCRIPT: Conflict Event Classification Using NLP and Machine Learning
AUTHOR: [Your Name]
DATE: [Insert Date]
DESCRIPTION:
This script performs **automated text classification** of conflict-related events 
using **Natural Language Processing (NLP) and Machine Learning**.
It follows these steps:

1. **Preprocess the Data:**
   - Creates a dummy variable for **Boko Haram** mentions.
   - Cleans and processes text for improved classification.

2. **Categorize Events Based on Keywords:**
   - Uses keyword matching to classify events into predefined categories:
     - **Teacher or School**
     - **Children and Student**
     - **Kidnapping**
     - **Bombing**
     - **Strike and Clash**
     - **Other** (default)
   - Converts categorical labels into numerical format for ML.

3. **Filter Data for Specific Years:**
   - Extracts events from **2010, 2011, 2013, 2014, 2015, 2016, 2018, 2019**.

4. **Train a Machine Learning Model:**
   - Uses **TF-IDF Vectorization** for text representation.
   - Trains a **Random Forest Classifier** to predict event categories.

5. **Evaluate the Model:**
   - Generates a classification report with precision, recall, and F1-score.

6. **Export the Processed Data:**
   - Saves the **filtered and categorized dataset** for further analysis.
"""

# Import necessary libraries
import re
import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.ensemble import RandomForestClassifier
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import LabelEncoder
from sklearn.metrics import classification_report, accuracy_score, confusion_matrix

# Load the dataset (Assumed to be already loaded in a DataFrame called 'data')
# Ensure 'NOTES' and 'YEAR' columns exist in the dataset
if 'NOTES' not in data.columns or 'YEAR' not in data.columns:
    raise ValueError("Dataset must contain 'NOTES' and 'YEAR' columns.")

# Step 1: Create a dummy variable for Boko Haram mentions
data['Boko_Haram'] = data['NOTES'].apply(lambda x: 1 if 'Boko Haram' in x else 0)

# Step 2: Preprocess the text data
def preprocess_text(text):
    """
    Function to clean and preprocess text data.
    - Converts text to lowercase
    - Removes words with 1 or 2 characters
    - Removes punctuation
    - Removes extra whitespace
    - Handles missing values
    """
    if pd.isnull(text):
        return ""
    text = text.lower()  # Convert to lowercase
    text = re.sub(r'\b\w{1,2}\b', '', text)  # Remove short words (1-2 characters)
    text = re.sub(r'[^\w\s]', '', text)  # Remove punctuation
    text = re.sub(r'\s+', ' ', text).strip()  # Remove extra spaces
    return text

# Apply text preprocessing
data['Processed_Notes'] = data['NOTES'].apply(preprocess_text)

# Step 3: Define categories based on keywords
categories = {
    'Teacher or School': ['teacher', 'school', 'education', 'academic'],
    'Children and Student': ['children', 'student', 'kid', 'pupil'],
    'Kidnapping': ['kidnapping', 'abduct', 'hostage'],
    'Bombing': ['bombing', 'bomb', 'explosive', 'landmine', 'ied'],
    'Strike and Clash': ['strike', 'clash', 'conflict', 'fight', 'attack']
}

# Default category assignment
data['Event_Category'] = 'Other'

def categorize_event(notes):
    """
    Function to classify events based on keywords.
    Returns the first matched category or 'Other' if no match is found.
    """
    notes = notes.lower()
    for category, keywords in categories.items():
        if any(keyword in notes for keyword in keywords):
            return category
    return 'Other'

# Apply event categorization
data['Event_Category'] = data['Processed_Notes'].apply(categorize_event)

# Step 4: Encode categorical labels
label_encoder = LabelEncoder()
data['Encoded_Category'] = label_encoder.fit_transform(data['Event_Category'])

# Step 5: Filter data for specific years
years_to_include = [2010, 2011, 2013, 2014, 2015, 2016, 2018, 2019]
filtered_data = data[data['YEAR'].isin(years_to_include)]

# Prepare data for training
X = filtered_data['Processed_Notes']
y = filtered_data['Encoded_Category']

# Split the dataset into training and test sets (80-20 split)
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)

# Step 6: Create a text classification pipeline
pipeline = Pipeline([
    ('tfidf', TfidfVectorizer(max_features=5000)),  # Convert text to TF-IDF features
    ('clf', RandomForestClassifier(n_estimators=100, random_state=42))  # Random Forest classifier
])

# Train the model
pipeline.fit(X_train, y_train)

# Predict categories for test set
y_pred = pipeline.predict(X_test)

# Step 7: Evaluate the model
accuracy = accuracy_score(y_test, y_pred)
conf_matrix = confusion_matrix(y_test, y_pred)
classification_rep = classification_report(y_test, y_pred, target_names=label_encoder.classes_)

# Print evaluation metrics
print("Model Accuracy: {:.2f}%".format(accuracy * 100))
print("\nClassification Report:\n", classification_rep)
print("\nConfusion Matrix:\n", conf_matrix)

# Step 8: Save the classification report as a CSV file
classification_report_df = pd.DataFrame(classification_report(y_test, y_pred, output_dict=True)).transpose()
classification_report_path = '/mnt/data/Classification_Report.csv'
classification_report_df.to_csv(classification_report_path)

# Display filtered and categorized data
import ace_tools as tools
tools.display_dataframe_to_user(name="Filtered and Categorized Conflict Event Data", dataframe=filtered_data)

# Step 9: Export the filtered and categorized dataset
export_path = '/mnt/data/Filtered_Categorized_Conflict_Event_Data.csv'
filtered_data.to_csv(export_path, index=False)

# Return file paths for reference
classification_report_path, export_path