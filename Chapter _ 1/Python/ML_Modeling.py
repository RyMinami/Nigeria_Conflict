
"""
SCRIPT: Conflict Event Classification - Model Comparison
AUTHOR: [Your Name]
DATE: [Insert Date]
DESCRIPTION:
This script evaluates multiple **machine learning classifiers** for conflict 
event classification. It follows these steps:

1. **Define and Train Models**:
   - **Logistic Regression**
   - **Support Vector Machine (SVM)**
   - **Naive Bayes**
   - **Gradient Boosting Classifier**
   
2. **Evaluate Model Performance**:
   - Uses **Precision, Recall, and F1-Score**.
   - Stores results in a structured dictionary.

3. **Compare Models and Visualize Results**:
   - Converts results into a **DataFrame** for comparison.
   - Generates a **bar chart** to compare F1-Scores across models.
"""

# Import necessary libraries
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.pipeline import Pipeline
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.linear_model import LogisticRegression
from sklearn.svm import SVC
from sklearn.naive_bayes import MultinomialNB
from sklearn.ensemble import GradientBoostingClassifier
from sklearn.metrics import classification_report

# Prepare results dictionary
results = {}

# Function to evaluate and store results
def evaluate_model(model, X_train, X_test, y_train, y_test, model_name):
    """
    Trains the model, predicts test data, evaluates performance, 
    and stores results in a dictionary.
    """
    model.fit(X_train, y_train)
    y_pred = model.predict(X_test)
    report = classification_report(y_test, y_pred, target_names=label_encoder.classes_, output_dict=True)
    results[model_name] = report

# Define Machine Learning Pipelines
pipelines = {
    'Logistic Regression': Pipeline([
        ('tfidf', TfidfVectorizer(max_features=5000)),
        ('clf', LogisticRegression(max_iter=1000, random_state=42))
    ]),
    'Support Vector Machine': Pipeline([
        ('tfidf', TfidfVectorizer(max_features=5000)),
        ('clf', SVC(kernel='linear', random_state=42))
    ]),
    'Naive Bayes': Pipeline([
        ('tfidf', TfidfVectorizer(max_features=5000)),
        ('clf', MultinomialNB())
    ]),
    'Gradient Boosting': Pipeline([
        ('tfidf', TfidfVectorizer(max_features=5000)),
        ('clf', GradientBoostingClassifier(n_estimators=100, random_state=42))
    ])
}

# Train and evaluate all models
for model_name, pipeline in pipelines.items():
    evaluate_model(pipeline, X_train, X_test, y_train, y_test, model_name)

# Convert results to a DataFrame for easy comparison
performance_data = []
for model, metrics in results.items():
    f1_scores = {label: metrics[label]['f1-score'] for label in label_encoder.classes_}
    f1_scores['Model'] = model
    performance_data.append(f1_scores)

# Create DataFrame for performance comparison
performance_df = pd.DataFrame(performance_data)
performance_df.set_index('Model', inplace=True)

# Display the performance comparison DataFrame
import ace_tools as tools
tools.display_dataframe_to_user(name="Model Performance Comparison", dataframe=performance_df)

# Plot the F1-Scores for each category across models
plt.figure(figsize=(12, 6))
performance_df.plot(kind='bar', figsize=(12, 6), colormap='viridis', alpha=0.75)
plt.title("F1-Scores by Model for Conflict Event Classification")
plt.xlabel("Model")
plt.ylabel("F1-Score")
plt.xticks(rotation=45, ha="right")
plt.legend(title="Event Category")
plt.grid(axis='y', linestyle='--', alpha=0.7)
plt.tight_layout()

# Show plot
plt.show()

# Export the performance data to a CSV file
export_path = '/mnt/data/Model_Performance_Comparison.csv'
performance_df.to_csv(export_path)

# Return file path for reference
export_path