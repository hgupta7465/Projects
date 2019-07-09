Employee turnover refers to the percentage of workers who leave an organization and are replaced by new employees. 
It is very costly for organizations, where costs include but not limited to: separation, vacancy, recruitment, training and replacement. 
On average, organizations invest between four weeks and three months training new employees. This investment would be a loss for the 
company if the new employee decided to leave the first year. Furthermore, organizations such as consulting firms would suffer from 
deterioration in customer satisfaction due to regular changes in Account Reps and/or Consultants that would lead to loss of businesses 
with clients.

In this project, I have worked on simulated HR data from kaggle to build a classifier that helps us predict what kind of employees will 
be more likely to leave given some attributes. Such classifier would help an organization predict employee turnover and be pro-active 
in helping to solve such costly matter. We’ll restrict ourselves to use the most common classifiers: Random Forest, 
Gradient Boosting Trees, K-Nearest Neighbors, Logistic Regression and Support Vector Machine.

The data has 14,999 examples (samples). Below are the features and the definitions of each one:

satisfaction_level: Level of satisfaction {0–1}.
last_evaluationTime: Time since last performance evaluation (in years).
number_project: Number of projects completed while at work.
average_montly_hours: Average monthly hours at workplace.
time_spend_company: Number of years spent in the company.
Work_accident: Whether the employee had a workplace accident.
left: Whether the employee left the workplace or not {0, 1}.
promotion_last_5years: Whether the employee was promoted in the last five years.
department: Department the employee works for.
salary: Relative level of salary {low, medium, high}.
