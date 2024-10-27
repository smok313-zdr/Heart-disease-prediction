# Heart-disease-prediction

### Background

The goal of the project is to present selected classification algorithms that represent supervised machine learning for heart disease prediction. Classification is one of the most common tasks in machine learning. It involves assigning objects to specific classes or categories based on their features or attributes. In this case, we assign patients to the class "HeartDisease", which is a discrete variable that takes two values: TURE and FALSE. 

The classification algorithms used in the project are: K-Nearest Neighbors (KNN), Support Vector Machines (SVM), decision trees, and Random forest.

### Results

By using parameter tuning of the generated machine learning models and cross-validation, it is possible to achieve a prediction accuracy of approximately 73-84% on the test set.

| Algorithm      | Accuracy | Balanced Accuracy | Sensitivity | Specificity |
|----------------|----------|-------------------|-------------|-------------|
| KNN            | 0.8133   | 0.8461            | 0.9630      | 0.7292      |
| SVM            | 0.84     | 0.8377            | 0.8182      | 0.8571      |
| Decision trees | 0.7333   | 0.7329            | 0.7027      | 0.7632      |
| Random forest  | 0.8133   | 0.8129            | 0.7838      | 0.8421      |

<img src="https://github.com/user-attachments/assets/3f6397d4-0022-4de8-8831-afc3238c9f75" >

### Further notices

The project presents the following approach: removing missing values and normalizing the data before splitting the set into training and test. In order to search for a more accurate model, it would be necessary to first divide the set into training and test, then handle the missing values, for example, through imuputation (using the mean or mode). It is permissible to remove columns that contribute nothing and contain about 60%+ missing data. In a further step, rescaling of variables should be performed on the training sample, and then with identical parameters applied to the test sample. 
