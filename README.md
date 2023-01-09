# Predicting-Satisfaction-Of-Airline-Customers
<p align="justify">

### Project Overview

- Airline customers sastisfaction is predicted using machine learning algorithms like support vector model, artificial neural networks and random forest 

- The dataset is based on an airline passenger satisfaction survey. It is obtained from Kaggle which can be accessed via this link -> https://www.kaggle.com/datasets/teejmahal20/airline-passenger-

- There are a total of 25 attributes and 103,904 records in the dataset. However, there are two attributes namely,"XP and "id" which will not be included in our model building as these attributes do not contribute any meaningful interpretation in our model building.

- The detailed report can be accessed here -> https://github.com/zhiming97/Predicting-Satisfaction-Of-Airline-Customers/blob/master/Readme.pdf

### Results

<img width="701" alt="image" src="https://user-images.githubusercontent.com/97498951/211236757-bcc9aa63-335b-4a56-b80a-ae8ddc4f6844.png">

- The SVM (Kernel : Gaussian RBF, Cost Parameter = 3) is the model that outperforms all the other models with the highest accuracy. 
- However, random forest is chosen as the most appropriate mode because it is less computationally expensive and is able to train a model within a short period of time compared to ANN and SVM.
 - Finally, online boarding, inflight wifi service, and airline classes are labelled as top three most important factors which can greatly affect the satisfaction of a customer.
</p>
