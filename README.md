# House-Price-Prediction
The prediction is based on the data set "House Sales in King County"

The dataset we used, named as “House Sales in King County”, is from Kaggle. The link is listed below: 
  https://www.kaggle.com/harlfoxem/housesalesprediction. 

Data Preparation:
The original dataset, consisting of 21 variables, contains 21,207 records of house sale information for King County, 
which locates in state of Washington, including the biggest city, Seattle, of the state. 

  1. Price that exceeds average price plus 3 standard deviations were identified as outliers and were removed.
  2. 'id' and 'zipcode' are excluded because they do not have impact on model performance.
  3. The ‘date’ column is eliminated because sale date does not have strong impact on price.
  4. To evaluate the actual lives of the buildings, a new variable ‘Age’ is created by using current year minus the year built.While constructing prediction models, we used the newly created variable ‘Age’ to replace the ‘yr_built’ column. 
  5. Excluded ‘long’ and ‘lat’ field, which indicate the longitude and latitude of the house.
  6. Created a dummy variable for ‘yr_renovated’. yr_renovated=0 if the house has not been renovated, otherwise equals 1 regardless of the actual renovated year.
  7. ‘sqft_above’ and ‘sqft_basement’ fields, whose sum equals the value of ‘sqft_living’, were removed to eliminate the strong linear dependency among the three variables.
  
  
  
