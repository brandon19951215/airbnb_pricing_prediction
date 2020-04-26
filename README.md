# airbnb_pricing_prediction
Linear Regression Model predicting the price of Airbnb listings from NewYork and Singapore

The business goal is to create a model that can predict the price of listings in both Singapore and New York City. 
Benefits for hosts: 
  - More information about how to improve listing information and set prices to attract more stayers.
Benefits for renters:
  - A useful tool to check for any overpriced deal when searching for an Airbnb in either city and can help ensure the quality of a considered listing.

Three models created
1. Intuitive Variable Selection Model: 
  - Provides further intuitions on the Airbnb market in summer in business context. 
  - It is highly interpretable, but the relatively smaller R2 Value implies that the model is still not accurate enough to capture the whole picture.
2. Stepwise Regression Model 
  - Provides more accurate explanation of the data but is not intuitive enough or easily comprehensible. 
  - Moreover, it still includes too many variables that are not significant or informative.
3. The Final Model 
  - Provides the most accurate and intuitive explanation to the data. 
  - It should be noted that the final Singapore model has a higher AIC than New York because there are less observations available.
