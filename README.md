# Modeling the Women Business and Law (WBL) Index Score in Indonesia
Modeling and forecasting the Women Business and Law (WBL) Index score is crucial for assessing and understanding policy developments that promote gender equality in the business sector in Indonesia. This study uses the annual WBL score data from the World Bank's annual report for Indonesia. Five methods were employed to model the WBL Index score: Naive Model, Double Moving Average (DMA), Double Exponential Smoothing (DES), Time Series Regression, and Autoregressive Integrated Moving Average (ARIMA). Time series regression and ARIMA analysis results indicated that neither method met the established assumptions, rendering them inadequate for this task. The focus of the analysis, therefore, shifted to the Naive, DMA, and DES methods. While the DES (AAN) model initially appeared promising based on training data, its performance deteriorated significantly on new data, indicating a lack of robustness. The DMA model with n=5 demonstrated the most consistent performance, achieving the lowest Root Mean Square Error (RMSE) and Mean Square Error (MSE) during testing, making it the preferred model for this study.

## Objectives 
1. **Assessing Methods:** To apply and compare various forecasting techniques to predict the WBL Index score for Indonesia.
2. **Understanding Gender Equality Progress:** To use the forecasted WBL scores as a means to understand the trajectory of gender equality in Indonesian business policies.
3. **Model Selection:** To identify the most reliable forecasting model, with the primary focus on minimizing forecasting errors (RMSE, MSE) during testing.

## Data Overview
The dataset for this project was sourced from the World Bank Open Data Platform, specifically from the Women, Business, and the Law (WBL) index for Indonesia. The WBL index measures gender equality across various dimensions of business law, assessing the laws and regulations that impact women’s economic inclusion and business opportunities.

The dataset can be accessed at the following URL: [Women Business and Law Index for Indonesia](https://data.worldbank.org/indicator/SG.LAW.INDX?locations=ID).

### Features of the Dataset:
- **Year:** The year of the WBL Index score.
- **Score:** A score ranging from 0 to 100, where a score of 100 indicates full legal equality between men and women.

## Methods
1. **Naive Model:** The simplest forecasting method, assuming that the value of the WBL Index score at the next time step will be the same as the current period.
2. **Double Moving Average (DMA):** A smoothing method used to remove noise and reveal the underlying trend by averaging over a specified number of periods (n=5 in this case)
3. **Double Exponential Smoothing (DES):** A more advanced smoothing method that adjusts for trends and accounts for both level and trend components in time series data.
4. **Time Series Regression:** A regression approach that models the WBL Index score as a function of time, allowing the capture of linear or non-linear relationships between time and the score.
5. **ARIMA (Autoregressive Integrated Moving Average):** A powerful method for forecasting that combines autoregression, differencing, and moving averages to model time series data with seasonality and trends.

## Model Evaluation Metrics
1. **Root Mean Square Error (RMSE):** Measures the square root of the average squared differences between predicted and actual values. Lower RMSE indicates better performance.
2. **Mean Squared Error (MSE):** Similar to RMSE, but without the square root. It emphasizes larger errors, making it useful for penalizing big mistakes.
3. **Mean Absolute Percentage Error (MAPE):** Measures the average of the absolute percentage errors between predicted and actual values, providing a scale-independent error measure.
4. **Mean Absolute Error (MAE):** calculates the average of the absolute differences between the actual and predicted values. Unlike RMSE, MAE does not square the errors, meaning it treats all deviations equally.

## Results
1. **Naive Model:** The Naive Model performed adequately but was too simplistic for long-term forecasting, as it failed to capture any trends or patterns in the WBL Index score data.
2. **Double Moving Average (DMA) - n=5:** The DMA model showed consistent performance on both training and testing datasets. It had the lowest RMSE and MSE values during testing, making it the most reliable model for this task.
3. **Double Exponential Smoothing (DES) - AAN Model:** Initially, DES appeared to be the best-performing model on training data, but it suffered from significant performance degradation when tested on new data, indicating overfitting and a lack of robustness.
4. **Time Series Regression & ARIMA:** Both methods failed to meet the necessary assumptions (such as normality of residuals and homoscedasticity), rendering them ineffective for this particular dataset.

## Conclusion
After evaluating all the models, the Double Moving Average (DMA) with n=5 was the best-performing model for predicting the WBL Index score for Indonesia. Although the DES (AAN) model seemed promising based on training data, its performance dropped significantly on new data, making the DMA model a more robust and reliable choice. This study highlights the importance of using simple, yet consistent methods like DMA for time series forecasting, especially when complex models like ARIMA fail to meet assumptions.

## Requirements
The following R libraries are required to run the project:
- `forecast`
- `tseries`
- `ggplot2`
- `Metrics`

## How to Run
1. **Data Preprocessing:** Clean and format the WBL Index score data for time series analysis.
2. **Model Training:** Train each of the five models (Naive, DMA, DES, Time Series Regression, ARIMA) on the training data.
3. **Model Testing:** Evaluate the models on new (test) data using RMSE, MSE, MAPE, and MAE.
4. **Selection:** Based on the RMSE, MSE, MAPE, and MAE values, select the best model for forecasting the WBL Index score.

## Contributors

2501971742 - Alisha Zahra Saadiya

2301869600 - Patrice Agustin
