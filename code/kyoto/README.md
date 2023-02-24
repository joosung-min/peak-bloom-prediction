# Cherry blossom prediction: Kyoto, JAPAN

## Data availability


## Data processing steps

1. Data size 
2. Select close cities
  * PCA + Euclidean distance matrix
3. Compute AGDD according to the Chill-day method
4. Select predictors and response variable
  * Selected according to the data availability for March and April 2023
  * Response: 
    * is_bloom: 1 if bloom, 0 otherwise. Determined on /data/kyoto.csv
  * Predictors:
    * tmin: daily minimum temperature
    * tmax: daily maximum temperature
    * daily_Cd: daily chill-day by the equations on **paper**
    * daily_Ca: daily anti-chill day by the equations on **paper**. Equivalent to the Growth Degree Days (GDD).
    * Cd_cumsum: Accumulated daily_Cd.
    * Ca_cumsum: Accumulated daily_Ca. Equivalent to the Accumulated Growth Degree Days (AGDD).
    * lat, long, alt, month, day

## Model selection
  * We use lightgbm (light gradient boosting machine).
    * Known to be one of the fastest + well performing gbm algorithm.
    * Can handle data label imbalance well (option available for weighted sampling)
  * The data size is large enough + the feature dimension is not too large.
  * However, not enough data size for deep-based algorithms (may overfit easily).

## Model fitting
  * We use a K-fold cross-validation to find the optimal set of parameters.
  * Parameters:
    * boosting:
    * max_bin:
    * learning_rate:
    * min_data_in_leaf:
    * bagging_fraction:
    * feature_fraction:
  * Metric: binary_logloss
 
## Model evaluation
  * ROC curve:
  * Feature importance:
  * Test set performance:

## Final prediction
  * Weather data (tmin, tmax) for March and April 2023 obtained from AccuWeather.
  * Computed daily_Cd, daily_Ca, Cd_cumsum, and Ca_cumsum in the same way as the previous data.
  * The model with a threshold probability = 0.5 for labeling "is_bloom == 1" predicts that Kyoto's cherry blossom day is on -.
