# 2023 Peak Cherry Blossom Prediction

 Author: Unknown, Affiliation: Unknown

## 1. Introduction

This project aims to train statistical machine learning models and predict the peak cherry blossom blooming dates for the year 2023 and beyond in four cities: Kyoto, Liestal, Vancouver, and Washington D.C. Owing to the data availability, we implement a variety of different methods  - from simple linearity-based models to temperature-based full-scale machine learning models. This report describes the narratives and steps for data preprocessing, model selection and evaluation, and predictions for each city. We hope our methods aid many experts and cherry blossom lovers in making cherishing memories.

# 2. Methods

### 2.1 Kyoto

**2.1.1. Data processing**

Kyoto has the largest number of historical temperature and bloom dates data, providing an ideal condition for training a machine-learning method. Especially, rich historical temperature data enables feature engineering using a phenology tool called the two-step chill-day model from Cesaraccio et al. 2004. 

![**Figure 1. Fig.1 from Jung et. al.(2005) shows the concept of the two-step phenological model for predicting flowering dates.**](Cherry%20blossom%20prediction%208addccba315b44928ffe9a5a327d6124/Screenshot_2023-02-25_at_1.29.40_PM.png)

**Figure 1. Fig.1 from Jung et. al.(2005) shows the concept of the two-step phenological model for predicting flowering dates.**

They state that the floral trees must undergo a chilling period ($R_{c}$), reach the dormancy release day, and then accumulate heat days until the  threshold day ($T_{c}$, in the figure, it’s at 0) before the buds burst. The flowers bloom after accumulating more heat days and reach the flowering threshold day ($R_h$). This is a variant of the accumulated growing degree days first defined by Reaumur et al., in 1735, but it can more flexibly respond to temperature changes by varying the starting date of accumulating the growing degree days (Jung et al., 2005). We follow the procedure described in Cesaraccio et al., 2004 to compute the daily chill days ($C_d$) and anti-chill days ($C_a$), as shown in Table 1. Here, the threshold temperature $T_{c}$ varies between the species. 

![Table 1. Equations to compute chill days and anti-chill days using daily maximum ($T_x$), minimum ($T_n$), mean ($T_m$) temperatures, and the threshold ($T_c$) for the floral budding.](Cherry%20blossom%20prediction%208addccba315b44928ffe9a5a327d6124/Screenshot_2023-02-25_at_1.53.18_PM.png)

Table 1. Equations to compute chill days and anti-chill days using daily maximum ($T_x$), minimum ($T_n$), mean ($T_m$) temperatures, and the threshold ($T_c$) for the floral budding.

We first obtained daily temperature data from NOAA using the *rnoaa* package in R. Missing values were imputed using the predictive mean matching algorithm implemented in the *mice* package, and **then the daily and accumulated chill days and anti-chill days were computed. We used the thresholds for the floral budding ($T_c = 7 \degree C$ ) from Jung et. al., and used a grid search to find the chill days ($R_c$) required until dormancy release and the flowering day that minimized the RMSE between the latest 10 observed bloom dates and the predicted dates.

Kyoto’s historical bloom dates provided to us start from the year 812. However, the available weather data for Kyoto on the rnoaa package starts from 1953, significantly limiting the data size for model training and testing. To increase the sample size, we selected Japanese cities with similar 5-year historical bloom dates to Kyoto. We first pulled the geographical information (longitude, latitude, and altitudes)and the bloom dates for the years 2017 to 2020 from Japanese cities on `japan.csv` dataset as columns, and performed principal component analysis. Then, we computed the Euclidean distances of the principal components from one city to another and selected the top 10 cities that were closest to Kyoto: Fukui, Gifu, Nagoya, Tsu, Tottori, Hikone, Kobe, Wakayama, Takamatsu, and Osaka. Each city’s chill days and anti-chill days were computed in the same process as Kyoto, starting from the earliest year on the available temperature data (as early as 1954). After extracting the March and April data and under-sampling steps, we had a total of 1392 available observations for the model training and testing.

**2.1.2. Model selection and training**

A relatively large data size allowed us to utilize a full-scale machine-learning method. However, training a neural network usually requires overwhelming efforts to fine-tune the model to avoid overfitting and huge computational power. Also, our data is tabular and well-structured, so there is no need to use a sledgehammer to crack a nut. Furthermore, our model should be able to easily handle possible interaction effects among the predictors we use. We found that the tree-based methods are well suited for this data type and task, especially the gradient-boosted machines. As a result, we chose *LightGBM* (Ke et al., 2017) as our method for the 2023 prediction.

The task for our prediction model is a binary classification. For each day in the data, we set the response variable `is_bloom` as 1 if the cherry blossom bloomed on that day, and 0 otherwise. The trained model will output the probability of being observed as the peak bloom day. However, our dataset for this task is severely imbalanced, as there is only one peak bloom date per year. Even after filtering out only March and April days, the ratio was 61:1. Our initial model trained on the imbalanced data using the log-loss and area under the curve (AUC) metric resulted in poor test performances due to high false negative rates (the model predicted everything as negative). We performed various techniques to mitigate the class imbalancedness: weighted sampling, synthetic minority oversampling technique (SMOTE), under-sampling, and focal loss (Lin et al., 2017). Weighted sampling and focal loss did not improve the high false negative rate, and SMOTE overfitted the training data. Although under-sampling is often not recommended due to possible loss of important information and biases in the sampled majority class, the model trained using the data from random under-sampling performed the best in our analyses.

The predictors we used for our modelling process are as follows: 

| Predictor | Description |
| --- | --- |
| lat, long, alt | Geographical information of each city |
| tmax, tmin | Daily maximum temperature |
| daily_Cd, daily_Ca | Daily chill day and anti-chill day |
| Cd_cumsum, Cd_cumsum | Accumulated chill day and anti-chill day |
| month, day | Month and day of the bloom day |

We only included the features we can obtain for prediction for the year 2023. For instance, the amount of precipitation (`prcp`) is available in the historical climate data but not for the future data, hence not included as a predictor. 

We split the data into training, and test sets prior to model training. We used the observations in the years after 2015 as our test sets and every observation before as our training set. And then, we performed 8-fold cross-validation to find an optimal set of parameters for the LightGBM algorithm. The parameters that we searched across are described in Table 3. We used `binary_logloss` as the loss function for our model training.

| Parameter | Description | Default | Value found |
| --- | --- | --- | --- |
| boosting | Boosting algorithm used.  | gbdt | gbdt |
| learning_rate | Learning rate of the boosting algorithm. | 0.1 | 0.1 |
| max_bin | Maximum number of bins used for LightGBM. Larger number of bins may cause overfitting. | 255 | 511 |
| min_data_in_leaf | Number of minimum samples in leaves. Smaller numbers may cause overfitting. | 20 | 40 |
| max_depth | Maximum depth of trees. Deeper trees may cause overfitting. | -1(no limit) | 5 |
| feature_fraction | Fraction of features used for each split. Higher fractions may cause slower learning speed 
& predictions depend on a small number of features. | 1 | 1 |
| bagging_fraction | Sample fraction used for every n stage determined by bagging_freq.  May help mitigate overfitting. | 1 | 0.8 |
| bagging_freq | Sample bagging frequency. | 0 (no bagging) | 5 |

**2.1.3 Model evaluation and interpretation**

Our final model showed a mean absolute error on the test years (2015 to 2022) … 

*LightGBM* provides feature importance according to the performance gained by each feature. Our model shows that the most important feature for 

this classification task was `Ca_cumsum`, followed by … and …, which is not too far apart from our expectations. 

**2.1.3 Prediction for 2023 and beyond**

Our final model predicts the peak cherry blossom day in Kyoto will be announced as **March 31st (day-of-year = 90).**

Our method cannot make forecasts for the years beyond 2023 because it depends on the temperature data from October of the previous year to April of the target year, none of which are available for the years 2024 to 2032. For forecasting, we use a piecewise regression method, having `year` as the predictor, and`bloom_doy` as the response over the last 100 years of Kyoto’s historical observations. We chose piecewise regression under the assumption that there might be at least one statistically significant change in the `bloom_doy` trend during the century due to climate changes. The analysis using the *segmented* R package found that there are two changepoints: 1928 and 2012. The forecasts made from this model for 2024 to 2032 in day-of-year (doy) are (89, 88, 88, 87, 86, 86, 85, 85, 84), respectively.

### 2.2 Liestal

**2.2.1 Data processing**

Similar to the process for Kyoto, we found 15 closest Swiss cities using their historical `bloom_doy`s and geographical locations. However, many cities in Switzerland had insufficient temperature data available on NOAA to train a chill-day-based model. Instead, we computed daily growing degree days (GDD) and accumulated GDD (AGDD) as used in the USA-NPN data. The daily GDD is computed as $(T_{max} +T_{min})/2$ if it’s above 0, and 0 otherwise, starting from January 1st of each year. Additionally, Liestal has fewer historical bloom observations, which made our training and test set smaller than in the Kyoto case.

**2.2.2 Model selection and training**

We trained a LightGBM using an under-sampled dataset with the same cross-validation step as in the Kyoto case. The difference is in the predictor set in which `daily_Ca`, `daily_Cd`, `Ca_cumsum`, `Cd_cumsum` are replaced with `daily_GDD` and `AGDD`.

**2.2.3 Model evaluation and interpretation**

The final model’s performance on the test set did not perform as well as the Kyoto model. We believe it’s due to a large variation in the AGDD on the bloom days across the chosen cities. Feature importance showed that AGDD contributed to the performance gain the most, followed by … and ….

The weak performance of our model and the variation in the AGDD across the cities got us thinking that perhaps we can build another weak learner and make our prediction from the ensemble of the two models. This idea led us to build a mixed-effects model. We set `city` as the random effect, `year` and `altitude` as our fixed effects, and `bloom_doy` as our response. We did not include the latitude and longitude as our effects since we believe the variable `city` represents them. The result showed that a large portion of the variability in `bloom_doy` is explained by `city`, as we expected. The F-statistics from ANOVA on the fixed effects were large, which supported our model. 

**2.2.4 Prediction for the year 2023 and beyond**

We present the final prediction for Liestal 2023 as the average of the outputs from our two models, which is … . Forecasts for the years 2024 to 2032 were made from the mixed-effects model only since we do not have the temperature data for those years. The forecasted doys are : (87 87 87 86 86 86 85 85 85), respectively.

### 2.3 Vancouver

**2.3.1 Data processing**

Data processing for Vancouver was hard because we had only one historical observation given to us. We chose the cities that are relatively close to Vancouver by first filtering out the cities that had similar `bloom_doy` (86 $\pm$  2) in 2022 as Vancouver , and then excluded a city with the opposite `bloom_doy` trend to the other cities. As a result, we had 7 cities that we used as the proxies for Vancouver, all of them from Switzerland. 

**2.3.2 Model selection and training**

Learnt from the Liestal case, we knew that we won’t be able to build a strong model without any training data for Vancouver. Therefore, we built two weak models: first, AGDD-based LightGBM using the entire city available in our hands as the data, and the second being the linear regression model using the proxy cities we found. The lightGBM model went through the same data processing and cross-validation steps as in the Liestal case, and the linear regression model was built by having `bloom_doy` as the response, and `year`, `latitude`, `longitude`, and `altitude` as the predictors.

**2.3.3 Prediction for 2023 and beyond**

We present our prediction for 2023 as the mean of the outputs from the two models we constructed. For forecasting for the years 2024 to 2032, we only use the linear model. 

### 2.4 Washington D.C.

**2.3.1 Data processing**

We used USA-NPN dataset as our primary dataset for model training. We first selected the features that were deemed relevant to our analysis, including `AGDD` as one of the predictors, and `Phenophase_Status` as the response. We removed the observations that were missing the values for the response variable.

**2.3.2 Model selection and training**

We trained a LightGBM model owing to the relatively large data size, with the same under-sampling and model optimization steps as in the Liestal case.

**2.3.3 Model evaluation and interpretation**

Our final model showed weak performance. We believe it’s due to the high variability in AGDD across the States. Owing to the lack of historical bloom observations in DC before the 2020 on the USA-NPN data, we fitted a linear regression model using Washington D.C’s `bloom_doy` data on `data/washingtondc.csv` as our second model. We present our result as the average of the outputs from the two models. For forecasting for year 2024 to 2032, only the linear model is used.

## 3. Conclusion

We believe combining the phenology-based models and the gradient-boosted trees algorithm can be a promising cherry blossom date prediction method. It not only uses the phenological information but also accounts for other information that may be necessary, including geographical location, giving even more flexibility and robust predictions than purely phenological methods. On the other hand, our methods showed some clear limitations as well. For the temperature-based methods, the prediction heavily depends on the availability and accuracy of the temperature forecasts. Insufficiency in any one of them can critically impact the models’ performance. 

Cesaraccio, C., D. Spano, R. L. Snyder, and P. Duce, 2004: Chilling and forcing model to predict bud-burst of crop and forest species. Agricultural and Forest Meteorology 126, 1-13

Réaumur RA. Observations du thermomètre faites à Paris pendant l’année 1735, comparées avec celles qui ont été faites sous la ligne, à l'Isle de France, à Alger et quelques unes de nos iles de l'Amérique. Mémoires l’Académie R des Sci.:545-76.

# Narrative Format for a Data Analysis Project

When it comes to conducting a data analysis project, it is important to have a well-structured narrative that outlines the key aspects of the project. The narrative should provide a clear and concise overview of the project, including its purpose, methodology, findings, and recommendations. In this document, we will provide a format for a narrative for a data analysis project that is around 1000 words in length.

## Introduction

The introduction should provide an overview of the project and its purpose. This section should be concise and provide a clear understanding of what the project is about. The introduction should answer the following questions:

- What is the project about?
- What is the purpose of the project?
- What are the key objectives of the project?

## Methodology

The methodology section should outline the approach that was used to conduct the data analysis. This section should be detailed and provide a clear understanding of how the data was collected, analyzed, and validated. The methodology section should answer the following questions:

- What data sources were used?
- How was the data collected?
- What analytical techniques were used?
- How was the data validated?

## Findings

The findings section should provide a summary of the key insights that were derived from the data analysis. This section should be structured in a way that is easy to understand and should highlight the most important findings. The findings section should answer the following questions:

- What are the key insights that were derived from the data analysis?
- What patterns or trends were identified?
- What correlations or relationships were discovered?

## Recommendations

The recommendations section should provide actionable recommendations that are based on the findings of the data analysis. These recommendations should be practical and feasible to implement. The recommendations section should answer the following questions:

- What are the key recommendations that were derived from the data analysis?
- How can these recommendations be implemented?
- What are the potential benefits of implementing these recommendations?

## Conclusion

The conclusion section should summarize the key points of the narrative and provide a clear understanding of the overall impact of the project. The conclusion should answer the following questions:

- What are the key takeaways from the project?
- What is the overall impact of the project?
- What are the potential next steps for the project?

In conclusion, a well-structured narrative is essential for a data analysis project. The narrative should provide a clear and concise overview of the project, including its purpose, methodology, findings, and recommendations. By following the format outlined in this document, you can create a narrative that is around 1000 words in length and effectively communicates the key aspects of your data analysis project.

The narrative format for a data analysis project should generally be in complete sentences, with clear and concise language. However, bullet points can be used to organize information in a clear and structured way, especially in the methodology and findings sections. It is important to use bullet points judiciously and not rely on them too heavily so that the narrative maintains its coherence and readability.
