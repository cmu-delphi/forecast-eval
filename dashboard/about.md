## FORECAST EVALUATION DASHBOARD

This dashboard was developed by:

* Jed Grabman (Delphi Group, Google Fellow)
* Kate Harwood (Delphi Group, Google Fellow)
* Chris Scott (Delphi Group, Google Fellow)

with the Forecast Evaluation Research Collaborative:

* Nicholas Reich (Reich Lab)
* Jacob Bien (Delphi Group)
* Logan Brooks (Delphi Group)
* Estee Cramer (Reich Lab)
* Daniel McDonald (Delphi Group)
* [The COVID-19 Forecast Hub Team](https://covid19forecasthub.org/doc/team/)

This data can also be viewed in a weekly report on the Forecast Hub site.
### Forecast Evaluation Research Collaborative

The Forecast Evaluation Research Collaborative was founded by:

* Carnegie Mellon University [Delphi Group](https://delphi.cmu.edu)
* UMass-Amherst [Reich Lab](https://reichlab.io/)

Both groups are funded by the CDC as Centers of Excellence for Influenza and COVID-19 Forecasting. We have partnered together on this project to focus on providing a robust set of tools and methods for evaluating the performance of epidemic forecasts.  
  
The collaborative’s mission is to help epidemiological researchers gain insights into the performance of their forecasts and lead to more accurate forecasting of epidemics.  
  
Both groups lead initiatives related to COVID-19 data and forecast curation. The Reich Lab created and maintains the [COVID-19 Forecast Hub](https://covid19forecasthub.org/), a collaborative effort with over 80 groups submitting forecasts to be part of the official [CDC COVID-19 ensemble forecast](https://www.cdc.gov/coronavirus/2019-ncov/covid-data/mathematical-modeling.html). The Delphi Group created and maintains COVIDcast, a platform for [epidemiological surveillance data](https://delphi.cmu.edu/covidcast/), and runs the [Delphi Pandemic Survey via Facebook](https://delphi.cmu.edu/covidcast/surveys/).
  
The Forecaster Evaluation Dashboard is a collaborative project, which has been made possible by the 13 pro bono Google.org Fellows who have spent 6 months working full-time with the Delphi Group. Google.org is [committed](https://www.google.org/covid-19/) to the recovery of lives and communities that have been impacted by COVID-19 and investing in developing the science to mitigate the damage of future pandemics.  
  
### About the Data

#### **Sources**

**Observed values** are from the [COVID-19 Data Repository](https://github.com/CSSEGISandData/COVID-19) by the Center for Systems Science and Engineering (CSSE) at Johns Hopkins University.  
  
**Forecaster predictions** are drawn from the [COVID-19 Forecast Hub GitHub repository](https://github.com/reichlab/covid19-forecast-hub/)  
  
Data for the dashboard is pulled once a week from these sources, on Tuesdays.  

#### **Terms**

*   **Forecaster**
    
    A model producing quantile predictions
    
*   **Forecast**
    
    A set of data that includes quantile target variable predictions for a certain horizon, epidemiological week, and location 
    
*   **Target Variable**
    
    What the forecast is predicting, ie: “weekly incident cases”
    
*   **Horizon**
    
    The duration of time between when the prediction was made and the predicted event in units of epidemiological weeks.
    
*   **Epidemiological week (MMWR week)**
    
    Week that starts on a Sunday. If the day on which the forecast is being made is a Sunday or Monday, the next epidemiological week is the week that starts on that Sunday (going back a day if it is Monday). If the forecast is being made on Tuesday-Saturday, the next epidemiological week is the week that starts on the subsequent Sunday, following [CDC convention](https://wwwn.cdc.gov/nndss/document/MMWR_week_overview.pdf).
    
*   **Point Forecast**
    
  The value that each forecaster picks as their “most likely” prediction. Usually this is the median (50% quantile prediction), but forecasters can specify alternative Point Forecasts different from the median. 
  
*   **Geo Type**
    
    States, territories or U.S. as a nation

#### **Dashboard Inclusion Criteria**

*   Includes only weekly deaths incidence and weekly case incidence target variables
*   Includes only horizon < 5 weeks ahead
*   Includes only geo values that are 2 characters (states / territories / nation)
*   Includes only non-NA target dates (if the date is not in yyyy/mm/dd, the prediction will not be included)
*   Includes only predictions with at least 3 quantile values
*   Includes only one file per forecaster per week (according to forecast date). That file must be from a Sunday or Monday. If both are present, we keep the Monday data.
*   If a forecaster updates a file after that Monday, we do not include the new predictions

#### **Notes on the Data**

*   When totaling over all locations, these locations include states and territories and do not include nationwide forecasts. We only include states and territories common to the selected forecasters (over all time) that have data for at least one location.
*   We do include revisions of observed values, meaning the scores for forecasts made in the past can change. Scores change as our understanding of the truth changes.