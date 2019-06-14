# Model Comparison

------------------

Each row summaries the results for a log linear model type.

Model Types:
* **M0:** This model assumes equal capture probability and homogeneity.  
* **Mt:** This model assumes homogeneity.  
* **Mh:** This model assumes equal capture probability.  
* **M0:** This model assumes neither.  

Heterogeneity Types:
* **Normal:** The log odds of capture follows a Normal distribution.
* **Darrosh:** The log odds of capture _among those who were not captured_ follows a Normal distribution.
* **Poisson:** The log odds of capture _among those who were not captured_ follows a Poisson distribution.
* **Gamma:** The log odds of capture _among those who were not captured_ follows a Gamma distribution.

--------------------

Columns are defined as follows:
* **Population Size:** This is the population size as estimated by each model.
* **strerr:** This is the standard error of the population size.
* **AIC:** This is the Akaike Information Criterion, and is a good measure to use to select which model to report. Lower is better.
* **BIC:** This is the Bayesian Information Criterion, and is also a good measure to use to select which model to report. It generally favors simpler model (i.e. ones with more assumptions) than the AIC. Lower is better.
