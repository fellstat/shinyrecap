# Select Output

These are the types of log linear models supported by this application. Use the AIC or BIC from the previous tab to help decide which one to use.

There are two main assumptions that separate the models.
* **Equal Capture Probability** This assumption is met if all capture events have the same probability of capturing an individual. This is generally false in public health domains, but might be reasonable if each capture event has the same size.
* **Homogeneity** This assumption is met if every individual in the population has equal probability of being captured. This is often not the case. However, splitting the analysis into subsets can alleviate this issue if there is homogeneity within those subsets. For example, if younger individuals are more likely to be captured, you can do two analyses one in the young group and one in the old group and then add the two population size estimates together to get an overall population size estimate.

The model assumptions are as follows:
* **M0:** This model assumes equal capture probability and homogeneity.  
* **Mt:** This model assumes homogeneity.  
* **Mh:** This model assumes equal capture probability.  
* **M0:** This model assumes neither.  
