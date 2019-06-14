# DGA Prior Type

Two families of priors are supported. The first is a "non-informative" prior equal to 1 / (Population Size). In the absence of any information, this is a reasonable prior to choose. Depending on the maximum that is chosen, it may look as though it is a flat line in the distribution plot. This is because it is an improper prior with a very fat tail.

In most cases there is some idea as to the overall likelihood of population sizes. For example, the # of men who have sex with men in a population is likely less than 20% of the # of males in the population. The log-normal distribution is supported to help specify your knowledge about population size prior to collecting the CRC data.
