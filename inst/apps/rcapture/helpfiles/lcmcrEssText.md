# Effective Sample Size

This algorithm draws samples from the posterior distribution to estimate population size. These samples are correlated, and the effective sample size is the effective number of "independent" samples we have. If this is too low, the population size estimate will vary considerably from one run to the next. Increase the # of samples until this is high enough (> 1000 preferably) that there is little difference in the estimates from one run to the next.
