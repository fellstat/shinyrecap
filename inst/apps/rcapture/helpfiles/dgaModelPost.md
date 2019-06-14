# DGA Individual Posteriors

The DGA posterior distribution is composed as the weighted average of the posteriors of a collection of models assuming different types of interactions between the capture events. This table provides summaries for each of the models in the collection.

* **Interaction:** This column describes the form of the dependency between capture events. `|` indicates a conditional independence and `-` indicates a dependence. So `1-3|2|4` means that the first and third captures are related, and captures 2 and 4 are independent.
* **Posterior Probability (%):** This is the likelihood that this model dependency is the true one and is the weight that is given to the model when averaging them together.
* **Expected Pop. Size:** This is the model's point estimate for the population size.
