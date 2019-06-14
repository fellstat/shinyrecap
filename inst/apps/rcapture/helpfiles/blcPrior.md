# BLC Prior

The BLC model controls the number of latent groups via a "stick breaking process." The details of this are quite complex and there is not a particularly intuitive way to map the prior parameters into something interpretable.

* **Maximum Number of Groups:** The maximum number of latent groups to consider. Increasing this parameter should have little impact on the results. It just needs to be "large enough," as the underlying method figures out how many groups are likely to exist.
* **Prior Shape/Scale:** These control the prior distribution for the number of groups. While the defaults are reasonable, you can explore the effect of prior specification by altering them.
