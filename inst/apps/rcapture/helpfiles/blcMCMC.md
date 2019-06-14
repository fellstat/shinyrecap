# MCMC Sampling

These are algorithm convergence controls and should be changed in response to issues raised by the model diagnostics.

----------------------

| Problem | Solution
| ------ | --------
| Too low an effective sample size (e.g. <500) and or the trace plot shows auto correlation. | Increase either **# of samples** or **Thinning** |
| There is a trend at the beginning of the trace plot | Increase **Burn In** |
| Algorithm is taking too long to run | Decrease **# of samples** or **Thinning** |

----------------------

Definitions:
* **# of samples:** The number of MCMC samples to draw from the posterior.
* **Thinning:** Only sample every nth realization from the MCMC chain.
* **Burn in:** Drop the first n realizations from the MCMC chain.
