# modeling_attitude_change
Portfolio 3 for the course Advanced Cognitive Modeling (S2024) from study group 1. 

The code in this repository holds code for model comparion between a simple bayes and a weighted bayes approach to analyse how the opinion of an individual changes implicitly after being informed about the group-level opinion.

The scripts to run the code can be found in the `src` folder. To replicate the results, follow the steps below:

1. Clone the repository
2. Place the data from the *Socially Learned Attitude Change is not reduced in Medicated Patients with Schizophrenia* paper by Simonsen et al. (2019) in a folder called `data` in the root of the repository.
3. Run simple bayes on simulated data and real data
```
Rscript src/simple_bayes_sim.R
Rscript src/simple_bayes_data.R
Rscript src/simple_bayes_plot.R
```
4. Run weighted bayes on simulated data and real data
```
Rscript src/weighted_bayes_sim.R
Rscript src/weighted_bayes_data.R
Rscript src/weighted_bayes_plot.R
```
