# Can we predict if people will choose a certain option out of a set?

This repository contains data files and scripts to run machine learning algorithms using the function `automl` from h2o.ai in R.

Documentation from h2o ai can be found at: https://docs.h2o.ai/h2o/latest-stable/h2o-docs/automl.html

See https://hackmd.io/@dlholf/manuscript#About-the-dataset for explanation about the dataset.

Data is contained in the folder 'data'.

There are 5 script files in the folder 'scripts'.

1. data_clean.R cleans and combines the raw data file into a form ready for analysis.
2. add_strategies.R codes participants' choices according to whether it is: 

* The best equally-weight average by %RI
* The best traffic light bands (on average)
* Minimising one of the following nutrients' %RI: sugar, salt, energy, fat
* Minimising one of the following nutrients' TFL band: sugar, salt, fat

3. run_models.R runs the h2o.automl models for 9 different criteria (5 based on %RI, 4 based on TFL bands) and extracts metrics (SHAP values etc.) from each model. It also plots and saves SHAP and PD plots.

4. surrogate_plots.R does the same as 3, but with 'Yes'/'No' coding of the binary choices, such that we can plot surrogate trees using these responses instead.

5. metrics.R analyses the extracted metrics from 3.
