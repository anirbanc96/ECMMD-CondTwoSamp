# R Scripts for Simulation study on Validation of Calibration in Classification Models

This subfolder contains five R scripts for the simulated experiments on validation of calibration in classification models. Each script performs a specific task using the custom R package `NNCDT`, which is located in the parent directory of this repository.

## Prerequisites

Before running any of the scripts, ensure that the `NNCDT` package is installed. This package is a custom-built tool designed specifically for the operations in these scripts. To install the `NNCDT` package, follow the instructions provided in the parent repository's README.

## Script Descriptions

1. **`Type I Error.R`:** This script empirically evaluates the Type-I Error for sample sizes $n = 75, 100$. The implemented tests include:
   - ECMMD - Asymptotic and Derandomized
   - SKCE

3. **`Power.R`:** This script empirically evaluates the Power for sample sizes $n = 75, 100$. The implemented tests include:
   - ECMMD - Asymptotic and Derandomized
   - SKCE

4. **`Time Comparison.R`:** Compares the computation time of SKCE and ECMMD based tests.

5. **`Figure.R`:** Plots the empirical Type-I error and Power of the above implemented tests. This script has to be run after `Type I Error.R` and `Power.R` scripts.

6. **`Plot_Time.R`:** Plots a visual comparison of computation time of SKCE and ECMMD based tests. This script has to be run after the `Time Comparison.R` script.

