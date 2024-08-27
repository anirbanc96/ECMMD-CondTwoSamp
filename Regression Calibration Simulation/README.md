# Simulation study on Validation of Calibration in Classification Models

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

4. **`Time Comparison.R`:** Compares the computation time of SKCE and ECMMD based tests for $n=100$.

5. **`Figure.R`:** Plots the empirical Power and provides table for the empirical Type I error of the above implemented tests. This script has to be run after `Type I Error.R` and `Power.R` scripts.

6. **`Plot_Time.R`:** Plots a visual comparison of computation time of SKCE and ECMMD based tests. This script has to be run after the `Time Comparison.R` script.

## How to Run the Scripts

1. Ensure that the `NNCDT` package is installed and loaded in your R environment.

2. Make sure that all required packages are installed. These packages are listed at the top of each script.

3. Run the scripts in the following order to reproduce the analysis pipeline:
   - `Type I Error.R`
   - `Power.R`
   - `Time Comparison.R`
   - `Figure.R`
   - `Plot_Time.R`

4. Each script is designed to be self-contained, but they should be executed in the above order to maintain the workflow integrity.

## Files Produced by the Scripts

The following types of files are produced by these scripts:

1. **RDS Files:**
   - RDS files produced by `Type I Error.R` and `Power.R` store the empirical Type I Error and Power values. 'Time Comparison.R` also produces RDS file to store time comparison results.

2. **PDF Files:**
   - The 'Figure.R' and `Plot_Time.R` builds figures in `.pdf` format from the above RDS files for, Power and Time comparison respectively.

3. **TeX Files:**
   - The `Figure.R` builds table of empirical Type I Error comparison in `.tex` format.

## License

This project is licensed under the MIT License. See the [LICENSE](../LICENSE) file in the parent directory for details.
