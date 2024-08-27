# R Scripts for Conditional Goodness-of-Fit

This subfolder contains two R scripts that are part of the Conditional Goodness-of-Fit experiments. These scripts rely on the custom R package `NNCDT`, which is located in the parent directory of this repository.

## Prerequisites

Before running the scripts, ensure that the `NNCDT` package is installed. This package is a custom-built tool designed specifically for the tasks performed by these scripts. To install the `NNCDT` package, follow these steps:

## Script Descriptions

1. **`Conditional Goodness-of-Fit.R`:** This script performs the experiment on Conditional Goodness-of-fit test using the finite sample and derandomized ECMMD based test and the KCSD test.

2. **`Figure.R`:** This script writes a table comparing empirical Type I error and empirical Power of the implemented tests.

## How to Run the Scripts

1. Ensure that the `NNCDT` package is installed and loaded in your R environment.

2. Run the scripts in the following order:
   - `Conditional Goodness-of-Fit.R`
   - `Figure.R`

## Files Produced by the Scripts

- **RDS Files:** `Conditional Goodness-of-Fit.R` generates RDS files `Rho0.rds` and `Rho10.rds` which contains empirical Type I Error and Power values respectively.
   
- **PDF and TeX Files:** `Figure.R` uses the above RDS files to generate a `.pdf` file of power comparison and `.tex` file of Type I Error comparison.

## License

This project is licensed under the MIT License. See the [LICENSE](../LICENSE) file in the parent directory for details.
