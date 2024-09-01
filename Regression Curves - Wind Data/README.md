# Regression Curves - Wind Energy Data

This folder contains a dataset and an R script that performs the comparison of Regression Curves experiment. The `Inland_Offshore_Wind` folder includes four CSV files, from which we use the two files `Offshore Wind Farm Dataset2(WT3).csv` and `Offshore Wind Farm Dataset2(WT4).csv` corresponding to Offshore wind farms in our experiment.

## Prerequisites

Before running the R script, ensure that the `NNCDT` package is installed. This package is a custom-built tool available in the parent directory of this repository. To install the `NNCDT` package, follow these steps:

```r
# Navigate to the parent directory
setwd("..")

# Install the NNCDT package
devtools::install_local("NNCDT")
```

## How to Run the Script `Regression Example.R`

1. **Load the Data**: The R script automatically loads the data from the `Inland_Offshore_Wind` folder. Ensure that the CSV files are in the correct format and located in the `Inland_Offshore_Wind` folder.

2. **Run the Script**: Execute the `Regression Example.R` script in your R environment to generate the frequency plots and the $p$-values for testing equality of conditional means and conditional distributions.

## Notes

- Ensure that your R environment is correctly set up with all the necessary dependencies, which can be found at the top of the script `Regression Example.R`.

## License

This project is licensed under the MIT License. See the [LICENSE](../LICENSE) file in the parent directory for details.
