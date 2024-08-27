# Experiments for [A Kernel-Based Conditional Two-Sample Test Using Nearest Neighbors](https://arxiv.org/abs/2407.16550)

This repository contains the experiments conducted for the paper titled "[A Kernel-Based Conditional Two-Sample Test Using Nearest Neighbors](https://arxiv.org/abs/2407.16550)." The repository is organized into six different folders, each representing a separate experiment. Additionally, there is a custom R package named `NNCDT`, which is essential for running experiments in the folders `Classification Calibration Simulation`, `Conditional Goodness-of-Fit`, `Regression Calibration Simulation` and `Regression Curves - Wind Data`. 

## Repository Structure

- **CIFAR10-Calibration**: Contains all files and scripts related to validating calibration of convolutional neural networks on binary classification taks using the CIFAR-10 dataset.
- **Classification Calibration Simulation**: Contains all files and scripts related to the simulated experiments on validation of calibration in binary classification models.
- **Conditional Goodness-of-Fit**: Contains all files and scripts related to the experiment on Conditional Goodness-of-Fit tests. This experiment relies on the `NNCDT` package.
- **Regression Calibration Simulation**: Contains all files and scripts related to the simulated experiments on validation of calibration in regression models. This experiment relies on the `NNCDT` package. 
- **Regression Curves - Wind Data**: Contains all files and scripts related to comparison of Regression Curves using [Wind Energy Dataset](https://aml.engr.tamu.edu/book-dswe/dswe-datasets/). This experiment relies on the `NNCDT` package.
- **Simulation Based Inference**: Contains all files and scripts related to the experiments on validation of emulators in Simulation Based Inference.
- **NNCDT**: Contains the custom R package `NNCDT`, which is used in experiments the folders `Classification Calibration Simulation`, `Conditional Goodness-of-Fit`, `Regression Calibration Simulation` and `Regression Curves - Wind Data`. The NNCDT package contains functions to implement the ECMMD based asymptotic, finite-samples and derandomized tests. We also provide functions to implement the KCSD and SKCE based tests in the package.

## Prerequisites

Before running the experiments in the folders `Classification Calibration Simulation`, `Conditional Goodness-of-Fit`, `Regression Calibration Simulation` and `Regression Curves - Wind Data`. you need to install the `NNCDT` package. This package is custom-built and located in the `NNCDT` folder within this repository.

### Installing the NNCDT Package

To install the `NNCDT` package, follow these steps:

1. Clone this repository to your local machine:

    ```bash
    git clone https://github.com/anirbanc96/ECMMD-CondTwoSamp.git
    cd ECMMD-CondTwoSamp
    ```

2. Open R or RStudio and set your working directory to the repository root:

    ```r
    setwd("[Path to Your Cloned Repository]")
    ```

3. Install the `NNCDT` package using `devtools`:

    ```r
    # Install the devtools package if you haven't already
    install.packages("devtools")

    # Install the NNCDT package from the local directory
    devtools::install_local("NNCDT")
    ```

4. Load the `NNCDT` package in your R session:

    ```r
    library(NNCDT)
    ```

## Running the Experiments

- **CIFAR10-Calibration and Simulation Based Inference:** These experiments do not require the `NNCDT` package. Simply navigate to the respective folder and run the scripts as described within each folder.

- **Experiments in folders `Classification Calibration Simulation`, `Conditional Goodness-of-Fit`, `Regression Calibration Simulation` and `Regression Curves - Wind Data`:** After installing the `NNCDT` package, navigate to the respective experiment folder and run the scripts. The `NNCDT` package will be loaded automatically if installed.

## Notes

- Please refer to the README or instructions within each folder for futher details on each experiment.
- **CIFAR10-Calibration and Simulation Based Inference**: These experiments use Python and Jupyter Notebook in addition to R scripts. More details about the setup and dependencies can be found in the `Simulation Based Inference` folder.
- Make sure your R environment is set up with all the necessary packages listed in the scripts.

## License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.

## Citation

If you use this code in your research, please cite the paper:

[Paper Citation in your preferred format]

