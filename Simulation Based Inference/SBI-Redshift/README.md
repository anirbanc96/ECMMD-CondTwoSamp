# Experiment: Validation of Conditional Density for Photometric Redshift

This subfolder contains two separate experiments from the validation of Conditional Density for Photometric Redshift section, organized into individual subfolders, as well as a set of Python scripts sourced from an external repository. The experiments have been conducted using **Google Colab** with a **T4 GPU**.

## Folder Structure

- **[Experiment1](https://github.com/anirbanc96/ECMMD-CondTwoSamp/tree/main/Simulation%20Based%20Inference/SBI-Redshift/Experiment%201)**: This subfolder contains all files related to Experiment 1, where comparisons are done for different values of $\mu$, including notebooks, data, and any results generated.
- **[Experiment2](https://github.com/anirbanc96/ECMMD-CondTwoSamp/tree/main/Simulation%20Based%20Inference/SBI-Redshift/Experiment%202)**: This subfolder contains all files related to Experiment 2, where comparisons are done for different values of $n$ and $K$, including notebooks, data, and any results generated.
- **[DensityPlots](https://github.com/anirbanc96/ECMMD-CondTwoSamp/tree/main/Simulation%20Based%20Inference/SBI-Redshift/DensityPlots)**: Contains plots of the marginal density.
- **[GalaxyImage](https://github.com/anirbanc96/ECMMD-CondTwoSamp/tree/main/Simulation%20Based%20Inference/SBI-Redshift/GalaxyImage)**: Contains images of galaxies with $\lambda = 0.75$ and $\alpha = -\frac{\pi}{4},\frac{\pi}{4}$.
- **[Scripts](https://github.com/anirbanc96/ECMMD-CondTwoSamp/tree/main/Simulation%20Based%20Inference/SBI-Redshift/Scripts)**: A collection of Python scripts used in both experiments, sourced from an external repository. These scripts provide essential functions and utilities required for running the experiments.

## Python Scripts

The `scripts` folder includes Python scripts that were adapted from [CDE-conformal](https://github.com/zhao-david/CDE-conformal). These scripts are integral to the operation of the experiments and have been used directly within the notebooks in both Experiment 1 and Experiment 2.

## How to Use

To replicate or explore the experiments:

1. **Navigate to the Experiment Subfolder**:
    - Choose between `Experiment1` and `Experiment2` and navigate to the respective subfolder.

2. **Follow the Instructions**:
    - Each experiment subfolder contains its own `README.md` file that guide you through running the experiment.

## Package Requirement

- Any additional package requirements for the experiments are specified within the notebooks or the subfolder-specific `README.md` files.
- The Python scripts in the `scripts` folder do not require additional installations beyond what is already mentioned in the respective notebooks.

## Notes

- The Python scripts sourced from the external repository have been checked for compatibility with the current experiments. However, if you are running these scripts outside of the provided environment (e.g., Google Colab), please ensure that all dependencies are properly installed.
- The external scripts are adapted from [CDE-conformal](https://github.com/zhao-david/CDE-conformal). Adjustments were made to the original scripts to align them with the usage in Experiment 1 and Experiment 2. 

## License

This subfolder and its contents, including modifications to the Python scripts from the external repository, are licensed under the same terms as the main repository. See the [LICENSE](../../LICENSE) file for details.

The original Python scripts from the external repository retain their original license, which can be found at [CDE-conformal](https://github.com/zhao-david/CDE-conformal).
