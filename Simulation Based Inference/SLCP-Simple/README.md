# Experiment: Simple Likelihood Complex Posterior (SLCP)

This subfolder contains the files related to the experiment titled **SLCP-simple**. The experiment has been conducted in **Google Colab** using a **T4 GPU**.

## Package Requirement

To replicate this experiment, please note the following:

- The only external package required for this experiment is `sbi`.
- The `sbi` package is installed directly within the `.ipynb` files, so no additional setup is necessary beyond running the notebooks in Google Colab.
- All other necessary libraries and dependencies are included by default in the Google Colab environment.

## Folder Contents

- **[SLCP_with_SBI_MDN_and_NSF.ipynb](https://github.com/anirbanc96/ECMMD-CondTwoSamp/blob/main/Simulation%20Based%20Inference/SLCP-Simple/SLCP_with_SBI_MDN_and_NSF.ipynb)**: This notebook conducts the experiment on validation of posterior emulation using the proposed ECMMD based test.
- **[SLCP_Plot.R](https://github.com/anirbanc96/ECMMD-CondTwoSamp/blob/main/Simulation%20Based%20Inference/SLCP-Simple/SLCP_Plot.R)**: An R script used for plotting the results of the experiment.
- **[Data](https://github.com/anirbanc96/ECMMD-CondTwoSamp/tree/main/Simulation%20Based%20Inference/SLCP-Simple/Data)**: Data on proportion of rejections for the MDN and NSF based emulators. `SLCP_data.csv` combines data from both experiments for ease of plotting.
- **[Figures](https://github.com/anirbanc96/ECMMD-CondTwoSamp/tree/main/Simulation%20Based%20Inference/SLCP-Simple/Figures)**: A folder containing the figures that are included in the related article.

## License

This experiment is licensed under the same terms as the main repository, which can be found in the [LICENSE](../../LICENSE) file.
