# Experiment: TwoMoons

This subfolder contains the files related to the experiment titled **TwoMoons**. The experiment has been conducted in **Google Colab** using a **T4 GPU**.

## Package Requirement

To replicate this experiment, please note the following:

- The only external packages required for this experiment are `sbi` and `sbibm`.
- The `sbi` and `sbibm` packages are installed directly within the `.ipynb` files, so no additional setup is necessary beyond running the notebooks in Google Colab.
- All other necessary libraries and dependencies are included by default in the Google Colab environment.

## Folder Contents

- **Two_Moons_True_Posterior.ipynb**: This notebook generates the images of the reference posterior (using the `sbibm` pacakge) .
- **experiment_part2.ipynb**: This notebook conducts the experiment on validation of posterior emulation using the proposed ECMMD based test.
- **TwoMoonsPlot.R**: An R script used for plotting the results of the experiment.
- **Data**: Data on proportion of rejections for the MDN and NSF based emulators. `twomoons_data.csv` combines data from both experiments for ease of plotting.
- **Figures**: A folder containing the figures that are included in the related article.

## License

This experiment is licensed under the same terms as the main repository, which can be found in the [LICENSE](../../LICENSE) file.
