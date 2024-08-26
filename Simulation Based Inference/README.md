# Experiments in this Subfolder

This subfolder contains experiments on **Simulation Based Inference**, each organized into its own subfolder. All experiments have been conducted in **Google Colab** using a **T4 GPU**.

## How to Use

To explore or replicate these experiments, follow these steps:

1. **Navigate to the Subfolders**:
    - Each experiment is contained within its own subfolder under this directory. Navigate to the subfolder of the experiment you are interested in.

2. **Open Google Colab**:
    - Go to [Google Colab](https://colab.research.google.com/).
    - Upload or open the `.ipynb` files from the specific experiment subfolder.

3. **Set Up the Colab Environment**:
    - In Google Colab, go to **Runtime** > **Change runtime type**.
    - Set the **Hardware accelerator** to **GPU**.
    - Ensure that the **GPU type** is **T4** (typically assigned by default, but you can check in the session's hardware details).

## Folder Structure

- **/[TwoMoons](https://github.com/anirbanc96/ECMMD-CondTwoSamp/tree/main/Simulation%20Based%20Inference/TwoMoons)**: Contains all files related to TwoMoons Benchmarking experiment, including the Jupyter notebooks and R scripts. Further details can be found in the subfolder.
- **/[SLCP-simple](https://github.com/anirbanc96/ECMMD-CondTwoSamp/tree/main/Simulation%20Based%20Inference/SLCP-Simple)**: Contains all files related to SLCP Benchmarking experiment, with a similar structure.
- **/[SBI-Redshift](https://github.com/anirbanc96/ECMMD-CondTwoSamp/tree/main/Simulation%20Based%20Inference/SBI-Redshift)**: Contains all files related to the experiment on validation of Conditional Density for Photometric Redshift. This folder is subdived into two subfolders conatining the Jupyter notebooks and R scripts of the two experiments conducted in the article. Further details can be found in the subfolder. 

## Requirements

Though these experiments are designed to run in Google Colab, you can also run them locally if you have the required setup. The individual requirements can be found in the subfolders.

## Notes

- These experiments were run on a T4 GPU in Google Colab. Running them in a different environment might yield different performance and results.
- Make sure your Colab environment has the necessary resources available before starting any experiment.

## License

All content in this subfolder is licensed under the same terms as the main repository, which can be found in the [LICENSE](../LICENSE) file.

## Acknowledgements

- Thanks to the developers of Google Colab for providing a free and easy-to-use platform with powerful GPUs.
