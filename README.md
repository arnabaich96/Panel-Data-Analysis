
# Interactive Trial Data Analysis

## Overview
The Interactive Trial Data Analysis app is designed to provide advanced data visualization and imputation techniques for clinical trial data. It employs the `mice` package to handle missing data and offers dynamic, interactive visualizations to enhance data interpretation.

## Features
The app includes multiple components each tailored to specific analysis needs:
- **Data Imputation App**: Enables the imputation of missing data for C-peptide and Glucose measurements using multiple imputation methods.
- **Summary Plots App**: Provides tools for visualizing summary statistics for the clinical trial data.
- **AUC Calculation App**: Calculates the Area Under the Curve (AUC) for selected measurements and visualizes the results.
- **Index Calculation App**: Computes and visualizes the index for Glucose and C-peptide measurements based on AUC.

## Installation
To run this app, you will need R installed on your system along with the Shiny package and other dependencies.

### Prerequisites
- R (Version 4.0 or later recommended)
- RStudio (Optional, but recommended for ease of use)

### Required R Packages
```{{r, eval=FALSE}}
install.packages(c("shiny", "plotly", "dplyr", "mice", "shinyjs", "gt", "shinyWidgets", "shinycssloaders"))
```

## Running the App
To run the app locally:

1. Clone or download this repository to your local machine.
2. Open `app.R` in RStudio or your preferred R environment.
3. Load the required libraries and run the app using:

```{{r, eval=FALSE}}
library(shiny)
runApp("app.R")
```
## Usage
After launching the app, navigate through the various tabs to access different functionalities:

- Set imputation parameters and run the imputation.
- View and interact with the generated plots and tables for both raw and imputed data.
- Calculate and visualize AUC and Index values, adjusting parameters as needed for detailed analysis.

## Contributing
Contributions to enhance the app or add new features are welcome. Please fork the repository and submit pull requests with your proposed changes.

## License
This project is open-sourced under the MIT License. See the LICENSE file for more details.

## Contact
For any queries or further assistance with setting up or running the app, please contact:

- [Arnab Aich](mailto:arnab.aich99@gmail.com)
