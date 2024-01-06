# IMDb Score Prediction Project

## Overview

This project aims to construct predictive models for IMDb scores, providing insights into the potential success of new movies. Leveraging IMDb data, robust models are developed to predict scores, benefiting both moviegoers and the film industry.

## Objectives

- **Target Audience**: Designed for movie enthusiasts and industry professionals.
- **Scope**: Focused on accurately predicting IMDb scores for twelve upcoming blockbusters.

## Methodology

1. **Data Exploration**: In-depth analysis of IMDb dataset to understand variable characteristics.
2. **Model Building**: Identification of key variables and data preprocessing for accurate predictions.
3. **Model Selection**: Evaluation of models based on Mean Square Error (MSE), significance of variables, and realistic outcomes.

## Data Description and Feature Engineering

- **Data Cleaning and Transformation**: Removal of non-contributory identifiers and conversion of character columns to categorical types.
- **Exploratory Data Analysis (EDA)**: Analysis of summary statistics and visualization of IMDb score distributions.
- **Feature Engineering**: Application of grouping and binary feature creation for regression modeling.

## Model Selection and Rationale

- **Approach**: Use of polynomial regression, log-transformed IMDb score models, and spline predictions.
- **Predictor Selection**: Retention of variables with p-values around or below 0.05.
- **Polynomial Degree and Knots in Spline**: Selection based on minimizing RMSE and MSE.

## Results

- **Predictive Power**: Final model achieved an R-squared of 0.4297.
- **Out-of-Sample Performance**: Achieved an MSE of 0.718 using K-fold cross-validation.
- **Final Variables**: Includes variables related to movie investment, marketing, and genres.
- **Score Estimates**: Model produced score estimates ranging from approximately 3 to 9.

## Conclusion

The final model offers accurate IMDb score predictions, demonstrating significant capability to estimate movie success and serving as a valuable tool for the film industry.
