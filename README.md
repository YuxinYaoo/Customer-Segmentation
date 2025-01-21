# E-commerce Data Analysis and Customer Segmentation

This project performs data analysis and customer segmentation using an e-commerce dataset. The analysis includes data cleaning, RFM segmentation, K-means clustering, and visualizations to explore sales trends and product performance. Below is a detailed explanation of the workflow and purpose of the project.

## Table of Contents
- [Overview](#overview)
- [Data Cleaning and Preprocessing](#data-cleaning-and-preprocessing)
- [RFM Analysis](#rfm-analysis)
- [K-means Clustering](#k-means-clustering)
- [Visualizations](#visualizations)
- [Requirements](#requirements)

## Overview
The goal of this project is to understand customer behavior, identify distinct customer segments, and analyze sales trends. By leveraging RFM (Recency, Frequency, Monetary) analysis and K-means clustering, we segment customers into groups to inform marketing strategies. Additionally, we visualize sales trends and product performance to identify areas for growth.

## Data Cleaning and Preprocessing
1. **Data Loading**:
   - Load the e-commerce data from a CSV file.

2. **Handling Invalid Values**:
   - Replace non-positive values in `Quantity` and `UnitPrice` columns with `NA`.
   - Drop rows with missing values.

3. **Feature Transformation**:
   - Convert `InvoiceNo`, `StockCode`, `CustomerID`, and `Country` to factors.
   - Parse `InvoiceDate` as a date.
   - Create a new column `total_dollar` as `Quantity * UnitPrice`.

## RFM Analysis
- Calculate the following metrics for each customer:
  - **Recency**: Days since the customer's last transaction.
  - **Frequency**: Number of unique transactions.
  - **Monetary**: Average revenue per transaction.
- Transform the `monetary` variable using a log transformation for normalization.

## K-means Clustering
1. Prepare the RFM data (Recency, Frequency, Monetary).
2. Perform K-means clustering:
   - Use the elbow method to determine the optimal number of clusters.
   - Visualize the within-cluster sum of squares.
3. Segment customers into 5 clusters and visualize the results.

## Visualizations
- **Top-Selling Products**:
  - Bar plot showing the most frequently purchased products.

- **Sales Trends**:
  - Monthly revenue trends.
  - Daily revenue trends.

- **Country-wise Sales**:
  - Total sales by year and country.
  - Filtered visualization for countries excluding the United Kingdom.

## Requirements
The following libraries are required:
- `data.table`
- `dplyr`
- `ggplot2`
- `tidyr`
- `knitr`
- `rmarkdown`


---

### Note
Ensure the dataset is in the expected format with columns such as `InvoiceNo`, `StockCode`, `InvoiceDate`, `CustomerID`, `Quantity`, `UnitPrice`, and `Description`. This script assumes the presence of a date column formatted as `%m/%d/%Y`.

Feel free to modify and expand upon this project as needed. Contributions and suggestions are welcome!

