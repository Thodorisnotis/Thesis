**Optimal Sampling Locations for Soil Mapping on a Given Budget**

This repository contains the code and resources for the Applied Data
Science Master Thesis titled "Optimal Sampling Locations for Soil
Mapping on a Given Budget." This study focuses on determining the most
effective sampling strategies for predicting soil clay content using
Digital Soil Mapping (DSM) techniques with a constrained budget.

**Abstract**

This research explores optimal sampling strategies for soil mapping with
a focus on predicting soil clay content using Random Forest models. The
study compares the efficacy of Simple Random Sampling (SRS) and
Conditioned Latin Hypercube Sampling (cLHS). Key findings indicate that
SRS generally offers lower RMSE values and higher predictive accuracy.
Additionally, a mixed-method approach combining 25% high-cost,
high-accuracy sampling with 75% low-cost, lower-accuracy sampling
achieves the best balance between accuracy and cost-efficiency.

**Methodology**

1.  **Data Processing**:

    -   Geo-referenced soil samples from Ebergötzen, Germany.

    -   Raster files providing topographical and environmental
        attributes.

    -   Geometric transformations to improve model accuracy.

2.  **Model Training**:

    -   Random Forest models used for predicting soil clay content.

    -   Evaluation of SRS and cLHS sampling techniques.

    -   Mixed-method approach for optimal cost-efficiency.

3.  **Evaluation**:

    -   RMSE used to compare prediction accuracy.

    -   Multiple repetitions to ensure robust results.

    -   Visualization of predicted clay content maps.

**Results**

-   **SRS vs. cLHS**: SRS provided lower RMSE values, indicating higher
    accuracy.

-   **Mixed-Method Approach**: A combination of 25% high-cost,
    high-accuracy and 75% low-cost, lower-accuracy sampling provided the
    best results.

**Usage**

To run the analysis, clone the repository and execute the R scripts in
the scripts/ directory. Ensure all data files are in the appropriate
paths as specified in the scripts.

**License**

This project is licensed under the MIT License.

For further details, refer to the full thesis document in the thesis/
directory or visit the [<u>GitHub
repository</u>](https://github.com/Thodorisnotis/Thesis.git).

Feel free to contact Thodoris Notis for any questions or further
information regarding this project.
