# breastCancerPredictions
Project based in R to create linear and logistic regression models for predicting breast cancer outcome


## notes
- **Open Source Data Link:**
  - https://wiki.cancerimagingarchive.net/pages/viewpage.action?pageId=70226903
- **Questions:**
  - Which modality[ies] of data would be most effective at predicting patient outcome (secondary image characteristics, demographics, clinical data)?
  - Does the MRI equipment have any confounding influence on the imaging data and patient outcome prediction?
  - Can we demonstrate if the models trained on imputed and nonimputed differ in performance?
- **Methods:**
  - Logistic Regression
  - Linear Regression
  - Dimension Reduction and Clustering
  - F, T tests for parameter significance
- **Data Modality:**
  - Clinical, Demographic, Ultrasound, MRI, MRI equipment
  - Need to determine which of each to interpret
    - Consider text vs categorical clincal and demographic data
    - Consider which of 529 image characteristics may be useful (perhaps explore what has worked for other research)
