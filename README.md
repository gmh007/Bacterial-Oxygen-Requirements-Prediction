# Bacterial-Oxygen-Requirements-Prediction

# Introduction
   This project predicts bacterial oxygen preference (aerobic, anaerobic, or facultative)
   using protein domain characteristics. We then use these prediction models to identify 
   functional genes related to the oxygen preference phenotype.
   
   First, we functionally annotate protein sequences using Pfam_scan. We extract the 
   annotation results to create a machine learning matrix (see the data format examples 
   in the "Example" folder or the pfam.txt file inside pfam.zip). Bacteria are categorized 
   into aerobes, anaerobes, and facultative anaerobes for machine learning classification.
   
   We used several machine learning algorithms including: Random Forest, SVM, Decision Tree,
   Naive Bayes, and GBDT (Gradient Boosting Decision Tree).
   
# File Descriptions
   Code: This folder contains the R scripts used in our experiments. To reproduce the results, 
   you'll need to modify the file paths within the scripts to match your own data locations.
   
   Multiple_machine_learning_algorithms.R: R code for classification using multiple machine 
   learning algorithms.
   
   Random_forest.R: R code specifically for Random Forest classification.
   
   Data: This folder contains the data used in our experiments.
   
   design_1813_3_1.csv: A CSV file containing the machine learning group assignments. 
   The data is split into training and testing sets with a 3:1 ratio. Class distribution: 
   aerobe:anaerobe:facultative = 953:574:286.
   
   pfam.txt: A text file containing the processed matrix derived from the Pfam_scan annotation 
   results. Extract this file from pfam.zip.
   
   Example: This folder contains example data to illustrate the expected data formats.
   
   Result: This folder contains the results from running the Random Forest algorithm 20 times.
   
# Usage Instructions
   All code is designed to be run in a Linux environment. R and the necessary R packages 
   (e.g., those for Random Forest, SVM) must be installed.
   
   You will need the design_1813_3_1.csv file and the pfam.txt matrix file.
   
   Within the "Code" folder, modify the file paths in the R scripts to point to the location 
   of your data. Then, run the scripts to generate the results.
   
