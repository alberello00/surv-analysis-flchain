## Survival Analysis using the "flchain" Dataset
# Overview
The analysis is done using data coming from bone marrow samples, the focus is on two particular proteins involved in the body's immune responce. The proteins are called "Serum free light chains (FLC)" (type Kappa and type Lambda). In plasma cells of the bone marrow the concentration of the two proteins is balanced, in normal circumstances. However, in certain disease conditions, such as multiple myeloma or other plasma cell disorders, the production of one type of FLC may increase.  

Serum free light chains (FLCs) are typically measured using a laboratory test called serum protein electrophoresis (SPEP). SPEP is a type of blood test that separates the different proteins in the blood based on their electrical charge and size. During the test, a small amount of blood is drawn from the patient and placed on a special gel. An electrical current is then applied to the gel, which causes the proteins to move along the gel at different speeds based on their charge and size.

The data is taken from residents of the county of Omlster (Rochester) in Minnesota, the study was done on 2/3 of the population aged over 50 years. 

# Goals

# Contents
The "flchain" dataset is contained in the survival dataset in R. 
The dataset contains the following variables:
- age: age in years at the time of the study
- sex: gender (male or female)
- sample.yr: year of sample collection
- kappa: serum kappa free light chain (FLC) concentration (mg/L)
- lambda: serum lambda FLC concentration (mg/L)
- flc.grp: FLC group (based on the ratio of kappa to lambda FLC concentrations)
- creatinine: serum creatinine concentration (mg/dL)
- mgus: monoclonal gammopathy of undetermined significance (MGUS) status (yes or no)
- futime: follow-up time in days from sample collection to death or censoring
- death: indicator variable for death (1 if death occurred during follow-up, 0 if subject was censored)
# Requirements

# Acknowledgments




