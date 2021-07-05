# Biodiversity_offset_effectiveness
This repository contains code and input data used in the submitted paper 'Madagascar's biggest mine is set to achieve No Net Loss of Forest'

# Input data

- Final_control.dbf
- Sample_TTF3.dbf
- Sample_ANK3.dbf
- Sample_CFAM3.dbf
- Sample_CZ3.dbf

These are the input data for the matching and outcome regressions. Each observation contains covariate and outcome variable values at sample pixel locations (denoted in X,Y co-ordinates) from the four offsets and the control area. The outcome variable is Tree Loss Year from the Global Forest Change dataset of Hansen et al (2013) and refers to the year in which a pixel was deforested between 2001 and 2019. If the pixel was not deforested during this period the value is set as 0.

- ANK_var.dbf
- CFAM_var.dbf
- CZ_var.dbf
- Torotorofotsy_var.dbf

These database files contain forest cover and annual tree loss data for the whole area of each biodiversity offset. These data were used to quantify the avoided deforestation following protection. 

# Scripts

1) Choosing_matching_spec. This details the alternative matching specifications trialled before selecting the main specification. 
2) Offset_Effectiveness_Final2 is the code for the matching and outcome regressions using the main matching specification. This produces the main results presented in the paper.
3) ... 



