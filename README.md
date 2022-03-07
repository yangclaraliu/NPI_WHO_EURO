# The changing impact of non-pharmaceutical interventions on SARS-CoV-2 transmission within the context of variants of concern and increasing vaccination in the WHO European Region

Authors: Charlie Diamond, Richard Pebody, Mark Jit, Yang Liu

  
  *Correspondence to: yang.liu@lshtm.ac.uk* 
  
## Abstract  

#### Background 

Throughout the ongoing COVID-19 pandemic, public health and social measures (PHSMs) have been used extensively across Europe to reduce the transmission of SARS-CoV-2. Estimating their individual effectiveness is beneficial to public health planning intervention planning, yet challenging in practice due to structural confounding. In addition, early evidence may be outdated with the emergence, and subsequent dominance, of more transmissible variants of concern (VOCs) and increasing vaccination coverage. We seek to identify the potentially changing effectiveness of PHSMs as the pandemic has progressed. 

#### Methods 

We adapted an existing panel regression model to estimate the effectiveness of 14 categories of PHSMs on the transmission of SARS-CoV-2 (as measured by the time-varying reproduction number, Rt) while adjusting for COVID-19 vaccine coverage across 3 distinct time periods characterised by different dominating SARS-CoV-2 strains (Wild Type (WT), Alpha and Delta), across 49 WHO European Region countries. We included a hierarchical cluster analysis to identify any potential structural confounding in the timing and intensity of PHSMs. 

#### Results 

We found consistent evidence for negative association between workplace closure, the cancelation of public events, restrictions on gatherings and face covering policy on reduced Rt. The effects of vaccine coverage have also been shown to significantly reduce Rt, as expected. Whilst we also observed evidence for the potential effectiveness of school closure throughout the WT and the Alpha dominating periods, no association was found during the Delta dominating period.  Evidence was also mixed for international travel controls and economic responses. Due to temporal clustering between many interventions, we were unable to confidently estimate the effectiveness of every PHSM.

#### Conclusions 

Estimating the effects of PHSMs on the transmission of SARS-CoV-2 remains challenging due to the nature of the observational approach and the temporal clustering in PHSMs implementation. Here, we were able to present the effectiveness of some, but not all PHSM. The effects of vaccines in reducing the transmission of SARS-CoV-2 is evident alongside PHSMs.


### Code

To reproduce the analysis, these files should be run in the following order:  

-`0_load_data.R`: Load all data (if `data/joined.RDS` does not exist yet) and packages/ custom functions needed.  

-`1_descriptive_plots.R`: Descriptive plots.

-`2_panel_regression.R`: Regression models.

-`3_cluster_analysis.R`: Temporal clustering exercise using hierarchical clustering + statistical significant based on bootstrapping.  


