# The changing impact of non-pharmaceutical interventions on SARS-CoV-2 transmission within the context of variants of concern and increasing vaccination in the WHO European Region

Authors: Charlie Diamond, Richard Pebody, Mark Jit, Yang Liu

  
  *Correspondence to: yang.liu@lshtm.ac.uk* 
  
## Abstract  

#### Background 

Throughout the ongoing COVID-19 pandemic, public health and social measures (PHSMs) have been used extensively across Europe to reduce the transmission of SARS-CoV-2. Estimating their individual effectiveness is beneficial to public health planning intervention planning, yet challenging in practice due to structural confounding. In addition, early evidence may be outdated with the emergence, and subsequent dominance, of more transmissible variants of concern (VOCs) and increasing vaccination coverage. We seek to identify the potentially changing effectiveness of PHSMs as the pandemic has progressed. 

#### Methods 

We adapted an existing panel regression model to estimate the effectiveness of 14 categories of non-pharmaceutical PHSMs as well as the effect of vaccine coverage on the transmission of SARS-CoV-2 (as measured by the time-varying reproduction number, Rt) across 3 distinct time periods characterised by different dominating SARS-CoV-2 strains (Wild Type (WT), Alpha and Delta), across 49 WHO European Region countries. We conducted a hierarchical cluster analysis to identify any temporal clustering of PHSMs.

#### Results 

We found strongest evidence for the effectiveness of school closure, workplace closure, the cancellation of public events and restrictions on gatherings on reducing Rt. However, the effectiveness of school closure may have diminished during the Delta dominant period. The effects of high vaccine coverage within a population were also shown to be associated with significant reductions in Rt. Mild evidence for the effectiveness of face covering policies and contact tracing on reducing transmission was also found. We found weak or no evidence for negative associations between all other PHSMs investigated and Rt. Due to temporal clustering between many interventions, we were unable to confidently estimate the effectiveness of every PHSM.

#### Conclusions 

Estimating the effects of PHSMs on the transmission of SARS-CoV-2 remains challenging due to the nature of the observational approach, the temporal clustering in PHSMs implementation and emergence of variants. Here, we were able to present the effectiveness of some, but not all PHSMs. The effects of vaccines in reducing the transmission of SARS-CoV-2 is evident alongside PHSMs.


### Code

To reproduce the analysis, these files should be run in the following order:  

-`V8_load_data.R`: Load all data (if `data/joined_all_V8.RDS` does not exist yet) and packages/ custom functions needed.  

-`V8_1_descriptive_plots.R`: Descriptive plots.

-`V8_2_panel_regression.R`: Regression models.

-`V8_3_cluster_analysis.R`: Temporal clustering exercise using hierarchical clustering + statistical significant based on bootstrapping.  


