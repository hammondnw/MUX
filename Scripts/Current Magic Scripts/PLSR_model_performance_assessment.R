#*****************************************************************
#* TITLE:  PLSR Model Performance Correlation Assessment
#*                     
#* AUTHORS: Nick Hammond                                         
#* LAST UPDATED: 31 Oct 2022
#*                                    
#* NOTES: This script analyzes the correlation between multiple variables related to
#*  PLSR model performance to see what drives R2 and RMSEP
#*  - Data are derived from Tables 1 and SI1 in the MUX manuscript
#*                                  
#*        
#*****************************************************************
library(tidyverse)

model_results = tibble(ID = c("TFe_epi_TO","TFe_hypo_TO", "SFe_epi_TO", "SFe_hypo_TO",
                              "TMn_epi_TO","TMn_hypo_TO", "SMn_epi_TO", "SMn_hypo_TO",
                              "TFe_epi_OO","TFe_hypo_OO", "SFe_epi_OO", "SFe_hypo_OO",
                              "TMn_epi_OO","TMn_hypo_OO", "SMn_epi_OO", "SMn_hypo_OO"),
                       n = c(35,34,35,34,36,35,35,35,47,46,48,45,47,46,47,46),
                       RMSEP_med = c(0.26,0.15,0.33,3.0,0.32,0.13,0.6,0.15,0.13,0.43,
                                     0.3,0.75,0.17,0.22,0.4,0.23),
                       R2 = c(0.97,0.84,0.73,0.06,0.97,0.91,0.96,0.90,0.73,0.65,0.79,
                              0.75,0.61,0.58,0.36,0.57),
                       comps = c(5,3,5,3,5,4,5,3,4,4,4,4,4,4,4,4),
                       cal_sd = c(0.34,1.29,0.02,0.06,0.19,0.54,0.17,0.56,
                                  0.11,1.85,0.15,2.28,0.01,0.19,0.005,0.19),
                       cal_med = c(0.43,3.49,0.06,0.02,0.19,1.52,0.1,1.47,0.52,2.81,0.27,1.91,
                                   0.03,0.65,0.01,0.64),
                       cal_range = c(1.26, 4.48, 0.10, 0.30, 0.72, 1.77, 0.64, 1.73, 
                                     0.40, 6.82, 0.57, 7.04, 0.03, 0.93, 0.02, 0.94))

# examine data
jpeg('MUX_PLSR_performance_assessment_111522.jpeg', width = 190, height =160, units = 'mm', res = 600)
pairs(model_results[,2:8], gap=0)
dev.off()
cor(model_results[,2:8]) # correlation matrix gives R-values for each variable pair

# run multivariate regression
attach(model_results)
summary(lm(R2~n+comps+cal_sd+cal_med+cal_range),corr=TRUE)
summary(lm(RMSEP_med~n+comps+cal_sd+cal_med+cal_range),corr=TRUE)
detach(model_results)

# Try removing outlier (SFE_Hypo_TO)
model_results = model_results[-4,]

# examine data
#jpeg('MUX_PLSR_performance_assessment_103122.jpeg', width = 190, height =160, units = 'mm', res = 600)
pairs(model_results[,2:8], gap=0)
#dev.off()
cor(model_results[,2:8]) # correlation matrix gives R-values for each variable pair

# run multivariate regression
attach(model_results)
summary(lm(R2~n+comps+cal_sd+cal_med+cal_range),corr=TRUE)
summary(lm(RMSEP_med~n+comps+cal_sd+cal_med+cal_range),corr=TRUE)
detach(model_results)

