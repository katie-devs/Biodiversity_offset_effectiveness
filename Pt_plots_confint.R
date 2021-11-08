
# -----------------------TTF ----------------------------------#

summary(TTFa)        # Only Year is significant predictor. No significant difference in the relationship between Year and log annual defor between control and treated samples.

# Plot the significant line from the parallel trends regression lm(log_annual_defor ~ Year*TreatedF, data= TTF_data_before)

TTFa$fitted.values

# Add to TTF_data_before dataframe as this supplies the points to be plotted.

TTF_data_before$Year_fit <- 0

# Only interested in the fitted values for the Year line for the control group. This is the first 13 values

TTF_data_before$Year_fit <- rep(TTFa$fitted.values[1:13])

# Add columns for Upper and Lower Confidence intervals

TTF_data_before$Upper <- 0
TTF_data_before$Lower <- 0

# Use predict function to get the confidence intervals around the fitted values. 

TTF_new_dat <- data.frame(Year = c(1:13), TreatedF = 0)                # Tell predict we want to predict values for Years 1-13 for the control group (as this is the significant trend).
TTF_new_dat$TreatedF <- factor(TTF_new_dat$TreatedF, levels = c(0,1))  # Make TreatedF a factor

TTF_cont_pred <- data.frame(predict(TTFa, newdata = TTF_new_dat, interval = 'confidence'))  

# Add the predicted confidence intervals around the fitted values to the dataframe
# Because there is no significant difference in the slope of the relationship between Year and Deforestation between the control and treated samples
# I want the values for the treated sample to be the same. (NB. this is why I didn't use the whole set of fitted values from the model because they
# are slightly different for the treated sample, however this difference is not significant)

TTF_data_before[, c(9,10)] <- rep(TTF_cont_pred[, c(2,3)])

names(TTF_data_before)[c(9,10)] <- c("Lower", "Upper")   # Re-order column names

TTFa_plot <- ggplot(TTF_data_before, aes(x=Year, y=log_annual_defor, group = Sample, color= Sample))+
  geom_point(size=3.5, shape = 18)+
  scale_color_manual(values=c("TTF" = "#56B4E9","Cont" = "grey50"))+
  geom_line(aes(x = Year, y = Year_fit), colour = "navy", size = 1)+                    # Plot fitted values Year and Upper and Lower CIs
  geom_line(aes(x = Year, y = Lower), linetype = "dashed", colour = "navy", size = 1)+
  geom_line(aes(x = Year, y = Upper), linetype = "dashed", colour = "navy", size = 1)+
  scale_y_continuous(name = "log( deforestation+1 )", breaks = seq(0,4.5,0.5))+ 
  scale_x_continuous(name= "Year", breaks = seq(1,13,2), labels = c("2001", "2003", "2005", "2007","2009", "2011", "2013"))+
  theme_classic()+
  theme(legend.position = "none", axis.title.x = element_text(size=10, face="bold", vjust=0.5), axis.title.y= element_text(size=10, face="bold", vjust = 3),
        axis.text.x = element_text(face="bold", color = "grey15"), axis.text.y=element_text(face="bold", color = "grey15"))+
  ggtitle("Torotorofotsy")
 

# ------------------------- CZ ----------------------------------#

# For the Conservation Zone only the Treated term is significant. This means there is no significant trend 
# in deforestation over time at the 95% confidence interval in either the control or the treated group. However, on average
# deforestation is lower in the treated sample than the control sample (because the coefficent of TreatedF1 is negative)

CZa_res <- tidy(summary(CZa))
CZa_res <- cbind(CZa_res, confint(CZa))
names(CZa_res)[c(6,7)] <- c("Lower", "Upper")

# Intercept = the average deforestation in the pre-intervention period for the control sample 
# Upper and Lower confidence intervals of this estimate provided by confint

# Intercept + Treated coefficient gives the average deforestation for the treated sample (1.42 +- 1.599)
# Intercept + Upper Confidence interval of the estimated Treated coefficient gives the upper confidence interval of average deforestation in the treated sample.
# Intercept + Lower confidence interval of the estimated Treated coefficient gives the lower confidence interval of the average deforestation in the treated sample

# When plotted the two confidence intervals overlap but because the upper confidence interval of the Treated Sample is below the mean estimate of the Control Sample
# solid grey line, the difference is still significant.# 


CZa_plot <- ggplot(CZ_data_before, aes(x=Year, y=log_annual_defor, group = Sample, color= Sample))+
  geom_point(size=3.5, shape = 18)+
  geom_segment(x= 1, xend = 8, y = CZa_res[1,2], yend = CZa_res[1,2],  color = "grey50", size = 1)+     # Intercept
  geom_segment(x= 1, xend = 8, y =  CZa_res[1,2] + CZa_res[3,2], yend = CZa_res[1,2] + CZa_res[3,2], color = "#F0E442", size= 1)+  # Intercept - estimate of TreatedF1.
  
  geom_segment(x= 1, xend = 8, y = CZa_res[1,6], yend = CZa_res[1,6],  color = "grey50", size = 1, linetype = "dashed")+     # Lower CI for control 
  geom_segment(x= 1, xend = 8, y =  CZa_res[1,7], yend = CZa_res[1,7], color = "grey50", size= 1, linetype = "dashed")+  # Upper CI for control
  
  geom_segment(x= 1, xend = 8, y =  CZa_res[1,2] + CZa_res[3,6], yend = CZa_res[1,2] + CZa_res[3,6], color = "#F0E442", size= 1, linetype = "dashed")+  # Lower CI for treated (Intercept + lower CI of treated estimate) 
  geom_segment(x= 1, xend = 8, y =  CZa_res[1,2] + CZa_res[3,7], yend = CZa_res[1,2] + CZa_res[3,7], color = "#F0E442", size= 1, linetype = "dashed")+  # Upper CI for treated (Intercept + upper CI of treated estimate)
  
  scale_color_manual(values=c("#F0E442","grey50" ))+
  scale_y_continuous(name = "log( deforestation+1 )", breaks = seq(-0.5,3.5,0.5))+ 
  scale_x_continuous(name= "Year", breaks = seq(1,13,2), labels = c("2001", "2003", "2005", "2007","2009", "2011", "2013"))+
  expand_limits(x=13, y = -1.5)+
  theme_classic()+
  theme(legend.title = element_blank(), axis.title.x = element_text(size=10, face="bold", vjust=0.5), axis.title.y= element_text(size=10, face="bold", vjust = 3),
        axis.text.x = element_text(face="bold", color = "grey15"), axis.text.y=element_text(face="bold", color = "grey15"))+
  ggtitle("Conservation Zone")


# -------------------- CFAM --------------------------------#

summary(CFAMa)

# Year and the interaction between Year and Treated status are significant. This means that there is a significant trend in deforestation over time in the control sample (Year)
# and the treated sample, but the slope of this relationship differs significantly between the two samples. Need to plot both the time trend for each sample

# Extract fitted values from regression and append to data frame used for plotting

CFAM_data_before$Year_fit <- CFAMa$fitted.values
CFAM_data_before$Upper <- 0
CFAM_data_before$Lower <- 0

# Use predict to get the confidence intervals around the fitted values.
# Predict deforestation each year for the control (0) and treated sample (1) using the regression and get the upper and lower confidence
# intervals around these estimates.

new_dat_cont <- data.frame(Year = c(1:12), TreatedF = 0)
new_dat_cont$TreatedF <- factor(new_dat_cont$TreatedF, levels = c(0,1))

new_dat_treat <- data.frame(Year = c(1:12), TreatedF = 1)
new_dat_treat$TreatedF <- factor(new_dat_treat$TreatedF, levels = c(0,1))
  
CFAM_cont_pred <- data.frame(predict(CFAMa, newdata= new_dat_cont, interval = 'confidence'))
CFAM_treat_pred <- data.frame(predict(CFAMa, newdata= new_dat_treat, interval = 'confidence'))

# Append fitted values plus upper and lower CIs generated using predict to the dataframe used for plotting. 

CFAM_data_before[c(1:12), c(9,10)] <- CFAM_treat_pred[,c(2,3)]     # Treated values
CFAM_data_before[c(13:24), c(9,10)] <- CFAM_cont_pred[,c(2,3)]     # Control values

CFAMc_plot <- ggplot(CFAM_data_before, aes(x=Year, y=log_annual_defor, group = Sample, color= Sample))+
  geom_point(size=3.5, shape = 18)+
  geom_line(aes(x = Year, y = Year_fit), size =1)+
  geom_line(aes(x= Year, y = Lower), linetype = "dashed", size =1)+
  geom_line(aes(x= Year, y = Upper), linetype = "dashed", size = 1)+
  scale_color_manual(values=c("chartreuse3","grey50"))+
  scale_y_continuous(name = "log( deforestation+1 )", breaks = seq(0,5,0.5))+ 
  scale_x_continuous(name= "Year", breaks = seq(1,13,2), labels = c("2001", "2003", "2005", "2007","2009", "2011", "2013"))+
  expand_limits(x=13)+
  theme_classic()+
  theme(legend.title = element_blank(), axis.title.x = element_text(size=10, face="bold", vjust=0.5), axis.title.y= element_text(size=10, face="bold", vjust = 3),
        axis.text.x = element_text(face="bold", color = "grey15"), axis.text.y=element_text(face="bold", color = "grey15"))+
  ggtitle("CFAM")

