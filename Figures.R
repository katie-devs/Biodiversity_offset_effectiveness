

#                                 1) Covariate Balance plot

cov_balance_plot <- function(input, colour1, name){
  result <- ggplot(input, aes(x= factor(sample, level= c("Non-matched", "matched")), y=SMD_1_cal, group= covariates))+
    geom_line(color= colour1)+
    geom_text_repel(data=subset(input, sample=="Non-matched"), label= variables, hjust=1.3, direction="y", size = 3.5, colour = "grey15", segment.color = colour1)+
    geom_point(size=2.5, colour = colour1) +
    annotate("rect", xmin = 0, xmax = Inf, ymin = -0.25, ymax = 0.25,alpha = .2)+
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40")+
    scale_x_discrete(name="Sample")+
    scale_y_continuous(name="Standardised Difference in Means", breaks = round(seq(min(input[,3]), max(input[,3]) , 0.2), digits = 1))+
    #scale_y_continuous(name="Standardised Difference in Means", breaks = seq(-1.4, 0.8, 0.2), labels = scales::comma)+
    theme_classic()+
    theme(legend.title = element_blank(), axis.title.x = element_text(size=10, face="bold", vjust= 0.5), axis.title.y= element_text(size=10, face="bold", vjust = 3),
          axis.text.x = element_text(face="bold", color = "grey15"), axis.text.y=element_text(face="bold", color = "grey15"))+
    ggtitle(name)
  return(result)
  }


input <- cov_balance_TTF2
colour1 <- "#56B4E9"
name <- "Torotorofotsy"


cov_balance_plot(cov_balance_ANK2, "#E69F00", "Ankerana")
cov_balance_plot(cov_balance_CFAM2, "chartreuse3", "CFAM")
cov_balance_plot(cov_balance_CZ2,"#F0E442","Conservation Zone")
cov_balance_plot(cov_balance_TTF2, "#56B4E9", "Torotorofotsy")



# ----------------------------------------------------------------------------------------------------------------------------------------------------------------#


#                                  2) Percentage Annual Deforestation over whole study period 

defor_plot2 <- function(a,c,d){
  ggplot(a, aes(x=Year, y=Perc_Annual_Defor, group= Sample, colour=Sample))+
    geom_point(size= 3.5, shape = 18)+
    #geom_line(size=0.75)+
    geom_vline(xintercept = c, linetype="dashed", color = "brown", size= 0.75)+
    scale_color_manual(values=c("#CC3300","#FFCC33" ))+
    scale_y_continuous(name="Annual Deforestation Rate")+
    scale_x_continuous(name= "Year", breaks = seq(1,19,2), labels = c("2001", "2003", "2005", "2007", "2009", "2011", "2013", "2015", "2017", "2019"))+
    theme_classic()+
    theme(legend.title = element_blank(), axis.title.x = element_text(size=10, face="bold", vjust=0.5), axis.title.y= element_text(size=10, face="bold", vjust = 3),
          axis.text.x = element_text(face="bold", color = "grey15"), axis.text.y=element_text(face="bold", color = "grey15"))+
    ggtitle(d)
}


defor_plot2(annual_defor_ANK, 11, "Ankerana")
defor_plot2(annual_defor_CFAM, 13, "CFAM")
defor_plot2(annual_defor_CZ, 9, "Conservation Zone")
defor_plot2(annual_defor_TTF, 14, "Torotorofotsy")

ggplot(annual_defor_ANK, aes(x=Year, y=Perc_Annual_Defor, group= Sample, colour=Sample))+
  geom_point(size= 3.5, shape = 18)+
  geom_vline(xintercept = 11, linetype="dashed", color = "grey25", size= 0.75)+
  scale_color_manual(values=c("#E69F00","grey50"))+
  scale_y_continuous(name="Annual Deforestation Rate (%)", breaks = seq(0, 4, 1))+
  scale_x_continuous(name= "Year", breaks = seq(1,19,2), labels = c("2001", "2003", "2005", "2007", "2009", "2011", "2013", "2015", "2017", "2019"))+
  theme_classic()+
  theme(legend.title = element_blank(), axis.title.x = element_text(size=10, face="bold", vjust=0.5), axis.title.y= element_text(size=10, face="bold", vjust = 3),
        axis.text.x = element_text(face="bold", color = "grey15"), axis.text.y=element_text(face="bold", color = "grey15"))+
  ggtitle("Ankerana")+
  ggsave("Graphics/ANK_defor_rate_final.jpeg")

ggplot(annual_defor_CFAM, aes(x=Year, y=Perc_Annual_Defor, group= Sample, colour=Sample))+
  geom_point(size= 3.5, shape = 18)+
  geom_vline(xintercept = 13, linetype="dashed", color = "grey25", size= 0.75)+
  scale_color_manual(values=c("chartreuse3", "grey50"))+
  scale_y_continuous(name="Annual Deforestation Rate (%)", breaks = seq(0, 7, 1))+
  scale_x_continuous(name= "Year", breaks = seq(1,19,2), labels = c("2001", "2003", "2005", "2007", "2009", "2011", "2013", "2015", "2017", "2019"))+
  theme_classic()+
  theme(legend.title = element_blank(), axis.title.x = element_text(size=10, face="bold", vjust=0.5), axis.title.y= element_text(size=10, face="bold", vjust = 3),
        axis.text.x = element_text(face="bold", color = "grey15"), axis.text.y=element_text(face="bold", color = "grey15"))+
  ggtitle("CFAM")+
  ggsave("Graphics/CFAM_defor_rate_final.jpeg")

ggplot(annual_defor_CZ, aes(x=Year, y=Perc_Annual_Defor, group= Sample, colour=Sample))+
  geom_point(size= 3.5, shape = 18)+
  geom_vline(xintercept = 9, linetype="dashed", color = "grey25", size= 0.75)+
  scale_color_manual(values=c("#F0E442", "grey50"))+
  scale_y_continuous(name="Annual Deforestation Rate (%)", breaks = seq(0, 5, 1))+
  scale_x_continuous(name= "Year", breaks = seq(1,19,2), labels = c("2001", "2003", "2005", "2007", "2009", "2011", "2013", "2015", "2017", "2019"))+
  theme_classic()+
  theme(legend.title = element_blank(), axis.title.x = element_text(size=10, face="bold", vjust=0.5), axis.title.y= element_text(size=10, face="bold", vjust = 3),
        axis.text.x = element_text(face="bold", color = "grey15"), axis.text.y=element_text(face="bold", color = "grey15"))+
  ggtitle("CZ")+
  ggsave("Graphics/CZ_defor_rate_final.jpeg")

ggplot(annual_defor_TTF, aes(x=Year, y=Perc_Annual_Defor, group= Sample, colour=Sample))+
  geom_point(size= 3.5, shape = 18)+
  geom_vline(xintercept = 14, linetype="dashed", color = "grey25", size= 0.75)+
  scale_color_manual(values=c("#56B4E9", "grey50"))+
  scale_y_continuous(name="Annual Deforestation Rate (%)", breaks = seq(0, 12.5, 1))+
  scale_x_continuous(name= "Year", breaks = seq(1,19,2), labels = c("2001", "2003", "2005", "2007", "2009", "2011", "2013", "2015", "2017", "2019"))+
  theme_classic()+
  theme(legend.title = element_blank(), axis.title.x = element_text(size=10, face="bold", vjust=0.5), axis.title.y= element_text(size=10, face="bold", vjust = 3),
        axis.text.x = element_text(face="bold", color = "grey15"), axis.text.y=element_text(face="bold", color = "grey15"))+
  ggtitle("Torotorofotsy")+
  ggsave("Graphics/TTF_defor_rate_final.jpeg")



# ----------------------------------------------------------------------------------------------------------------------------------------------------------------#


#                                 3) Parallel trends                                      
                                        
ANKa_plot <- ggplot(ANK_data_before, aes(x=Year, y=log_annual_defor, group = Sample, color= Sample))+
  geom_point(size= 3.5, shape = 18)+
  scale_color_manual(values=c("#E69F00","grey50"))+
  scale_y_continuous(name = "log( deforestation+1 )", breaks = seq(0,4,0.5))+ 
  scale_x_continuous(name= "Year", breaks = seq(1,13,2), labels = c("2001", "2003", "2005", "2007", "2009", "2011", "2013"))+
  expand_limits(x= 13) +
  theme_classic()+
  theme(legend.title = element_blank(), axis.title.x = element_text(size=10, face="bold", vjust=0.5), axis.title.y= element_text(size=10, face="bold", vjust = 3),
        axis.text.x = element_text(face="bold", color = "grey15"), axis.text.y=element_text(face="bold", color = "grey15"))+
  ggtitle("Ankerana")+
  ggsave("ANK_pt_fit_FINAL.jpeg")


# CFAM  # 

CFAMc_plot <- ggplot(CFAM_data_before, aes(x=Year, y=log_annual_defor, group = Sample, color= Sample))+
  geom_point(size=3.5, shape = 18)+
  #geom_smooth(method = "lm", se= TRUE)+
  geom_segment(x = 1, y= 2.44, xend = 12, yend = 4.52, colour = "grey50", size = 1)+
  geom_segment(x = 1, y= 1.375, xend = 12, yend = 0.91, colour = "chartreuse3", size = 1)+
    #geom_abline(slope = CFAMc$coefficients[2], intercept = CFAMc$coefficients[1], color = "green4", size= 1)+
  #geom_abline(slope= CFAMc$coefficients[2] + CFAMc$coefficients[4], intercept = CFAMc$coefficients[1] + CFAMc$coefficients[3], color = "chartreuse3", size =1)+
  scale_color_manual(values=c("chartreuse3","grey50"))+
  scale_y_continuous(name = "log( deforestation+1 )", breaks = seq(0,5,0.5))+ 
  scale_x_continuous(name= "Year", breaks = seq(1,13,2), labels = c("2001", "2003", "2005", "2007","2009", "2011", "2013"))+
  expand_limits(x=13)+
  theme_classic()+
  theme(legend.title = element_blank(), axis.title.x = element_text(size=10, face="bold", vjust=0.5), axis.title.y= element_text(size=10, face="bold", vjust = 3),
        axis.text.x = element_text(face="bold", color = "grey15"), axis.text.y=element_text(face="bold", color = "grey15"))+
  ggtitle("CFAM")+
  ggsave("CFAM_pt_fit_FINAL.jpeg")


CFAMc$fitted.values # <-- y and yend taken from here. 



# CZ #


CZb_plot <- ggplot(CZ_data_before, aes(x=Year, y=log_annual_defor, group = Sample, color= Sample))+
  geom_point(size=3.5, shape = 18)+
  geom_segment(x= 1, xend = 8, y = CZb$coefficients[1], yend = CZb$coefficients[1],  color = "grey50", size = 1)+     # Intercept
  geom_segment(x= 1, xend = 8, y =  CZb$coefficients[1]+ CZb$coefficients[3], yend = CZb$coefficients[1]+ CZb$coefficients[3], color = "#F0E442", size= 1)+  # Intercept - estimate of TreatedF1.
  scale_color_manual(values=c("#F0E442","grey50" ))+
  scale_y_continuous(name = "log( deforestation+1 )", breaks = seq(-0.5,3.5,0.5))+ 
  scale_x_continuous(name= "Year", breaks = seq(1,13,2), labels = c("2001", "2003", "2005", "2007","2009", "2011", "2013"))+
  expand_limits(x=13, y = -0.5)+
  theme_classic()+
  theme(legend.title = element_blank(), axis.title.x = element_text(size=10, face="bold", vjust=0.5), axis.title.y= element_text(size=10, face="bold", vjust = 3),
        axis.text.x = element_text(face="bold", color = "grey15"), axis.text.y=element_text(face="bold", color = "grey15"))+
  ggtitle("Conservation Zone")+
  ggsave("CZ_pt_fit_FINAL.jpeg")

summary(CZb)


# TTF # 


TTFa_plot <- ggplot(TTF_data_before, aes(x=Year, y=log_annual_defor, group = Sample, color= Sample))+
  geom_point(size=3.5, shape = 18)+
  geom_segment(x=1, y = 2.02, xend = 13, yend = 3.45, colour = "grey50", size =1)+
  #geom_smooth(method = "lm", se= TRUE)+
  #geom_abline(slope = TTFa$coefficients[2], intercept = TTFa$coefficients[1], size =1, color = "blue3")+
  #geom_line(aes(x = Year, y = fit), size = 1)+
  #geom_abline(slope = confint(TTFa)[2,1], intercept = confint(TTFa)[1,1])+
  #geom_abline(slope = confint(TTFa)[2,2], intercept = confint(TTFa)[1,2])+
  scale_color_manual(values=c("TTF" = "#56B4E9","Cont" = "grey50"))+
  scale_y_continuous(name = "log( deforestation+1 )", breaks = seq(0,4.5,0.5))+ 
  scale_x_continuous(name= "Year", breaks = seq(1,13,2), labels = c("2001", "2003", "2005", "2007","2009", "2011", "2013"))+
  theme_classic()+
  theme(legend.title = element_blank(), axis.title.x = element_text(size=10, face="bold", vjust=0.5), axis.title.y= element_text(size=10, face="bold", vjust = 3),
        axis.text.x = element_text(face="bold", color = "grey15"), axis.text.y=element_text(face="bold", color = "grey15"))+
  ggtitle("Torotorofotsy")+
  ggsave("TTF_pt_fit_FINAL.jpeg")

TTFa$fitted.values

summary(TTFa)

# ----------------------------------------------------------------------------------------------------------------------------------------------------------------#


#                                 4) Plot ATT from individual DiD and overall fixed effects


# Add fixed effects results to ATT_all

ATT_all[4,] <- 0
rownames(ATT_all)[4] <- "Overall"
ATT_all[4,1] <- (exp(coef(FE_all))-1)*100
ATT_all[4, 2:3] <- (exp(confint(FE_all))-1)*100
ATT_all$Sample <- factor(c("ANK", "CZ", "TTF", "Overall"), levels = c("ANK", "CZ", "TTF", "Overall"))


cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442")
#               grey      orange      sky blue  green       yellow

#                                               "chartreuse3" <- this is a much better green for CFAM

# Re-order for bar plot


ggplot(ATT_all, aes(x = Sample, y= ATT, fill = Sample))+
  geom_col(colour = "black", width = c(0.525, 0.27, 0.2, 1.5))+
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = c(0.4, 0.15, 0.1, 0.9))+
  scale_fill_manual(values = c("#E69F00", "#F0E442", "#56B4E9", NA))+
  scale_x_discrete(name = "Offset")+
  coord_cartesian(ylim=c(-150, 150))+
  scale_y_continuous(name = "Treatment Effect (% difference in deforestation)", breaks = seq(-150, 150, 50))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_classic()+
  theme(legend.position = "none", axis.title.x = element_text(size=11, vjust=1), axis.title.y= element_text(size=11, vjust = 1),
        axis.text.x = element_text(size = 11, color = "grey15"), axis.text.y=element_text(size= 11, color = "grey15"))

1.5* 0.13


# ----------------------------------------------------------------------------------------------------------------------------------------------------------------#


#                               5) Plot observed vs. counterfactual deforestation



a <- data.frame(t(Impact_defor[, c(2,3,6)]))
a[1:12, 1] <- data.frame(c(a[, "X10"], a[, "X12"], a[, "X7"], a[, "X1"]))
a$X <- rep(c("Observed", "Counterfactual", "Avoided Deforestation"),4)
a <- a[, c(1,5)]
a$Offset <- rep(c("ANK", "CZ", "TTF", "Overall"), each=3)
a <- a[, c(3,2,1)]
names(a) <- c("Offset", "X", "Total_Defor_Ha")
a$Upper_CI <- 0
a$Lower_CI <- 0
a$Upper_CI[c(2,5,8,11)] <- Impact_defor$Counterfactual_Defor_Upper
a$Lower_CI[c(2,5,8,11)] <- Impact_defor$Counterfactual_Defor_Lower
a$Upper_CI[c(3,6,9, 12)] <- (Impact_defor$Impact_defor_Upper)*-1
a$Lower_CI[c(3,6,9, 12)] <- (Impact_defor$Impact_defor_Lower)*-1
a$Offset <- factor(a$Offset, levels = c("ANK", "CZ", "TTF", "Overall"))

a$X <- factor(a$X, levels = c("Observed", "Counterfactual", "Avoided Deforestation"))

line <- data.frame(Y = -2064, Offset = "Overall")
line$Offset <- factor(line$Offset, levels = c(NA, NA, NA, "Overall"))

ggplot(a, aes(x = X, y= Total_Defor_Ha, fill = Offset))+
  geom_bar(stat = "identity", width = 0.9, colour = "black")+
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.4)+
  geom_hline(yintercept = 0, linetype = "dashed", colour = "red", size = 0.71)+
  geom_hline(data = line, aes(yintercept = Y), linetype = "dashed", colour = "chartreuse 4", size = 0.75)+
  facet_wrap(~ Offset)+
  scale_fill_manual(values = c("#E69F00", "#F0E442", "#56B4E9","#999999"))+
  scale_x_discrete(labels= c("Observed", "Counterfactual", "Avoided\nDeforestation"))+
  scale_y_continuous(name = "Total deforestation after protection (ha)", limits = c(-5500, 5500), breaks = seq(-5000, 5000, 2500))+
  theme_classic()+
  theme(legend.position = "none", axis.title.x = element_blank(), axis.title.y= element_text(size=11, vjust = 2),
        axis.text.x = element_text(size = 11, color = "grey15", angle = 90, hjust = 0.5), axis.text.y=element_text(size= 11, color = "grey15"),
        )
  
line <- data.frame(Y = -2064, Offset = "Overall")
line$Offset <- factor(line$Offset, levels = c(NA, NA, NA, "Overall"))

