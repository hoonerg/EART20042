# Load packages
library(readxl)
library(openair)
library(ggplot2)
library(dplyr)
library(ggmap)
library(PerformanceAnalytics)
library(fmsb)
library(car)

# Import dataset
FW_data <- read_excel("C:/Users/hoonc/Desktop/FW_data_report.xlsx")

###### Multiple regression analysis - SOC
  
  model_SOC_1<-lm(formula = SOC ~ pH + TN + Moisture + SR, data = FW_data)
  summary(model_SOC_1)
  plot(model_SOC_1)
  
  model_SOC_2<-lm(formula = SOC ~ pH + Moisture + SR, data = FW_data)
  summary(model_SOC_2)
  plot(model_SOC_2)
  
  # Compare two models (+-TN)
  anova(model_SOC_1, model_SOC_2)
  anova(model_SOC_2, model_SOC_1)
  
  
  # Check Multicollinearity
  vif(model_SOC_2)
  vif(model_SOC_1)
  
  # Predicted R squared
  
  pred_r_squared <- function(linear.model) {
    #' Use anova() to get the sum of squares for the linear model
    lm.anova <- anova(linear.model)
    #' Calculate the total sum of squares
    tss <- sum(lm.anova$'Sum Sq')
    # Calculate the predictive R^2
    pred.r.squared <- 1-PRESS(linear.model)/(tss)
    
    return(pred.r.squared)
  }
  
  PRESS <- function(linear.model) {
    #' calculate the predictive residuals
    pr <- residuals(linear.model)/(1-lm.influence(linear.model)$hat)
    #' calculate the PRESS
    PRESS <- sum(pr^2)
    
    return(PRESS)
  }
  
  pred_r_squared(model_SOC_2)
  pred_r_squared(model_pH_2)
  pred_r_squared(model_TN_2)
  pred_r_squared(model_M_2)
  pred_r_squared(model_SR_1)
  
  summary(model_SOC_2)
  summary(model_pH_2)
  summary(model_TN_2)
  summary(model_M_2)
  summary(model_SR_1)
  
  
  
  # Rank relative importance
  relweights <- function(fit,...){
    R <- cor(fit$model)
    nvar <- ncol(R)
    rxx <- R[2:nvar, 2:nvar]
    rxy <- R[2:nvar, 1]
    svd <- eigen(rxx)
    evec <- svd$vectors
    ev <- svd$values
    delta <- diag(sqrt(ev))
    lambda <- evec %*% delta %*% t(evec)
    lambdasq <- lambda ^ 2
    beta <- solve(lambda) %*% rxy
    rsquare <- colSums(beta^2)
    rawwgt <- lambdasq %*% beta^2
    import <- (rawwgt/rsquare)*100
    import <- as.data.frame(import)
    row.names(import) <- names(fit$model[2:nvar])
    names(import) <- "Weights"
    import <- import[order(import),1, drop = FALSE]
    dotchart(import$Weights, labels = row.names(import),
             xlab = "% of Relative Importance", pch = 19,
             main = "Relative Importance of Predictor Variables",
             sub = paste("Total R-Square=", round(rsquare, digits = 3)),
             ...)
    return(import)
  }
  
  relweights(model_SOC_2, col="blue")
  
  # Visualising relative importance
  plotRelWeights=function(fit){
    data<-relweights(fit)
    data$Predictors<-rownames(data)
    p<-ggplot(data=data,aes(x=reorder(Predictors,Weights),y=Weights,fill=Predictors))+ 
      geom_bar(stat="identity",width=0.5)+
      ggtitle("Relative Importance of Predictor Variables")+
      xlab("Independent Variables") +
      ylab(paste0("Relative Importance",attr(data,"R-square"),"(%)"))+
      geom_text(aes(y=Weights-0.1,label=paste(round(Weights,1),"%")),hjust=1)+
      guides(fill=FALSE)+
      coord_flip()
    p
  }
  
  plotRelWeights(model_SOC_2)
  plotRelWeights(model_SOC_1)

  plotRelWeights(model_SOC_2)
  plotRelWeights(model_pH_2)
  plotRelWeights(model_TN_2)
  plotRelWeights(model_M_2)
  plotRelWeights(model_SR_1)
  
  
#### Multiple Regression Analysis - pH -n(model)=1
  model_pH_1<-lm(formula = pH ~ SOC + TN + Moisture + SR, data = FW_data)
  summary(model_pH_1)
  plot(model_pH_1)
  # - SOC
  model_pH_2 <- lm(formula = pH ~ TN + Moisture + SR, data = FW_data)
  summary(model_pH_2)
  
  
  # VIF validation
  vif(model_pH_1)
  vif(model_pH_2)

  # Compare models
  anova(model_pH_1, model_pH_2)
  
  # Visualising
  relweights(model_pH_2, col="blue")
  plotRelWeights(model_pH_2)
  plotRelWeights(model_pH_1)
  
#### Multiple Regression Analysis - Moisture -n(model)=1
  model_M_1<-lm(formula = Moisture ~ SOC + TN + pH + SR, data = FW_data)
  summary(model_M_1)
  plot(model_pH_1)
  
  # - TN
  model_M_2<-lm(formula = Moisture ~ SOC + pH + SR, data = FW_data)
  summary(model_M_2)
  # VIF validation
  vif(model_M_1)
  vif(model_M_2)
  anova(model_M_1, model_M_2)
  
  # Visualising
  relweights(model_pH_2, col="blue")
  plotRelWeights(model_M_1)
  
 #### Multiple Regression Analysis - TN -n(model)=1
  model_TN_1<-lm(formula = TN ~ SOC + SR + Moisture + pH, data = FW_data)
  summary(model_TN_1)
  plot(model_SR_1)
  
  #@#@# 2- moisture
  model_TN_2 <- lm(formula = TN ~ SOC+ SR + pH, data = FW_data)
  summary(model_TN_2)
  # 3- SOC
  model_TN_3 <- lm(formula = TN ~ SR + Moisture + pH, data = FW_data)
  summary(model_TN_3)
  # VIF validation
  vif(model_TN_1)
  vif(model_TN_2)
  vif(model_TN_3)
  
  # Compare models
  anova(model_TN_1,model_TN_2)
  anova(model_TN_1,model_TN_3)
  anova(model_TN_2,model_TN_3)
  
  # Visualising
  relweights(model_pH_2, col="blue")
  plotRelWeights(model_pH_2)
  plotRelWeights(model_pH_1)
#### Multiple Regression Analysis - SR -n(model)=1
  model_SR_1<-lm(formula = SR ~ SOC + TN + Moisture + pH, data = FW_data)
  summary(model_SR_1)
  plot(model_SR_1)
  #@#@# - Moisture
  model_SR_2 <- lm(formula = SR ~ SOC+ TN + pH, data = FW_data)
  summary(model_SR_2)
  # - TN
  model_SR_3 <- lm(formula = SR ~ SOC+ Moisture + pH, data = FW_data)
  summary(model_SR_3)
  # - SOC
  model_SR_4 <- lm(formula = SR ~ TN + Moisture + pH, data = FW_data)
  # - pH
  model_SR_5 <- lm(formula = SR ~ SOC + Moisture + TN, data = FW_data)
  # VIF validation
  vif(model_SR_1)
  vif(model_SR_2)
  vif(model_SR_3)
  vif(model_SR_4)
  vif(model_SR_5)
  
  # Compare models
  anova(model_SR_1, model_SR_2)
  anova(model_SR_1, model_SR_3)
  anova(model_SR_1, model_SR_4)
  anova(model_SR_1, model_SR_5)
  
  # Visualising
  relweights(model_pH_2, col="blue")
  plotRelWeights(model_pH_2)
  plotRelWeights(model_pH_1)
  
# Corplot
corPlot(FW_data)
chart.Correlation(FW_data, histogram = FALSE, method = "pearson", main = "Correlation Matrix")

# VIF
vif(FW_data)
summary(FW_data)
var(FW_data)

VIF(lm(SOC ~ .,data = FW_data))
VIF(lm(pH ~ .,data = FW_data))
VIF(lm(TN ~ .,data = FW_data))
VIF(lm(Moisture ~ .,data = FW_data))
VIF(lm(SR ~ .,data = FW_data))

ggplot(FW_data aes(x = )) 


#CO2-CH4 plot
ggplot (data2, aes (x=CO2_ppm, y=CH4_ppb)) +
  geom_point (aes(), shape=20)+
  theme_classic ()+
  labs (x="CO2 (ppm)", y="CH4 (ppb)") + 
  theme (axis.title.x = element_text (family ='sans' , face = 2,    color = 'black', size=16)) +
  theme (axis.title.y = element_text (family ='sans' , face = 2, color = 'black', size=16)) +
  theme (axis.text.x = element_text (family ='sans' , face = 2, color = 'black', size=12)) +
  theme (axis.text.y = element_text (family ='sans' , face = 2, color = 'black', size=12)) +
  theme (title = element_text (family ='sans' , face = 2, color = 'black', size=15)) +
  geom_smooth(method = "loess", level = 0.95) +
  geom_smooth(method = "lm", level = 0.95)

#Correlation coefficient bw CO2 and CH4
cor24 <- cor(data2["CO2_ppm"], data2["CH4_ppb"], use = 'complete.obs', method = 'pearson')
print(cor24)

#CO-CO2 plot
ggplot (data3, aes (x=CO_ppb, y=CO2_ppm)) +
  geom_point(aes(), shape=20) +
  theme_classic()+
  labs (x="CO (ppb)", y="CO2 (ppm)") +
  theme (axis.title.x = element_text (family ='sans' , face = 2,    color = 'black', size=16)) +
  theme (axis.title.y = element_text (family ='sans' , face = 2, color = 'black', size=16)) +
  theme (axis.text.x = element_text (family ='sans' , face = 2, color = 'black', size=12)) +
  theme (axis.text.y = element_text (family ='sans' , face = 2, color = 'black', size=12)) +
  theme (title = element_text (family ='sans' , face = 2, color = 'black', size=15)) +
  geom_smooth(method = "loess", level = 0.95) +
  geom_smooth(method = "lm", level = 0.95)

#Extra ##CO-CH4 plot
ggplot (data3, aes (x=CO_ppb, y=CH4_ppb)) +
  geom_point(aes(), shape=20) +
  theme_classic()+
  labs (x="CO (ppb)", y="CH4 (ppb)") +
  theme (axis.title.x = element_text (family ='sans' , face = 2,    color = 'black', size=16)) +
  theme (axis.title.y = element_text (family ='sans' , face = 2, color = 'black', size=16)) +
  theme (axis.text.x = element_text (family ='sans' , face = 2, color = 'black', size=12)) +
  theme (axis.text.y = element_text (family ='sans' , face = 2, color = 'black', size=12)) +
  theme (title = element_text (family ='sans' , face = 2, color = 'black', size=15)) +
  geom_smooth(method = "loess", level = 0.95) +
  geom_smooth(method = "lm", level = 0.95)

#Correlation coefficient bw CO and CO2
cor02 <- cor(data2["CO_ppb"], data2["CO2_ppm"], use = 'complete.obs', method = 'pearson')
print(cor02)
#Correlation coefficient bw CO and CO2
#cor02 <- cor(data2$CO_ppb, data2$CO2_ppm, use = 'complete.obs', method = 'pearson')
#print(cor02)

#Correlation coefficient bw CO and CO2
cor04 <- cor(data2["CO_ppb"], data2["CH4_ppb"], use = 'complete.obs', method = 'pearson')
print(cor04)

#Correlation plot of all data
corPlot(data2)
corPlot(data3) #Stronger cor bw CO and CH4
corPlot(data4) #-1 weaker

###################Exercise 2##############

#CO2-Altitude plot
ggplot (data4, aes (x=CO2_ppm, y=ALT)) +
  geom_point(aes(), shape=20) +
  theme_classic()+
  labs (x="CO2 (ppm)", y="Altitude (m)") +
  theme (axis.title.x = element_text (family ='sans' , face = 2,    color = 'black', size=16)) +
  theme (axis.title.y = element_text (family ='sans' , face = 2, color = 'black', size=16)) +
  theme (axis.text.x = element_text (family ='sans' , face = 2, color = 'black', size=12)) +
  theme (axis.text.y = element_text (family ='sans' , face = 2, color = 'black', size=12)) +
  theme (title = element_text (family ='sans' , face = 2, color = 'black', size=15)) +
  geom_smooth(method = "loess", level = 0.95, aes(color = "red")) +
  geom_smooth(method = "lm", level = 0.95)

#Correlation coefficient bw CO2 and Altitude  
cor2alt <- cor(data2["CO2_ppm"], data2["ALT"], use = 'complete.obs', method = 'pearson')
print(cor2alt)

#CH4-Altitude plot
ggplot (data3, aes (x=CH4_ppb, y=ALT)) +
  geom_point(aes(), shape=20) +
  coord_cartesian(ylim = c(0, 3500)) +
  theme_classic()+
  labs (x="CH4 (ppb)", y="Altitude (m)") +
  theme (axis.title.x = element_text (family ='sans' , face = 2,    color = 'black', size=16)) +
  theme (axis.title.y = element_text (family ='sans' , face = 2, color = 'black', size=16)) +
  theme (axis.text.x = element_text (family ='sans' , face = 2, color = 'black', size=12)) +
  theme (axis.text.y = element_text (family ='sans' , face = 2, color = 'black', size=12)) +
  theme (title = element_text (family ='sans' , face = 2, color = 'black', size=15)) +
  geom_smooth(method = "loess", level = 0.95, aes(color = "red")) +
  geom_smooth(method = "lm", level = 0.95)

#Correlation coefficient bw CO2 and Altitude  
cor4alt <- cor(data2["CH4_ppb"], data2["ALT"], use = 'complete.obs', method = 'pearson')
print(cor4alt)
