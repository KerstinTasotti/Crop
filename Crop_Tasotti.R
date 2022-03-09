
#  title: 'HarvardX: PH125.9x Data Science - MovieLense project'
# author: "Kerstin Tasotti"
#   date: '2022-02-23'
# output: html_document
# editor_options: default
  
  

##########################################################
# Create train set, validation set (final hold-out test set)
##########################################################



if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(Metrics)) install.packages("Metrics", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(tinytex)) install.packages("tinytex", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(ggplot2)
library(Metrics)
library(caret)
library(data.table)
library(readr)
library(tinytex)

## Crop dataset:

### <https://www.kaggle.com/atharvaingle/crop-recommendation-dataset>

### <https://www.kaggle.com/atharvaingle/crop-recommendation-dataset/download>

# Crop dataset:
# https://github.com/KerstinTasotti/Crop.git



### Full dataset:
Crop_rec <- read.csv("https://raw.githubusercontent.com/KerstinTasotti/Crop/main/Crop_recommendation.csv", header=TRUE,stringsAsFactors = FALSE)

### Validation set will be 10% of Crop data
set.seed(1, sample.kind="Rounding") 
train_index <- createDataPartition(y=Crop_rec$label, p=0.9, list=FALSE, time=1)
train<- Crop_rec[train_index,]
validation <- Crop_rec[-train_index,]



### ANALYSIS OF THE MODEL ###
# Display the structure and summary statistic values of the data
str(train)

head(train) %>% print.data.frame

train %>% group_by(label) %>% summarise(count=n()) %>% arrange(desc(count))
m <- 90

summary(train)

# Show unique labels
unique(train$label)

# NITROGEN
train %>%group_by(label)%>%summarise(mean(N)) %>% print.data.frame()
train %>% group_by(label) %>% ggplot(aes(x=reorder(label,N/m),y=N/m)) + geom_bar(stat = "identity",  fill="steelblue") +labs(x="Crops", y="Nitrogen [ppm]", title="Nitrogen content for optimal crops") +  theme(text = element_text(size=15))+coord_flip()
ggplot(train,aes(N)) +geom_histogram(bins=30, fill="steelblue",color="black")+labs(x="Nitrogen [ppm]",y="n")+ggtitle("Distribution of Nitrogen")
ggplot(train,aes(N)) +geom_density()+labs(x="Nitrogen [ppm]",y="Density")+ggtitle("Distribution of Nitrogen")

# Phosphorus
train %>%group_by(label)%>%summarise(mean(P)) %>% print.data.frame()
train %>% group_by(label) %>% ggplot(aes(x=reorder(label,P/m),y=P/m)) + geom_bar(stat = "identity",  fill="steelblue") +labs(x="Crops", y="Phosphorus [ppm]", title="Phosphorus content for optimal crops") +  theme(text = element_text(size=15))+coord_flip()
ggplot(train,aes(P)) +geom_histogram(bins=30, fill="steelblue",color="black")+labs(x="Phosphorus [ppm]",y="n")+ggtitle("Distribution of Phosphorus")
ggplot(train,aes(P)) +geom_density()+labs(x="Phosphorus [ppm]",y="Density")+ggtitle("Distribution of Phosphorus")

# Potassium
train %>%group_by(label)%>%summarise(mean(K)) %>% print.data.frame()
train %>% group_by(label) %>% ggplot(aes(x=reorder(label,K/m),y=K/m)) + geom_bar(stat = "identity",  fill="steelblue") +labs(x="Crops", y="Potassium [ppm]", title="Potassium content for optimal crops") +  theme(text = element_text(size=15))+coord_flip() 
ggplot(train,aes(K)) +geom_histogram(bins=30, fill="steelblue",color="black")+labs(x="Potassium [ppm]",y="n")+ggtitle("Distribution of Potassium")
ggplot(train,aes(K)) +geom_density()+labs(x="Potassium [ppm]",y="Density")+ggtitle("Distribution of Potassium")

# pH
train %>%group_by(label)%>%summarise(mean(ph)) %>% print.data.frame()
train %>% group_by(label) %>% ggplot(aes(x=reorder(label,ph/m),y=ph/m)) + geom_bar(stat = "identity",  fill="steelblue") +labs(x="Crops", y="pH", title="pH for optimal crops") +  theme(text = element_text(size=15))+coord_flip() 
ggplot(train,aes(ph)) +geom_histogram(bins=30, fill="steelblue",color="black")+labs(x="pH",y="n")+ggtitle("Distribution of pH") 
ggplot(train,aes(ph)) +geom_density()+labs(x="pH",y="Density")+ggtitle("Distribution of pH")

# Temperature
train %>%group_by(label)%>%summarise(mean(temperature)) %>% print.data.frame()
train %>% group_by(label) %>% ggplot(aes(x=reorder(label,temperature/m),y=temperature/m)) + geom_bar(stat = "identity",  fill="steelblue") +labs(x="Crops", y="Temperature [degrees C]", title="Temperature for optimal crops") +  theme(text = element_text(size=15))+coord_flip()
ggplot(train,aes(temperature)) +geom_histogram(bins=30, fill="steelblue",color="black")+labs(x="Temperature [degrees C]",y="n")+ggtitle("Distribution of Temperature")
ggplot(train,aes(temperature)) +geom_density()+labs(x="Temperature",y="Density")+ggtitle("Distribution of Temperature")

# Rainfall
train %>%group_by(label)%>% summarise(mean(rainfall)) %>% print.data.frame()
train %>% group_by(label) %>% ggplot(aes(x=reorder(label,rainfall/m),y=rainfall/m)) + geom_bar(stat = "identity",  fill="steelblue") +labs(x="Crops", y="Rainfall [mm]", title="Rainfall for optimal crops") +  theme(text = element_text(size=15))+coord_flip()
ggplot(train,aes(rainfall)) +geom_histogram(bins=30, fill="steelblue",color="black")+labs(x="Rainfall [mm]",y="n")+ggtitle("Distribution of Rainfall")
ggplot(train,aes(rainfall)) +geom_density()+labs(x="Rainfall [mm]",y="Density")+ggtitle("Distribution of Rainfall")

# Humidity
train %>%group_by(label)%>%summarise(mean(humidity)) %>% print.data.frame()
train %>% group_by(label) %>% ggplot(aes(x=reorder(label,humidity/m),y=humidity/m)) + geom_bar(stat = "identity",  fill="steelblue") +labs(x="Crops", y="Humidity [%]", title="Humidity for optimal crops") +  theme(text = element_text(size=15))+coord_flip()
ggplot(train,aes(humidity)) +geom_histogram(bins=30, fill="steelblue",color="black")+labs(x="Humidity [%]",y="n")+ggtitle("Distribution of Humidity")
ggplot(train,aes(humidity)) +geom_density()+labs(x="Humidity [%]",y="Density")+ggtitle("Distribution of Humidity")

# SOIL CONDITION RANGE 
box1 <- ggplot(train,aes(y=N, group=label, x=label, fill=label))+stat_boxplot(geom="errorbar",width=0.5)+geom_boxplot()+ labs(y="Nitrogen") + theme (axis.text.x=element_text(angle=90, size=7),legend.position="none", axis.title.x=element_blank())
box2 <- ggplot(train,aes(y=K, group=label, x=label, fill=label))+stat_boxplot(geom="errorbar",width=0.5)+geom_boxplot()+ labs(y="Potassium") + theme (axis.text.x=element_text(angle=90, size=7),legend.position="none", axis.title.x=element_blank())
box3 <- ggplot(train,aes(y=P, group=label, x=label, fill=label))+stat_boxplot(geom="errorbar",width=0.5)+geom_boxplot()+ labs(y="Phosphorus") + theme (axis.text.x=element_text(angle=90, size=7),legend.position="none", axis.title.x=element_blank())
box4 <- ggplot(train,aes(y=ph, group=label, x=label, fill=label))+stat_boxplot(geom="errorbar",width=0.5)+geom_boxplot()+ labs(y="pH") + theme (axis.text.x=element_text(angle=90, size=7),legend.position="none", axis.title.x=element_blank())
grid.arrange(box1,box3,box2,box4, nrow=2)

# ENVIRONMENT CONDITION RANGE
box5 <- ggplot(train,aes(y=temperature, group=label, x=label, fill=label))+stat_boxplot(geom="errorbar",width=0.5)+geom_boxplot()+ labs(y="Temperature") + theme (axis.text.x=element_text(angle=90, size=7),legend.position="none", axis.title.x=element_blank())
box6 <- ggplot(train,aes(y=rainfall, group=label, x=label, fill=label))+stat_boxplot(geom="errorbar",width=0.5)+geom_boxplot()+ labs(y="Rainfall") + theme (axis.text.x=element_text(angle=90, size=7),legend.position="none", axis.title.x=element_blank())
box7 <- ggplot(train,aes(y=humidity, group=label, x=label, fill=label))+stat_boxplot(geom="errorbar",width=0.5)+geom_boxplot()+ labs(y="Humidity") + theme (axis.text.x=element_text(angle=90, size=7),legend.position="none", axis.title.x=element_blank())
grid.arrange(box5,box6,box7, nrow=2)

# 4. RESULTS - MACHINE LEARNING ALGORITHM

# 4.1. FIRST MODEL
# Compute the dataset mean temperature
mu <- mean(train$temperature)
mu

# Test results on the first prediction
naive_rmse <- RMSE(validation$temperature, mu)
naive_rmse

# Check result and save RMSE in a data frame
rmse_results <- data_frame (method="Just the average", RMSE=naive_rmse)
rmse_results


# 4.2. NITROGEN EFFECT
# Compute the dataset mean temperature
mu<- mean(train$temperature)

# Plot Histogram of Nitrogen effect
train %>% group_by(N) %>% summarise(b_N=mean(temperature-mu)) %>% ggplot(aes(b_N)) +geom_histogram(bins=10, color="black", fill="steelblue")

# Test  and save results of prediction with Nitrogen effect (NAs change into 0)
N_avgs <- train %>% group_by(N) %>% summarise(b_N=mean(temperature-mu))
predicted_temperature <- mu + validation %>%left_join(N_avgs, by="N") %>% pull(b_N)
predicted_temperature[is.na(predicted_temperature)]<- 0
N_effect_rmse <- RMSE(validation$temperature,predicted_temperature)
N_effect_rmse
rmse_results <- bind_rows(rmse_results, data_frame(method="Nitrogen Effect Model", RMSE=N_effect_rmse))
rmse_results

# 4.3. PHOSPHORUS EFFECT

# Compute the dataset mean temperature
mu<- mean(train$temperature)

# Plot Histogram of Phosphorus effect
train %>% group_by(P) %>% summarise(b_P=mean(temperature-mu)) %>% ggplot(aes(b_P)) +geom_histogram(bins=10, color="black", fill="steelblue")
 
# Test  and save results of prediction with Phosphorus effect (NAs change into 0)
P_avgs <- train %>% left_join(N_avgs, by="N")%>% group_by(P) %>%summarise(b_P=mean(temperature-mu-b_N))
predicted_temperature <- validation %>%left_join(N_avgs, by="N") %>% left_join(P_avgs, by="P") %>%  mutate (pred=mu+b_N +b_P)%>% pull(pred)
predicted_temperature[is.na(predicted_temperature)]<- 0
P_effect_rmse <- RMSE(predicted_temperature,validation$temperature)
P_effect_rmse
rmse_results <- bind_rows(rmse_results, data_frame(method="Phosphorus Effect Model", RMSE=P_effect_rmse))
rmse_results

# 4.4. POTASSIUM EFFECT

# Compute the dataset mean temperature
mu<- mean(train$temperature)


# Plot Histogram of Potassium effect
train %>% group_by (K) %>% summarise(b_K=mean(temperature-mu))%>% ggplot(aes(b_K)) +geom_histogram(bins=30, color="black", fill="steelblue")

# Test  and save results of prediction with Potassium effect
K_avgs <- train %>% group_by(K) %>% left_join(N_avgs, by="N") %>% left_join(P_avgs, by="P")%>%  summarise(b_K=mean(temperature-mu-b_N-b_P))
predicted_temperature <- validation %>%left_join(K_avgs, by="K") %>% left_join(N_avgs, by="N")%>%left_join(P_avgs, by="P")  %>%  mutate (pred=mu+b_N +b_P+b_K)%>% pull(pred)
predicted_temperature[is.na(predicted_temperature)]<- 0
K_effect_rmse <- RMSE(predicted_temperature,validation$temperature)
K_effect_rmse
rmse_results <- bind_rows(rmse_results, data_frame(method="Potassium Effect Model", RMSE=K_effect_rmse))
rmse_results


# 4.5. CHOOSING THE PENALTY TERMS

# Compute the dataset mean temperature
mu<- mean(train$temperature)

#Set sequence of  0 - 10 , distance=0.25. Calculate the lowest RMSE. 
lambdas <-seq(0,10,0.1)
mu<- mean(train$temperature)

rmse2 <- sapply(lambdas,function(l){
  mu<-mean(train$temperature)
  b_N_reg<-train %>% group_by(N)%>% summarise(b_N_reg=sum(temperature-mu)/(n()+l))
  b_P_reg<-train %>% group_by(P) %>% left_join(b_N_reg, by="N") %>% summarise(b_P_reg=sum(temperature-mu-b_N_reg)/(n()+l))
  b_K_reg<-train %>% group_by(K) %>% left_join(b_N_reg, by="N") %>% left_join(b_P_reg, by="P") %>% summarise(b_K_reg=sum(temperature-mu-b_N_reg-b_P_reg)/(n()+l))
  
  predicted_temperature <- validation %>%  left_join(b_N_reg, by="N")%>%  left_join(b_P_reg,  by="P")%>% left_join(b_K_reg, by="K")%>%   mutate(pred=mu+b_N_reg+b_P_reg+b_K_reg) %>% pull(pred)
  predicted_temperature[is.na(predicted_temperature)]<- 0
  return(RMSE(predicted_temperature, validation$temperature))})


# Plot Relationship between lambda and RMSE
qplot(lambdas,rmse2)

lambda_low<- lambdas[which.min(rmse2)]
lambda_low


# Compute the dataset mean temperature
mu<- mean(train$temperature)

# Test,calculate and save the RMSE with optimal lambda
mu<- mean(train$temperature)
N_reg<-train %>% group_by(N)%>% summarise(N_reg=sum(temperature-mu)/(n()+lambda_low))
P_reg<-train %>% group_by(P) %>% left_join(N_reg, by="N") %>% summarise(P_reg=sum(temperature-mu-N_reg)/(n()+lambda_low))
K_reg<-train %>% group_by(K) %>% left_join(N_reg, by="N") %>% left_join(P_reg, by="P") %>% summarise(K_reg=sum(temperature-mu-N_reg-P_reg)/(n()+lambda_low))
predicted_temperature <- validation %>%  left_join(N_reg, by="N")%>%  left_join(P_reg,  by="P")%>% left_join(K_reg, by="K")%>%   mutate(pred=mu+N_reg+P_reg+K_reg) %>% pull(pred)
predicted_temperature[is.na(predicted_temperature)]<- 0
All_reg_effect_rmse <- RMSE(predicted_temperature,validation$temperature)
All_reg_effect_rmse
rmse_results <- bind_rows(rmse_results, data_frame(method="Regularized Model with optimal lambda", RMSE=All_reg_effect_rmse))
rmse_results


#### Appendix ###
print ("Operation System:")
version

