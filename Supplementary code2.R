##load the packages used in the project####
library(caret)
library(randomForest)
library(pipeR)
library(humidity)
library(weathermetrics)
library(mgcv)
library(rpart)
library(rpart.plot)
library(data.table)
library(tidyverse)
library(vctrs)
library(RColorBrewer)
library(geosphere)
library(terra)
library(ggplot2)
library(ggmap)
library(maps)
library(dplyr)
library(mapdata)
##load the data####
mosquito.end <- read.csv(file.choose())
mosquito <- mosquito.end[,c(3,4,5,6,7,8,10,11,12,13)]

##Temperature, rainfall, water vapor pressure conversion####
##convert the temperature from fahrenheit to celsius
mosquito$temperature <- fahrenheit.to.celsius(mosquito$temperature)

#mosquito$dewpoint <- humidity.to.dewpoint(mosquito$temperature,mosquito$humidity,temperature.metric='celsius')
##convert the unit of rainfall from inches to mm
mosquito$mm.rainfall <- mosquito$rain*25.4

##calculate the water vapor pressure through the temperature and humidity####
length(mosquito$temperature)
for (q in 1:215) {
  mosquito$SVP[q] <- SVP(mosquito$temperature[q],isK=F)
  mosquito$WVP[q] <- WVP2(mosquito$humidity[q],mosquito$SVP[q])/1000
}
write.csv(mosquito, "mosquito.csv")

##calculate the day length based on the packages####
Tuskegee <- 30.68149
Montgomery <- 32.39492
Tuscaloosa <- 33.21522
Birmingham <- 33.37755

Tuskegee.daylength <- tapply(daylength(Tuskegee, 1:365),rep(1:12, c(31,28,31,30,31,30,31,31,30,31,30,31)), mean)

Montgomery.daylength <- tapply(daylength(Montgomery, 1:365),rep(1:12, c(31,28,31,30,31,30,31,31,30,31,30,31)), mean)

Tuscaloosa.daylength <- tapply(daylength(Tuscaloosa, 1:365),rep(1:12, c(31,28,31,30,31,30,31,31,30,31,30,31)), mean)

Birmingham.daylength <- tapply(daylength(Birmingham, 1:365),rep(1:12, c(31,28,31,30,31,30,31,31,30,31,30,31)), mean)

daylength <- data.frame(Tuskegee.daylength, Montgomery.daylength, Tuscaloosa.daylength, Birmingham.daylength)
write.csv(daylength, "daylength.csv")
##Write the function used in this project####
MAE <- function(predict, observe){
  mean(abs(predict - observe))
}

MSE <- function(predict, observe){
  mean((predict-observe)^2)
}

NMSE <- function(predict, observe){
  mean((predict-observe)^2)/mean((mean(observe) - observe)^2)
}


#Rsquare <- function(predict,observe){
#  1-var(observe-predict)/var(observe)
#}

Rsquare2 <- function(predict,observe){
  1-(sum((observe-predict)^2)/sum((observe-mean(observe))^2))
}



##run the random forest model for whole mosquito population 100 times####
##set up a blank list for recording the random forest 
train.all <- list()
##set up a blank list for importance of each environmental factors 
importance.all <- list()
##set up a blank vector for Rsquare, mae, mse, nmse 
Rsquare.all <- vector()
R.2.package.all <- vector()
mae.all<- vector()
mae.package.all <- vector()
mse.all<- vector()
rmse.package.all <- vector()
nmse.all<- vector()
##set up a blank list for importance value
IncMSE.all <- list()
IncNodePurity.all <- list()
##set up a blank list for top environmental factors
top <- list()
predictset <- list()
##repeat 100 times
cycle <- 1:100
for (i in cycle){
  ##set up the train data and test data
  select_train <- sample(215, 215*0.7)
  train_data <- mosquito[select_train, ]
  test_data <- mosquito[-select_train, ]
  ##random forest building and record it to train list 
  train.all[[i]] <- randomForest(mosquito~wind+mm.rainfall+daylength+temperature+WVP, data= train_data, importance=T)
  ##record the importance of each environmental factors to importance list
  importance.all[[i]] <- train.all[[i]]$importanceSD
  ##IncMSE, record this value to the blank list
  IncMSE.all[[i]] <- importance(train.all[[i]])[,1]
  IncNodePurity.all[[i]] <- importance(train.all[[i]])[,2]
  ##find the importance value > 0 
  e <- data.frame(IncMSE.all[[i]],IncNodePurity.all[[i]])
  top[[i]] <- row.names(e[e$IncMSE.all>0,])
  ##predict the new set based on THIS (i) random forest model
  predictset[[i]] <- predict(train.all[[i]], test_data)
  ##calculate the R square
  Rsquare.all[i] <- Rsquare2(predictset[[i]], test_data$mosquito)
  ##mae, mse, nmse 
  mae.all[i] <- MAE(predictset[[i]],test_data$mosquito)
  mse.all[i] <- MSE(predictset[[i]],test_data$mosquito)
  nmse.all[i]<- NMSE(predictset[[i]],test_data$mosquito)
}

predictset.true <- cbind(unlist(predictset))
write.csv(predictset.true, 'predictset true.csv')
predictset.true2 <- read.csv('predictset true.csv')

##read the text again
library(readr)
trainforest1 <- read_csv("trainforest1.txt", 
                         col_names = FALSE)
View(trainforest1)

##find the content of teh cycle number and var explained
var.explained <- seq(from=0,to=800, by=8)
cycle.number<-var.explained +1

var.expained.content <- trainforest1[var.explained,]

cycle.number.content <- trainforest1[cycle.number[1:100],]

##find the value of IncMSE.content 
IncMSE.all.content <- data.frame(t(sapply(IncMSE.all,c)))
colnames(IncMSE.all.content) <- c('IncMSE.wind', 'IncMSE.rain', 'IncMSE.pressure', 'IncMSE.daylength', 'IncMSE.humidity', 'IncMSE.temperature')
length(IncMSE.all.content)
##find the value of IncNodePurity.all 
IncNodePurity.all.content <- data.frame(t(sapply(IncNodePurity.all,c)))
colnames(IncNodePurity.all.content) <- c('IncNodePurity.wind', 
                                         'IncNodePurity.rain', 
                                         'IncNodePurity.pressure', 
                                         'IncNodePurity.daylength', 
                                         'IncNodePurity.humidity', 
                                         'IncNodePurity.temperature')

length(IncNodePurity.all.content)

###print the whole data out
whole.data <- data.frame(cycle.number.content,
                         var.expained.content,
                         
                         IncMSE.all.content,
                         IncNodePurity.all.content, 
                         Rsquare.all, mae.all,mse.all,nmse.all
)

length(whole.data[1,])
b <- vector()
for (a in c(1:16)) {
  b[a] <- mean(whole.data[,a])
  
}
whole.data[101,] <- b


c <- vector()
for (a in c(4:16)){
  c[a] <- sd(whole.data[,a])
}
whole.data[102,] <- c

row.names(whole.data) <- c(1:100, 'mean.value','sd.value')

options(max.print=10000000) 
sink('whole data.txt')
whole.data
sink()

##predict the mosquito population in the southeast america based on the random forest model####
daylength_data <- read.csv(choose.files())
save.image()
i=1

for (i in 1:12) {
  data <- fread(input=paste0("C:/Users/yzw0093/OneDrive - Auburn University/paper/A ecology/Mosquito population with nan/data collection/weather data/OneDrive_1_10-30-2022/climate_dataframe/climate_df_",i,".txt"), sep = " ")
  colnames(data) <- c('x','y','mm.rainfall','temperature','WVP','wind')
  data$daylength <- daylength_data[,i+1]
  rf.prdict <- matrix(nrow = 1731207, ncol = 100)
  for (a in 1:100) {
    rf.prdict[,a] <- predict(train.all[[a]], data)
  }
  data$mosquito <- rowMeans(rf.prdict)
  write.csv(data, paste0(i,".csv"))
  rm(data)
}

save.image()
##Create the mosquito population prediction heat map based on the data####
for (i in 1:12) {
  
  cluster_df <- read_csv(paste0("C:/Users/yzw0093/OneDrive - Auburn University/paper/A ecology/R analysis/",i,".csv"))%>% select(x,y,mosquito)
  
  names(cluster_df)[3] <- "mean"
  
  cluster_df.pretty_breaks <- seq(0,max(cluster_df$mean), length=11)
  
  cluster_df.brks <- cluster_df.pretty_breaks
  
  cluster_df.labels <- c(round(cluster_df.brks,2)[-c(1,length(cluster_df.brks))]," ")
  
  cluster_df$brks <- cut(cluster_df$mean, 
                         breaks = cluster_df.brks, 
                         include.lowest = TRUE, 
                         labels = cluster_df.labels)
  
  brks_scale <- levels(cluster_df$brks)
  labels_scale <- (brks_scale)
  
  display.brewer.pal(10, "BrBG")
  coul <- brewer.pal(10, "BrBG")
  
  barplot(rep(1, length(coul)), col = coul , main="BrBG") 
  names(coul) <- labels_scale
  pt=11
  
  
  map <- ggplot()+
    geom_raster(data = cluster_df, aes(x = x, y = y,fill=brks))+
    scale_fill_manual(values = (coul),
                      breaks = (brks_scale),
                      labels = (labels_scale),
                      guide = guide_legend(
                        direction = "vertical",
                        keyheight = unit(5, units = "mm"),
                        keywidth = unit(4, units = "mm"),
                        title = "population",
                        title.position = "bottom",
                        # exactly at the right end of each legend key
                        # title.hjust = 0.5,
                        label.hjust = 0,
                        label.vjust = 1.1,
                        # label.theme = element_text(angle = 0),
                        ncol = 1,
                        reverse = T,# also the guide needs to be reversed
                        label.position = "right"
                      )
    )+
    theme_bw(base_size = pt)+ 
    theme(
      panel.background = element_rect(fill = '#ffffff', colour = 'black'),#D5DBDB
      panel.grid.minor=element_blank(),
      panel.grid.major=element_blank(),
      panel.border = element_rect(fill=NA, colour = "black", size=1.),#size
      #legend.title=element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),#x,y:longitude,latitude
      axis.text=element_text(color="black",size=9),
      legend.position = "right",
      #legend.spacing.y = unit(-0.1, 'cm'),legend.spacing.x = unit(-0.1, 'cm') 
      plot.title = element_text(hjust = 0.5),
      # plot.title = element_text(color = "red", size = 12, face = "bold"),
    )+ 
    labs(title = "month")
  
  ggsave(paste0("map",i,".tiff"),
         map,
         width = 13 , height = 10, units = "cm",
         compression="lzw",
         dpi = 900)
}
##run the random forest model for different aedes mosquito species 100 times####
albopictus <- read.csv(file.choose())
triseriatus <- read.csv(file.choose())
japonicus <- read.csv(file.choose())


albopictus.2 <- cbind(albopictus$population,mosquito[,c(2,3,5,6,8,10,11,13,15)])
colnames(albopictus.2) <- c('mosquito','city','date','DATE','wind','daylength','temperature','month','mm.rainfall','WVP')

triseriatus.2 <- cbind(triseriatus$population,mosquito[,c(2,3,5,6,8,10,11,13,15)])
colnames(triseriatus.2) <- c('mosquito','city','date','DATE','wind','daylength','temperature','month','mm.rainfall','WVP')

japonicus.2 <- cbind(japonicus$population,mosquito[,c(2,3,5,6,8,10,11,13,15)])
colnames(japonicus.2) <- c('mosquito','city','date','DATE','wind','daylength','temperature','month','mm.rainfall','WVP')

##set up a blank list for recording the random forest 
train.all.albopictus <- list()
train.all.triseriatus <- list()
train.all.japonicus <- list()
##set up a blank list for importance of each environmental factors 
importance.all.albopictus <- list()
importance.all.triseriatus <- list()
importance.all.japonicus <- list()
##set up a blank vector for Rsquare, mae, mse, nmse 
Rsquare.all.albopictus <- vector()
Rsquare.all.triseriatus <- vector()
Rsquare.all.japonicus <- vector()

mae.all.albopictus<- vector()
mae.all.triseriatus<- vector()
mae.all.japonicus<- vector()

mse.all.albopictus<- vector()
mse.all.triseriatus<- vector()
mse.all.japonicus<- vector()

nmse.all.albopictus<- vector()
nmse.all.triseriatus<- vector()
nmse.all.japonicus<- vector()
##set up a blank list for importance value
IncMSE.all.albopictus <- list()
IncNodePurity.all.albopictus <- list()

IncMSE.all.triseriatus <- list()
IncNodePurity.all.triseriatus <- list()

IncMSE.all.japonicus <- list()
IncNodePurity.all.japonicus <- list()
##set up a blank list for top environmental factors

predictset.albopictus <- list()
predictset.triseriatus <- list()
predictset.japonicus <- list()


##repeat 100 times##
cycle <- 1:100
for (i in cycle){
  ##set up the train data and test data
  select_train <- sample(215, 215*0.7)
  
  train_data.albopictus <- albopictus.2[select_train, ]
  test_data.albopictus <- albopictus.2[-select_train, ]
  ##random forest building and record it to train list 
  train.all.albopictus[[i]] <- randomForest(mosquito~wind+mm.rainfall+daylength+temperature+WVP, data= train_data.albopictus, importance=T)
  ##record the importance of each environmental factors to importance list
  importance.all.albopictus[[i]] <- train.all.albopictus[[i]]$importanceSD
  ##%IncMSE, record this value to the blank list
  IncMSE.all.albopictus[[i]] <- importance(train.all.albopictus[[i]])[,1]
  IncNodePurity.all.albopictus[[i]] <- importance(train.all.albopictus[[i]])[,2]
  ##predict the new set based on THIS (i) random forest model
  predictset.albopictus[[i]] <- predict(train.all.albopictus[[i]], test_data.albopictus)
  ##calculate the R square
  Rsquare.all.albopictus[i] <- Rsquare2(predictset.albopictus[[i]], test_data.albopictus$mosquito)
  ##mae, mse, nmse 
  mae.all.albopictus[i] <- MAE(predictset.albopictus[[i]],test_data.albopictus$mosquito)
  mse.all.albopictus[i] <- MSE(predictset.albopictus[[i]],test_data.albopictus$mosquito)
  nmse.all.albopictus[i]<- NMSE(predictset.albopictus[[i]],test_data.albopictus$mosquito)
  
  train_data.triseriatus <- triseriatus.2[select_train, ]
  test_data.triseriatus <- triseriatus.2[-select_train, ]
  ##random forest building and record it to train list 
  train.all.triseriatus[[i]] <- randomForest(mosquito~wind+mm.rainfall+daylength+temperature+WVP, data= train_data.triseriatus, importance=T)
  ##record the importance of each environmental factors to importance list
  importance.all.triseriatus[[i]] <- train.all.triseriatus[[i]]$importanceSD
  ##%IncMSE, record this value to the blank list
  IncMSE.all.triseriatus[[i]] <- importance(train.all.triseriatus[[i]])[,1]
  IncNodePurity.all.triseriatus[[i]] <- importance(train.all.triseriatus[[i]])[,2]
  ##predict the new set based on THIS (i) random forest model
  predictset.triseriatus[[i]] <- predict(train.all.triseriatus[[i]], test_data.triseriatus)
  ##calculate the R square
  Rsquare.all.triseriatus[i] <- Rsquare2(predictset.triseriatus[[i]], test_data.triseriatus$mosquito)
  ##mae, mse, nmse 
  mae.all.triseriatus[i] <- MAE(predictset.triseriatus[[i]],test_data.triseriatus$mosquito)
  mse.all.triseriatus[i] <- MSE(predictset.triseriatus[[i]],test_data.triseriatus$mosquito)
  nmse.all.triseriatus[i]<- NMSE(predictset.triseriatus[[i]],test_data.triseriatus$mosquito)
  
  train_data.japonicus <- japonicus.2[select_train, ]
  test_data.japonicus <- japonicus.2[-select_train, ]
  ##random forest building and record it to train list 
  train.all.japonicus[[i]] <- randomForest(mosquito~wind+mm.rainfall+daylength+temperature+WVP, data= train_data.japonicus, importance=T)
  ##record the importance of each environmental factors to importance list
  importance.all.japonicus[[i]] <- train.all.japonicus[[i]]$importanceSD
  ##%IncMSE, record this value to the blank list
  IncMSE.all.japonicus[[i]] <- importance(train.all.japonicus[[i]])[,1]
  IncNodePurity.all.japonicus[[i]] <- importance(train.all.japonicus[[i]])[,2]
  ##predict the new set based on THIS (i) random forest model
  predictset.japonicus[[i]] <- predict(train.all.japonicus[[i]], test_data.japonicus)
  ##calculate the R square
  Rsquare.all.japonicus[i] <- Rsquare2(predictset.japonicus[[i]], test_data.japonicus$mosquito)
  ##mae, mse, nmse 
  mae.all.japonicus[i] <- MAE(predictset.japonicus[[i]],test_data.japonicus$mosquito)
  mse.all.japonicus[i] <- MSE(predictset.japonicus[[i]],test_data.japonicus$mosquito)
  nmse.all.japonicus[i]<- NMSE(predictset.japonicus[[i]],test_data.japonicus$mosquito)
}
##redirect the randomforest content to the text 
sink("trainforest_albopictus.txt")
train.all.albopictus
sink()
sink("trainforest_triseriatus.txt")
train.all.triseriatus
sink()
sink("trainforest_japonicus.txt")
train.all.japonicus
sink()

########albopictus##
##read the text again
library(readr)
train.all.albopictus <- read_csv("trainforest_albopictus.txt", 
                                 col_names = FALSE)


##find the content of the cycle number and var explained
var.explained <- seq(from=0,to=800, by=8)
cycle.number<-var.explained +1

var.expained.content.albopictus <- train.all.albopictus[var.explained,]

cycle.number.content.albopictus <- train.all.albopictus[cycle.number[1:100],]

##find the value of IncMSE.content 
IncMSE.all.content.albopictus <- data.frame(t(sapply(IncMSE.all.albopictus,c)))
colnames(IncMSE.all.content.albopictus) <- c('IncMSE.wind', 'IncMSE.rain', 'IncMSE.daylength', 'IncMSE.temperature', 'IncMSE.WVP')
length(IncMSE.all.content.albopictus)
##find the value of IncNodePurity.all 
IncNodePurity.all.content.albopictus <- data.frame(t(sapply(IncNodePurity.all.albopictus,c)))
colnames(IncNodePurity.all.content.albopictus) <- c('IncNodePurity.wind', 
                                                    'IncNodePurity.rain', 
                                                    
                                                    'IncNodePurity.daylength', 
                                                    
                                                    'IncNodePurity.temperature',
                                                    'IncNodePurity.WVP')

length(IncNodePurity.all.content.albopictus)

###print the whole data out
whole.data.albopictus <- data.frame(cycle.number.content.albopictus,
                                    var.expained.content.albopictus,
                                    
                                    IncMSE.all.content.albopictus,
                                    IncNodePurity.all.content.albopictus, 
                                    Rsquare.all.albopictus, mae.all.albopictus,mse.all.albopictus,nmse.all.albopictus
)

length(whole.data.albopictus[1,])
b <- vector()
for (a in c(1:16)) {
  b[a] <- mean(whole.data.albopictus[,a])
  
}
whole.data.albopictus[101,] <- b


c <- vector()
for (a in c(1:16)){
  c[a] <- sd(whole.data.albopictus[,a])
}
whole.data.albopictus[102,] <- c

row.names(whole.data.albopictus) <- c(1:100, 'mean.value','sd.value')

options(max.print=10000000) 
sink('whole data albopictus.txt')
whole.data.albopictus
sink()
########triseriatus##
##read the text again
library(readr)
train.all.triseriatus <- read_csv("trainforest_triseriatus.txt", 
                                  col_names = FALSE)


##find the content of teh cycle number and var explained
var.explained <- seq(from=0,to=800, by=8)
cycle.number<-var.explained +1

var.expained.content.triseriatus <- train.all.triseriatus[var.explained,]

cycle.number.content.triseriatus <- train.all.triseriatus[cycle.number[1:100],]

##find the value of IncMSE.content 
IncMSE.all.content.triseriatus <- data.frame(t(sapply(IncMSE.all.triseriatus,c)))
colnames(IncMSE.all.content.triseriatus) <- c('IncMSE.wind', 'IncMSE.rain', 'IncMSE.daylength', 'IncMSE.temperature', 'IncMSE.WVP')
length(IncMSE.all.content.triseriatus)
##find the value of IncNodePurity.all 
IncNodePurity.all.content.triseriatus <- data.frame(t(sapply(IncNodePurity.all.triseriatus,c)))
colnames(IncNodePurity.all.content.triseriatus) <- c('IncNodePurity.wind', 
                                                     'IncNodePurity.rain', 
                                                     
                                                     'IncNodePurity.daylength', 
                                                     
                                                     'IncNodePurity.temperature',
                                                     'IncNodePurity.WVP')

length(IncNodePurity.all.content.triseriatus)

###print the whole data out
whole.data.triseriatus <- data.frame(cycle.number.content.triseriatus,
                                     var.expained.content.triseriatus,
                                     
                                     IncMSE.all.content.triseriatus,
                                     IncNodePurity.all.content.triseriatus, 
                                     Rsquare.all.triseriatus, mae.all.triseriatus,mse.all.triseriatus,nmse.all.triseriatus
)

length(whole.data.triseriatus[1,])
b <- vector()
for (a in c(1:16)) {
  b[a] <- mean(whole.data.triseriatus[,a])
  
}
whole.data.triseriatus[101,] <- b


c <- vector()
for (a in c(1:16)){
  c[a] <- sd(whole.data.triseriatus[,a])
}
whole.data.triseriatus[102,] <- c

row.names(whole.data.triseriatus) <- c(1:100, 'mean.value','sd.value')

options(max.print=10000000) 
sink('whole data triseriatus.txt')
whole.data.triseriatus
sink()
########japonicus##
##read the text again
library(readr)
train.all.japonicus <- read_csv("trainforest_japonicus.txt", 
                                col_names = FALSE)


##find the content of teh cycle number and var explained
var.explained <- seq(from=0,to=800, by=8)
cycle.number<-var.explained +1

var.expained.content.japonicus <- train.all.japonicus[var.explained,]

cycle.number.content.japonicus <- train.all.japonicus[cycle.number[1:100],]

##find the value of IncMSE.content 
IncMSE.all.content.japonicus <- data.frame(t(sapply(IncMSE.all.japonicus,c)))
colnames(IncMSE.all.content.japonicus) <- c('IncMSE.wind', 'IncMSE.rain', 'IncMSE.daylength', 'IncMSE.temperature', 'IncMSE.WVP')
length(IncMSE.all.content.japonicus)
##find the value of IncNodePurity.all 
IncNodePurity.all.content.japonicus <- data.frame(t(sapply(IncNodePurity.all.japonicus,c)))
colnames(IncNodePurity.all.content.japonicus) <- c('IncNodePurity.wind', 
                                                   'IncNodePurity.rain', 
                                                   
                                                   'IncNodePurity.daylength', 
                                                   
                                                   'IncNodePurity.temperature',
                                                   'IncNodePurity.WVP')

length(IncNodePurity.all.content.japonicus)

###print the whole data out
whole.data.japonicus <- data.frame(cycle.number.content.japonicus,
                                   var.expained.content.japonicus,
                                   
                                   IncMSE.all.content.japonicus,
                                   IncNodePurity.all.content.japonicus, 
                                   Rsquare.all.japonicus, mae.all.japonicus,mse.all.japonicus,nmse.all.japonicus
)

length(whole.data.japonicus[1,])
b <- vector()
for (a in c(1:16)) {
  b[a] <- mean(whole.data.japonicus[,a])
  
}
whole.data.japonicus[101,] <- b


c <- vector()
for (a in c(1:16)){
  c[a] <- sd(whole.data.japonicus[,a])
}
whole.data.japonicus[102,] <- c

row.names(whole.data.japonicus) <- c(1:100, 'mean.value','sd.value')

options(max.print=10000000) 
sink('whole data japonicus.txt')
whole.data.japonicus
sink()
save.image()

##Predict the each aedes mosquito population in the southeast america####
daylength_data <- read.csv(file.choose())

for (i in 1:12) {
  data <- fread(input=paste0("C:/Users/yzw0093/OneDrive - Auburn University/paper/A ecology/Mosquito population with nan/data collection/weather data/OneDrive_1_10-30-2022/climate_dataframe/climate_df_",i,".txt"), sep = " ")
  colnames(data) <- c('x','y','mm.rainfall','temperature','WVP','wind')
  data$daylength <- daylength_data[,i+1]
  
  rf.prdict.albopictus <- matrix(nrow = 1731207, ncol = 100)
  rf.prdict.triseriatus <- matrix(nrow = 1731207, ncol = 100)
  rf.prdict.japonicus <- matrix(nrow = 1731207, ncol = 100)
  
  for (a in 1:100) {
    rf.prdict.albopictus[,a] <- predict(train.all.albopictus[[a]], data)
    rf.prdict.triseriatus[,a] <- predict(train.all.triseriatus[[a]], data)
    rf.prdict.japonicus[,a] <- predict(train.all.japonicus[[a]], data)
  }
  mosquito.albopictus <- rowMeans(rf.prdict.albopictus)
  data.albopictus <- cbind(data,mosquito.albopictus)
  write.csv(data.albopictus, paste0("albopictus",i,".csv"))
  rm(data.albopictus)
  
  mosquito.triseriatus <- rowMeans(rf.prdict.triseriatus)
  data.triseriatus <- cbind(data,mosquito.triseriatus)
  write.csv(data.triseriatus, paste0("triseriatus",i,".csv"))
  rm(data.triseriatus)
  
  mosquito.japonicus <- rowMeans(rf.prdict.japonicus)
  data.japonicus <- cbind(data,mosquito.japonicus)
  write.csv(data.japonicus, paste0("japonicus",i,".csv"))
  rm(data.japonicus)
}

save.image()







##Create the each aedes mosquito population prediction heat map based on the prediction####
for (i in 1:12) {
  
  albopictus.data <- read_csv(paste0("C:/Users/yzw0093/OneDrive - Auburn University/paper/A ecology/R Analysis/different species/albopictus",i,".csv"))%>% select(x,y,mosquito.albopictus)
  triseriatus.data <-read_csv(paste0("C:/Users/yzw0093/OneDrive - Auburn University/paper/A ecology/R Analysis/different species/triseriatus",i,".csv"))%>% select(x,y,mosquito.triseriatus)
  japonicus.data <-  read_csv(paste0("C:/Users/yzw0093/OneDrive - Auburn University/paper/A ecology/R Analysis/different species/japonicus",i,".csv"))%>% select(x,y,mosquito.japonicus)
  
  species.data <- cbind(albopictus.data,triseriatus.data,japonicus.data)
  
  
  names(species.data)[3] <- 'mean.al'
  names(species.data)[6] <- 'mean.tr'
  names(species.data)[9] <- 'mean.ja'
  
  species.data.pretty.breaks.al <- seq(0,250, length=10)
  species.data.pretty.breaks.tr <- seq(0,85, length=10)
  species.data.pretty.breaks.ja <- seq(0,170, length=10)
  
  
  cluster_df.brks.al <- species.data.pretty.breaks.al
  cluster_df.brks.tr <- species.data.pretty.breaks.tr
  cluster_df.brks.ja <- species.data.pretty.breaks.ja
  
  cluster_df.labels.al <- c(round(cluster_df.brks.al,2)[-c(1,length(cluster_df.brks.al))]," ")
  cluster_df.labels.tr <- c(round(cluster_df.brks.tr,2)[-c(1,length(cluster_df.brks.tr))]," ")
  cluster_df.labels.ja <- c(round(cluster_df.brks.ja,2)[-c(1,length(cluster_df.brks.ja))]," ")
  
  species.data$brks.al <- cut(species.data$mean.al, 
                              breaks = cluster_df.brks.al, 
                              include.lowest = TRUE, 
                              labels = cluster_df.labels.al)
  
  species.data$brks.tr <- cut(species.data$mean.tr, 
                              breaks = cluster_df.brks.tr, 
                              include.lowest = TRUE, 
                              labels = cluster_df.labels.tr)
  
  species.data$brks.ja <- cut(species.data$mean.ja, 
                              breaks = cluster_df.brks.ja, 
                              include.lowest = TRUE, 
                              labels = cluster_df.labels.ja)
  
  
  brks_scale.al <- levels(species.data$brks.al)
  labels_scale.al <- (brks_scale.al)
  
  brks_scale.tr <- levels(species.data$brks.tr)
  labels_scale.tr <- (brks_scale.tr)
  
  brks_scale.ja <- levels(species.data$brks.ja)
  labels_scale.ja <- (brks_scale.ja)
  
  
  display.brewer.pal(9, "Reds")
  coul.al <- brewer.pal(9, "Reds")
  
  display.brewer.pal(9, "Greens")
  coul.tr <- brewer.pal(9, "Purples")
  
  display.brewer.pal(9, "Blues")
  coul.ja <- brewer.pal(9, "Blues")
  
  barplot(rep(1, length(coul.al)), col = coul.al , main="Reds") 
  names(coul.al) <- labels_scale.al
  
  barplot(rep(1, length(coul.tr)), col = coul.tr , main="Purples") 
  names(coul.tr) <- labels_scale.tr
  
  barplot(rep(1, length(coul.ja)), col = coul.ja , main="Greens") 
  names(coul.ja) <- labels_scale.ja
  
  pt=11
  
  species.data <- species.data[,c(1,2,10,11,12)]
  
  
  map.al <- ggplot()+
    geom_raster(data = species.data, aes(x = x, y = y,fill=brks.al))+
    scale_fill_manual(values = (coul.al),
                      breaks = (brks_scale.al),
                      labels = (labels_scale.al),
                      guide = guide_legend(
                        direction = "vertical",
                        keyheight = unit(5, units = "mm"),
                        keywidth = unit(4, units = "mm"),
                        title = "population",
                        title.position = "bottom",
                        # exactly at the right end of each legend key
                        # title.hjust = 0.5,
                        label.hjust = 0,
                        label.vjust = 1.1,
                        # label.theme = element_text(angle = 0),
                        ncol = 1,
                        reverse = T,# also the guide needs to be reversed
                        label.position = "right"
                      )
    )+
    theme_bw(base_size = pt)+ 
    theme(
      panel.background = element_rect(fill = '#ffffff', colour = 'black'),#D5DBDB
      panel.grid.minor=element_blank(),
      panel.grid.major=element_blank(),
      panel.border = element_rect(fill=NA, colour = "black", size=1.),#size
      #legend.title=element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),#x,y:longitude,latitude
      axis.text=element_text(color="black",size=9),
      legend.position = "right",
      #legend.spacing.y = unit(-0.1, 'cm'),legend.spacing.x = unit(-0.1, 'cm') 
      plot.title = element_text(hjust = 0.5),
      # plot.title = element_text(color = "red", size = 12, face = "bold"),
    )+ 
    labs(title = "month")
  
  map.tr <- ggplot()+
    geom_raster(data = species.data, aes(x = x, y = y,fill=brks.tr))+
    scale_fill_manual(values = (coul.tr),
                      breaks = (brks_scale.tr),
                      labels = (labels_scale.tr),
                      guide = guide_legend(
                        direction = "vertical",
                        keyheight = unit(5, units = "mm"),
                        keywidth = unit(4, units = "mm"),
                        title = "population",
                        title.position = "bottom",
                        # exactly at the right end of each legend key
                        # title.hjust = 0.5,
                        label.hjust = 0,
                        label.vjust = 1.1,
                        # label.theme = element_text(angle = 0),
                        ncol = 1,
                        reverse = T,# also the guide needs to be reversed
                        label.position = "right"
                      )
    )+
    theme_bw(base_size = pt)+ 
    theme(
      panel.background = element_rect(fill = '#ffffff', colour = 'black'),#D5DBDB
      panel.grid.minor=element_blank(),
      panel.grid.major=element_blank(),
      panel.border = element_rect(fill=NA, colour = "black", size=1.),#size
      #legend.title=element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),#x,y:longitude,latitude
      axis.text=element_text(color="black",size=9),
      legend.position = "right",
      #legend.spacing.y = unit(-0.1, 'cm'),legend.spacing.x = unit(-0.1, 'cm') 
      plot.title = element_text(hjust = 0.5),
      # plot.title = element_text(color = "red", size = 12, face = "bold"),
    )+ 
    labs(title = "month")
  
  
  map.ja <- ggplot()+
    geom_raster(data = species.data, aes(x = x, y = y,fill=brks.ja))+
    scale_fill_manual(values = (coul.ja),
                      breaks = (brks_scale.ja),
                      labels = (labels_scale.ja),
                      guide = guide_legend(
                        direction = "vertical",
                        keyheight = unit(5, units = "mm"),
                        keywidth = unit(4, units = "mm"),
                        title = "population",
                        title.position = "bottom",
                        # exactly at the right end of each legend key
                        # title.hjust = 0.5,
                        label.hjust = 0,
                        label.vjust = 1.1,
                        # label.theme = element_text(angle = 0),
                        ncol = 1,
                        reverse = T,# also the guide needs to be reversed
                        label.position = "right"
                      )
    )+
    theme_bw(base_size = pt)+ 
    theme(
      panel.background = element_rect(fill = '#ffffff', colour = 'black'),#D5DBDB
      panel.grid.minor=element_blank(),
      panel.grid.major=element_blank(),
      panel.border = element_rect(fill=NA, colour = "black", size=1.),#size
      #legend.title=element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),#x,y:longitude,latitude
      axis.text=element_text(color="black",size=9),
      legend.position = "right",
      #legend.spacing.y = unit(-0.1, 'cm'),legend.spacing.x = unit(-0.1, 'cm') 
      plot.title = element_text(hjust = 0.5),
      # plot.title = element_text(color = "red", size = 12, face = "bold"),
    )+ 
    labs(title = "month")
  
  
  
  
  
  
  ggsave(paste0("map albopictus",i,".tiff"),
         map.al,
         width = 13 , height = 10, units = "cm",
         compression="lzw",
         dpi = 900)
  ggsave(paste0("map triseriatus",i,".tiff"),
         map.tr,
         width = 13 , height = 10, units = "cm",
         compression="lzw",
         dpi = 900)
  ggsave(paste0("map japonicus",i,".tiff"),
         map.ja,
         width = 13 , height = 10, units = "cm",
         compression="lzw",
         dpi = 900)
  
}
save.image()


##Create the map based on average mosquito collection monthly####
states<- map_data('state')
al_df <- subset(states, region == "alabama")
counties <- map_data("county")
al_county <- subset(counties, region == "alabama")
al_base <- ggplot(data = al_county, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "ivory") 


al_data <- inner_join(al_county,A_complete_mosquito_population_,by="subregion")


ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)
elbow_room1 <- al_base + 
  geom_polygon(data = al_data,aes(x = long, y = lat,fill = population),color = "white")+
  geom_polygon(color = "black", fill = NA) +
  theme_bw() +
  ditch_the_axes

elbow_room1+scale_fill_gradient(low = 'khaki',high='tomato',name="mosquito population")
##Create the heat map for other map#####
##Get the data other factor data of southeast america###
us_boundary <- vect("E:/OneDrive - Auburn University/paper/A ecology/R Analysis/whole mosquito map/Other_map/treecover_population_elevation/southeast_shp/southeast.shp")
us_extent <- ext(us_boundary)
us_extent <- ext(-96, -73, 24, 40)
crs(us_boundary, describe=TRUE, proj=TRUE)
crs_target = "+proj=longlat +datum=WGS84 +no_defs"
crs(us_boundary) <- crs_target #us boundary

inputfile <- ("E:/OneDrive - Auburn University/paper/A ecology/R Analysis/whole mosquito map/other_map/treecover_population_elevation/treecover_population_elevation/elevation_southeast.tif")
raster <- rast(inputfile)
elevation_southeast <- crop(
  mask(raster, us_boundary),
  us_extent) 

inputfile <- ("E:/OneDrive - Auburn University/paper/A ecology/R Analysis/whole mosquito map/other_map/treecover_population_elevation/treecover_population_elevation/population_southeast.tif")
raster <- rast(inputfile)
population_southeast <- crop(
  mask(raster, us_boundary),
  us_extent) 

inputfile <- ("E:/OneDrive - Auburn University/paper/A ecology/R Analysis/whole mosquito map/other_map/treecover_population_elevation/treecover_population_elevation/treecover_southeast.tif")
raster <- rast(inputfile)
treecover_southeast <- crop(
  mask(raster, us_boundary),
  us_extent)

inputfile <- ("E:/OneDrive - Auburn University/paper/A ecology/R Analysis/whole mosquito map/other_map/treecover_population_elevation/treecover_population_elevation/watercontentt_southeast.tif")
raster <- rast(inputfile)
watercontent_southeast <- crop(
  mask(raster, us_boundary),
  us_extent)

##Create the heatmap for elevation###
rasterset.elevation <- as.data.frame(elevation_southeast, xy = T) %>% na.omit()
elevation <- setDT(rasterset.elevation)
sum(is.na(elevation))
setnames(elevation, c("x","y","elevation"))
fwrite(elevation,"elevation.csv")
names(elevation)[3] <- "mean"
elevation.pretty_breaks <- seq(0,max(elevation$mean), length=10)
elevation.brks <- elevation.pretty_breaks
elevation.labels <- c(round(elevation.brks,2)[-c(1,length(elevation.brks))]," ")
elevation$brks <- cut(elevation$mean, 
                      breaks = elevation.brks, 
                      include.lowest = TRUE, 
                      labels = elevation.labels)

brks_scale <- levels(elevation$brks)
labels_scale <- (brks_scale)
display.brewer.pal(9, "YlGnBu")
display.brewer.all()
coul <- brewer.pal(9, "YlGnBu")
barplot(rep(1, length(coul)), col = coul , main="YlGnBu") 
names(coul) <- labels_scale
pt=11


map.elevation <- ggplot()+
  geom_raster(data = elevation, aes(x = x, y = y,fill=brks))+
  scale_fill_manual(values = (coul),
                    breaks = (brks_scale),
                    labels = (labels_scale),
                    guide = guide_legend(
                      direction = "vertical",
                      keyheight = unit(5, units = "mm"),
                      keywidth = unit(4, units = "mm"),
                      title = "population",
                      title.position = "bottom",
                      # exactly at the right end of each legend key
                      # title.hjust = 0.5,
                      label.hjust = 0,
                      label.vjust = 1.1,
                      # label.theme = element_text(angle = 0),
                      ncol = 1,
                      reverse = T,# also the guide needs to be reversed
                      label.position = "right"
                    )
  )+
  theme_bw(base_size = pt)+ 
  theme(
    panel.background = element_rect(fill = '#ffffff', colour = 'black'),#D5DBDB
    panel.grid.minor=element_blank(),
    panel.grid.major=element_blank(),
    panel.border = element_rect(fill=NA, colour = "black", size=1.),#size
    #legend.title=element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),#x,y:longitude,latitude
    axis.text=element_text(color="black",size=9),
    legend.position = "right",
    #legend.spacing.y = unit(-0.1, 'cm'),legend.spacing.x = unit(-0.1, 'cm') 
    plot.title = element_text(hjust = 0.5),
    # plot.title = element_text(color = "red", size = 12, face = "bold"),
  )+ 
  labs(title = "elevation")

ggsave("map elevation2.tiff",
       map.elevation,
       width = 13 , height = 10, units = "cm",
       compression="lzw",
       dpi = 900)



##Create the heatmap for the human population###
rasterset.population <- as.data.frame(log(population_southeast), xy = T) %>% na.omit()
population <- setDT(rasterset.population)
sum(is.na(population))
setnames(population, c("x","y","population"))
fwrite(population,"population.csv")
names(population)[3] <- "mean"
population.pretty_breaks <- seq(0,max(population$mean), length=11)
population.brks <- population.pretty_breaks
population.labels <- c(round(population.brks,2)[-c(1,length(population.brks))]," ")
population$brks <- cut(population$mean, 
                       breaks = population.brks, 
                       include.lowest = TRUE, 
                       labels = population.labels)

brks_scale <- levels(population$brks)
labels_scale <- (brks_scale)
display.brewer.pal(10, "BrBG")
display.brewer.all()
coul <- brewer.pal(10, "BrBG")
barplot(rep(1, length(coul)), col = coul , main="BrBG") 
names(coul) <- labels_scale
pt=11
map.population <- ggplot()+
  geom_raster(data = population, aes(x = x, y = y,fill=brks))+
  scale_fill_manual(values = (coul),
                    breaks = (brks_scale),
                    labels = (labels_scale),
                    guide = guide_legend(
                      direction = "vertical",
                      keyheight = unit(5, units = "mm"),
                      keywidth = unit(4, units = "mm"),
                      title = "population",
                      title.position = "bottom",
                      # exactly at the right end of each legend key
                      # title.hjust = 0.5,
                      label.hjust = 0,
                      label.vjust = 1.1,
                      # label.theme = element_text(angle = 0),
                      ncol = 1,
                      reverse = T,# also the guide needs to be reversed
                      label.position = "right"
                    )
  )+
  theme_bw(base_size = pt)+ 
  theme(
    panel.background = element_rect(fill = '#ffffff', colour = 'black'),#D5DBDB
    panel.grid.minor=element_blank(),
    panel.grid.major=element_blank(),
    panel.border = element_rect(fill=NA, colour = "black", size=1.),#size
    #legend.title=element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),#x,y:longitude,latitude
    axis.text=element_text(color="black",size=9),
    legend.position = "right",
    #legend.spacing.y = unit(-0.1, 'cm'),legend.spacing.x = unit(-0.1, 'cm') 
    plot.title = element_text(hjust = 0.5),
    # plot.title = element_text(color = "red", size = 12, face = "bold"),
  )+ 
  labs(title = "population")

ggsave("map population.tiff",
       map.population,
       width = 13 , height = 10, units = "cm",
       compression="lzw",
       dpi = 900)

##Create the heatmap of treecover###
rasterset.treecover <- as.data.frame(log(treecover_southeast), xy = T) %>% na.omit()
treecover <- setDT(rasterset.treecover)
sum(is.na(treecover))
setnames(treecover, c("x","y","treecover"))
fwrite(treecover,"treecover.csv")
names(treecover)[3] <- "mean"
treecover.pretty_breaks <- seq(0,max(treecover$mean), length=10)
treecover.brks <- treecover.pretty_breaks
treecover.labels <- c(round(treecover.brks,2)[-c(1,length(treecover.brks))]," ")
treecover$brks <- cut(treecover$mean, 
                      breaks = treecover.brks, 
                      include.lowest = TRUE, 
                      labels = treecover.labels)

brks_scale <- levels(treecover$brks)
labels_scale <- (brks_scale)
display.brewer.pal(9, "Greens")
display.brewer.all()
coul <- brewer.pal(9, "Greens")
barplot(rep(1, length(coul)), col = coul , main="Greens") 
names(coul) <- labels_scale
pt=11
map.treecover <- ggplot()+
  geom_raster(data = treecover, aes(x = x, y = y,fill=brks))+
  scale_fill_manual(values = (coul),
                    breaks = (brks_scale),
                    labels = (labels_scale),
                    guide = guide_legend(
                      direction = "vertical",
                      keyheight = unit(5, units = "mm"),
                      keywidth = unit(4, units = "mm"),
                      title = "treecover",
                      title.position = "bottom",
                      # exactly at the right end of each legend key
                      # title.hjust = 0.5,
                      label.hjust = 0,
                      label.vjust = 1.1,
                      # label.theme = element_text(angle = 0),
                      ncol = 1,
                      reverse = T,# also the guide needs to be reversed
                      label.position = "right"
                    )
  )+
  theme_bw(base_size = pt)+ 
  theme(
    panel.background = element_rect(fill = '#ffffff', colour = 'black'),#D5DBDB
    panel.grid.minor=element_blank(),
    panel.grid.major=element_blank(),
    panel.border = element_rect(fill=NA, colour = "black", size=1.),#size
    #legend.title=element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),#x,y:longitude,latitude
    axis.text=element_text(color="black",size=9),
    legend.position = "right",
    #legend.spacing.y = unit(-0.1, 'cm'),legend.spacing.x = unit(-0.1, 'cm') 
    plot.title = element_text(hjust = 0.5),
    # plot.title = element_text(color = "red", size = 12, face = "bold"),
  )+ 
  labs(title = "treecover")

ggsave("map treecover2.tiff",
       map.treecover,
       width = 13 , height = 10, units = "cm",
       compression="lzw",
       dpi = 900)

save.image()



