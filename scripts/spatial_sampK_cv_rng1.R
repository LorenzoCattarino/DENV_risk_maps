
setwd("C:/Users/fergnm/Dropbox (SPH Imperial College)/MalariaStuff/BRT")
setwd("D:/Dropbox (SPH Imperial College)/MalariaStuff/BRT")

source("map_fn.r")
library(ranger)
library(ggplot2)
library(plyr)
library(data.table)
library(matrixStats)
library(readr)
library(weights)

set.seed(79172)


full.data <- read.csv("data/env_for_points.csv")
LnL.data<- read.csv("data/Fits_Time_ALL_trans_mean.csv")
LnL.probs<-LnL.data[,3:52]
LnL.data$LnL.max=apply(LnL.probs,1,max)
LnL.probs<-LnL.probs[,]-LnL.data$LnL.max
LnL.probs<-exp(LnL.probs)
LnL.data$LnL.sum=apply(LnL.probs,1,sum)
LnL.probs<-LnL.probs[,]/LnL.data$LnL.sum


env.data<-as.data.frame(as.matrix(fread("data/FTMap_62046.txt",header=TRUE,sep="\t")))
env.data$dtMnt=env.data$dt_new_const-env.data$nt_new_const

full.data$dtMnt=full.data$dt_new_const-full.data$nt_new_const

Kstep=log(2000)/50

model.data.all<- full.data[full.data$syear>=1985,]
model.LnL.probs<-LnL.probs[full.data$syear>=1985,]
model.data.all$lnK.max<-log(1000)-Kstep*(50-apply(model.LnL.probs, 1, which.max))
model.LnL.vals<-array(rep(1:50,each=nrow(model.LnL.probs)),dim(model.LnL.probs))
model.LnL.m1<-model.LnL.probs*(log(1000)-Kstep*(50-model.LnL.vals))
model.LnL.m2<-model.LnL.m1*(log(1000)-Kstep*(50-model.LnL.vals))
model.data.all$lnK.mean<-apply(model.LnL.m1, 1, sum)
model.data.all$lnK.var<-apply(model.LnL.m2, 1, sum)-model.data.all$lnK.mean*model.data.all$lnK.mean
model.data.all$new.weight<-1/model.data.all$lnK.var
model.data.all$new.weight<-ifelse(model.data.all$new.weight>5,5,model.data.all$new.weight)

Nfits=50

grid.size=5
cell.fraction=0.5
train.fraction=1

corr.fits<-NULL
corr.fits$run<-seq(Nfits)
corr.fits$train<-rep(0,Nfits)
corr.fits$valid<-rep(0,Nfits)
rms.fits<-NULL
rms.fits$run<-seq(Nfits)
rms.fits$train<-rep(0,Nfits)
rms.fits$valid<-rep(0,Nfits)

predict.vector<-NULL
mod.name.base="A85.Lnk.RNG_spatial_prob50B.wSA"


ptm <- proc.time()
for(i in corr.fits$run)
{
mod.name=paste(mod.name.base,i,sep="")

# draw random distance values 
rd <- runif(n=1, min=0, max=grid.size)
rd2 <- runif(n=1, min=0, max=grid.size)
   
# add rd to lat.grid and long.grid variables 
model.data.all$lat.grid <- floor((model.data.all$latitude-rd)/grid.size)
model.data.all$long.grid <- floor((model.data.all$longitude-rd2)/grid.size)
min.long <- min(model.data.all$long.grid)
width.long <- max(model.data.all$long.grid)-min.long+1
min.lat <- min(model.data.all$lat.grid)

model.data.all$cell<-(model.data.all$lat.grid-min.lat)*width.long+model.data.all$long.grid-min.long

cell.unique<-as.data.frame(unique(model.data.all$cell))
colnames(cell.unique)[1]<-"cell"
cell.unique$pick<-floor(runif(nrow(cell.unique),0,1/cell.fraction))
cell.unique.N<-nrow(cell.unique)
model.data<-merge(x = model.data.all, y = cell.unique, by = "cell", all.x = TRUE)
#model.data<-join(model.data.all,cell.unique)
model.data$pick<-ifelse(model.data$pick==0,1,0)
cell.count<-as.data.frame(table(model.data$cell))
colnames(cell.count)[1]<-"cell"
colnames(cell.count)[2]<-"cell.count"
model.data<-merge(x = model.data, y = cell.count, by = "cell", all.x = TRUE)
#model.data<-join(model.data,cell.count,by = "cell",type="left")
model.data<-model.data[,c(2:ncol(model.data),1)]

#WeightsA<-model.data$Weight*model.data$Area_85/(model.data$Area_85+999)/sqrt(model.data$count_id_85)
#model.data$RF.weight<-1/model.data$cell.count
#model.data$RF.weight<-model.data$Weight*model.data$WeightA_85/sqrt(model.data$count_id_85)

model.data$RF.weight<-model.data$new.weight

model.data.N=eval(nrow(model.data))
train.N=floor(train.fraction*sum(model.data$pick==1))
model.training.index=seq(model.data.N)
model.training.set<-sample(model.training.index,train.N,replace=FALSE,prob=model.data$pick)

model.data.train<-model.data[model.training.set,]
model.data.valid<-model.data[-model.training.set,]
#plot(model.data.train$longitude,model.data.train$latitude)

#x.cols=c(57:92,159) #specify covariates
#x.cols=c(57:59,62,68,74,80,81,92) #specify covariates

#x.cols=c(57,58,163,61,63,64,68,92) #specify covariates - 163 is calculated one
#x.cols=c(49:52,92,109:111,114,115,123,124,126,127,129,130,159)
x.cols=c("mean_rain","cov_nt","max_dt","range_rain","evimin","mir_c2","LC6","LC7","ldens20","altitude")
y.col=c("lnK.max")
all.cols=c(y.col,x.cols)

x.data<-model.data[,all.cols]

x.data.train<-model.data.train[,all.cols]
x.data.valid<-model.data.valid[,all.cols]
y.data<-model.data[,y.col]
y.data.train<-model.data.train[,y.col]
y.data.valid<-model.data.valid[,y.col]

RFmodel <- ranger(lnK.max ~ .,data=x.data.train,num.trees=1000,case.weights=model.data.train$RF.weight,write.forest=TRUE)
cat(paste("..", i, ".."))
if (.Platform$OS.type == "windows") flush.console()

y.data.train.pred<-RFmodel$predictions
#dev.new()
#plot(y.data.train,y.data.train.pred)

y.data.valid.pred<-predict(RFmodel,x.data.valid)$predictions
#dev.new()
#plot(y.data.valid,y.data.valid.pred)

#y.data.pred<-predict(RFmodel,x.data)$predictions
#dev.new()
#plot(y.data,y.data.pred)

corr.fits$train[i]<-wtd.cors(y.data.train,y.data.train.pred,model.data.train$RF.weight)
corr.fits$valid[i]<-wtd.cors(y.data.valid,y.data.valid.pred,model.data.valid$RF.weight)

rms.fits$train[i]<-sqrt(weighted.mean((y.data.train-y.data.train.pred)^2,model.data.train$RF.weight))
rms.fits$valid[i]<-sqrt(weighted.mean((y.data.valid-y.data.valid.pred)^2,model.data.valid$RF.weight))

#dev.new()
#varImpPlot(RFmodel)

#predict low res

predict.data<-env.data[,x.cols]
predict.env<-predict(RFmodel, predict.data)$predictions
predict.vector <- cbind(predict.vector,predict.env)
#predict.output<-data.frame(id=env.data$id,longitude=env.data$longitude,latitude=env.data$latitude,pred_LnK=predict.vector[,i])
#write.table(predict.output,paste(mod.name,"_pred.csv",sep=""),sep=",", row.names = F)

}
proc.time() - ptm

mod.name=paste(mod.name.base,"_av",sep="")

predict.vector.mean<-rowMeans(predict.vector)
predict.vector.sd<-rowSds(predict.vector)

mapdat<-data.frame(predict.vector.mean,env.data)
colnames(mapdat)[1]<-"logK"
delta.deg=0.2
# create a vector of ordered (grid) values of latitude & longitude
lat = seq(min(mapdat$latitude),max(mapdat$latitude),by=delta.deg)
lon = seq(min(mapdat$longitude), max(mapdat$longitude),by=delta.deg)
par(mfrow=c(1,1))
dev.new()
print.map(mapdat$logK,mapdat$longitude,mapdat$latitude,"Carrying Capacity",paste(mod.name,"_map.png",sep=""),-1,7)

mod.name=paste(mod.name.base,"_sd",sep="")

mapdat<-data.frame(predict.vector.sd,env.data)
colnames(mapdat)[1]<-"logK"
delta.deg=0.2
# create a vector of ordered (grid) values of latitude & longitude
lat = seq(min(mapdat$latitude),max(mapdat$latitude),by=delta.deg)
lon = seq(min(mapdat$longitude), max(mapdat$longitude),by=delta.deg)
par(mfrow=c(1,1))
dev.new()
print.map(mapdat$logK,mapdat$longitude,mapdat$latitude,"Carrying Capacity SD",paste(mod.name,"_map.png",sep=""),0,1.5)

mean(corr.fits$train)
mean(corr.fits$valid)
mean(rms.fits$train)
mean(rms.fits$valid)

