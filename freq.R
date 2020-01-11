#load library
library(dplyr)
library(glmnet)
library(stringr)

#read data
data.all.raw <- read.csv("data.all.1.csv")
data.all.raw <- data.all.raw[1:67,]

#extract the name of variables
name.all <- colnames(data.all.raw)
##independent variables
ind.ini <- which(name.all=="Initial_diamiter_pre")
ind.last <- which(name.all=="new_pupil_prepost")
name.ind <- name.all[ind.ini:ind.last]

#process missing values
##record the location of missing values
ind.miss <- list()
for(p in 1:nrow(data.all.raw)){
  for(i in 2:ncol(data.all.raw)){
    if(is.na(data.all.raw[p,i])){
      ind.miss <- c(ind.miss,
                    list(c(data.all.raw[p,1],colnames(data.all.raw)[i])))
    }
  }
}
##replace missing values with mean
###y.1 = stai_pre
y.1 <- data.all.raw$stai_pre
y.1[is.na(y.1)] = mean(y[!is.na(y.1)])
###y.2 = stai_post
y.2 <- data.all.raw$stai_post
y.2[is.na(y.2)] = mean(y[!is.na(y.2)])
###y.3 = relax_pre
relax <- select(data.all.raw,starts_with("relax"))
for(col in 1:ncol(relax)){
  relax[is.na(relax[,col]),col] = mean(relax[!is.na(relax[,col]),col])
}
y.3 <- select(relax,ends_with("_pre")) 
y.3 <- apply(y.3,1,mean)
###y.4 = relax_post
y.4 <- select(relax,ends_with("_post"))
y.4 <- apply(y.4, 1, mean)

#lasso regression
##prepare scaled independent variables
x <- data.all.raw[,c(ind.ini:ind.last)]
x.scaled <- scale(x)
###omit first-orderly dependent varibales
x.scaled <- x.scaled[,!str_detect(colnames(x),"prepost") &
                     !str_detect(colnames(x),"Initial.Bottom") &
                     !str_detect(colnames(x),"Initial.final")]
x.scaled.pre <- x.scaled[,str_detect(colnames(x.scaled),"_pre")] 
x.scaled.post <- x.scaled[,str_detect(colnames(x.scaled),"_post")]

##conduct lasso regression
###pre
####prepare dependent variabled
y <- y.3
las_pre <- glmnet(x.scaled.pre,y,
                family="gaussian",alpha = 1)
plot(las_pre,xvar="lambda",label=TRUE)
##investigate loo-cv
las_pre.cv <- cv.glmnet(x.scaled.pre,y,
                      family="gaussian",alpha = 1)
plot(las_pre.cv)
lambda_pre <- las_pre.cv$lambda.min #best value of lambda
##investigate precision of prediction
est.y <- predict(las_pre,x.scaled.pre,s=lambda_pre,
                   type="response")
plot(y,est.y)
###post
####prepare dependent variabled
y <- y.4
las_post <- glmnet(x.scaled.post,y,
                family="gaussian",alpha = 1)
plot(las_post,xvar="lambda",label=TRUE)
##investigate loo-cv
las_post.cv <- cv.glmnet(x.scaled.post,y,
                      family="gaussian",alpha = 1)
plot(las_post.cv)
lambda_post <- las.1.cv$lambda.min #best value of lambda
##investigate precision of prediction
est.y <- predict(las_post,x.scaled.post,s=lambda_post,
                   type="response")
plot(y,est.y)
