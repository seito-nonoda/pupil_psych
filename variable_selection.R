#load library
library(dplyr)
library(glmnet)

#read data
data.pupil.raw <- read.csv("ug_pupil.csv")
data.psych.raw <- read.csv("ug_psychdata.csv")

#make data including all variables
data.all.raw <- inner_join(data.pupil.raw,data.psych.raw,
                           by="X")

num.ind <- ncol(data.pupil.raw)-3 #number of independent variables
num.dep <- ncol(data.psych.raw)-3 #number of dependent variables

#replace names with ID
data.all.raw[,1] <- c(1:nrow(data.all.raw))

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
##replace missing values with 0
data.miss_0 <- data.all.raw
data.miss_0[is.na(data.all.raw)] = 0

#extract names of variables
##independent variables
name.ind <- colnames(data.pupil.raw[,4:(num.ind+3)])
##dependent variables
name.dep <- colnames(data.psych.raw[,4:(num.dep+3)])
name.dep <- name.dep[1:78]

#lasso regression
##prepare scaled independent variables
x <- data.miss_0[,c(4:(num.ind+3))]
x.scaled <- scale(x)
x.scaled.pre <- x.scaled[,1:10]
x.scaled.post <- x.scaled[,11:20]
##prepare dependent variabled(CFI pre)
# tipi.all <- select(data.miss_0,starts_with("TIPI"))
# tipi.all <- tipi.all[,1:5]
# y <- apply(tipi.all,MARGIN = 1,sum)
##conduct lasso regression
y.1 <- data.miss_0$CFI_PRE_total
las.1 <- glmnet(x.scaled.pre,y,
                family="gaussian",alpha = 1)
plot(las.1,xvar="lambda",label=TRUE)
##investigate loo-cv
las.1.cv <- cv.glmnet(x.scaled.pre,y,
                      family="gaussian",alpha = 1)
plot(las.1.cv)
lambda1 <- las.1.cv$lambda.min #best value of lambda
##investigate precision of prediction
est.y.1 <- predict(las.1,x.scaled.pre,s=lambda1,
                   type="response")
plot(y,est.y.1)