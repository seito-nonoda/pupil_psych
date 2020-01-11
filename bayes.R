#load library
library(dplyr)
library(glmnet)
library(stringr)
library(rstan)
library(rstudioapi)
library(bayesplot)
library(tibble)

#read data
data.all.raw <- read.csv("data.all.1.csv")
data.all.raw <- data.all.raw[1:67,]
num.person <- nrow(data.all.raw) #number of people

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

#prepare independent variables
x <- data.all.raw[,c(ind.ini:ind.last)]
x.scaled <- scale(x)
##omit first-orderly dependent varibales
x.scaled <- x.scaled[,!str_detect(colnames(x),"prepost") &
                       !str_detect(colnames(x),"Initial.Bottom") &
                       !str_detect(colnames(x),"Initial.final")]
x.scaled.pre <- x.scaled[,str_detect(colnames(x.scaled),"_pre")] 
x.scaled.post <- x.scaled[,str_detect(colnames(x.scaled),"_post")]
##extract variables based on lasso regression
ind.las <- c(1,3)
num.ind <- length(ind.las)
x.bayes <- x.scaled.pre[,ind.las]
x.bayes <- cbind(1,x.bayes)

#conduct regression analysis
data <- list(N=nrow(x.scaled.pre),M=ncol(x.bayes),
             y=y.3,x=x.bayes)
model.reg <- stan_model("reg.stan")
fit.reg <- sampling(model.reg,data=data,
                    chains=3,cores=3)
##check convergence
stan_trace(fit.reg,pars="beta[1]")
stan_hist(fit.reg,pars="beta[1]")
stan_dens(fit.reg,pars="beta[1]",separate_chains = TRUE)
##see scatter plot of true and predictive values
beta1 <- rstan::extract(fit.reg,pars="beta[1]") %>% 
  unlist()
beta1.map <- density(beta1)$x[which.max(density(beta1)$y)]
beta2 <- rstan::extract(fit.reg,pars="beta[2]") %>% 
  unlist()
beta2.map <- density(beta2)$x[which.max(density(beta2)$y)]
beta3 <- rstan::extract(fit.reg,pars="beta[3]") %>% 
  unlist()
beta3.map <- density(beta3)$x[which.max(density(beta3)$y)]
sigma <- rstan::extract(fit.reg,pars="sigma") %>% 
  unlist()
sigma.map <- density(sigma)$x[which.max(density(sigma)$y)]
coef <- matrix(c(beta1.map,beta2.map,beta3.map),
               ncol = 1)
pred.point <- x.bayes %*% coef
true_pred <- data.frame("true"=y,"pred"=pred.point)
scat <- ggplot(true_pred,
               aes(x=true,y=pred))
scat <- scat + geom_point(size=2,alpha=1,color="blue")
scat
##compute p-value(bayesian)
postmat <- as.matrix(fit.reg)
num.sample <- nrow(postmat)
T.pred <- postmat[,c(1:3)]%*%t(x.bayes) %>% 
  apply(1,mean)
T.true <- mean(y)
pvalue <- sum(T.pred>T.true)/num.sample
pvalue
##compute (R)MSE
y.eap <- postmat[,c(1:3)]%*%t(x.bayes) %>% 
  apply(2,mean)
MSE <- mean((y-y.eap)**2)
RMSE <- sqrt(MSE)
##see post predictive distribution
pred <- c()
for(p in 1:num.person){
 pred.person = x.bayes[p,]%*%t(postmat[,c(1:(1+num.ind))])
 pred <- rbind(pred,pred.person)
}
pred <- t(pred)
colnames(pred)=rep("p",times=num.person)
pred <- as_tibble(pred)
draw_pred <- function(data,y,i){
  #data:sample from post ditribution
  #y:true values
  #i:id of person"
  data <- data[,i]
  fig <- ggplot(data,aes(x=p))
  fig <- fig + geom_density(size=0.5,alpha=0.5,fill="red")
  fig <- fig + geom_vline(xintercept=y[i])
  fig
}
draw_pred(pred,y,1)
##distribution of realized y and predicted y
allpred <- as.matrix(pred) %>% 
  as.vector()
allreal <- rep(y,times=nrow(pred))
alldata <- data.frame("pred"=allpred,"real"=allreal)
dist <- ggplot(alldata,aes(y=..density..))
dist <- dist + geom_histogram(aes(x=real),
                              bins=30,
                              position="identity",
                              alpha=0.5,
                              fill="red")
dist <- dist + geom_density(aes(x=pred),
                            stat="density",
                            position="identity",
                            alpha=0.5,
                            fill="blue")
dist
