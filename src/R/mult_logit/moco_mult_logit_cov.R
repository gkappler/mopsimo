# Experimental. No warranty.

# Load required libraries
library(lme4)
library(ggplot2)
library(R2jags)

# Load data
data <- read.table("../../../data/FS_MOCO.tab", header=TRUE)

# Create matrix with response patterns
check<-apply(cbind(data$pow, data$aff, data$ach),1,sum)
select=which(check==1 | check==0)
data<-data[select,]
resp<-cbind(data$pow, data$aff, data$ach)
 
# Extract unique picture an person ids
unique_pid<-unique(data$pid)
unique_pic<-unique(data$pic)

# Convert data in long format
n=dim(data)[1]
id<-c(0)
item<-c(0)
choice<-rep(1,n)


for(i in 1:n)
{
 id[i]= which(data$pid[i]==unique_pid)
 item[i]=which(data$pic[i]==unique_pic)
 cat<-which(resp[i,]==1)+1
 if(length(cat)!=0)
 {
   choice[i]<-cat
 }   
}

# Check the choice variable for sanity
hist(choice)

# Prepare data for JAGS
y<-choice[1:n]
id<-id[1:n]
item<-item[1:n]

# Setting prior scale of the scaled inverse Wishart 
# distribution to the identity matrix.
# See e.g. Gelman and Hill (2007), p. 377

W=diag(3)

# Join data into a list
data<-list("y","n", "item", "id", "W")

# Define requested parameters
parameters<-c( "beta1","beta2","beta3", "theta1", "theta2", "theta3",
              "sigma.theta1", "sigma.theta2", "sigma.theta3",
              "rho_theta12", "rho_theta13", "rho_theta23")

# Inits are in a mess, feel free to improve
#inits <- function(){
#list(tau1= 0, tau2=0, tau3=0, sigma1=rep(1,6), sigma2=rep(1,6), sigma3=rep(1,6),
#     theta1=rep(0,81), theta2=rep(0,81), theta3=rep(0,81), sigma.sigma1=1
#     sigma.sigma2=1, sigma.sigma3=1, sigma.theta1=1, sigma.theta2=1, sigma.theta3=1)
#}


# Fit model (Chuck Norris does not need inits ;) )
output<-jags(data, inits=NULL, parameters, model.file="moco_mult_logit_cov.txt",
    n.chains=2, n.iter=4000, n.burnin=2000)

attach.jags(output)

# Estimate reliabilities by Andrich's method.

var_theta1<-var(apply(theta1,2,mean))
error1<-mean(apply(theta1,2,var))
rel1<-var_theta1/(var_theta1+error1)

var_theta2<-var(apply(theta2,2,mean))
error2<-mean(apply(theta2,2,var))
rel2<-var_theta2/(var_theta2+error2)

var_theta3<-var(apply(theta3,2,mean))
error3<-mean(apply(theta3,2,var))
rel3<-var_theta3/(var_theta3+error3)

          