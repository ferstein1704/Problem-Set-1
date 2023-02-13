# Estimation 

# Construimos nuestro vector de datos, dropeo la variable Centro
data <- aux_df %>% ungroup()%>% select(-c(ID, centro))

# Suponiendo que el modelo es logit:

################################################################################
# Calculates the maximum likelihood estimates of a logistic regression model
#
# fmla : model formula
# x : a [n x p] dataframe with the data. Factors should be coded accordingly
#
# OUTPUT
# beta : the estimated regression coefficients
# vcov : the variane-covariance matrix
# ll : -2ln L (deviance)
#
################################################################################
mle.probit = function(fmla, data)
{
  # Define the negative log likelihood function
  logl <- function(theta,x,y){
    y <- y
    x <- as.matrix(x)
    beta <- theta[1:ncol(x)]
    # Use the log-likelihood of the Bernouilli distribution, where p is
    # defined as the logistic transformation of a linear combination
    # of predictors, according to logit(p)=(x%*%beta)
    loglik <- sum(y*log(pnorm(x%*%beta))+(1-y)*log(1 -pnorm(x%*%beta)))
    return(-loglik)
  }
  # Prepare the data
  outcome = rownames(attr(terms(fmla),"factors"))[1]
  dfrTmp = model.frame(data)
  x = as.matrix(model.matrix(fmla, data=dfrTmp))
  y = as.numeric(as.matrix(data[,match(outcome,colnames(data))]))
  # Define initial values for the parameters
  theta.start = rep(0,(dim(x)[2]))
  names(theta.start) = colnames(x)
  # Calculate the maximum likelihood
  mle = optim(theta.start,logl,x=x,y=y,hessian=T)
  out = list(beta=mle$par,vcov=solve(mle$hessian),ll=2*mle$value)
}
###############################################################################

fmla = as.formula("D~edad+noroeste+sureste+noreste+occidente+n_menores+n_mayores+a_escolaridad") #Create model formula
probit = mle.probit(fmla, data) #Estimate coefficients
probit

