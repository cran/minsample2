#' Prints the minimum size of the sample required to get epsilon neighborhood for given value of epsilon for Normal Distribution
#' @import stats
#' @export
#' @param n a vector of proposed sample size
#' @param eps a vector of the precision level
#' @param mu the location parameter for the underlying distribution, here normal distribution(mean)
#' @param sigma the scale parameter for the underlying distribution, here normal distribution(standard deviation)
#' @description This package helps determining the minimum sample size required to attain some pre-fixed precision level
#' @details in any distribution for a large sample the mean-squared error gradually tends to zero, the minimum number depends on the precision level i.e. the pre-fixed eplison
#' @references Methods for this process is described in A.M.Gun,M.K.Gupta,B.Dasgupta(2019,ISBN:81-87567-81-3).
#' @return report: the data frame containing the minimum value of the sample size corresponding to the pre-fixed epsilon
#' @examples l_norm(1:5,0.5,3,1)
#' @name l_norm


l_norm=function(n,eps,mu=0,sigma=1){
  T1=data.frame(epsilon=0,size=0,integral=0)
  report=data.frame(epsilon=0,n=0)
  R=1000
  for(i in 1:length(eps)){
    for(j in 1:length(n)){
      xbar=replicate(R,mean(rnorm(n[j],mu,sigma)))
      y=sqrt(n[j])*(xbar-mu)/sigma

      w=0.01
      u=seq(-10,10,w)
      F_n=c()
      for (k in 1:length(u)){
        F_n[k]=mean(y<=u[k])
      }
      diff=(F_n-pnorm(u))^2
      T1[j,]=c(eps[i],n[j],sum(diff[-1]+diff[-length(u)])*w)
    }
    T2=T1[T1$integral<eps,]
    n[j]=T2$size[which.max(T2$integral)]
    report[i,]=c(eps[i],n[j])
  }
  return(report)
}







#' Prints the minimum size of the sample required to get epsilon neighborhood for given value of epsilon for Exponential Distribution
#' @export
#' @import stats
#' @param n a vector of proposed sample size
#' @param eps a vector of the precision level
#' @param theta the parameter for the underlying distribution, here Exponential Distribution
#' @description This package helps determining the minimum sample size required to attain some pre-fixed precision level.
#' @details in any distribution for a large sample the mean-squared error gradually tends to zero, the minimum number depends on the precision level i.e. the pre-fixed eplison.
#' @references Methods for this process is described in A.M.Gun,M.K.Gupta,B.Dasgupta(2019,ISBN:81-87567-81-3).
#' @return report: the data frame containing the minimum value of the sample size corresponding to the pre-fixed epsilon
#' @examples l_exp(1:5,0.5,1)
#' @name l_exp


l_exp=function(n,eps,theta=1){
  T1=data.frame(epsilon=0,size=0,integral=0)
  report=data.frame(epsilon=0,n=0)
  R=1000
  for(i in 1:length(eps)){
    for(j in 1:length(n)){
      xbar=replicate(R,mean(rexp(n[j],theta)))
      y=sqrt(n[j])*(xbar-theta)/theta^2

      w=0.01
      u=seq(0.01,100,w)
      F_n=c()
      for (k in 1:length(u)){
        F_n[k]=mean(y<=u[k])
      }
      diff=(F_n-pexp(u))^2
      T1[j,]=c(eps[i],n[j],sum(diff[-1]+diff[-length(u)])*w)
    }
    T2=T1[T1$integral<eps,]
    n[j]=T2$size[which.max(T2$integral)]
    report[i,]=c(eps[i],n[j])
  }
  return(report)
}


