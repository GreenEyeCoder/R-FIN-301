# V0 = current value of stock
# D1 = dividend in one period from now
# k = estimate cost of equity.
# g = constant growth rate of the company's
#     dividends for an infinite time.

GGM <- function(D0,k,g){
  D1 = D0*(1+g);
  V0 = D1/(k-g)
  return (V0);
} 

SGM <- function(D0,g,t,cg,ce){
  cat(sprintf("Step 1\n"))
  for(i in 1:t ){
    if (i == 1){
      D = round(D0*(1+g),4)
      cat(sprintf("D%i = %0.4f X (1 + %0.3f) = %0.4f\n",i,D0,g,D))
    }
    else{
      D = append(D,round(D[i-1]*(1+g),4))
    }
    cat(sprintf("D%i = %0.4f X (1 + %0.3f) = %0.4f\n",i,D[i-1],g,D[i]))
  }
  
  cat(sprintf("Step 2\n"))
  Dt = D[length(D)]*(1+cg)
  cat(sprintf("Dt = %0.4f X (1    + %0.3f)  =   %0.4f\n",D[length(D)],cg,Dt))
  
  Vt = round(Dt/(ce-cg),4)
  cat(sprintf("Vt = %0.4f / (%0.3f - %0.3f)  =  %0.4f\n",Dt,ce,cg,Vt))
  for (i in 1:t){
    if(i == 1 ){
      V0 = D[1]/(1+ce)^i
    }
    else{
      V0 = V0 + D[i]/(1+ce)^i
    }
  }
  V0 = round(Vt/(1+ce)^t + V0,2)
  cat(sprintf("Step 3\n"))
  for(i in 1:t-1){
    cat(sprintf("%0.4f / (1 + %0.3f)^%i + ",D[i],ce,i))
  }
  cat(sprintf("(%0.4f + %0.4f) / (1 + %0.3f)^%i = %0.3f\n",D[t],Vt,ce,t,V0))
}

# MC = Market capitalization
# OS = Outstanding shares

#Market Value per Share 
MVPS <- function(MC,OS){
  return(MC/OS)
}

# MVPS = Market value per share
# EPS  = Earnings per share 

# Price to earings ratio
PE <- function(MVPS,EPS){
  return (MVPS/EPS)
}

# Price to share Ratio
PSRatio <- function(MC,Revenue){
  return(MC/Revenue)
}

# Price to book ratio
# Shareholder's Equity
PBRatio <- function(MC,SE){
  return(MC/SE)
}


