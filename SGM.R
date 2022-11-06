SGM <- function(D0,g,t,cg,ce){
  cat(sprintf("Step 1\n"))
  for(i in 1:t ){
    if (i == 1){
      D = round(D0*(1+g),4)
      cat(sprintf("D%i = %0.4f X (1 + %0.2f) = %0.4f\n",i,D0,g,D))
      }
    else{
    D = append(D,round(D[i-1]*(1+g),4))
    }
    cat(sprintf("D%i = %0.4f X (1 + %0.2f) = %0.4f\n",i,D[i-1],g,D[i]))
  }
  
  cat(sprintf("Step 2\n"))
  Dt = D[length(D)]*(1+cg)
  cat(sprintf("Dt = %0.4f X (1    + %0.2f)  =   %0.4f\n",D[length(D)],cg,Dt))
  
  Vt = round(Dt/(ce-cg),4)
  cat(sprintf("Vt = %0.4f / (%0.2f - %0.2f)  =  %0.4f\n",Dt,ce,cg,Vt))
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
    cat(sprintf("%0.4f / (1 + %0.2f)^%i + ",D[i],ce,i))
  }
    cat(sprintf("(%0.4f + %0.4f) / (1 + %0.2f)^%i = %0.2f\n",D[t],Vt,ce,t,V0))
 }

SGM(1.45,0.15,4,0.06,0.11)
