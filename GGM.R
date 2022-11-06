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
