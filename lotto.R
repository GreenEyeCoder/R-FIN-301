# 5 balls (1-69)
# powerballs (1-26)

balls <- 1:69
powerBalls <- 1:26

winningLottoNumbers <- c(sample(balls,5),sample(powerBalls,1))
yourNumbers <- c(sample(balls,5),sample(powerBalls,1))

# first function would find out what percentage of numbers where winning 
# lotto numbers

match(yourNumbers,winningLottoNumbers,nomatch = 0)

