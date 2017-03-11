library("ggplot2")
#Initializing
ladder.df <- data.frame(start=c(3,11), end=c(13,17))
snake.df <- data.frame(start=c(10,16,18), end=c(5,2,8))
out <-function()
{
  curLoc <- 0 # Current location
  nroll <- 0 # Number of rolls
  snakes <- 0 # Number of snakes encountered
  ladders <- 0 # Number of ladders encountered
  
  # Keep rolling dice and moving until reach 100 or greater ending the game
  while(curLoc < 20) {
    roll <- sample(6,size = 1,replace = TRUE,prob = c(1/6,1/6,1/6,1/6,1/6,1/6)) # generate random number between [1 to 6]
    curLoc <- curLoc + roll # increase position
    nroll <- nroll + 1 # increase number of rolls
    # Need to check if we landed on a ladder or snake and move forward or back
    if (any(ladder.df$s %in% curLoc)) {
      curLoc <- ladder.df$e[ladder.df$s %in% curLoc]
      ladders <- ladders + 1
    }
    if (any(snake.df$s %in% curLoc)) {
      curLoc <- snake.df$e[snake.df$s %in% curLoc]
      snakes <- snakes + 1
    }
    out.info <- list(No_of_rolls=nroll, No_of_ladder=ladders, No_of_snakes=snakes)
    
    
  }
  return (nroll)
}
# Expectation
plays<-replicate(1000,out())
mean_play<-mean(plays)
print(mean_play)
#Graph Plot
freq<-table(plays)
percent<-freq/10
frq = as.data.frame(percent)
plot(frq, main="Percentage chance to win in N rolls", ylab="Percentage", xlab="No of Rolls")