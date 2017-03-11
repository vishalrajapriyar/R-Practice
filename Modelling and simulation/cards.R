deck=read.table("deck.csv",sep = ",",header = TRUE,stringsAsFactors = FALSE)

#Setup function
setup<-function(deck){
  DECK<-deck
  
  DEAL<-function(){
    card<-deck[1,]
    assign("deck",deck[-1,],envir = parent.env(environment()))
    card
  }
  
  SHUFFLE<-function(){
    random<-sample(1:52,size=52)
    assign("deck",DECK[random,],envir = parent.env(environment()))
  }
  
  list(deal=DEAL,shuffle=SHUFFLE)
}

#Initializing variables to play the game
cards<-setup(deck)
deal<-cards$deal
shuffle<-cards$shuffle

#Use deck_GOW to play the Game of War
deck_GOW<-deck
deck_GOW$value[c(13,26,39,52)]<-14

#Use Deck_GOWS to play the Game of War (If deck was shuffled)
deck_GOWS<-shuffle()
deck_GOWS$value[deck_GOWS$face=="ace"]<-14

#Use deck_GOH for playing the Game of Hearts
deck_GOH<-deck
deck_GOH$value<-0
deck_GOH$value[deck_GOH$suit=="hearts"]<-1
queenOfSpades<-deck_GOH$face=="queen"& deck_GOH$suit=="spades"
deck_GOH[queenOfSpades,]<-13

#Use deck_GOB for playing the Game of BlackJack
deck_GOB<-deck
facecard<-deck_GOB$face %in% c("king","queen","jack")
deck_GOB$value[facecard]<-10
deck_GOB$value[deck_GOB$face=="ace"]<-NA

