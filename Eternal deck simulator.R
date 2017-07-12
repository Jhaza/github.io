##Preparation: make a folder with a .txt file for each decklist you want to analyze,
##as well as the spreadsheet with card power/faction requirements found here:
##<link to spreadsheet>

##Set number of turns to consider. Default is 12 (chosen by fair die roll).
nTurns = 12;




##Put the path for the folder below, in quotes.
setwd("C:/Users/Charlie/Google Drive/Shared - Elspeth/Eternal Script")

##Replace "Rakano Aggro.txt" below with the name of the text file with the deck list you want to analyze.
deck_raw <- read.delim("Rakano Aggro.txt", sep="\n", colClasses="character",col.names="cards",header=FALSE)
##Reads in the decklist file.

cardlist <- read.csv("Card Data.csv",header=TRUE)
##Reads in the card data file. Rename "Card Data.csv" above as appropriate. 

num_cards <- as.numeric(substr(deck_raw[,1],1,2))
##Extracts the counts of each card

tmp <- list()
ind <- list()
for (i in c(1:nrow(deck_raw))){
  tmp[i] <- trimws(substr(deck_raw[i,1],3, regexpr("(",deck_raw[i,1],fixed=TRUE)[1]-2),which="both")
  ind[i] <- which(cardlist$Card.Name %in% tmp[i])
}
card_name <- unlist(tmp)
card_index <- unlist(ind)
##Extracts the name of each card, and the index in the "cardlist" for each card.




deck <- data.frame(num_cards, card_name, cardlist[card_index,c(4:9)])
names <- c("Num", "Names", "Color1", "Color2", "PCost", "Inf1", "Inf2", "IsPow")
colnames(deck) <- names
deck$IsPow <- as.logical(deck$IsPow == "Power")
##Combines the the various data pieces we've found into a dataframe; 
##converts the "type" column from the card list into a binary "is power" variable.

deck <- deck[rep(row.names(deck),deck$Num),1:8]
##Expands the "deck" dataframe so that each row corresponds to a single instance of a card.
##ie, going from one row for my 4x Sandstorm Titans to 4 rows, one for each.

tCards <- nrow(deck)



is_valid_draw <- FALSE
while(!is_valid_draw){
  ind <- sample(tCards, replace=FALSE)
  is_valid_draw <- as.logical((sum(deck[ind[c(1:7)],8])>=1)*(sum(deck[ind[c(1:6)],8])<=6))
}
##Tests to see if a given deck order produces a valid draw, using the rules for the first draw.

is_valid_mul <- FALSE
mul_draw <- sample(c(2:4),1)
while(!is_valid_mul){
  ind <- sample(tCards, replace=FALSE)
  is_valid_mul <- (sum(deck[ind[c(1:7)],8])==mul_draw)
}
##Tests to see if a given deck order produces a valid draw for a mulligan.


is_drawn <- rep.int(FALSE,tCards)
can_play <- deck[,8]
##This initializes the "can_play" variable to the "is_power" variable; so, power cards can always be played with no delay or check, and all other cards default to 0 and require a check.

delay <- deck[,8]-1
draws_first <- sample(c(0,1),1)
cur_draw <- 7 + draws_first
##Randomly assigns you to either go or draw first, then draws appropriately.

cur_turn <- 1
is_drawn[ind[c(1:cur_draw)]] <- TRUE
##Initialize a bunch of variables used later on.

pow_num <- min(cur_turn, sum(deck[is_drawn,8]))
inf <- list("Red"=0, "White"=0, "Black"=0,"Blue"=0,"Green"=0)
inf$Red <- sum((deck[is_drawn,8]==1)*(deck[is_drawn,3]=="Red"|deck[is_drawn,4]=="Red"))
inf$White <- sum((deck[is_drawn,8]==1)*(deck[is_drawn,3]=="White"|deck[is_drawn,4]=="White"))
inf$Blue <- sum((deck[is_drawn,8]==1)*(deck[is_drawn,3]=="Blue"|deck[is_drawn,4]=="Blue"))
inf$Black <- sum((deck[is_drawn,8]==1)*(deck[is_drawn,3]=="Black"|deck[is_drawn,4]=="Black"))
inf$Green <- sum((deck[is_drawn,8]==1)*(deck[is_drawn,3]=="Green"|deck[is_drawn,4]=="Green"))
##Finds the number of power cards available, and the number of influence symbols of each color.

for(i in ind[is_drawn]){
  ##Checks each card that has been drawn
 if ((can_play[i]==0) & (as.numeric(deck[i,5])<=cur_turn)){
   ##Only considers cards that haven't previously been flagged as playable, and with power costs less than or equal to the current turn number.
   if(as.numeric(deck[i,5])<=pow_num){
     ##Separate checks for mono and bi-colored cards. 
     if(deck[i,4]!=""){
      can_play[i] <- (deck[i,6] <= unlist(inf[as.character(deck[i,3])]))&(deck[i,7] <= (unlistinf[as.character(deck[i,4])]))
     } else {
      can_play[i] <- deck[i,6] <= unlist(inf[as.character(deck[i,3])] )
     }
      if (can_play[i]==1){
        delay[i] <- cur_turn - as.numeric(unlist(deck[i,5]))
        ##Updates the delay entry if a card became playable this turn. 
     }
   }
 }
}

##This code takes care of the first turn as a special case.
##The following while-loop iterates through all following turns.


while((cur_turn <= nTurns)&(cur_draw < tCards)){
  cur_turn <- cur_turn +1
  while((cur_draw < tCards) & (is_drawn[ind[cur_draw+1]]==TRUE)){
    cur_draw <- cur_draw +1
  } 
  ##Iterates through the deck until it finds a card that hasn't been drawn.
  if(is_drawn[ind[cur_draw+1]]==FALSE){
    ##This check is necessary in the case where you go through the entire deck and the last card has already been drawn.
    cur_draw <- cur_draw+1
    is_drawn[ind[cur_draw]] <- TRUE
    ##Draws the next card.
    
    pow_num <- min(cur_turn, sum(deck[is_drawn,8]))
    ##Updates the number of available power.

    

    if(deck[ind[cur_draw],8]==TRUE & deck[cur_draw,3]!="None"){
      inf[as.character(deck[cur_draw,3])] <- as.numeric(inf[as.character(deck[cur_draw,3])]) + 1
      if(deck[cur_draw,4]!=""){
        inf[as.character(deck[cur_draw,4])] <- as.numeric(inf[as.character(deck[cur_draw,4])]) + 1     
      }
    }
    
    

    
    
    ##If the drawn card is a power card, update the appropriate influence types. 
    for(i in ind[is_drawn]){
      if (can_play[i]==FALSE & deck[i,8]==FALSE & (as.numeric(deck[i,5])<=cur_turn)){
        if(as.numeric(deck[i,5])<=pow_num){
          if(deck[i,3]=="None"){
            can_play[i] <- 1
          } else if(deck[i,4]!=""){
            can_play[i] <- (as.numeric(deck[i,6]) <= inf[as.character(deck[i,3])])&(as.numeric(deck[i,7]) <= inf[as.character(deck[i,4])])
          } else {
            can_play[i] <- as.numeric(deck[i,6]) <= inf[as.character(deck[i,3])] 
          }
          if (can_play[i]==1&pow_num==as.numeric(deck[i,5])){
            delay[i] <- cur_turn - as.numeric(deck[i,5])
          } else if(can_play[i]==1){
            delay[i] = 0 
          }
        }
      }
    }
    ##Goes through each card that has not yet been marked playable, checks if it is now playable, and updates the delay variable if appropriate. Same logic as above.
    
  
  }
}






##Visualizations thoughts:
##Consider "delay" - (turn played on) - (power cost)
##Delay by power cost (regardless of number of cards at each cost level)
##Delay by turn



