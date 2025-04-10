# Shuffle Process A

# Function to place a card in a pile at any spot if cards are already there
place_card <- function(spot, new_card){
  if(length(spot) == 1){
    return(c(spot, new_card)[sample(2)])  # return the 2 cards in random order
  } else{
    placement <- sample(length(spot)+1, size=1)
    if(placement == 1){
      return(c(new_card, spot))
    } else if(placement == (length(spot) + 1)) {
      return(c(spot, new_card))
    } else{
      return(c(spot[1:(placement-1)], new_card, spot[placement:length(spot)]))
    }
  }
}

# Function to move a card to a new location
make_move <- function(deck){
  # pick card, not location
  to_move <- sample(52, size=1)
  
  # now get the card's location
  ind <- which(sapply(deck, function(e) is.element(to_move, e)))

  # determine if and where it will move to -- 4/8/25 still in progress here
  move <- sample(c(-1, 0, 1), size=1, prob = c(0.25, 0.5, 0.25))
  
  # Do nothing if it doesn't move, otherwise move it (not done yet)
  if(move !=0){
    if(ind == 1 & move == -1){
      new_ind <- 52
    } else if(ind == 52 & move == 1){
      new_ind <- 1
    } else {
      new_ind <- ind + move
    }

    deck[[new_ind]] <- place_card(deck[[new_ind]], to_move)
    deck[[ind]] <- deck[[ind]][-which(deck[[ind]]==to_move)]
    # need to remove card from where it came, too
  }  
  return(deck)
}




# Wrapper function
# its input should be a deck in vector format
shuffleA <- function(deck, iterations){
  deck <- as.list(deck)
  for(i in 1:iterations){
    deck <- make_move(deck)
  }
  return(unlist(deck))
}

set.seed(12)
deck <- 1:52
shuffleA(deck,1000)


# Shuffle Process B

# its input should be a deck in vector format
# Following Remark 26: move cards one at a time, inserting them at a random position
# in the pile at their destination

shuffleB <- function(deck, p, t){
  n <- length(deck)
  deck <- as.list(deck)
  for(i in 1:t){
    # sweep to the right
    moves <- (rgeom(n, p) + 1) # rgeom in R is failures; paper defines it as trials
    for(j in 1:n){  
      ind <- which(sapply(deck, function(e) is.element(j, e))) # get the location of the card to move
      new_ind <- ind + moves[j] # go to the right
      if(new_ind > 52){
        new_ind <- 52 # do not go past 52
      } 
      
      if(new_ind != ind){
        deck[[new_ind]] <- place_card(deck[[new_ind]], j) # Remark 26: insert at random position
        deck[[ind]] <- deck[[ind]][-which(deck[[ind]]==j)] # remove it from its previous position
      }
    }
    
    # sweep to the left
    moves <- (rgeom(n, p) + 1) # rgeom in R is failures; paper defines it as trials
    for(j in 1:n){  
      ind <- which(sapply(deck, function(e) is.element(j, e)))
      new_ind <- ind - moves[j] # go to the left
      if(new_ind < 1){
        new_ind <- 1 # do not go past 1
      }
      
      if(new_ind != ind){
        deck[[new_ind]] <- place_card(deck[[new_ind]], j)
        deck[[ind]] <- deck[[ind]][-which(deck[[ind]]==j)]
      }
    }  

  }
  return(unlist(deck))
}
