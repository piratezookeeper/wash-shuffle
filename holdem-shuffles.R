# These functions are heavily borrowed from:
# Merz, David W., and Peter B. Chi. "Knowing When to Fold'em: A Monte Carlo Exploration of Card 
# Shuffling and How Poker Players Can Gain an Advantage." UNLV Gaming Research & Review Journal 
# 26.1 (2022): 7.

# The following functions create the necessary data for wash-shuffle-plots.R

# as given by (Chi, Merz, 2022)
riffle_shuffle = function(deck) {
  cut_point = rbinom(n = 1,
                     size = length(deck),
                     prob = 1 / 2)
  left = deck[1:cut_point]
  right = deck[(cut_point + 1):length(deck)]
  riffled_deck = c()
  for (i in 1:length(deck)) {
    A = length(left)
    B = length(right)
    random = runif(1)
    odds_left_heap_drop = A / (A + B)
    if (random < odds_left_heap_drop) {
      card_added = left[A]
      left = left[-A]
      A = A - 1
    } else {
      card_added = right[B]
      right = right[-B]
      B = B - 1
    }
    riffled_deck = c(card_added, riffled_deck)
  }
  return(riffled_deck)
}

many_riffle_shuffles = function(deck, iter) {
  for (i in 1:iter) {
    deck = riffle_shuffle(deck)
  }
  return(deck)
}

# as given by (Chi, Merz, 2022)
strip_shuffle = function(deck) {
  A = rbinom(n = 1,
             size = length(deck),
             prob = 1 / 4)
  B = rbinom(n = 1,
             size = (length(deck) - A),
             prob = 1 / 3)
  C = rbinom(n = 1,
             size = (length(deck) - A - B),
             prob = 1 / 2)
  D = length(deck) - C - B - A
  strip = c(tail(deck, D), deck[(A + B + 1):(A + B + C)], deck[(A + 1):(A +
                                                                          B)], deck[1:A])
  return(strip)
}

# as given by (Chi, Merz, 2022)
deck_cut = function(deck) {
  cut_pos = rbinom(n = 1,
                   size = length(deck),
                   prob = 1 / 2)
  cut_deck = c(deck[(cut_pos + 1):length(deck)], deck[1:cut_pos])
  return(cut_deck)
}

# as given by (Chi, Merz, 2022)
holdem_shuffle = function(deck) {
  shuffled_deck = many_riffle_shuffles(1:52, 2)
  shuffled_deck = strip_shuffle(shuffled_deck)
  shuffled_deck = many_riffle_shuffles(shuffled_deck, 1)
  shuffled_deck = deck_cut(shuffled_deck)
  return(shuffled_deck)
}

# as given by (Chi, Merz, 2022)
deck_cut_at_prob = function(deck, cut_odds) {
  z = cut_odds
  cards_cut = rbinom(n = 1,
                     size = length(deck),
                     prob = z)
  
  if (cards_cut == length(deck)) {
    return(deck)
  }
  deck_1 = c()
  for (x in 1:length(deck)) {
    if (x <= cards_cut) {
      deck_1 = append(deck_1, deck[x])
    }
  }
  return(deck_1)
}

# as given by (Chi, Merz, 2022)
rest_of_deck = function(deck, deck1) {
  deck_2 = c()
  if (length(deck1) == length(deck)) {
    return(deck_2)
  }
  for (x in (length(deck1) + 1):length(deck)) {
    deck_2 = append(deck_2, deck[x])
  }
  return(deck_2)
}

better_shuffle = function(deck) {
  shuffled_deck = many_riffle_shuffles(1:52, 7)
  return(shuffled_deck)
}

# as given by (Chi, Merz, 2022)
deal_holdem = function(numpl, deck, shuffle) {
  shuffled_deck = shuffle
  
  hands = matrix(shuffled_deck[1:(numpl * 2)], ncol = 2)
  
  hands = lapply(seq_len(nrow(hands)), function(i) hands[i,])
  board = c(shuffled_deck[(numpl * 2 + 2):(numpl * 2 + 4)], 
            shuffled_deck[numpl * 2 + 6], 
            shuffled_deck[numpl * 2 + 8])
  hands = append(hands, list("board" = board))
  burn_cards = c(shuffled_deck[(numpl * 2 + 1)], 
                 shuffled_deck[numpl * 2 + 5], 
                 shuffled_deck[numpl * 2 + 7])
  hands = append(hands, list("burn cards" = burn_cards))
  return(hands)
}

# as given by (Chi, Merz, 2022)
deal_holdem_odds = function(sims, numpl, shuffle_type) {
  flop = integer(52)
  turn = integer(52)
  river = integer(52)
  burned = integer(52)
  empty_vect = integer(52)
  prob_of_card = list()
  for (x in 1:numpl) {
    prob_of_card = append(prob_of_card, list(empty_vect))
  }
  for (x in 1:sims) {
    shuffle = shuffle_type(1:52)
    dealt_hand = deal_holdem(numpl, 1:52, shuffle)
    z = 0
    for (hand in dealt_hand) {
      z = z + 1
      
      if (length(hand) == 2) {
        card1 = hand[1]
        card2 = hand[2]
        prob_of_card[[z]][card1] = prob_of_card[[z]][card1] + 1
        prob_of_card[[z]][card2] = prob_of_card[[z]][card2] + 1
        
      } else if (length(hand) == 5) {
        #adding one to the specific position of the flop, turn, and river for each time
        #that number card comes up. I.e. if the board flops a 29, 1 is added to
        #the 29th position of the vector "flop"
        flop[hand[1]] = flop[hand[1]] + 1
        flop[hand[2]] = flop[hand[2]] + 1
        flop[hand[3]] = flop[hand[3]] + 1
        turn[hand[4]] = turn[hand[4]] + 1
        river[hand[5]] = river[hand[5]] + 1
        prob_of_card[[z]] = list(flop, turn, river)
        
      } else{
        #same as board cards, except
        burned[hand[1]] = burned[hand[1]] + 1
        burned[hand[2]] = burned[hand[2]] + 1
        burned[hand[3]] = burned[hand[3]] + 1
        prob_of_card[[z]] = list(burned)
      }
    }
  }
  #converting the counts to percentages
  for (i in 1:numpl) {
    vec = unlist(prob_of_card[i])
    vec = vec / (sims) * 100
    prob_of_card[i] = list(vec)
  }
  board = prob_of_card[[numpl + 1]]
  for (i in 1:3) {
    z = unlist(board[i])
    if (i == 1) {
      z = z / (sum(z)) * 100 * 3
      prob_of_card[[1 + numpl]][i] = list(z)
    } else{
      z = z / (sum(z)) * 100
      prob_of_card[[1 + numpl]][i] = list(z)
    }
    
  }
  
  burn = unlist(prob_of_card[numpl + 2])
  burn = burn / sum(burn) * 100 * 3
  prob_of_card[[2 + numpl]] = list("burn cards" = burn)
  names(prob_of_card[[numpl + 1]]) = c("flop", "turn", "river")
  
  return(prob_of_card)
}


# parent function to combine wash and casino functions from wash-shuffle.R
wash_and_casino = function(deck) {
  deck = wash(deck, 3)
  deck = casino_shuffle(deck)
  deck = array(deck)
  return(deck)
}

# as given by (Chi, Merz, 2022)
card_neighbors_counts = function(sims, shuffle_type, cards) {
  clumps_odds = list()
  bef =  matrix(0, nrow = 52, ncol = 52)
  before1 = list()
  for (i in 1:cards) {
    before1 = append(before1, list(bef))
  }
  for (i in 1:sims) {
    shuf = shuffle_type(1:52)
    
    for (x in 1:52) {
      for (z in 1:cards) {
        pos = match(x, shuf)
        if ((pos - z) > 0) {
          before = shuf[pos - z]
          before1[[z]][x, before] = before1[[z]][x, before] + 1
        }
      }
    }
  }
  list_of_maxs = list()
  for (i in 1:cards) {
    vals = c()
    for (x in 2:52) {
      before_val = before1[[i]][x, (x - 1)]
      vals = append(vals, before_val)
    }
    list_of_maxs = append(list_of_maxs, list(vals))
  }
  #names(list_of_maxs) = c(1:cards)
  return(list_of_maxs)
}

# as given by (Chi, Merz, 2022)
alt_shuffle5 = function(deck){
  shuffled_deck = many_riffle_shuffles(deck,5)
  shuffled_deck = strip_shuffle(shuffled_deck)
  shuffled_deck = deck_cut(shuffled_deck)
  return(shuffled_deck)
}

# as given by (Chi, Merz, 2022)
alt3_shuffle1 = function(deck){
  shuffled_deck = many_riffle_shuffles(deck,2)
  shuffled_deck = strip_shuffle(shuffled_deck)
  shuffled_deck = many_riffle_shuffles(shuffled_deck,1)
  shuffled_deck = deck_cut(shuffled_deck)
  return(shuffled_deck)
}

# as given by (Chi, Merz, 2022)
alt3_shuffle2 = function(deck){
  shuffled_deck = many_riffle_shuffles(deck,1)
  shuffled_deck = strip_shuffle(shuffled_deck)
  shuffled_deck = many_riffle_shuffles(shuffled_deck,2)
  shuffled_deck = deck_cut(shuffled_deck)
  return(shuffled_deck)
}

# as given by (Chi, Merz, 2022)
alt3_shuffle3 = function(deck){
  shuffled_deck = many_riffle_shuffles(deck,3)
  shuffled_deck = strip_shuffle(shuffled_deck)
  shuffled_deck = deck_cut(shuffled_deck)
  return(shuffled_deck)
}

# as given by (Chi, Merz, 2022)
alt3_shuffle4 = function(deck){
  shuffled_deck = strip_shuffle(deck)
  shuffled_deck = many_riffle_shuffles(shuffled_deck,3)
  shuffled_deck = deck_cut(shuffled_deck)
  return(shuffled_deck)
}

# as given by (Chi, Merz, 2022)
alt3_shuffle3 = function(deck){
  shuffled_deck = many_riffle_shuffles(deck,3)
  shuffled_deck = strip_shuffle(shuffled_deck)
  shuffled_deck = deck_cut(shuffled_deck)
  return(shuffled_deck)
}

# as given by (Chi, Merz, 2022)
alt4_shuffle5 = function(deck){
  shuffled_deck = many_riffle_shuffles(deck,4)
  shuffled_deck = strip_shuffle(shuffled_deck)
  
  shuffled_deck = deck_cut(shuffled_deck)
  return(shuffled_deck)
}

# as given by (Chi, Merz, 2022)
alt_shuffle5 = function(deck){
  shuffled_deck = many_riffle_shuffles(deck,5)
  shuffled_deck = strip_shuffle(shuffled_deck)
  shuffled_deck = deck_cut(shuffled_deck)
  return(shuffled_deck)
}

# as given by (Chi, Merz, 2022)
average_neighbor = function(sims, shuffle, cards) {
  maxs = card_neighbors_counts(sims, shuffle, cards)
  before_after_vals = c()
  for (i in 1:cards) {
    val = (sum(unlist(maxs[i])) * 100) / (sims * 51)
    before_after_vals = append(before_after_vals, val)
  }
  names(before_after_vals) = c(1:cards)
  return(before_after_vals)
}

# run average neighbors for 100000 iterations each
# generate average neighbors for all 52 card slots
iterations = 100000
cards = 52

# takes like 30 minutes
tic()
hold = average_neighbor(iterations, holdem_shuffle, cards)
wash_neighbors = average_neighbor(iterations, wash_and_casino, cards)
riffle = average_neighbor(iterations, better_shuffle, cards)
alt3_1 = average_neighbor(iterations,alt3_shuffle1,cards)
alt3_2 = average_neighbor(iterations,alt3_shuffle2,cards)
alt3_3 = average_neighbor(iterations,alt3_shuffle3,cards)
alt3_4 = average_neighbor(iterations,alt3_shuffle4,cards)
alt5 = average_neighbor(iterations, alt_shuffle5, cards)
alt3_3 = average_neighbor(iterations,alt3_shuffle3,cards)
alt4_5 = average_neighbor(iterations,alt4_shuffle5,cards)
alt5 = average_neighbor(iterations,alt_shuffle5,cards)
toc()
