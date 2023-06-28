# Initialize left/right vector structure for deck
init_deck = function(deck) {
  # Random partitions for columns to for left/right vectors
  cut1 <- rbinom(n = 1, size = 52, prob = 0.25)
  cut2 <- rbinom(n = 1,
                 size = (52 - cut1),
                 prob = (1 / 3)) + cut1
  cut3 <- rbinom(n = 1,
                 size = (52 - cut2),
                 prob = 0.5) + cut2
  left = deck[c(rev(1:cut1), (cut2 + 1):cut3)]
  right = deck[c(rev((cut3 + 1):52), (cut1 + 1):cut2)]
  
  # Deck will now be a list of left and right vectors
  deck = list(left = left, right = right)
  return(deck)
}

# Perform circular roll by n values on a single vector while deck in vector form
roll = function(vec, n) {
  # Circular roll on vector by n steps
  len = length(vec)
  if (len == n) {
    new_vec = vec # If rolling entire vector, then output unchanged
  }
  else {
    new_vec = c(vec[(n + 1):len],vec[1:n])
  }
  return(new_vec)
}

# Return truncated Poisson random variable with mean = lambda
tpois = function(lambda, max = 52) {
  # Returns truncated Poisson random variable with finite max value
  vec = c(rep(0, max))
  for (x in 1:max) {
    vec[x] = ((lambda ^ x) * exp(-lambda)) / factorial(x)
  }
  denom = sum(vec)
  vec = vec / denom
  out = sample(1:max, size = 1, prob = vec)
  return(out)
}

# Convert deck structure to four columns form
# Must be used prior to slicing
to_col = function(deck) {
  if (length(deck) == 4) {
    print("Deck already in column form.")
    return()
  }
  
  left = deck$left
  right = deck$right
  left_mid = ceiling(length(left) / 2)
  right_mid = ceiling(length(right) / 2)
  
  # Generate column form
  col1 = rev(left[1:left_mid])
  col2 = rev(right[1:right_mid])
  col3 = left[(left_mid + 1):length(left)]
  col4 = right[(right_mid + 1):length(right)]
  
  new_deck = list(col1 = col1, col2 = col2, col3 = col3, col4 = col4)
  
  # Check for missing or extra values
  sanity_check = sum(new_deck$col1, new_deck$col2, 
                     new_deck$col3, new_deck$col4)
  if (sanity_check != 1378) {
    print("Deck integrity lost at to_col.")
    return()
  }
  else {
    return(new_deck)
  }
}

# Convert deck structure back to left/right vectors after slicing is complete
to_vec = function(deck) {
  if (length(deck) == 2) {
    print("Deck already in vector form.")
    return()
  }
  
  left = c(rev(deck$col1), deck$col3)
  right = c(rev(deck$col2), deck$col4)
  new_deck = list(left = left, right = right)
  
  # Check for missing or extra values
  sanity_check = sum(new_deck$left, new_deck$right)
  if (sanity_check != 1378) {
    print("Deck integrity lost at to_vec.")
    return()
  }
  else {
    return(new_deck)
  }
}

# Generalized slice function to randomly slice group of cards from one column to another
# Rebalance arg used if a left or right vector is below the set minimum length
slice = function(donor, receiver, rebalance = FALSE) {
  sanity = sum(donor, receiver)
  
  # Define variables
  len_don = length(donor)
  len_rec = length(receiver)
  n = tpois(lambda = 4, max = (len_don - 2)) # Number of cards to donate
  i = ceiling((len_don - n) / 2) # Index at donor before n cards begin
  mid = ceiling(length(receiver) / 2) # Midpoint of receiver where slice inserts
  
  # Move cards
  if (rebalance == FALSE) {
    slice = donor[(i + 1):(i + n)]
    new_receiver = c(receiver[1:mid],
                     slice,
                     receiver[(mid + 1):len_rec])
    new_donor = c(donor[1:i],
                  donor[(i + n + 1):len_don])
  }
  else {
    slice = donor[(i + 1):(i + n)]
    new_receiver = c(receiver, slice)
    new_donor = c(donor[1:i],
                  donor[(i + n + 1):len_don])
  }
  out = list(donor = new_donor, receiver = new_receiver)
  
  # Check for missing or extra values
  sanity_check = sum(out$donor, out$receiver)
  if (sanity != sanity_check) {
    print("Deck integrity lost at slice.")
    return()
  }
  else {
    return(out)
  }
}

# Gather function is exact same from version 1
gather = function(deck) {
  # Inputs list of left and right vectors
  vec1 = deck[[1]]
  vec2 = deck[[2]]
  
  # Slice vectors into columns
  col1 = vec1[ceiling(length(vec1) / 2):length(vec1)]
  col2 = vec1[1:(ceiling(length(vec1) / 2) - 1)]
  col3 = vec2[ceiling(length(vec2) / 2):length(vec2)]
  col4 = vec2[1:(ceiling(length(vec2) / 2) - 1)]
  columns = list(col1, col2, col3, col4)
  
  # Need initial column lengths to generate n variable in while loop
  col_len = list(length(col1), length(col2), length(col3), length(col4))
  
  # Create an empty deck
  deck = c()
  counter = 52
  cols = 4
  
  while (counter != 0) {
    i = rbinom(1, (cols - 1), 0.5) + 1 # More likely to sample inner columns
    len = length(columns[[i]])
    n = rbinom(1, (col_len[[i]] - 1), 0.5) + 1
    
    if (len > n) {
      slice = tail(columns[[i]], n)
      deck = c(slice, deck)
      columns[[i]] = columns[[i]][1:(len - n)]
      counter = counter - n
    }
    else if (len == n) {
      slice = columns[[i]]
      deck = c(slice, deck)
      columns[i] = NULL
      counter = counter - n
      cols = cols - 1
    }
    else if (len < n) {
      slice = columns[[i]]
      deck = c(slice, deck)
      columns[i] = NULL
      counter = counter - len
      cols = cols - 1
    }
  }
  return(deck)
}

# This parent wash function does not yet work
# Currently working on building a wash function from the bottom up
wash = function(deck, cycles) {
  deck = init_deck(deck)
  
  min_cards = 5 # Minimum cards single vector may have
  in_out_dist = c(0.3, 0.7) # Inner shuffle occurs 70% of the time
  
  for (i in 1:cycles) {
    # Left hand movement
    n = tpois(3, max = length(deck$left))
    deck$left = roll(deck$left, n)
    len_left = length(deck$left)
    len_right = length(deck$right)
    
    slice_count1 = tpois(1.5, 5)
    
    for (j in 1:slice_count1) {
      # Pick which columns to slice
      deck = to_col(deck) # To column form
      if(len_left <= min_cards) {
        # Left vector is too short
        # Rebalance columns 1 and 2
        sliced = slice(deck$col2, deck$col1, rebalance = TRUE)
        deck$col1 = sliced$receiver
        deck$col2 = sliced$donor
        # Rebalance columns 3 and 4
        sliced = slice(deck$col4, deck$col3, rebalance = TRUE)
        deck$col3 = sliced$receiver
        deck$col4 = sliced$donor
      }
      else if (len_right <= min_cards) {
        # Right vector is too short
        # Rebalance columns 1 and 2
        sliced = slice(deck$col1, deck$col2, rebalance = TRUE)
        deck$col1 = sliced$donor
        deck$col2 = sliced$receiver
        # Rebalance columns 3 and 4
        sliced = slice(deck$col3, deck$col4, rebalance = TRUE)
        deck$col3 = sliced$donor
        deck$col4 = sliced$receiver
      }
      else {
        # Neither vector is too short
        inner = sample(c(0, 1), size = 1, prob = in_out_dist)
        if (inner == TRUE) {
          sliced = slice(deck$col3, deck$col2)
          deck$col2 = sliced$receiver
          deck$col3 = sliced$donor
        }
        else if (inner == FALSE) {
          sliced = slice(deck$col1, deck$col2)
          deck$col1 = sliced$donor
          deck$col2 = sliced$receiver
        }
      }
      deck = to_vec(deck) # Back to vector form
    }
    
    # Right hand movement
    n = tpois(3, max = length(deck$right))
    deck$right = roll(deck$right, n)
    len_left = length(deck$left)
    len_right = length(deck$right)
    
    slice_count2 = tpois(1.5, 5)
    
    for (j in 1:slice_count2) {
      # Pick which columns to slice
      deck = to_col(deck) # To column form
      if(len_left <= min_cards) {
        # Left vector is too short
        # Rebalance columns 1 and 2
        sliced = slice(deck$col2, deck$col1, rebalance = TRUE)
        deck$col1 = sliced$receiver
        deck$col2 = sliced$donor
        # Rebalance columns 3 and 4
        sliced = slice(deck$col4, deck$col3, rebalance = TRUE)
        deck$col3 = sliced$receiver
        deck$col4 = sliced$donor
      }
      else if (len_right <= min_cards) {
        # Right vector is too short
        # Rebalance columns 1 and 2
        sliced = slice(deck$col1, deck$col2, rebalance = TRUE)
        deck$col1 = sliced$donor
        deck$col2 = sliced$receiver
        # Rebalance columns 3 and 4
        sliced = slice(deck$col3, deck$col4, rebalance = TRUE)
        deck$col3 = sliced$donor
        deck$col4 = sliced$receiver
      }
      else {
        # Neither vector is too short
        inner = sample(c(0, 1), size = 1, prob = in_out_dist)
        if (inner == TRUE) {
          sliced = slice(deck$col2, deck$col3)
          deck$col2 = sliced$donor
          deck$col3 = sliced$receiver
        }
        else if (inner == FALSE) {
          sliced = slice(deck$col4, deck$col3)
          deck$col3 = sliced$receiver
          deck$col4 = sliced$donor
        }
      }
      deck = to_vec(deck)
    }
  }
  
  deck = gather(deck)
  return(deck)
}
