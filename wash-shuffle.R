# Function to shift a vector by n steps.
shift = function(vec, n) {
  len = length(vec)
  new_vec = c(vec[(len - n):len], vec[1:(len - n - 1)])
  return(new_vec)
}


# Function to randomly swap two cards to each others' locations.
swap_inner = function(vec1, vec2) {
  # Slice vectors into only inner columns
  vec1_inner = vec1[ceiling(length(vec1) / 2):length(vec1)]
  vec2_inner = vec2[ceiling(length(vec2) / 2):length(vec2)]
  
  len1 = length(vec1_inner)
  len2 = length(vec2_inner)
  
  # Probability of the swap coming from vec1 to vec2
  p = len1 / (len1 + len2)
  condition = sample(0:1, 1, prob = c(p, (1 - p)))
  # Choose which vector to swap from
  if (condition == 1) {
    swap_out = vec1_inner
    swap_in = vec2_inner
  }
  else {
    swap_out = vec2_inner
    swap_in = vec1_inner
  }
  
  # Inner swap card index will be binomial distributed
  # Ensure index will never be first or last element
  i = rbinom(1, (length(swap_out) - 1), 0.5) + 1
  
  # Choose group size (1 to 6 cards)
  # Use truncated Poisson for distribution
  n = rbinom(1, 6, 0.5)
  #n = tpois(4)
  
  # Code if-else for if n=0 then break
  # I cannot get this to work for tpois because sometimes n==0
  # The same problem will persist from swap_outer function as well
  if (n == 0) {
    return()
  }
  else {
    # Swap cards
    if ((i + n) > length(swap_out)) {
      # Then card group is [(i-n):i]
      swap_in = c(swap_in[1:(i - n - 1)],
                  swap_out[(i - n):i],
                  swap_in[(i - n):length(swap_in)])
      swap_out = c(swap_out[1:(i - n - 1)],
                   swap_out[(i + 1):length(swap_out)])
    }
    else {
      # Then card group is [i:n]
      swap_in = c(swap_in[1:(i - 1)],
                  swap_out[i:(i + n)],
                  swap_in[i:length(swap_in)])
      swap_out = c(swap_out[1:(i - 1)],
                   swap_out[(i + n + 1):length(swap_out)])
    }
    
    # Join vector halves back together
    if (condition == 1) {
      # Inner swap was left to right
      vec1_new = c(vec1[1:(ceiling(length(vec1) / 2) - 1)], swap_out)
      vec2_new = c(vec2[1:(ceiling(length(vec2) / 2) - 1)], swap_in)
    }
    else {
      # Inner swap was right to left
      vec1_new = c(vec1[1:(ceiling(length(vec1) / 2) - 1)], swap_in)
      vec2_new = c(vec2[1:(ceiling(length(vec2) / 2) - 1)], swap_out)
    }
    
    left = vec1_new
    right = vec2_new
  }
}


swap_outer = function(vec1, vec2) {
  # Slice vectors into columns
  vec1_outer = vec1[1:(ceiling(length(vec1) / 2) - 1)]
  vec2_inner = vec2[ceiling(length(vec2) / 2):length(vec2)]
  vec1_inner = vec1[ceiling(length(vec1) / 2):length(vec1)]
  vec2_outer = vec2[1:(ceiling(length(vec2) / 2) - 1)]
  
  len1_outer = length(vec1_outer)
  len2_inner = length(vec2_inner)
  len1_inner = length(vec1_inner)
  len2_outer = length(vec2_inner)
  
  # Probability of the swap coming from vec1 to vec2
  p = len1_outer / (len1_outer + len2_outer)
  condition = sample(0:1, 1, prob = c(p, (1 - p)))
  
  # Choose which vector to swap from
  if (condition == 1) {
    out_to_in = sample(0:1, 1)
    if (out_to_in == 1) {
      swap_out = vec1_outer
      swap_in = vec2_inner
    }
    else if (out_to_in == 0) {
      swap_out = vec2_inner
      swap_in = vec1_outer
    }
  }
  else {
    out_to_in = sample(0:1, 1)
    if (out_to_in == 1) {
      swap_out = vec2_outer
      swap_in = vec1_inner
    }
    else if (out_to_in == 0) {
      swap_out = vec1_inner
      swap_in = vec2_outer
    }
  }
  
  # Inner swap card index will be binomial distributed
  # Ensure index will never be first or last element
  i = rbinom(1, (length(swap_out) - 1), 0.5) + 1
  
  # Choose group size (1 to 6 cards)
  # Use truncated Poisson for distribution
  n = rbinom(1, 2, 0.5)
  
  # Code if-else for if n=0 then break
  
  # Swap cards
  if ((i + n) > length(swap_out)) {
    # Then card group is [(i-n):i]
    swap_in = c(swap_in[1:(i - n - 1)],
                swap_out[(i - n):i],
                swap_in[(i - n):length(swap_in)])
    swap_out = c(swap_out[1:(i - n - 1)],
                 swap_out[(i + 1):length(swap_out)])
  }
  else {
    # Then card group is [i:n]
    swap_in = c(swap_in[1:(i - 1)],
                swap_out[i:(i + n)],
                swap_in[i:length(swap_in)])
    swap_out = c(swap_out[1:(i - 1)],
                 swap_out[(i + n + 1):length(swap_out)])
  }
  
  # Join vector halves back together
  if (condition == 1) {
    # Outer swap on left side
    if (out_to_in == 1) {
      left = c(swap_out, vec1_inner)
      right = c(vec2_outer, swap_in)
    }
    else if (out_to_in == 0) {
      left = c(swap_in, vec1_inner)
      right = c(vec2_outer, swap_out)
    }
  }
  else {
    # Outer swap on right side
    if (out_to_in == 1) {
      left = c(vec1_outer, swap_in)
      right = c(swap_out, vec2_inner)
    }
    else if (out_to_in == 0) {
      left = c(vec1_outer, swap_out)
      right = c(swap_in, vec2_inner)
    }
  }
}


# Function to gather cards back into a single deck.
gather = function(vec1, vec2) {
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
    
    if (n == 0) {
      next
    }
    else if (len > 0) {
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
    else if (columns[[i]] == 0) {
      next
    }
  }
  
  return(deck)
}


# Truncated poisson random variable
tpois = function(lambda, max = 52) {
  vec = c(rep(0, max))
  for (x in 1:max) {
    vec[x] = ((lambda ^ x) * exp(-lambda)) / factorial(x)
  }
  denom = sum(vec)
  vec = vec / denom
  out = sample(1:max, size = 1, prob = vec)
  return(out)
}


# riffle shuffle
riffle = function(deck) {
len = length(deck)
split = rbinom(n = 1, size = len, prob = 0.5)
A = deck[1:split]
B = deck[(split + 1):len]
deck_new = NULL # Empty deck

for (i in 1:len) {
  # Calculate probability of dropping from A
  p = length(A) / (length(A) + length(B))
  
  # Now simulate a single binomial
  # If true, drop from portion A
  if (rbinom(1, 1, prob = p)) {
    deck_new = c(deck_new, A[length(A)])
    A = A[-length(A)]
  }
  else {
    deck_new = c(deck_new, B[length(B)])
    B = B[-length(B)]
  }
}
return(deck_new)
}

# Box shuffle
box = function(deck) {
  A = rbinom(n = 1, size = length(deck), prob = 1 / 4)
  B = rbinom(n = 1, size = (length(deck) - A), prob = 1 / 3)
  C = rbinom(n = 1, size = (length(deck) - A - B), prob = 1 / 2)
  D = length(deck) - C - B - A
  box = c(tail(deck, D), 
          deck[(A + B + 1):(A + B + C)], deck[(A + 1):(A + B)], deck[1:A])
  return(box)
}


# Cut shuffle
cut = function(deck) {
  split = rbinom(1, 52, 0.5)
  A = deck[1:split]
  B = deck[(split + 1):52]
  new_deck = c(B, A)
  return(new_deck)
}

# Casino shuffle: riffle, riffle, strip, riffle, cut, deal
casino_shuffle = function(deck) {
  deck = riffle(deck)
  deck = riffle(deck)
  deck = box(deck)
  deck = riffle(deck)
  deck = cut(deck)
  return(deck)
}
