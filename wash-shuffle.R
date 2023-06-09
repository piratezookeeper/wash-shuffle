# Function to shift a vector by n steps.
shift = function(vec, n) {
  len = length(vec)
  new_vec = c(vec[(len - n):len], vec[1:(len - n - 1)])
  return(new_vec)
}


# Function to randomly swap two cards to each others' locations.
# Mike adding a few things:
# 1) Change swap verbiage to slice, more appropriate given the function
# 2) n is now tpois distributed with max = (k - 2)
# 3) i will not be determined by n. The idea is to have slice from swap_out always be generally centered
# which makes sure there will never be out of bounds or negative indices for slicing
# 4) The actual slicing from swap_out to swap_in becomes greatly simplified as a result of changes 2 and 3

slice_inner = function(vec1, vec2) { # 1)
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
  
  k = length(swap_out)
  n = tpois(lambda = 4, max = (k - 2)) # 2)
  # (k - 2) ensures entire swap_out vector will never be sliced as a whole
  i = ceiling((k - n) / 2) # 3)
  
  # Swap cards
  # 4)
  slice = swap_out[i:(i + n)]
  swap_in = c(swap_in[1:i],
              slice,
              swap_in[(i + 1):length(swap_in)])
  swap_out = c(swap_out[1:i],
               swap_out[(i + n + 1):k])
    
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

# Mike adding a few things:
# 1) Function name change to be consistent with slice_inner
# 2) Changes made consistent with those also made in slice_inner

slice_outer = function(vec1, vec2) { # 1)
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
  
  # 2)
  k = length(swap_out)
  n = tpois(lambda = 4, max = (k - 2))
  # (k - 2) ensures entire swap_out vector will never be sliced as a whole
  i = ceiling((k - n) / 2)
  
  # Swap cards
  slice = swap_out[i:(i + n)]
  swap_in = c(swap_in[1:i],
              slice,
              swap_in[(i + 1):length(swap_in)])
  swap_out = c(swap_out[1:i],
               swap_out[(i + n + 1):k])
  
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

# Wrapper function for wash shuffle
# Peter adding a few things:
# 1) make it a function of the deck, and number of cycles
# 2) add randomness to initial vectors 
# 3) n = tpois(3) -- max defaults to 52 but I think we need max to be the length of left/right
# 4) I think we need a return(deck) at the end or else it doesn't do anything
# 5) added randomness to inner/outer

# Mike adding a few things
# 1) Update function names

wash = function(deck, cycles) {
  
  # (2) This will make 4 segments that will be the same size on average, but adds randomness
  cut1 <- rbinom(n=1, size=52, prob=0.25)
  cut2 <- rbinom(n=1, size=(52-cut1), prob=(1/3)) + cut1
  cut3 <- rbinom(n=1, size=(52-cut2), prob=0.5) + cut2
  left = deck[c(1:cut1, rev((cut2+1):cut3))]
  right = deck[c(rev((cut3+1):52), (cut1+1):cut2)]
  
  
  for (i in 1:cycles) {
    n = tpois(3, max=length(left)) # (3) Random length of shift
    left = shift(left, n)
    
    for (j in 1:tpois(1.5, 5)) {
      outer_inner = sample(0:1, 1, prob=c(0.3, 0.7)) # making inner more likely
      
      if (outer_inner == 1) {
        slice_inner(left, right) # 1)
      }
      else {
        slice_outer(left, right) # 1)
      }
    }
    
    n = tpois(3, max=length(right)) # Random length of shift
    right = shift(right, n)
    
    for (j in 1:tpois(1.5, 5)) {
      outer_inner = sample(0:1, 1, prob=c(0.3, 0.7))
      
      if (outer_inner == 1) {
        slice_inner(left, right) # 1)
      }
      else {
        slice_outer(left, right) # 1)
      }
    }
  }
  
  deck = gather(left, right)
  return(deck) # (4) 
}
