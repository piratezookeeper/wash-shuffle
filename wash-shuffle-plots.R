# These functions are heavily borrowed from:
# Merz, David W., and Peter B. Chi. "Knowing When to Fold'em: A Monte Carlo Exploration of Card 
# Shuffling and How Poker Players Can Gain an Advantage." UNLV Gaming Research & Review Journal 
# 26.1 (2022): 7.

# The following is code for generating all plots

neighbor_bar_chart = function(sims, shuffle1, shuffle2, shuffle3, cards) {
  holdem = hold
  nums = c()
  for (i in 1:cards) {
    nums = append(nums, unlist(holdem[i]))
    nums = append(nums, unlist(riffle[i]))
    nums = append(nums, unlist(wash_neighbors[i]))
  }
  
  data <- data.frame(
    values = nums,
    group = rep(c(1:cards),
                each = 3),
    subgroup = c("Casino shuffle", "7 riffle shuffles", "Wash then casino")
  )
  
  ggplot(data,
         aes(
           x = group,
           y = values,
           fill = reorder(subgroup, 1:((cards) * 3))
         )) +
    geom_bar(stat = "identity",
             position = "dodge") +
    labs(
      title = "Probability of neighbor cards being n cards apart",
      x = "Cards apart",
      y = "Probability",
      fill = "shuffle"
    ) +
    geom_abline(
      intercept = (100 / 51),
      slope = (-100 / (52 * 51)),
      color = "black",
      linetype = "dashed"
    ) +
    geom_text(
      aes(label = round(values, 2)),
      vjust = -0.2,
      size = 7,
      position = position_dodge(0.9)
    ) +
    theme(plot.title = element_text(hjust = .5),
          text = element_text(size = 20))
}

ci_graph = function(sims, numpl, shuffle, name) {
  graphs = c()
  
  n = round(sqrt(numpl + 4))
  m = n + 1
  par(mfrow = c(n, m))
  shuffle = deal_holdem_odds(sims, numpl, shuffle)
  
  #odds of first five cards
  five_odds = list()
  for (i in 1:numpl) {
    odds = unlist(shuffle[[i]][1:5])
    five_odds = append(five_odds, list(odds))
  }
  board = shuffle[[numpl + 1]]
  for (i in 1:3) {
    odds = unlist(board[[i]][1:5])
    five_odds = append(five_odds, list(odds))
  }
  burn = unlist(shuffle[numpl + 2])
  odds = unlist(burn[1:5])
  names(odds) = c()
  five_odds = append(five_odds, list(odds))
  
  #mean values
  val = c()
  for (i in 1:(numpl + 4)) {
    if (i <= numpl) {
      val = append(val, 200 / 52)
    } else if (i == numpl + 1 | i == numpl + 4) {
      val = append(val, 300 / 52)
    } else{
      val = append(val, 100 / 52)
    }
  }
  
  cards = 1:5
  cards1 <- paste("card ", cards, sep = "")
  
  #iterating through each set of odds
  for (i in 1:(numpl + 4)) {
    if (i <= numpl) {
      x_axis <- paste("Player ", i, sep = "")
    } else if (i == numpl + 1) {
      x_axis = "Flop"
    } else if (i == numpl + 2) {
      x_axis = "Turn"
    } else if (i == numpl + 3) {
      x_axis = "River"
    } else if (i == numpl + 4) {
      x_axis = "Burn Cards"
    }
    odds = unlist(five_odds[i])
    #confidence intervals for specfic set of odds
    CI_uppers = c()
    CI_lowers = c()
    for (x in 1:5) {
      p = odds[x] / 100
      CI_upper = p + 1.96 * (sqrt((p * (1 - p)) / sims))
      CI_lower = p - 1.96 * (sqrt((p * (1 - p)) / sims))
      CI_uppers = append(CI_uppers, CI_upper)
      CI_lowers = append(CI_lowers, CI_lower)
    }
    
    data <- data.frame(
      group = rep(c(cards1)),
      F = unlist(five_odds[i]),
      L = CI_lowers * 100,
      U = CI_uppers * 100
    )
    
    require(ggplot2)
    p = ggplot(data, aes(x = group, y = F)) +
      geom_point(size = 1) +
      labs(title = name,
           x = x_axis, y = "Ci") +
      geom_errorbar(aes(
        ymax = U,
        ymin = L,
        width = .5
      )) +
      ylim(0, 2 * val[i]) +
      geom_hline(yintercept = val[i],
                 linetype = "dashed",
                 color = "red") +
      theme(plot.title = element_text(hjust = .5))
    graphs[[i]] = p
  }
  ggarrange(
    graphs[[1]],
    graphs[[2]],
    graphs[[3]],
    graphs[[4]],
    graphs[[5]],
    graphs[[6]],
    graphs[[7]],
    graphs[[8]],
    graphs[[9]],
    graphs[[10]],
    ncol = 3,
    nrow = 4
  )
}

ci_comparison_graph = function(sims, numpl, shuffle, name, shuffle2) {
  graphs = c()
  shuffle = deal_holdem_odds(sims, numpl, shuffle)
  shuffle2 = deal_holdem_odds(sims, numpl, shuffle2)
  #odds of first five cards
  five_odds = list()
  five_odds2 = list()
  for (i in 1:numpl) {
    odds = unlist(shuffle[[i]][-47:-6])
    odds2 = unlist(shuffle2[[i]][-47:-6])
    five_odds = append(five_odds, list(odds))
    five_odds2 = append(five_odds2, list(odds2))
  }
  board = shuffle[[numpl + 1]]
  board2 = shuffle2[[numpl + 1]]
  for (i in 1:3) {
    odds = unlist(board[[i]][-47:-6])
    five_odds = append(five_odds, list(odds))
    odds2 = unlist(board2[[i]][-47:-6])
    five_odds2 = append(five_odds2, list(odds2))
  }
  burn = unlist(shuffle[numpl + 2])
  odds = unlist(burn[-47:-6])
  names(odds) = c()
  five_odds = append(five_odds, list(odds))
  burn2 = unlist(shuffle2[numpl + 2])
  odds2 = unlist(burn2[-47:-6])
  names(odds2) = c()
  five_odds2 = append(five_odds2, list(odds2))
  
  #mean values
  val = c()
  for (i in 1:(numpl + 4)) {
    if (i <= numpl) {
      val = append(val, 200 / 52)
    } else if (i == numpl + 1 | i == numpl + 4) {
      val = append(val, 300 / 52)
    } else{
      val = append(val, 100 / 52)
    }
  }
  
  cards = c(1:5, 48:52)
  cards1 <- paste("card ", cards, sep = "")
  
  list_odds = list()
  for (x in 1:(numpl + 4)) {
    odds3 = c()
    for (i in 1:10) {
      odds3 = append(odds3, five_odds[[x]][i])
      odds3 = append(odds3, five_odds2[[x]][i])
      if (i == 10) {
        list_odds = append(list_odds, list(odds3))
      }
    }
  }
  
  #iterating through each set of odds
  for (i in 1:(numpl + 4)) {
    if (i <= numpl) {
      x_axis <- paste("Player ", i, sep = "")
    } else if (i == numpl + 1) {
      x_axis = "Flop"
    } else if (i == numpl + 2) {
      x_axis = "Turn"
    } else if (i == numpl + 3) {
      x_axis = "River"
    } else if (i == numpl + 4) {
      x_axis = "Burn Cards"
    }
    odds = unlist(list_odds[i])
    #confidence intervals for specfic set of odds
    CI_uppers = c()
    CI_lowers = c()
    for (x in 1:20) {
      p = odds[x] / 100
      CI_upper = p + 1.96 * (sqrt((p * (1 - p)) / sims))
      CI_lower = p - 1.96 * (sqrt((p * (1 - p)) / sims))
      CI_uppers = append(CI_uppers, CI_upper)
      CI_lowers = append(CI_lowers, CI_lower)
    }
    
    data <- data.frame(
      group = rep(c(cards1), each = 2),
      F = unlist(list_odds[i]),
      L = CI_lowers * 100,
      U = CI_uppers * 100,
      subgroup = c("Casino shuffle", " Wash and casino")
    )
    
    require(ggplot2)
    p = ggplot(data, aes(
      x = reorder(group, 1:((numpl + 4) * 2)),
      y = F,
      color = reorder(subgroup, 1:((numpl + 4) * 2))
    )) +
      geom_point(size = 1) +
      labs(
        title = name,
        x = x_axis,
        y = "percentage",
        color = "Shuffle"
      ) +
      geom_errorbar(aes(
        ymax = U,
        ymin = L,
        width = .5,
        color = subgroup
      )) +
      ylim(0, 2.2 * val[i]) +
      geom_hline(
        yintercept = val[i],
        linetype = "dashed",
        color = "black"
      ) +
      theme(
        plot.title = element_text(hjust = .5),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 15)
      )
    graphs[[i]] = p
  }
  ggarrange(
    graphs[[1]],
    graphs[[2]],
    graphs[[3]],
    graphs[[4]],
    graphs[[5]],
    graphs[[6]],
    graphs[[7]],
    graphs[[8]],
    graphs[[9]],
    graphs[[10]],
    ncol = 2,
    nrow = 5,
    common.legend = TRUE
  )
  
}

cards_guessed_right_v2 = function(shuffle, sims) {
  correc_vec = 0
  for (i in 1:sims) {
    og = 1:52
    correct = 1
    cards = shuffle(1:52)
    if (cards[1] == 1) {
      correct = correct + 1
    }
    if (cards[1] == 52) {
      top_card = cards[1]
      z = list()
      z[[1]] = og[1:(top_card - 1)]
    } else{
      top_card = cards[1]
      z = list()
      z[[1]] = og[1:(top_card - 1)]
      z[[2]] = og[(top_card + 1):52]
    }
    
    cards = cards[-1]
    
    for (x in 1:50) {
      max = 0
      for (i in 1:length(z)) {
        if (length(z[[i]]) > max) {
          max = length(z[[i]])
          lrg = i
        }
      }
      guess = z[[lrg]][1]
      og = og[-1 * (match(top_card, og))]
      
      if (cards[1] == guess) {
        correct = correct + 1
      }
      
      top_card = cards[1]
      for (i in 1:length(z)) {
        if (any(top_card %in% unlist(z[i]))) {
          if (length(unlist(z[[i]])) == 1) {
            z[[i]] = NULL
          } else if (z[[i]][1] == top_card) {
            z[[i]] = z[[i]][2:length(z[[i]])]
          } else if (z[[i]][length(z[[i]])] == top_card) {
            z[[i]] = z[[i]][1:(length(z[[i]]) - 1)]
          } else{
            index = top_card - z[[i]][1]
            zum = z[[i]]
            z[[i]] = zum[1:index]
            z[[length(z) + 1]] = zum[(index + 2):(length(zum))]
          }
        }
      }
      cards = cards[-1]
      
    }
    correc_vec = correc_vec + correct
  }
  return(correc_vec / sims)
}

another_variation_distance_bar_plot = function(sims, shuffle1, shuffle2) {
  holdem = cards_guessed_right_v2(shuffle1, sims)
  riffle = cards_guessed_right_v2(shuffle2, sims)
  
  data <- data.frame(
    values = c(holdem, riffle),
    subgroup = c("Casino Shuffle", "7 riffle shuffles")
  )
  
  ggplot(data,
         aes(
           x = reorder(subgroup, 1:2),
           y = values,
           fill = reorder(subgroup, 1:2),
           width = 0.5,
           size = 5
         )) +
    geom_bar(stat = "identity",
             position = "dodge") +
    labs(
      title = "Number of cards guessed correctly after shuffling",
      x = "Shuffle type",
      y = "Cards guessed correctly",
      fill = NULL
    ) +
    geom_hline(yintercept = 4.54,
               color = "black",
               linetype = "dashed") +
    geom_text(aes(label = round(values, 2)), vjust = -0.2, size = 7) +
    theme(
      plot.title = element_text(hjust = .5),
      text = element_text(size = 20),
      legend.position = "none"
    )
}

# comparison of probability of neighboring cards being n spaces apart
# for a variety of shuffles
                 nums = c()
for (i in 1:6) {
  nums = append(nums, unlist(hold[i]))
  nums = append(nums, unlist(alt3_3[i]))
  nums = append(nums, unlist(alt4_5[i]))
  nums = append(nums, unlist(alt5[i]))
  nums = append(nums, unlist(wash_neighbors[i]))
  
}

data <- data.frame(
  values = nums,
  group = rep(c(1:6),
              each = 5),
  subgroup = c(
    "Casino Shuffle",
    "Alt w. 3 riffles",
    "Alt w. 4 riffles",
    "Alt w. 5 riffles",
    "Wash and casino"
  )
)

ggplot(data,
       aes(
         x = group,
         y = values,
         fill = reorder(subgroup, 1:30)
       )) +
  geom_bar(stat = "identity",
           position = "dodge", ) +
  labs(title = "Probability of neighbor cards being n cards apart",
       x = "Cards apart",
       y = "Probability",
       fill = "shuffle") +
  geom_abline(
    intercept = (100 / 51),
    slope = (-100 / (52 * 51)),
    color = "black",
    linetype = "dashed"
  ) +
  theme(
    plot.title = element_text(size = 20, hjust = .5),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 15),
    text = element_text(size = 20)
  ) +
  scale_fill_manual(values = c("#F8766D", "#6BAED6", "#2CA25F", "#8856A7", "#00BFC4"))

# comparison of probability of neighboring cards being n spaces apart
# for all 52 card slots
nums = c()
for (i in 1:52) {
  nums = append(nums, unlist(hold[i]))
  nums = append(nums, unlist(wash_neighbors[i]))
}

data <- data.frame(
  values = nums,
  group = rep(c(1:52), each = 2),
  subgroup = rep(
    c("Casino Shuffle", "Wash and casino"),
    times = 52
  )
)

ggplot(data,
       aes(
         x = group,
         y = values,
         fill = reorder(subgroup, values)
       )) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Probability of neighbor cards being n cards apart",
       x = "Cards apart",
       y = "Probability",
       fill = "shuffle") +
  geom_abline(
    intercept = (100 / 51),
    slope = (-100 / (52 * 51)),
    color = "black",
    linetype = "dashed"
  ) +
  theme(
    plot.title = element_text(size = 20, hjust = .5),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 15),
    text = element_text(size = 20)
  ) +
  scale_fill_manual(values = c("#F8766D", "#6BAED6"))

# running the functions
neighbor_bar_chart(1, holdem_shuffle, better_shuffle, wash_and_casino, 6)
ci_graph(100000, 6, wash_and_casino, "wash shuffle")
ci_comparison_graph(100000,
                    6,
                    holdem_shuffle,
                    "Casino shuffle vs Wash and casino",
                    wash_and_casino)
another_variation_distance_bar_plot(1, holdem_shuffle, wash_and_casino)
