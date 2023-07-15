# stats_stud8 ##################################################################


# make a low odds ##############################################################

# A,2,3,4,5,6,7,8


# from starting with 3 low cards
# 
# need to make 2 other, different low cards
# 
# depends on number of seen cards, C
# depends on number of seen opponent low cards not ranked among hero lows, L
# 
# assuming 6-handed, C = 8 = 5 opponent up cards + 2 hero down cards + 1 hero up
# assuming no low up cards among opponents,
# 
# 4 more cards to come, 3 up, 1 down
# 
# need 2 cards from 5 ranks we don't hold, must be 2 distinct ranks
# 
# first needed out count = 20 = 5 ranks * 4 suits
# 
# second needed out count = 16 = 4 ranks * 4 suits
# 
# theoretical denominators for coming streets : 44, 43, 42, 41
# 
# total denominator = 44 * 43 * 42 * 41
# 
#*** numerators, scratch approximation : [(20) * (16) * (41) * (41)]
# 

numerator <- (20 * 16 * 41 * 41)
denominator <- 44 * 43 * 42 * 41

odds <- numerator / denominator

odds # 0.165

1 / odds # 6.0567

# H : hit, N : not hit, P(H) ~= 20or16 / 41 , P(N) ~= 21or25 / 41
# P(H) =20 / 41
# P(h) = 16 / 41
# P(N) = 25 / 41
# P(n) = 21 / 41
# 
# H h X X
# X H h X
# X X H h 
# H X h X
# H X X h
# X H X h
# 

################################################################################
# 
# monte carlo
# 

ranks <- 1:13
suits <- c("s",
           "h",
           "c",
           "d")

deck_full <- cbind(rep(ranks,
                       4),
                   c(rep(suits[1],
                         13),
                     rep(suits[2],
                         13),
                     rep(suits[3],
                         13),
                     rep(suits[4],
                         13)))

deck_full <- as.data.frame(deck_full)

names(deck_full) <- c("rank",
                      "suit")


deck_full[c(1,15,29),]

# define a function to pull a random card, returning the card and updated deck

pull_random <- function(deck,
                        number_cards = 1){
  # needs error-checking code...

  return_object <- list()
  
  number_cards_remaining <- dim(deck)[1]
  
  indices_cards_remaining <- seq(number_cards_remaining)
  
  indices_pull <- sample(x = indices_cards_remaining,
                         size = number_cards,
                         replace = FALSE)
  
  cards_pull <- deck[indices_pull,]
  
  deck_updated <- deck[-1 * indices_pull,]
  
  return_object$pull <- cards_pull
  
  return_object$deck_updated <- deck_updated
  
  return(return_object)
}


draw_3 <- pull_random(deck = deck_full,
                      number_cards = 3)

draw_3$pull

dim(draw_3$deck_updated)

#

deck_4 <- deck_full

draw_4 <- pull_random(deck = deck_4,
                      number_cards = 4)

draw_4$pull

dim(draw_4$deck_updated)

deck_4 <- draw_4$deck_updated
