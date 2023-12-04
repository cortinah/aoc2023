library(adventdrob)
library(tidyverse)

input <- advent_input(4, 2023)

# part 1
 input |> separate_wider_delim(x,':',names=c('card','nums')) -> input
 input$card <- as.numeric(substr(input$card,start = 5,8))

 input |> separate_wider_delim(nums,'|',names=c('winners','nums')) -> input
 input |> select(card, winners) -> winners
 input |> select(card, winners=nums) -> held

winners |> separate_longer_delim(winners,' ') -> winners
winners |> mutate(winners=as.numeric(winners)) |> drop_na() -> winners

held |> separate_longer_delim(winners,' ') -> held
held |> mutate(winners=as.numeric(winners)) |> drop_na() -> held

join <- inner_join(held,winners)

join |> group_by(card) |> summarize(points=n()) -> score

score |> mutate(score=2^(points-1)) -> score
sum(score$score)
# 20407

# Part 2
cards <- score |> select(card, matches=points)

all_cards <- tibble(card=1:192,q=1)

left_join(all_cards,cards,by='card') -> all_cards
all_cards |> replace_na(list(matches=0)) -> all_cards

all_cards |> select(card,matches,q) -> cards


for (i in 1:191) {

 matches <- as.numeric(cards[i,"matches"])
 q <- as.numeric(cards[i,"q"])

 if (matches>0) {
 range <- (i+1):(i+matches)
 cards[range,'q'] <- cards[range,'q']+q }

}
sum(cards$q,na.rm = T)

#23806951
