library(adventdrob)
library(tidyverse)

input <- advent_input(2, 2023)

# part 1
input |> separate_wider_delim(x,":", names=c("game","vals")) |>
separate_longer_delim(vals,';') |>
separate_wider_delim(vals,",",names=c('a','b','c'), too_few = 'align_start') -> output

output |> separate_wider_delim(game," ", names=c("temp","game")) |> select(-temp) -> output

output |> mutate(a=trimws(a), b=trimws(b), c=trimws(c)) -> output

output |> separate_wider_delim(a," ",names=c("anum","acol"),too_many = 'merge') |>
         separate_wider_delim(b," ",names=c("bnum","bcol"),too_many = 'merge') |>
         separate_wider_delim(c," ",names=c("cnum","ccol"),too_many = 'merge') -> output

output |> mutate(game=as.numeric(game), anum=as.numeric(anum),bnum=as.numeric(bnum),cnum=as.numeric(cnum)) -> output

output |>
  mutate(red = ifelse(acol=='red', anum, ifelse(bcol=="red", bnum, ifelse(ccol=='red',cnum,NA)))) |>
  mutate(green = ifelse(acol=='green', anum, ifelse(bcol=="green", bnum, ifelse(ccol=='green',cnum,NA)))) |>
  mutate(blue = ifelse(acol=='blue', anum, ifelse(bcol=="blue", bnum, ifelse(ccol=='blue',cnum,NA)))) -> output

output |> mutate(red=ifelse(is.na(red),0,red)) |> mutate(green=ifelse(is.na(green),0,green)) |>
  mutate(blue=ifelse(is.na(blue),0,blue)) -> output

output |> mutate(valid=ifelse( (red <= 12 & green <= 13 & blue <= 14) ,1,0)) -> output


output |> group_by(game) |> mutate(game_valid=min(valid)) -> output
output |> filter(game_valid==1) -> output1
sum(unique(output1$game))


## part 2
output |> group_by(game) |> mutate(minred=max(red), mingreen=max(green), minblue=max(blue)) -> output

output |> select(1,13:15) |> unique() -> output2
output2 |> mutate(power=(minred*mingreen*minblue)) -> output2
sum(output2$power)
