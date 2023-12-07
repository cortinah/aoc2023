library(adventdrob)
library(tidyverse)
library(magrittr)

input <- advent_input(5, 2023)

# part 1
seeds <- input[1,]
separate_wider_delim(seeds, x," ",names_sep = "_") -> seeds
seeds <- as.numeric(seeds[1,2:21])

seedtosoil <- input[4:25,]
seedtosoil |> separate_wider_delim(x, " ",names=c("dest","source","length")) |> mutate_all(as.numeric) -> seedtosoil
soiltofert <- input[28:54,]
soiltofert |> separate_wider_delim(x, " ",names=c("dest","source","length")) |> mutate_all(as.numeric) -> soiltofert
ferttowater <- input[57:94,]
ferttowater |> separate_wider_delim(x, " ",names=c("dest","source","length"))|> mutate_all(as.numeric) -> ferttowater
watertolight <- input[97:121,]
watertolight |> separate_wider_delim(x, " ",names=c("dest","source","length"))|> mutate_all(as.numeric) -> watertolight
lighttotemp <- input[124:143,]
lighttotemp |> separate_wider_delim(x, " ",names=c("dest","source","length"))|> mutate_all(as.numeric) -> lighttotemp
temptohumid <- input[146:171,]
temptohumid |> separate_wider_delim(x, " ",names=c("dest","source","length"))|> mutate_all(as.numeric) -> temptohumid
humidtoloc <- input[174:209,]
humidtoloc |> separate_wider_delim(x, " ",names=c("dest","source","length"))|> mutate_all(as.numeric) -> humidtoloc


lookup <- function(input, table) {
table <- arrange(table, source)
min <- table[1, "source"]
max <- table[nrow(table),"source"] + table[nrow(table),"length"] -1

if (input < min) return(input)
if (input > max) return(input)

i <- 1
while (i <= nrow(table)) {

if ((input >= table[i, "source"]) & (input <= table[i, "source"] + table[i,"length"] -1 ))
  return(as.numeric(table[i, "dest"] + input - table[i, "source"]) )
   i <- i+1
}
return(input)
}

map_dbl(seeds, ~lookup(.x, seedtosoil)) |>
map_dbl(~lookup(.x, soiltofert)) |>
map_dbl(~lookup(.x, ferttowater)) |>
map_dbl(~lookup(.x, watertolight)) |>
map_dbl(~lookup(.x, lighttotemp)) |>
map_dbl(~lookup(.x, temptohumid)) |>
map_dbl(~lookup(.x, humidtoloc)) |>
min()

# Part 2

loc <- function(seed) {

lookup(seed, seedtosoil) |>
lookup(soiltofert) |>
lookup(ferttowater) |>
lookup(watertolight) |>
lookup(lighttotemp) |>
lookup(temptohumid) |>
lookup(humidtoloc) }




minloc <- function(seed, range) {

  trymin <- loc(seed)

  for (x in seed:(seed+range-1)) {
    try <- loc(x)
    if (try < trymin) trymin <- try
  }
  return(trymin)
}

minloc(seeds[1],100)


















revlookup <- function(inputdest, table) {
  table <- arrange(table, dest)
  min <- table[1, "dest"]
  max <- table[nrow(table),"dest"] + table[nrow(table),"length"] -1

  if (inputdest < min) return(inputdest)
  if (inputdest > max) return(inputdest)

  i <- 1
  while (i <= nrow(table)) {

    if ((inputdest >= table[i, "dest"]) & (inputdest <= table[i, "dest"] + table[i,"length"] -1 ))
      return(as.numeric(table[i, "source"] + inputdest - table[i, "dest"]) )
    i <- i+1
  }
  return(inputdest)
}



map_dbl(1, ~revlookup(.x, humidtoloc)) |>
  map_dbl(~revlookup(.x, temptohumid)) |>
  map_dbl(~revlookup(.x, lighttotemp)) |>
  map_dbl(~revlookup(.x, watertolight)) |>
  map_dbl(~revlookup(.x, ferttowater)) |>
  map_dbl(~revlookup(.x, soiltofert)) |>
  map_dbl(~revlookup(.x, seedtosoil)) |> min()



#  3227795728 too high




map_dbl(889655708, ~lookup(.x, seedtosoil)) |>
  map_dbl(~lookup(.x, soiltofert)) |>
  map_dbl(~lookup(.x, ferttowater)) |>
  map_dbl(~lookup(.x, watertolight)) |>
  map_dbl(~lookup(.x, lighttotemp)) |>
  map_dbl(~lookup(.x, temptohumid)) |>
  map_dbl(~lookup(.x, humidtoloc)) |>
  min()
