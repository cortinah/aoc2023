library(adventdrob)
library(tidyverse)
library(stringr)
library(stringi)

input <- advent_input(1, 2023)

# part 1
get <- function(i) as.numeric(paste0(str_extract(i,pattern = "[1-9]"),str_extract(stri_reverse(i),pattern = "[1-9]")))

input$n <- get(input$x)

sum(input$n)

# part 2 from drob

input <- advent_input(1, 2023)

num <- c("one","two","three","four","five","six","seven","eight","nine")

input |> extract(x, "first", "(\\d|one|two|three|four|five|six|seven|eight|nine)", remove=F) |>
         extract(x, "last", ".*(\\d|one|two|three|four|five|six|seven|eight|nine)") |>
mutate(first=coalesce(as.numeric(first), match(first,num)),
       last=coalesce(as.numeric(last), match(last,num))) |>
  mutate(n=as.numeric(paste0(first,last))) |>
  summarize(sum(n))
