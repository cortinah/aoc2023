library(adventdrob)
library(tidyverse)

input <- advent_input(3, 2023)

# part 1
 input |> grid_tidy(x) -> tidy

 parts <- c("*","$","+","-","%","#","&","=", "/", "@")

 tidy |> filter(value %in% parts) -> symblist

 get <- function(srow, scol) return(tidy |> filter(row==srow & col==scol) |> pull(value))

 symbcount <- function(srow,scol) {

   checkright <- function(srow,scol) {
     v1 <- get(srow, scol+1)
     if(v1=='.') return(0)
     v1<-as.numeric(v1)
     v2 <- get(srow, scol+2)
     if(v2=='.') return(v1)
     v2<-as.numeric(v2)
     v3 <- get(srow, scol+3)
     if(v3=='.') return(10*v1 + v2)
     v3<-as.numeric(v3)
     return(100*v1 + 10*v2 + v3)
     }

   checkleft <- function(srow,scol) {
     v1 <- get(srow, scol-1)
     if(v1=='.') return(0)
     v1<-as.numeric(v1)
     v2 <- get(srow, scol-2)
     if(v2=='.') return(v1)
     v2<-as.numeric(v2)
     v3 <- get(srow, scol-3)
     if(v3=='.') return(10*v2 + v1)
     v3<-as.numeric(v3)
     return(100*v3 + 10*v2 + v1)
   }

   checktop <- function(srow,scol) {
     v0 <- get(srow-1, scol)
     if(v0=='.') return(checkleft(srow-1,scol) + checkright(srow-1,scol))
     v0 <- get(srow-1, scol-1)
     if(v0=='.') return(checkright(srow-1,scol-1))
     v0 <- get(srow-1, scol-2)
     if(v0=='.') return(checkright(srow-1,scol-2))
     v0 <- get(srow-1, scol-3)
     if(v0=='.') return(checkright(srow-1,scol-3))
   }

   checkbot <- function(srow,scol) {
     v0 <- get(srow+1, scol)
     if(v0=='.') return(checkleft(srow+1,scol) + checkright(srow+1,scol))
     v0 <- get(srow+1, scol-1)
     if(v0=='.') return(checkright(srow+1,scol-1))
     v0 <- get(srow+1, scol-2)
     if(v0=='.') return(checkright(srow+1,scol-2))
     v0 <- get(srow+1, scol-3)
     if(v0=='.') return(checkright(srow+1,scol-3))
   }
   checkright(srow,scol) + checkleft(srow,scol) + checktop(srow,scol) +
     + checkbot(srow,scol)
 }

 symblist <- symblist |> mutate(val=map2_dbl(symblist$row, symblist$col, ~symbcount(.x,.y)))


sum(symblist$val)

### Part 2
input <- advent_input(3, 2023)

input |> grid_tidy(x) -> tidy

parts <- c("*")

tidy |> filter(value %in% parts) -> symblist

get <- function(srow, scol) return(tidy |> filter(row==srow & col==scol) |> pull(value))

symbcount <- function(srow,scol) {

  checkright <- function(srow,scol) {
    v1 <- get(srow, scol+1)
    if(v1=='.') return(0)
    v1<-as.numeric(v1)
    v2 <- get(srow, scol+2)
    if(v2=='.') return(v1)
    v2<-as.numeric(v2)
    v3 <- get(srow, scol+3)
    if(v3=='.') return(10*v1 + v2)
    v3<-as.numeric(v3)
    return(100*v1 + 10*v2 + v3)
  }

  checkleft <- function(srow,scol) {
    v1 <- get(srow, scol-1)
    if(v1=='.') return(0)
    v1<-as.numeric(v1)
    v2 <- get(srow, scol-2)
    if(v2=='.') return(v1)
    v2<-as.numeric(v2)
    v3 <- get(srow, scol-3)
    if(v3=='.') return(10*v2 + v1)
    v3<-as.numeric(v3)
    return(100*v3 + 10*v2 + v1)
  }

  checktop <- function(srow,scol) {
    v0 <- get(srow-1, scol)
    if(v0=='.') return(0)
    v0 <- get(srow-1, scol-1)
    if(v0=='.') return(checkright(srow-1,scol-1))
    v0 <- get(srow-1, scol-2)
    if(v0=='.') return(checkright(srow-1,scol-2))
    v0 <- get(srow-1, scol-3)
    if(v0=='.') return(checkright(srow-1,scol-3))
  }

  checkbot <- function(srow,scol) {
    v0 <- get(srow+1, scol)
    if(v0=='.') return(0)
    v0 <- get(srow+1, scol-1)
    if(v0=='.') return(checkright(srow+1,scol-1))
    v0 <- get(srow+1, scol-2)
    if(v0=='.') return(checkright(srow+1,scol-2))
    v0 <- get(srow+1, scol-3)
    if(v0=='.') return(checkright(srow+1,scol-3))
  }

  nums <- c(checkright(srow,scol), checkleft(srow,scol),
            checktop(srow,scol),checkbot(srow,scol))

  if(nums[3]==0) nums <- c(nums, checkleft(srow-1,scol),checkright(srow-1,scol)) else
                 nums <- c(nums,0,0)

  if(nums[4]==0) nums <- c(nums, checkleft(srow+1,scol),checkright(srow+1,scol)) else
                 nums <- c(nums,0,0)

  if (sum(nums!=0)==2) return(prod(nums[(nums!=0)]))
  return(0)
  }

symblist <- symblist |> mutate(val=map2_dbl(symblist$row, symblist$col, ~symbcount(.x,.y)))

sum(symblist$val)

# 69527306
