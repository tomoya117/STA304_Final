#### Preamble ####
# Purpose: Clean the data downloaded from The World Bank and The White House
# Author: Tomoya Fujikawa
# Data: 27 April 2022
# Contact: tomoya.fujikawa@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the data and saved it to inputs/data


#### Workspace setup ####
# Use R Projects, not setwd().
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(dplyr)
library(knitr)
library(bookdown)
library(kableExtra)
library(ggplot2)

#Load in the data sets
Interest <- read_csv(here::here("inputs/Debt Interests.csv"))
Tax <- read_csv(here::here("inputs/Tax Revenue.csv"))
GDP <- read_csv(here::here("inputs/GDP.csv"))
Spending <- read_csv(here::here("inputs/Government Spending.csv"))
Debt <- read_csv(here::here("inputs/Government Debt.csv"))
SSC <- read_csv(here::here("inputs/Social Security Contributions.csv"))

# extracting data from 2000 to 2016 only
# extracting data for the US only
# create a column for years
# rename columns
# set values to be shown in billions
# clean data
Interest <- data.frame(Interest)
Interest <- subset(Interest, select = c(45:61))
Interest <- Interest[c(256), ]
Interest <- as.data.frame(t(Interest))
colnames(Interest) <- c('Value')
Interest <- 
  Interest |> 
  mutate(Year = c('2000', '2001', '2002', '2003', '2004', '2005', '2006', '2007', '2008', '2009', '2010', '2011', '2012', '2013', '2014', '2015', '2016'))
Interest <- Interest[, c(2, 1)]
Interest <- 
  Interest |>
  mutate(Value = Value / 1000000000)

Tax <- subset(Tax, select = c(45:61))
Tax <- Tax[c(256), ]
Tax <- as.data.frame(t(Tax))
colnames(Tax) <- c('Value')
Tax <- 
  Tax |> 
  mutate(Year = c('2000', '2001', '2002', '2003', '2004', '2005', '2006', '2007', '2008', '2009', '2010', '2011', '2012', '2013', '2014', '2015', '2016'))
Tax <- Tax[, c(2, 1)]
Tax <- as.data.frame(Tax)
Tax <- 
  Tax |>
  mutate(Value = Value / 1000000000)

GDP <- subset(GDP, select = c(45:61))
GDP <- GDP[c(256), ]
GDP <- as.data.frame(t(GDP))
colnames(GDP) <- c('Value')
GDP <- 
  GDP |> 
  mutate(Year = c('2000', '2001', '2002', '2003', '2004', '2005', '2006', '2007', '2008', '2009', '2010', '2011', '2012', '2013', '2014', '2015', '2016'))
GDP <- GDP[, c(2, 1)]
GDP <- 
  GDP |>
  mutate(Value = Value / 1000000000)

Spending <- subset(Spending, select = c(1, 2))
Spending <- Spending[c(57:73), ]
colnames(Spending) <- c('Year', 'Value')
Spending$Value <- as.numeric(gsub(",", "", Spending$Value))

Debt <- data.frame(Debt)
Debt <- subset(Debt, select = c(45:61))
Debt <- Debt[c(256), ]
Debt <- as.data.frame(t(Debt))
colnames(Debt) <- c('Value')
Debt <- 
  Debt |> 
  mutate(Year = c('2000', '2001', '2002', '2003', '2004', '2005', '2006', '2007', '2008', '2009', '2010', '2011', '2012', '2013', '2014', '2015', '2016'))
Debt <- Debt[, c(2, 1)]
Debt <- 
  Debt |>
  mutate(Value = Value / 1000000000)

SSC <- data.frame(SSC)
SSC <- subset(SSC, select = c(45:61))
SSC <- SSC[c(256), ]
SSC <- as.data.frame(t(SSC))
colnames(SSC) <- c('Value')
SSC <- 
  SSC |> 
  mutate(Year = c('2000', '2001', '2002', '2003', '2004', '2005', '2006', '2007', '2008', '2009', '2010', '2011', '2012', '2013', '2014', '2015', '2016'))
SSC <- SSC[, c(2, 1)]
SSC <- 
  SSC |>
  mutate(Value = Value / 1000000000)

Tax <- merge(Tax, SSC, by = "Year")
Tax <- 
  Tax |>
  mutate(Value = Value.x + Value.y)
Tax <- 
  Tax |>
  select(Year, Value)

#### Next data manipulations ####
# merge data sets to create a data frame for the table showing the three main variables
Table <- merge(Tax, Spending, by = "Year")
Table <- merge(Table, Interest, by = "Year")
# rename the columns
colnames(Table) <- c("Year", "Tax Revenue", "Gov't Spending", "Interest Payments")

# create the data frame and a column which shows the total cash outflow 
RHS <- merge(Spending, Interest, by = "Year")
colnames(RHS) <- c("Year", "Spending", "Interest")
RHS <-
  RHS |> 
  mutate(Outflow = Spending + Interest)
RHS <- RHS[, c(1, 4)]

# merge data frames and create a column which shows the budget deficit to show the MM theorem using numbers
MM <- merge(Tax, RHS, by = "Year")
colnames(MM) <- c("Year", "Inflow", "Outflow")
MM <- 
  MM |>
  mutate(Deficit = Outflow - Inflow)

# editing the MM data frame suitable for graph representation
MM <-
  MM |>
  select(Year, Inflow, Outflow, `Deficit`) |>
  gather(`Cash Flow`, Value, 2:4)

#creating a column for the debt increase
Debt <- 
  Debt |>
  mutate(Debt_Increase = c(0, diff(Value)))

#merging data frames and creating a column for the total inflow including the debt increase
LHS <- merge(Tax, Debt, by = "Year")
colnames(LHS) <- c("Year", "Tax Revenue", "Debt", "Debt_Increase")
LHS <- 
  LHS |>
  mutate(Inflow = `Tax Revenue` + Debt_Increase)

#merging the data frames to compare total cash inflow and outflow
MM2 <- merge(LHS, RHS, by = "Year")
MM2 <- 
  MM2 |>
  select(Year, Inflow, Outflow) |>
  mutate(Deficit = Outflow - Inflow) |>
  gather(`Cash Flow`, Value, 2:4)

         