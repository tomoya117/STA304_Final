#### Preamble ####
# Purpose: Visualize the cleaned data sets
# Author: Tomoya Fujikawa
# Data: 27 April 2022
# Contact: tomoya.fujikawa@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have cleaned and manipulated the data sets

#### Workspace setup ####
# show a brief summary of the US economy by the GDP histogram
GDP |>
  ggplot(aes(x = Year, y = Value)) +
  geom_col(position = "dodge", alpha = 0.8) +
  theme_minimal() + 
  labs(x = "Year", y = "GDP  (In Billions USD)", 
       title = "Figure 1: The GDP of the US from 2000 to 2016", caption = "The World Bank")

# show the three main variables using a simple table
Table |>
  select(Year, `Tax Revenue`, `Interest Payments`, `Gov't Spending`) |>
  slice(1:17) |>
  kable(
    caption = "The Finance of the US Government from 2000 to 2016 (In Billions)", 
    row.names = FALSE, 
    digits = 1,
    booktabs = TRUE, 
    linesep = ""
  )

# brief summary of the MM theorem shown in a table
MM |>
  select(Year, Inflow, Outflow, Deficit) |>
  slice(1:17) |>
  kable(
    caption = "The Cash Flows and the Budget Deficit of the US government from 2000 to 2016 (In Billions)", 
    col.names = c("Year", "Inflow", "Outflow", "Budget Deficit"),
    row.names = FALSE, 
    digits = 1,
    booktabs = TRUE, 
    linesep = ""
  )

# two MM graphs to compare the impact of adding increase in debt to the model 
MM |> 
  ggplot(aes(x = Year, y = Value, group = `Cash Flow`, fill = `Cash Flow`)) +
  geom_col(position = "dodge", alpha = 0.8) +
  theme_minimal() + 
  labs(x = "Year", y = "Value in USD (In Billions)", 
       title = str_wrap("Figure 3: The Representation of the Original Modigliani-Miller 
                        Theorem from 2000 to 2016", 60), caption = "The World Bank", 
       fill = "Cash Flows") + scale_x_discrete(guide=guide_axis(n.dodge=2))
MM2 |> 
  ggplot(aes(x = Year, y = Value, group = `Cash Flow`, fill = `Cash Flow`)) +
  geom_col(position = "dodge", alpha = 0.8) +
  theme_minimal() + 
  labs(x = "Year", y = "Value in USD (In Billions)", 
       title = str_wrap("Figure 4: The Representation of the Modified Modigliani-Miller 
                        Theorem from 2000 to 2016", 60), caption = "The World Bank", 
       fill = "Cash Flows") + scale_x_discrete(guide=guide_axis(n.dodge=2))