---
title: "ACTL1101 Assignment Part A"
author: "Pranav Sampath"
date: "2024 T2"
output:
  pdf_document: default
  word_document: default
  html_document:
    df_print: paged
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Algorithmic Trading Strategy

## Introduction

In this assignment, you will develop an algorithmic trading strategy by incorporating financial metrics to evaluate its profitability. This exercise simulates a real-world scenario where you, as part of a financial technology team, need to present an improved version of a trading algorithm that not only executes trades but also calculates and reports on the financial performance of those trades.

## Background

Following a successful presentation to the Board of Directors, you have been tasked by the Trading Strategies Team to modify your trading algorithm. This modification should include tracking the costs and proceeds of trades to facilitate a deeper evaluation of the algorithm’s profitability, including calculating the Return on Investment (ROI).

After meeting with the Trading Strategies Team, you were asked to include costs, proceeds, and return on investments metrics to assess the profitability of your trading algorithm.

## Objectives

1. **Load and Prepare Data:** Open and run the starter code to create a DataFrame with stock closing data.

2. **Implement Trading Algorithm:** Create a simple trading algorithm based on daily price changes.

3. **Customize Trading Period:** Choose your entry and exit dates.

4. **Report Financial Performance:** Analyze and report the total profit or loss (P/L) and the ROI of the trading strategy.

5. **Implement a Trading Strategy:** Implement a trading strategy and analyze the total updated P/L and ROI. 

6. **Discussion:** Summarise your finding.


## Instructions

### Step 1: Data Loading

Start by running the provided code cells in the "Data Loading" section to generate a DataFrame containing AMD stock closing data. This will serve as the basis for your trading decisions. First, create a data frame named `amd_df` with the given closing prices and corresponding dates. 

```{r load-data}

# Load data from CSV file
amd_df <- read.csv("AMD.csv")

# Convert the date column to Date type and Adjusted Close as numeric
amd_df$date <- as.Date(amd_df$Date)
amd_df$close <- as.numeric(amd_df$Adj.Close)

amd_df <- amd_df[, c("date", "close")]
```


##Plotting the Data
Plot the closing prices over time to visualize the price movement.
```{r plot}
plot(amd_df$date, amd_df$close,'l')
```


## Step 2: Trading Algorithm
Implement the trading algorithm as per the instructions. You should initialize necessary variables, and loop through the dataframe to execute trades based on the set conditions.

- Initialize Columns: Start by ensuring dataframe has columns 'trade_type', 'costs_proceeds' and 'accumulated_shares'.
- Change the algorithm by modifying the loop to include the cost and proceeds metrics for buys of 100 shares. Make sure that the algorithm checks the following conditions and executes the strategy for each one:
  - If the previous price = 0, set 'trade_type' to 'buy', and set the 'costs_proceeds' column to the current share price multiplied by a `share_size` value of 100. Make sure to take the negative value of the expression so that the cost reflects money leaving an account. Finally, make sure to add the bought shares to an `accumulated_shares` variable.
  - Otherwise, if the price of the current day is less than that of the previous day, set the 'trade_type' to 'buy'. Set the 'costs_proceeds' to the current share price multiplied by a `share_size` value of 100.
  - You will not modify the algorithm for instances where the current day’s price is greater than the previous day’s price or when it is equal to the previous day’s price.
  - If this is the last day of trading, set the 'trade_type' to 'sell'. In this case, also set the 'costs_proceeds' column to the total number in the `accumulated_shares` variable multiplied by the price of the last day.



```{r trading}
# Initialize columns for trade type, cost/proceeds, and accumulated shares in amd_df
amd_df$trade_type <- NA
amd_df$costs_proceeds <- 0  # Corrected column name
amd_df$accumulated_shares <- 0  # Initialize if needed for tracking

# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0

for (i in 1:nrow(amd_df)) {
# Fill your code here

  if (previous_price == 0) {
    amd_df$trade_type[i] <- "buy"
    amd_df$costs_proceeds[i] <- -amd_df$close[i]*share_size
    amd_df$accumulated_shares[i] <- accumulated_shares + share_size
    accumulated_shares <- accumulated_shares + share_size
    
    
  }
  
  else if (i == nrow(amd_df)) {
    amd_df$trade_type[i] <- "sell"
    
    amd_df$costs_proceeds[i] <- amd_df$close[i]*accumulated_shares
    
    accumulated_shares <- 0
  }
  
  else if (previous_price > amd_df$close[i]) {
    
    amd_df$trade_type[i] <- "buy"
    amd_df$costs_proceeds[i] <- -amd_df$close[i]*share_size
    accumulated_shares <- accumulated_shares + share_size
    
  }
  
  previous_price <- amd_df$close[i]
  
  
  amd_df$accumulated_shares[i] <- accumulated_shares
  
}
```


## Step 3: Customize Trading Period
- Define a trading period you wanted in the past five years 
```{r period}
# Fill your code here
start_period <- as.Date('2021-07-01')
end_period <- as.Date('2022-06-30')

new_amd_df <- amd_df[amd_df$date >= start_period & amd_df$date <= end_period,]

# Initialize columns for trade type, cost/proceeds, and accumulated shares in new_amd_df
new_amd_df$trade_type <- NA
new_amd_df$costs_proceeds <- 0  # Corrected column name
new_amd_df$accumulated_shares <- 0  # Initialize if needed for tracking

# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0


for (i in 1:nrow(new_amd_df)) {
# Fill your code here

  if (previous_price == 0) {
    
    new_amd_df$trade_type[i] <- "buy"
    new_amd_df$costs_proceeds[i] <- -new_amd_df$close[i]*share_size
    new_amd_df$accumulated_shares[i] <- accumulated_shares + share_size
    accumulated_shares <- accumulated_shares + share_size
    
  }
  
  else if (i == nrow(new_amd_df)) {
    new_amd_df$trade_type[i] <- "sell"
    
    new_amd_df$costs_proceeds[i] <- new_amd_df$close[i]*accumulated_shares
    
    accumulated_shares <- 0
  }
  
  else if (previous_price > new_amd_df$close[i]) {
    
    new_amd_df$trade_type[i] <- "buy"
    
    new_amd_df$costs_proceeds[i] <- -new_amd_df$close[i]*share_size
    
    accumulated_shares <- accumulated_shares + share_size
    
  }
  
  previous_price <- new_amd_df$close[i]
  
  new_amd_df$accumulated_shares[i] <- accumulated_shares
  
}


```


## Step 4: Run Your Algorithm and Analyze Results
After running your algorithm, check if the trades were executed as expected. Calculate the total profit or loss and ROI from the trades.

- Total Profit/Loss Calculation: Calculate the total profit or loss from your trades. This should be the sum of all entries in the 'costs_proceeds' column of your dataframe. This column records the financial impact of each trade, reflecting money spent on buys as negative values and money gained from sells as positive values.
- Invested Capital: Calculate the total capital invested. This is equal to the sum of the 'costs_proceeds' values for all 'buy' transactions. Since these entries are negative (representing money spent), you should take the negative sum of these values to reflect the total amount invested.
- ROI Formula: $$\text{ROI} = \left( \frac{\text{Total Profit or Loss}}{\text{Total Capital Invested}} \right) \times 100$$

```{r}
# Fill your code here
total_costs_proceeds <- 0
inv_capital <- 0

for (i in 1:nrow(new_amd_df)) {
  
  if(!is.na(new_amd_df$costs_proceeds[i])) {
    total_costs_proceeds <- total_costs_proceeds + new_amd_df$costs_proceeds[i]
  }
  
  if (!is.na(new_amd_df$trade_type[i]) && new_amd_df$trade_type[i] == "buy") {
    inv_capital <- inv_capital - new_amd_df$costs_proceeds[i]
  }
}

roi <- (total_costs_proceeds/inv_capital)*100

#still need to print everything
cat ("The ROI is:", roi, "%\n")
cat ("The total profit/loss made is $", total_costs_proceeds, "\n")
cat ("The total amount of capital invested is $", inv_capital, "\n")

```

## Step 5: Profit-Taking Strategy or Stop-Loss Mechanisum (Choose 1)
- Option 1: Implement a profit-taking strategy that you sell half of your holdings if the price has increased by a certain percentage (e.g., 20%) from the average purchase price.


```{r option}
# Fill your code here
# Initialize columns for trade type, cost/proceeds, and accumulated shares in amd_df
new_amd_df$trade_type <- NA
new_amd_df$costs_proceeds <- 0  # Corrected column name
new_amd_df$accumulated_shares <- 0  # Initialize if needed for tracking



# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0

total_shares_cost <- 0
no_shares_bought <- 0

for (i in 1:nrow(new_amd_df)) {
# Fill your code here

  if (previous_price == 0) {
    new_amd_df$trade_type[i] <- "buy"
    new_amd_df$costs_proceeds[i] <- -new_amd_df$close[i]*share_size
    accumulated_shares <- accumulated_shares + share_size
    
    total_shares_cost <- total_shares_cost - new_amd_df$costs_proceeds[i]
    
    avg_purchase_price <- total_shares_cost/accumulated_shares
    
  }
  
  else if (i == nrow(new_amd_df)) {
    new_amd_df$trade_type[i] <- "sell"
    
    new_amd_df$costs_proceeds[i] <- new_amd_df$close[i]*accumulated_shares
    accumulated_shares <- 0
  }
  else if (previous_price > new_amd_df$close[i]) {
    new_amd_df$trade_type[i] <- "buy"
    new_amd_df$costs_proceeds[i] <- -new_amd_df$close[i]*share_size
    accumulated_shares <- accumulated_shares + share_size
    
    
    total_shares_cost <- total_shares_cost - new_amd_df$costs_proceeds[i]
    
    avg_purchase_price <- total_shares_cost/accumulated_shares
  }
  else if (new_amd_df$close[i] >= avg_purchase_price*1.40) {
    new_amd_df$trade_type[i] <- "sell"
    new_amd_df$costs_proceeds[i] <- new_amd_df$close[i]*accumulated_shares/2
    accumulated_shares <- accumulated_shares/2
    total_shares_cost <- avg_purchase_price*accumulated_shares
  }
  
  previous_price <- new_amd_df$close[i]
  new_amd_df$accumulated_shares[i] <- accumulated_shares
  
  
  
}

```


## Step 6: Summarize Your Findings
- Did your P/L and ROI improve over your chosen period?
- Relate your results to a relevant market event and explain why these outcomes may have occurred.




```{r}
# Fill your code here and Disucss
total_costs_proceeds <- 0
inv_capital <- 0

for (i in 1:nrow(new_amd_df)) {
  
  if(!is.na(new_amd_df$costs_proceeds[i])) {
    total_costs_proceeds <- total_costs_proceeds + new_amd_df$costs_proceeds[i]
  }
  
  if (!is.na(new_amd_df$trade_type[i]) && new_amd_df$trade_type[i] == "buy") {
    inv_capital <- inv_capital - new_amd_df$costs_proceeds[i]
  }
}

roi <- (total_costs_proceeds/inv_capital)*100

#still need to print everything
cat ("The ROI is: ", roi, "%\n")
cat ("The total profit/loss made is $", total_costs_proceeds, "\n")
cat ("The total amount of capital invested is $", inv_capital, "\n")

```

Over the 2021-2022 financial year (July 2021 - June 2022), the initial trading strategy returned a ROI of -30.82%, upon incurring a loss of $453,089.00. The revised 'Profit-Taking' trading strategy, at 40% during this period was much more successful, returning a ROI of -9.36%, after only incurring a loss of  $137,545.00. Despite implementing a relatively successful second trading strategy, it was still extremely difficult to profit by trading AMD stocks over this period, signifying an unsuccessful financial year for the company. This was largely due to the high competitive pressure the chipmaker faced, along with the global semiconductor shortage.

AMD faced intense competition from other semiconductor companies such as Intel and NVIDIA. During 2021-2022, these companies also launched competitive products, which may have influenced AMD's market share and sales.  Another significant factor which may have affected AMD and other tech companies during this period was the global semiconductor shortage. This shortage, driven by increased demand for electronics during the COVID-19 pandemic, production delays, and supply chain disruptions, impacted AMD's ability to meet customer demand and maintain sales growth.

The main reason that the revised trading strategy effectively cut the losses and ROI down to -9.36% successfully was due to the volatile nature of the stock price during this year. Selling when the stock price reached 140% of the average purchase price ensured that the program was able to protect itself to a large extent from the fluctuations in share price by selling at the high points. 



