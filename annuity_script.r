#########################
# Annuity Calculator    #
#                       #
# Brian Hooper          #
# Heather McKinnon      #
# Divya Chandrika Kalla #
# CS567 - 1-29-2019     #
#########################

library(ggplot2)
#library(xlsx)

if(!file.exists("mortality.csv")) {
  stop("missing mortality table in file \"mortality.csv\"\n")
  quit(save = "no", status = 1)
} else if(!file.exists("input.csv")) {
  stop("missing input file \"input.csv\"\n")
} else if(!file.exists("ROI_input.csv")) {
  stop("missing roi input file \"ROI_input.csv\"\n")
}

if(!dir.exists("output")) {
  dir.create("output")
}

# Read mortality data
mortality_data <- read.csv(file = "mortality.csv", header = TRUE, sep = ",")
colnames(mortality_data) <- c("age", "mortality")
age = mortality_data[,1]
qx = mortality_data[,2]

# Read and assign input parameters
user_input <- read.csv(file="input.csv", header = TRUE, sep = ",")
ROI_input <- read.csv(file="ROI_input.csv", header = TRUE, sep = ",")
# colnames(user_input, c("input_age_start", "input_age_end", "maturity_age", "monthly_annuity", "interest_rate", "term_length", "iterations"))
# colnames(ROI_input, c("company_years", "ROI_interest", "investment_percent", "policy_sales_goal"))

if(length(user_input) < 1) {
  stop("user input file is empty\n")
} else if(length(user_input) < length(ROI_input)) {
  stop("ROI input file has too many rows\n")
} else if(ncol(user_input) != 7) {
  stop("user_input file is incorrectly formatted\n")
} else if(ncol(ROI_input) != 4) {
  stop("user_input file is incorrectly formatted\n")
}

roi_index = 1
for (input_index in 1:length(user_input$input_age_start)) {
  input_age_start = user_input[input_index,1]
  input_age_end = user_input[input_index,2]
  maturity_age = user_input[input_index,3]
  monthly_annuity = user_input[input_index,4]
  interest_rate = user_input[input_index,5]
  iterations = user_input[input_index,7]

  # Read ROI input data to for projection of company yearly profits
  company_years = ROI_input[roi_index,1]         
  ROI_interest = ROI_input[roi_index,2]         
  investment_percent = ROI_input[roi_index,3] 
  policy_sales_goal = ROI_input[roi_index,4]  # sales goal for number of policies sold per year
  
  if(roi_index < length((ROI_input$company_years))) {
    roi_index = roi_index + 1
  }
  
  path_name = paste("output/output_", input_index, "/", sep="")
  if(!dir.exists(path_name)) {
    dir.create(paste(path_name))
  }
  
  # Calculated initial variables
  d = interest_rate / (1 + interest_rate)
  im = 12 * (((1 + interest_rate) ** (1 / 12)) - 1)
  dm = 12 * (1 - (1 - d) ** (1 / 12))
  a12 = (interest_rate * d) / (im * dm)
  b12 = (interest_rate - im) / (im * dm)
  
  # Create life table
  life_table <- data.frame(age, qx)
  
  # Add k (curtate lifetime)
  life_table$k <- 1 + life_table$age
  
  # Calculate lx and dx
  life_table$lx[1] <- 10000000
  life_table$dx[1] <- life_table$lx[1] * life_table$qx[1]
  for (i in 2:length(life_table$qx)) {
    life_table$lx[i] <- life_table$lx[i-1] - life_table$dx[i-1]
    life_table$dx[i] <- life_table$lx[i] * life_table$qx[i]
    
  }
  
  # Add Px
  life_table$Px <- 1 - life_table$qx
  
  # Calculate v^k
  life_table$vk <- 1/(1 + interest_rate)^life_table$k
  
  # Calculate kPx, x=0
  life_table$kPx[1] <- life_table$Px[1]
  for (i in 2:length(life_table$Px)) {
    life_table$kPx[i] <- life_table$kPx[i-1] * life_table$Px[i]
  }
  
  # Calculate kEx, x=0
  life_table$kEx <- life_table$vk * life_table$kPx
  
  # Calculate ax
  life_table$ax[1] <- 1 + sum(life_table$kEx)
  for (i in 2:length(life_table$kEx)) {
    life_table$ax[i] <- 1 + sum(life_table$kEx[i:length(life_table$kEx)])/life_table$kEx[i-1]
  }
  
  # Calculate Ax
  d = interest_rate/(1 + interest_rate)
  life_table$Ax <- 1 - d * life_table$ax
  
  # Assigning variables for ease of use
  ax = life_table$ax
  Ax = life_table$Ax
  
  # Function for determining Whole Life Net Single Premium Profit for company
  #
  # @param in_age The input age for beginning the insurance policy
  # @param mat_age The age in which the policy matures
  # @return A double representing the Net Single Premium that was paid for the policy
  WNS_profit <- function(in_age, mat_age){
    xEy = (life_table$lx[mat_age + 1] / life_table$lx[in_age + 1]) * (1 / (1 + interest_rate)) ** (mat_age - in_age)
    return(monthly_annuity * 12 * (a12 * ax[mat_age + 1] - b12) * xEy)
  }
  
  # Function for determining Whole Life Net Single Premium loss for company
  # Occurs only when death_age > maturity_age
  # 
  # @param mat_age The age in which the policy matures
  # @param death_age The age in which the policy holder dies
  # @return A double representing the total paid out to the client for the policy
  WNS_loss <- function(mat_age, death_age)
    return ((death_age - mat_age) * monthly_annuity * 12)
  
  # Calculate gross profit or loss for Whole Life Net Single Premium
  #
  # @param in_age The input age for beginning the insurance policy
  # @param mat_age The age in which the policy matures
  # @param death_age The age in which the policy holder dies
  # @return A double representing the gross profit or loss for a single policy holder
  WNS_gross_profit <- function(in_age, mat_age, death_age){
    profit <- WNS_profit(in_age, mat_age)
    if (death_age <= maturity_age){
      # cat(sprintf("input age: %s death age: %s Profit: %.2f\n", in_age, death_age, profit))
      return (profit)
    }
    else{
      loss <- WNS_loss(mat_age, death_age)
      # cat(sprintf("input age: %s death age: %s Profit: %.2f loss: %.2f net: %.2f\n", in_age, death_age, profit, loss, (profit - loss)))
      return (profit - loss)
    }
  }
  
  # Display a single net premium price for user defined start and maturity age
  cat(sprintf("A sample whole life single net premium price for input age %s with maturity age %s and $%.2f monthly benefit: $%.2f\n\n",
              input_age_start, maturity_age, monthly_annuity,WNS_profit(input_age_start,maturity_age)))
  
  
  # Creating a table for the simulation data generated by the lifetimes simulations loop
  policy_table <- data.frame(StartAge = integer(), 
                             MatAge = integer(), 
                             DeathAge = integer(),
                             PolicyCost = double(),
                             isEarlyDeath = logical(),
                             GrossProfit = double())
  
  
  # Begin simulate a number of lifetimes (iterations)
  cat(sprintf("Beginning simulation of %s lifetimes...\n", iterations))
  startTime <- Sys.time()
  
  for(i in 1:iterations) {
    # Generate random integer starting age
    if(input_age_start >= input_age_end) {
      input_age = input_age_start
    } else {
      input_age = sample(input_age_start:input_age_end, 1)
    }
    
    # Pick a random death date based on mortality table
    death_age = input_age
    while(death_age < length(mortality_data$mortality) && runif(1, 0.0, 1.0) > mortality_data$mortality[death_age]) {
      death_age = death_age + 1  
    }
    
    # Add the simulated lifetime to policy table
    policy_table[nrow(policy_table)+1,] <- c(input_age, 
                                             maturity_age, 
                                             death_age, 
                                             WNS_profit(input_age, maturity_age), 
                                             (death_age <= maturity_age), 
                                             WNS_gross_profit(input_age, maturity_age, death_age))
  } # End simulate lifetimes
  
  endTime <- Sys.time()
  elapsedTime = endTime - startTime
  cat(sprintf("Time elapsed for processing: %.2f seconds. \n\n", elapsedTime))
  
  # Set year 0 policy sales, draws random sample from the simulated lifetimes above
  policies <- policy_table[sample(nrow(policy_table),policy_sales_goal),]
  policies$policyAge <- c(0)
  
  # Set variables for ROI simulation
  initial_profit <- sum(policies$PolicyCost) # sum of sales of all policies purchased in year 0
  year <- c(0)
  total_loss <- c(0)
  invested <- c(initial_profit * investment_percent)
  ROI <- c(0)
  sold_policies <- c(initial_profit)
  ROI_adjusted_profit <- c(initial_profit)
  
  # Begin loop for creating an ROI adjusted profit table
  cat(sprintf("Beginning projected profits for the next %s years...\n", company_years))
  startTime <- Sys.time()
  
  for (i in 2:company_years){
    # checking the current age of the policy holder (if mature, and the policy holder is not dead yet)
    one_year_loss = 0
    for (j in 1:nrow(policies)){
      if (isTRUE(policies$StartAge[j] + policies$policyAge[j] > policies$MatAge[j]) && isTRUE(policies$StartAge[j] + policies$policyAge[j] < policies$DeathAge[j])) {
        loss <- WNS_loss(policies$MatAge[j], policies$DeathAge[j])
        one_year_loss = one_year_loss + loss
      }
    }
    
    # add new policies sold
    new_policies <- policy_table[sample(nrow(policy_table),policy_sales_goal),]
    new_policies$policyAge <- c(0)
    policies <- rbind(policies,new_policies)
    policies$policyAge <- policies$policyAge + 1 # increment policy ages
    
    # concatenate data from loop to ROI variables
    year <- c(year, i-1)
    total_loss <- c(total_loss, one_year_loss)
    sold_policies <- c(sold_policies, sum(new_policies$PolicyCost))
    ROI <- c(ROI, invested[i-1] * ROI_interest) 
    # This reinvests the ROI for the year and the investment_percent value of the policies_sale_goal sold for the year 
    invested <- c(invested, ROI[i] + invested[i-1] + (sold_policies[i] * investment_percent) )
    ROI_adjusted_profit <- c(ROI_adjusted_profit, (invested[i] + sold_policies[i] - (sold_policies[i] * investment_percent) - one_year_loss))
  } # End yearly profit projections
  
  endTime <- Sys.time()
  elapsedTime = endTime - startTime
  cat(sprintf("Time elapsed for processing: %.2f seconds. \n\n", elapsedTime))
  
  # Add ROI variables to data frame
  ROI_tracker <- data.frame(year, total_loss, ROI, invested, sold_policies, ROI_adjusted_profit)
  

  #------------------------- Graphing -------------------------------
  # Graphing age effect on mortality
  age_qx_plot <- ggplot(life_table, aes(age, qx)) + 
    ggtitle("Age Effect on Percent Mortality (qx)") +
    labs(x = "Age", y = "Mortality (qx)") +
    theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14)) +
    geom_point(aes(age, qx), colour = "deepskyblue1", size = 1)
  print(age_qx_plot) 
  dev.copy(png,filename = paste(path_name, "age_mortality.png", sep = ""))
  dev.off()
  
  # Graphing ax on age
  age_ax_plot <- ggplot(life_table, aes(age, ax)) + 
    ggtitle("Age Effect on Annuity (ax) Expected Present Value") +
    labs(x = "Age", y = "Annuity (ax)") +
    theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14)) +
    geom_point(aes(age, ax), colour = "deepskyblue1", size = 1)
  print(age_ax_plot) 
  dev.copy(png,filename = paste(path_name, "age_annuity.png", sep = ""))
  dev.off()
  
  # Graphing increasing age with the user-defined maturity age and monthly benefit of Net Single Premium Prices
  WNS_age_data <- age[1:maturity_age]
  WNS_premium_data <- vector(mode = "double", length = length(WNS_age_data))
  for (i in 1:length(WNS_age_data)){
    WNS_premium_data[i] <- WNS_profit(i, maturity_age)
  }
  age_premium_plot <- ggplot(x = WNS_age_data, y = WNS_premium_data) + 
    ggtitle(paste("Premium prices from age 1 through", length(WNS_age_data),"with\nmaturity age", maturity_age,"and $", monthly_annuity,"monthly benefit")) +
    labs(x = "Age", y = "Whole Life Net Single Premium Price") +
    theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14)) +
    geom_point(aes(WNS_age_data, WNS_premium_data), colour = "deepskyblue1", size = 1) 
  print(age_premium_plot) 
  dev.copy(png,filename = paste(path_name, "age_premium.png", sep = ""))
  dev.off()
  
  # Histogram of deaths in the simulated lifetimes
  hist.death <- ggplot(policy_table, aes(DeathAge)) + 
    ggtitle(paste("Age of Deaths over", iterations, "Lifetimes")) +
    labs(x = "Age of Death", y = "Frequency") +
    theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14)) +
    geom_histogram(binwidth = 1, aes(y = ..count..), colour = "black", fill = "deepskyblue1")
  print(hist.death)
  dev.copy(png,filename = paste(path_name, "hist_death.png", sep = ""))
  dev.off()
  
  # Yearly ROI adjusted profits
  ROI_plot <- ggplot(ROI_tracker, aes(year, ROI_adjusted_profit)) + 
    ggtitle("Projected ROI Adjusted Gross Income") +
    labs(x = "Time (Years)", y = "Yearly Profit (Dollars)") +
    theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14)) +
    geom_point(aes(year, ROI_adjusted_profit), colour = "deepskyblue1", size = 1)
  print(ROI_plot)
  dev.copy(png,filename = paste(path_name, "roi_.png", sep = ""))
  dev.off()
  
  policy_table$PolicyCost <- format(round(policy_table$PolicyCost, digits = 2), nsmall = 2)
  policy_table$GrossProfit <- format(round(policy_table$GrossProfit, digits = 2), nsmall = 2)
  ROI_tracker[,4:6] <- format(round(ROI_tracker[,3:6], digits = 2), nsmall = 2)
  
  write.csv(policy_table, paste(path_name, "policies.csv", sep = ""), row.names = FALSE)
  write.csv(ROI_tracker, paste(path_name, "profit_projections.csv", sep = ""), row.names = FALSE)
}