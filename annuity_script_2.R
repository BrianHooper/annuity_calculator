#########################
# Annuity Calculator    #
#                       #
# Brian Hooper          #
# Heather McKinnon      #
# Divya Chandrika Kalla #
# CS567 - 1-29-2019     #
#########################

library(ggplot2)
library(plotly)

if(!file.exists("USLifeTables1999-2000TotalPopulationANB_TableNo2023.csv")) {
  stop("missing mortality table in file \"USLifeTables1999-2000TotalPopulationANB_TableNo2023.csv\"\n")
  quit(save = "no", status = 1)
} else if(!file.exists("lifetimes_input.csv")) {
  stop("missing input file \"lifetimes_input.csv\"\n")
} else if(!file.exists("fund_values_input.csv")) {
  stop("missing roi input file \"fund_values_input.csv\"\n")
}

if(!dir.exists("output")) {
  dir.create("output")
}

# Read mortality data
mortality_data <- read.csv(file = "USLifeTables1999-2000TotalPopulationANB_TableNo2023.csv", header = TRUE, sep = ",")
colnames(mortality_data) <- c("age", "mortality")
age = mortality_data[,1]
qx = mortality_data[,2]

# Read and assign input parameters
user_input <- read.csv(file="lifetimes_input.csv", header = TRUE, sep = ",")
ROI_input <- read.csv(file="fund_values_input.csv", header = TRUE, sep = ",")

if(length(user_input) < 1) {
  stop("user input file is empty\n")
} else if(length(user_input) < length(ROI_input)) {
  stop("ROI input file has too many rows\n")
} else if(ncol(user_input) != 6) {
  stop("user_input file is incorrectly formatted\n")
} else if(ncol(ROI_input) != 4) {
  stop("user_input file is incorrectly formatted\n")
}

roi_index = 1
for (input_index in 1:length(user_input$age_range_start)) {
  input_age_start = user_input[input_index,1]
  input_age_end = user_input[input_index,2]
  maturity_age = user_input[input_index,3]
  monthly_annuity = user_input[input_index,4]
  interest_rate = user_input[input_index,5]
  iterations = user_input[input_index,6]
  
  # Read ROI input data to for projection of company yearly profits
  company_years = ROI_input[roi_index,1]         
  ROI_interest = ROI_input[roi_index,2]         
  reinvestment_percent = ROI_input[roi_index,3] 
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
  
  # Function for determining the reserve at time, time_unit. E.g. first year of policy is time 1, second year is time 2, etc.
  #
  # @param in_age The input age for beginning the insurance policy
  # @param mat_age The age in which the policy matures
  # @param time_unit The unit of time (usually year) that describes the age of the policy
  # @return A double representing the Net Single Premium that was paid for the policy
  WNS_reserve <- function(in_age, mat_age, time_unit){
    xEy = (life_table$lx[mat_age + time_unit] / life_table$lx[in_age + time_unit]) * (1 / (1 + interest_rate)) ** (mat_age - in_age)
    return(monthly_annuity * 12 * (a12 * ax[mat_age + time_unit] - b12) * xEy)
  }
  
  
  # Function for determining Whole Life Net Single Premium Profit for company, aka policy premium price
  #
  # @param in_age The input age for beginning the insurance policy
  # @param mat_age The age in which the policy matures
  # @return A double representing the Net Single Premium that was paid for the policy
  WNS_profit <- function(in_age, mat_age){
    xEy = (life_table$lx[mat_age + 1] / life_table$lx[in_age + 1]) * (1 / (1 + interest_rate)) ** (mat_age - in_age)
    return(monthly_annuity * 12 * (a12 * ax[mat_age + 1] - b12) * xEy)
  }
  
  # Function for determining Whole Life Net Single Premium loss for company, aka benefit payout
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
              input_age_start, maturity_age, monthly_annuity, WNS_profit(input_age_start,maturity_age)))
  
  
  # Creating a table for the simulation data generated by the lifetimes simulations loop
  lifetimes <- data.frame(StartAge = integer(), 
                          MatAge = integer(), 
                          DeathAge = integer(),
                          policyAge = integer(),
                          PolicyCost = double(),
                          Reserve = double(),
                          benefitPayout = double())
                            

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
    lifetimes[nrow(lifetimes) + 1,] <- c(input_age, 
                                         maturity_age, 
                                         death_age,
                                         0, # policy age
                                         WNS_profit(input_age, maturity_age), 
                                         WNS_reserve(input_age, maturity_age, 1),
                                         0.0) # benefits paid out
  } # End simulate lifetimes
  
  endTime <- Sys.time()
  elapsedTime = endTime - startTime
  cat(sprintf("Time elapsed for processing: %.2f seconds. \n\n", elapsedTime))
  
  
  # Draws random sample from the simulated lifetimes above
  policies <- lifetimes[sample(nrow(lifetimes), policy_sales_goal),]
  
  # Begin loop for creating random starting policy holders for fund_value simulation
  cat(sprintf("Begin generating random starting policy holders for %s years...\n", company_years))
  startTime <- Sys.time()
  
  for (i in 2:company_years){
    # checking the current age of the policy holder (if mature, and the policy holder is not dead yet)
    one_year_payout = 0
    for (j in 1:nrow(policies)){
      matured = (policies$StartAge[j] + policies$policyAge[j] > policies$MatAge[j])       # has the policy matured?
      dead = (policies$StartAge[j] + policies$policyAge[j] > policies$DeathAge[j])        # has the policy holder died?
      
      # if matured and not dead
      if (isTRUE(matured) && isFALSE(dead)) {                                                        
        one_year_payout = 12 * monthly_annuity
        policies$benefitPayout[j] = one_year_payout
        policies$Reserve[j] = WNS_reserve(policies$StartAge[j], policies$MatAge[j], company_years + 1)
      }
      
      # if not matured and not dead
      else if(isFALSE(matured) && isFALSE(dead)) {
        policies$Reserve[j] = WNS_reserve(policies$StartAge[j], policies$MatAge[j], company_years + 1)
        policies$benefitPayout[j] = 0.0
      }
      
      # if dead
      else if(isTRUE(dead)){
        policies$Reserve[j] = 0.0
        policies$benefitPayout[j] = 0.0
      }
    
    }
    
    # add new policies sold
    new_policies <- lifetimes[sample(nrow(lifetimes),policy_sales_goal),]
    policies <- rbind(policies, new_policies)
    policies$policyAge <- policies$policyAge + 1 # increment policy ages

  } # End creating random starting policy holders for fund_value simulation
  
  endTime <- Sys.time()
  elapsedTime = endTime - startTime
  cat(sprintf("Time elapsed for processing: %.2f seconds. \n\n", elapsedTime))

  
  # -------------------- WIP - Fund Value Function ------------------- #
  
  # Begin loop for creating a fund value at time t table
  cat(sprintf("Beginning calculating aggregate account value...\n"))
  startTime <- Sys.time()
  
  # Take random sample from policies from random starting policy holders created
  # These policies have "policy ages" which we can use to have some mature policies at time zero
  # TODO have user input of number of starting fund_policies?
  fund_policies <- policies[sample(nrow(policies), 100),]
  
  year_num              <- c(0)
  num_premiums_sold     <- c(0)
  premium_sold_value    <- c(0)
  total_reserve         <- c(0)
  num_payouts           <- c(0)
  total_benefit_payout  <- c(0)
  yearly_ROI            <- c(0)
  yearly_ATP            <- c(0) # ATP is Accumulated Total Premium
  ATP_plus_ROI          <- c(0)
  fund_value            <- c(1000000) # TODO, what should the starting funds be?
  profit                <- c(0)
  accumulated_deaths    <- c(0)
  unmatured_policies    <- c(0)
  
  # TODO, have user input for number of years?
  years = 25
  
  for (year in 2:years){
      
    # For each year, calculate the benefit payout value and reserve
    # Also keeps track of number of payouts, total deaths, and unmatured policies
    one_year_payout = 0
    payouts = 0
    deaths = 0
    unmatured = 0
    for (j in 1:nrow(fund_policies)){
      matured = (fund_policies$StartAge[j] + fund_policies$policyAge[j] > fund_policies$MatAge[j])      # has the policy matured?
      dead = (fund_policies$StartAge[j] + fund_policies$policyAge[j] > fund_policies$DeathAge[j])       # has the policy holder died?
      
      # if not matured and not dead
      if(isFALSE(matured) && isFALSE(dead)) {
        fund_policies$Reserve[j] = WNS_reserve(fund_policies$StartAge[j], fund_policies$MatAge[j], year + 1)
        unmatured = unmatured + 1
        fund_policies$benefitPayout[j] = 0.0
      }
      
      # if matured and not dead
      else if(isTRUE(matured) && isFALSE(dead)) {                                                        
        one_year_payout = 12 * monthly_annuity
        fund_policies$benefitPayout[j] = one_year_payout
        fund_policies$Reserve[j] = WNS_reserve(fund_policies$StartAge[j], fund_policies$MatAge[j], year + 1)
        payouts = payouts + 1
      }

      # if dead
      else if(isTRUE(dead)){
        deaths = deaths + 1
        fund_policies$Reserve[j] = 0.0
        fund_policies$benefitPayout[j] = 0.0
      }
    }
    
    # Sell n premiums per year, add to fund_policies table
    # TODO do not "sell* more policies, remove this 
    # need to simulated "worst case" of not selling any more policies each year
    new_policies <- lifetimes[sample(nrow(lifetimes), policy_sales_goal),]
    fund_policies <- rbind(fund_policies, new_policies)

    year_num            <- c(year_num, year - 1)
    num_premiums_sold   <- c(num_premiums_sold, policy_sales_goal)
    premium_sold_value  <- c(premium_sold_value, sum(new_policies$PolicyCost))
    total_reserve       <- c(total_reserve, sum(fund_policies$Reserve))
    num_payouts         <- c(num_payouts, payouts)
    total_benefit_payout<- c(total_benefit_payout, sum(fund_policies$benefitPayout))
    yearly_ROI          <- c(yearly_ROI, 1 + ROI_interest)
    yearly_ATP          <- c(yearly_ATP, (fund_value[year - 1] + premium_sold_value[year])) # ATP is Accumulated Total Premium
    ATP_plus_ROI        <- c(ATP_plus_ROI, (yearly_ATP[year] * yearly_ROI[year]**12))
    fund_value          <- c(fund_value, (ATP_plus_ROI[year] - total_benefit_payout[year]))
    profit              <- c(profit, (fund_value[year] - total_reserve[year]))
    accumulated_deaths  <- c(accumulated_deaths, deaths)
    unmatured_policies  <- c(unmatured_policies, unmatured)
    
    # Increment policy ages after 1 year
    fund_policies$policyAge <- fund_policies$policyAge + 1 # increment policy ages   
  
  } # End num years
 
  # Add monthly values to fund_values table
  fund_table <- data.frame(year_num,
                           num_premiums_sold,
                           premium_sold_value,
                           total_reserve,
                           num_payouts,
                           total_benefit_payout,
                           yearly_ROI,
                           yearly_ATP,
                           ATP_plus_ROI,
                           fund_value,
                           profit,
                           accumulated_deaths,
                           unmatured_policies)
  
  endTime <- Sys.time()
  elapsedTime = endTime - startTime
  cat(sprintf("Time elapsed for processing: %.2f seconds. \n\n", elapsedTime))

  # Graphing fund value over time
  # NOTE: to add multiple lines, convert from wide to long data (use melt())
  fund_plot <- ggplot(fund_table, aes(year_num)) + 
    ggtitle("Monetary Value over Years") +
    labs(x = "Time (Years)", y = "Monetary Value") +
    theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14)) +
    geom_point(aes(y = profit), colour = "deepskyblue3", size = 1) + 
    geom_point(aes(y = fund_value), colour = "red", size = 1) + 
    geom_point(aes(y = total_benefit_payout), colour = "green", size = 1) + 
    geom_point(aes(y = premium_sold_value), colour = "orange", size = 1)
    
  print(fund_plot)
  
  # TODO make graph (like one above) with a legend 
  # Need to convert wide data to long before graphing
  # subset necessary columns
  #fund_table_sub <- fund_table[,3, 6, 10, 11]
 
  
  # # 3D Surface
  # p <- add_surface(plot_ly(x = fund_table$monthly_ATP, y = fund_table$premium_sold_value, z = cbind(fund_table$fund_value, fund_table$fund_value))) %>%
  # layout(scene = list(xaxis = list(title = 'Accumulated Total Premium'),
  #                     yaxis = list(title = 'Premiums Sold Value'),
  #                     zaxis = list(title = 'Fund Value')))
  # print(p)
  # 
  # # 3D Scatter
  # fund3D<-data.frame(ATP = as.factor(fund_table$monthly_ATP),
  #                    premiums = as.factor(fund_table$premium_sold_value),
  #                    fundValue = as.factor(fund_table$fund_value))
  # r <- plot_ly(fund3D, x = fund3D$ATP, y = fund3D$premiums, z = fund3D$fundValue) %>%
  #   add_markers() %>%
  #   layout(scene = list(xaxis = list(title = 'Accumulated Total Premium'),
  #                       yaxis = list(title = 'Premiums Sold Value'),
  #                       zaxis = list(title = 'Fund Value')))
  # print(r)
  # 
  # #htmlwidgets::saveWidget(as_widget(p), "Scattered3DFundValues.html")
  # 
  # # 3D with plot3D
  # library(plot3D)
  
  
  # -------------------- END WIP ------------------------------------- #
  
  
  #------------------------- Graphing -------------------------------
  # Graphing age effect on mortality
  age_qx_plot <- ggplot(life_table, aes(age, qx)) + 
    ggtitle("Age Effect on Percent Mortality (qx)") +
    labs(x = "Age", y = "Mortality (qx)") +
    theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14)) +
    geom_point(aes(age, qx), colour = "deepskyblue3", size = 1)
  print(age_qx_plot) 
  dev.copy(png,filename = paste(path_name, "age_mortality.png", sep = ""))
  dev.off()
  
  # Graphing ax on age
  age_ax_plot <- ggplot(life_table, aes(age, ax)) + 
    ggtitle("Age Effect on Annuity (ax) Expected Present Value") +
    labs(x = "Age", y = "Annuity (ax)") +
    theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14)) +
    geom_point(aes(age, ax), colour = "deepskyblue3", size = 1)
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
    geom_point(aes(WNS_age_data, WNS_premium_data), colour = "deepskyblue3", size = 1) 
  print(age_premium_plot) 
  dev.copy(png,filename = paste(path_name, "age_premium.png", sep = ""))
  dev.off()
  
  # Histogram of deaths in the simulated lifetimes
  hist.death <- ggplot(lifetimes, aes(DeathAge)) + 
    ggtitle(paste("Age of Deaths over", iterations, "Lifetimes")) +
    labs(x = "Age of Death", y = "Frequency") +
    theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14)) +
    geom_histogram(binwidth = 1, aes(y = ..count..), colour = "black", fill = "deepskyblue2")
  print(hist.death)
  dev.copy(png,filename = paste(path_name, "hist_death.png", sep = ""))
  dev.off()
  
  # # Yearly ROI adjusted profits
  # ROI_plot <- ggplot(ROI_tracker, aes(year, ROI_adjusted_profit)) + 
  #   ggtitle("Projected ROI Adjusted Gross Income") +
  #   labs(x = "Time (Years)", y = "Yearly Profit (Dollars)") +
  #   theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14)) +
  #   geom_point(aes(year, ROI_adjusted_profit), colour = "deepskyblue3", size = 1)
  # print(ROI_plot)
  # dev.copy(png,filename = paste(path_name, "roi_income.png", sep = ""))
  # dev.off()
  
  # # Adjusting table data to show 2 decimal precision for monetary values
  # lifetimes$PolicyCost <- format(round(lifetimes$PolicyCost, digits = 2), nsmall = 2)
  # lifetimes$GrossProfit <- format(round(lifetimes$GrossProfit, digits = 2), nsmall = 2)
  # ROI_tracker[,2:6] <- format(round(ROI_tracker[,2:6], digits = 2), nsmall = 2)
  # 
  # write.csv(lifetimes, paste(path_name, "policies.csv", sep = ""), row.names = FALSE)
  # write.csv(ROI_tracker, paste(path_name, "profit_projections.csv", sep = ""), row.names = FALSE)
}
