#########################
# Annuity Calculator    #
#                       #
# Brian Hooper          #
# Heather McKinnon      #
# Divya Chandrika Kalla #
#                       #
# Advisors:             #
# Dr. Donald Davendra   #
# Dr. Chin-Mei Chueh    #
#                       #
# Central WA University # 
# CS 567                #
# 1-29-2019             #
#########################

library(ggplot2)
library(plotly)
library(plot3D)
library(car)
library(rgl)
library(reshape2) # for melting wide data into long data

mort_file <- "mortality.csv"
if(!file.exists(mort_file)) {
  stop("missing mortality table in file " + mort_file + "\n")
  quit(save = "no", status = 1)
} else if(!file.exists("simulation_input.csv")) {
  stop("missing input file \"simulation_input.csv\"\n")
} 

if(!dir.exists("output")) {
  dir.create("output")
}

# Read mortality data
mortality_data <- read.csv(file = mort_file, header = TRUE, sep = ",")
colnames(mortality_data) <- c("age", "mortality")
age = mortality_data[,1]
qx = mortality_data[,2]

# Read and assign input parameters
user_input <- read.csv(file="simulation_input.csv", header = TRUE, sep = ",")

if(length(user_input) < 1) {
  stop("user input file is empty\n")
} else if(ncol(user_input) != 8) {
  stop("user_input file is incorrectly formatted\n")
}  

for (input_index in 1:length(user_input$age_range_start)) {
  input_age_start = user_input[input_index,1]
  input_age_end = user_input[input_index,2]
  maturity_age = user_input[input_index,3]
  monthly_annuity = user_input[input_index,4]
  interest_rate = user_input[input_index,5]
  iterations = user_input[input_index,6]
  company_years = user_input[input_index,7]
  ROI_interest = user_input[input_index,8] 
  
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
  

  # Function for determining Whole Life Net Single Premium Policy Price
  #
  # @param in_age The input age for beginning the insurance policy
  # @param mat_age The age in which the policy matures
  # @return A double representing the Net Single Premium that was paid for the policy
  WNS_premium <- function(in_age, mat_age){
    xEy = (life_table$lx[mat_age + 1] / life_table$lx[in_age + 1]) * (1 / (1 + interest_rate)) ** (mat_age - in_age)
    return(monthly_annuity * 12 * (a12 * ax[mat_age + 1] - b12) * xEy)
  }
  
  # Function for determining the reserve at time_unit (year)
  #
  # @param in_age The input age for beginning the insurance policy
  # @param time_unit The unit of time (year) that describes the age of the policy
  # @return A double representing the reserve value for the policy at that unit of time
  WNS_reserve <- function(in_age, time_unit, mat_age){
    xEy = (life_table$lx[mat_age + 1] / life_table$lx[in_age + 1]) * (1 / (1 + interest_rate)) ** (mat_age - in_age)
    return(monthly_annuity * 12 * (a12 * ax[in_age + time_unit] - b12) * xEy)
  } 

  
  # Function for determining the profit over time for given interests
  #
  # @param interest The input interest rate
  # @return A list of calculated profits
  interests_profit <- function(interest){
    yearly_interest <- c((1 + (interest/12))**12)
    f_value         <- c(fund_table$fund_value[1])
    p               <- c(fund_table$profit[1])
    
    for (year in 2:nrow(fund_table)){
      yearly_interest <- c(yearly_interest, (1 + (interest/12))**12)
      f_value         <- c(f_value, (f_value[year - 1] * yearly_interest[year]) - fund_table$total_benefit_payout[year - 1])
      p               <- c(p, (f_value[year] - fund_table$total_reserve[year]))
    }
    return(p)
  }
  
  
  # Creating a table for the simulation data generated by the lifetimes simulations loop
  lifetimes <- data.frame(StartAge = integer(), 
                          MatAge = integer(), 
                          DeathAge = integer(),
                          PolicyCost = double(),
                          Reserve = double(),
                          benefitPayout = double(),
                          isDead = logical())
                            

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
                                         WNS_premium(input_age, maturity_age), 
                                         WNS_reserve(input_age, 0, maturity_age),
                                         0.0,   # benefits paid out
                                         FALSE) # isDead?
  } # End simulate lifetimes
  
  endTime <- Sys.time()
  elapsedTime <- round(as.numeric(difftime(time1 = endTime, time2 = startTime, units = "secs")), 2)
  cat(sprintf("Time elapsed for processing: %.2f seconds. \n\n", elapsedTime))

    
  # -------------------- Fund Value and Profit Simulation ---------------------- #
  
  # Begin loop for creating a fund and profit value
  cat(sprintf("Beginning calculating aggregate account value for %s years...\n", company_years))
  startTime <- Sys.time()
  
  # Use generated lifetimes to project fund value and profit
  fund_policies <- lifetimes
  
  # Assign year zero values
  year_num              <- c(0)
  total_reserve         <- c(sum(fund_policies$Reserve))
  num_payouts           <- c(0)
  total_benefit_payout  <- c(0)
  yearly_ROI            <- c(1 + (ROI_interest/12)**12)
  fund_value            <- c(sum(fund_policies$PolicyCost) * yearly_ROI[1])
  profit                <- c(fund_value[1] - total_reserve[1])
  accumulated_deaths    <- c(0)
  unmatured_policies    <- c(0)
  
  # Begin profit simulation loop
  for (year in 2:(company_years + 1)){
      
    # For each year, calculate the benefit payout value and reserve
    # Also keeps track of number of payouts, total deaths, and unmatured policies
    one_year_payout = 0
    payouts = 0
    deaths = 0
    unmatured = 0
    for (j in 1:nrow(fund_policies)){
      matured = (fund_policies$StartAge[j] + year - 1 > fund_policies$MatAge[j])      # has the policy matured?
      dead = (fund_policies$StartAge[j] + year - 1 > fund_policies$DeathAge[j])       # has the policy holder died?
      
      # if not matured and not dead
      if(isFALSE(matured) && isFALSE(dead)) {
        fund_policies$Reserve[j] = WNS_reserve(fund_policies$StartAge[j], year - 1, fund_policies$MatAge[j])
        unmatured = unmatured + 1
        fund_policies$benefitPayout[j] = 0.0
      }
      
      # if matured and not dead
      else if(isTRUE(matured) && isFALSE(dead)) {                                                        
        one_year_payout = 12 * monthly_annuity
        fund_policies$benefitPayout[j] = one_year_payout
        fund_policies$Reserve[j] = WNS_reserve(fund_policies$StartAge[j], year - 1,  fund_policies$MatAge[j])
        payouts = payouts + 1
      }

      # if dead
      else if(isTRUE(dead)){
        deaths = deaths + 1
        fund_policies$isDead[j] = TRUE
        fund_policies$Reserve[j] = 0.0
        fund_policies$benefitPayout[j] = 0.0
      }
    }

    year_num            <- c(year_num, year - 1)
    total_reserve       <- c(total_reserve, sum(fund_policies$Reserve))
    num_payouts         <- c(num_payouts, payouts)
    total_benefit_payout<- c(total_benefit_payout, sum(fund_policies$benefitPayout))
    yearly_ROI          <- c(yearly_ROI, (1 + (ROI_interest/12))**12)
    fund_value          <- c(fund_value, (fund_value[year - 1] * yearly_ROI[year]) - total_benefit_payout[year - 1])
    profit              <- c(profit, (fund_value[year] - total_reserve[year]))
    accumulated_deaths  <- c(accumulated_deaths, deaths)
    unmatured_policies  <- c(unmatured_policies, unmatured)
    
  
  } # End profit simulation loop
 
  # Add yearly values to fund_values table
  fund_table <- data.frame(year_num,
                           total_reserve,
                           num_payouts,
                           total_benefit_payout,
                           yearly_ROI,
                           fund_value,
                           profit,
                           accumulated_deaths,
                           unmatured_policies)
  
  endTime <- Sys.time()
  elapsedTime <- round(as.numeric(difftime(time1 = endTime, time2 = startTime, units = "secs")), 2)
  cat(sprintf("Time elapsed for processing: %s seconds. \n\n", elapsedTime))
  
  
  # ------------------------ Graphing and Saving Output ------------------------------ #

  cat(sprintf("Creating graphs and saving to output file...\n"))
  startTime <- Sys.time()
  
  # Graphing a line plot of the profit and fund values over time
  long_fund_data <- melt(fund_table, id.vars = "year_num", 
                         measure.vars = c("total_reserve","total_benefit_payout","fund_value", "profit"),
                         variable.name = "Variables")
  fund_line_plot <- ggplot(data = long_fund_data, aes(x = year_num, y = value, colour = Variables)) + 
    ggtitle("Monetary Value over Years") +
    labs(x = "Time (Years)", y = "Monetary Value") +
    theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), 
          axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14)) +
    scale_color_manual(labels = c("Reserve", "Benefit Payout", "Fund Value", "Profit"),
                       values = c("orange", "red", "deepskyblue2", "green")) + geom_line(size = 1) 
    print(fund_line_plot)
    dev.copy(png,filename = paste(path_name, "fund_line_plot.png", sep = ""))
    dev.off()
  
  # Make a profit matrix for 3D graphing
  interests <- seq(0.05, 0.055, 0.0005)
  yearSeq <- fund_table$year_num
  profit_matrix <- matrix(,nrow = length(yearSeq), ncol = length(interests)) # empty matrix
  column <- 1
  for (i in interests){
    profit_matrix[,column] <- interests_profit(i)
    column <- column + 1
  }
  
  # A wrapper to saveWidget which compensates for arguable BUG in
  # saveWidget which requires `file` to be in current working
  # directory from https://github.com/ramnathv/htmlwidgets/issues/299
  #
  # @param widget to be saved
  # @param filename to be saved under
  saveWidgetFix <- function (widget, filename) {
    wd<-getwd()
    on.exit(setwd(wd))
    outDir <- paste(wd, path_name, sep="/")
    file <- basename(filename)
    setwd(outDir);
    htmlwidgets::saveWidget(as_widget(widget), file = filename)
  }
  
  # 3D Surface plot 
  profit_3D_surface <- add_surface(plot_ly(x = interests, 
                                           y = yearSeq, 
                                           z = profit_matrix)) %>%
  layout(title = "Varying Interest Rates on Profit Over Time",
         scene = list(xaxis = list(title = 'Interest'),
                      yaxis = list(title = 'Year'),
                      zaxis = list(title = 'Profit')))
  print(profit_3D_surface)
  saveWidgetFix(profit_3D_surface, "profit_3D_surface.html")

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
    WNS_premium_data[i] <- WNS_premium(i, maturity_age)
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
   
  # Adjusting table data to show 2 decimal precision for monetary values
  lifetimes$PolicyCost <- format(round(lifetimes$PolicyCost, digits = 2), nsmall = 2)
  lifetimes$Reserve <- format(round(lifetimes$Reserve, digits = 2), nsmall = 2)
  fund_table$total_reserve <- format(round(fund_table$total_reserve, digits = 2), nsmall = 2)
  fund_table$total_benefit_payout <- format(round(fund_table$total_benefit_payout, digits = 2), nsmall = 2)
  fund_table$fund_value <- format(round(fund_table$fund_value, digits = 2), nsmall = 2)
  fund_table$profit <- format(round(fund_table$profit, digits = 2), nsmall = 2)
  
  write.csv(lifetimes, paste(path_name, "policies.csv", sep = ""), row.names = FALSE)
  write.csv(fund_table, paste(path_name, "profit_projections.csv", sep = ""), row.names = FALSE)
  
  endTime <- Sys.time()
  elapsedTime <- round(as.numeric(difftime(time1 = endTime, time2 = startTime, units = "secs")), 2)
  cat(sprintf("Time elapsed for processing: %.2f seconds. \n\n", elapsedTime))
}
