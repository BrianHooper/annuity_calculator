#########################
# Annuity Calculator    #
#                       #
# Brian Hooper          #
# Heather McKinnon      #
# Kalla Divya Chandrika #
# CS567 - 1-29-2019     #
#########################

library(ggplot2)

# Read mortality data
mortality_data <- read.csv(file="DrewHeblerMortality.csv", header=TRUE, sep=",")
age = mortality_data[,1]
qx = mortality_data[,2]

# Read and assign input parameters
user_input <- read.csv(file="input.csv", header=TRUE, sep=",")
input_age_start = user_input[,1]
input_age_end = user_input[,2]
maturity_age = user_input[,3]
monthly_annuity = user_input[,4]
interest_rate = user_input[,5]
term_length = user_input[,6]
iterations = user_input[,7]

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

# ------------------ Functions ---------------------------------

# Function for determining Whole Life Net Single Premium Profit for company
WNS_profit <- function(in_age, mat_age){
  xEy = (lx[mat_age + 1] / lx[in_age + 1]) * (1 / (1 + interest_rate)) ** (mat_age - in_age)
  return(monthly_annuity * 12 * (a12 * ax[mat_age + 1] - b12) * xEy)
  
}

# Function for determining Whole Life Net Single Premium loss for company
# Occurs only when death_age > maturity_age
WNS_loss <- function(mat_age, death_age)
  return ((death_age - mat_age) * monthly_annuity * 12)
# if person died before or after maturity age

# Calculate net profit or loss for Whole Life Net Single Premium
WNS_net_profit <- function(in_age, mat_age, death_age){
  if (death_age < maturity_age){
#    profit = ((input_age - dead_age) * desired_monthly_benefit)
    return (WNS_profit(in_age, mat_age))
  }
  else{
#    loss = ((dead_age - maturity_age) * desired_monthly_benefit)
    return (WNS_profit(in_age, mat_age) - WNS_loss(mat_age, death_age))
  }
}

#------------------ Begin graphing -------------------------------

# Graphing age effect on mortality
age_qx_plot <- ggplot(life_table, aes(age, qx))
age_qx_plot + ggtitle("Age Effect on Percent Mortality (qx)") + geom_point(aes(age, qx), colour="#3366FF", size=1)

# Graphing ax on age
age_ax_plot <- ggplot(life_table, aes(age, ax))
age_ax_plot + ggtitle("Age Effect on Annuity (ax)") + geom_point(aes(age, ax), colour="#3366FF", size=1)

  # TODO Graphing profit trends after each life that is complete - need simulation data
  
  # Graphing increasing input age with a maturity age of 65 and monthly benefit of $1000
  # for (i in 1:99){
  #   age_benefit_plot <-
  # }

profit <- 0
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

  # Calculate profit
  profit <- profit + WNS_net_profit(input_age, maturity_age, death_age)
}
print(profit)


