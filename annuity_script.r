# Read mortality data
mortality_data <- read.csv(file="DrewHeblerMortality.csv", header=TRUE, sep=",")
age = mortality_data[,1]
qx = mortality_data[,2]

#lx = mortality_data[,1]
#ax = mortality_data[,2]
#Ax = mortality_data[,3]

# Read and assign input parameters
user_input <- read.csv(file="input.csv", header=TRUE, sep=",")
input_age = user_input[,1]
maturity_age = user_input[,2]
monthly_annuity = user_input[,3]
interest_rate = user_input[,4]
term_length = user_input[,5]

# Calculated constants
d = interest_rate / (1 + interest_rate)
im = 12 * (((1 + interest_rate) ** (1 / 12)) - 1)
dm = 12 * (1 - (1 - d) ** (1 / 12))
a12 = (interest_rate * d) / (im * dm)
b12 = (interest_rate - im) / (im * dm)



# ----------------- Equations added 1/27/19 ----------------------

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
# 
#for (i in 1:length(life_table$lx)) {
#  life_table$Px[i] <- life_table$lx[i] / life_table$lx[i-1]
#}

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
aX = (1 - life_table$Ax[input_age + 1]) / d
lx = life_table$lx
xEy = (lx[maturity_age + 1] / lx[input_age + 1]) * (1 / (1 + interest_rate)) ** (maturity_age - input_age)
yEyn = (lx[maturity_age + term_length + 1] / lx[maturity_age + 1]) * (1 / (1 + interest_rate)) ** term_length

#------------------ End added equations ---------------------

# Output
whole_net_single = monthly_annuity * 12 * (a12 * ax[maturity_age + 1] - b12) * xEy
whole_annual_premium = whole_net_single / (aX - ax[maturity_age + 1] * xEy)
whole_monthly_premium = (whole_net_single / (a12 * (aX - ax[maturity_age + 1] * xEy) - b12 * (1 - xEy))) / 12

n_year_net_single = monthly_annuity * 12 * (a12 * (ax[maturity_age + 1] - ax[maturity_age + term_length + 1] * yEyn) - b12 * (1 - yEyn)) * xEy
n_year_annual_premium = n_year_net_single / (aX - ax[maturity_age + 1] * xEy)
n_year_monthly_premium = (n_year_net_single / (a12 * (aX - ax[maturity_age + 1] * xEy) - b12 * (1 - xEy))) / 12

# cat("Whole life:\n")
# cat(sprintf("\tNet single: $%.2f\n", whole_net_single))
# cat(sprintf("\tAnnual Premium: $%.2f\n", whole_annual_premium))
# cat(sprintf("\tMonthly Premium: $%.2f\n", whole_monthly_premium))
# 
# cat("N-Term:\n")
# cat(sprintf("\tNet single: $%.2f\n", n_year_net_single))
# cat(sprintf("\tAnnual Premium: $%.2f\n", n_year_annual_premium))
# cat(sprintf("\tMonthly Premium: $%.2f\n", n_year_monthly_premium))

# Pick a random death date based on mortality table
death_age = 1
while(death_age < length(mortality_data$mortality) && runif(1, 0.0, 1.0) > mortality_data$mortality[death_age]) {
  death_age = death_age + 1
}


# Generate random integer starting age
# input_age = sample(age_low:age_high, 1)

# ------------------ Functions ---------------------------------
# Function for determining Whole Life Net Single Premium Profit for company
WNS_net_profit <- function(in_age, mat_age){
  xEy = (lx[mat_age + 1] / lx[in_age + 1]) * (1 / (1 + interest_rate)) ** (mat_age - in_age)
  return(monthly_annuity * 12 * (a12 * ax[mat_age + 1] - b12) * xEy)
  
}

#------------------ Begin graphing -------------------------------

# Graphing age effect on mortality
age_qx_plot <- ggplot(life_table, aes(age, qx))
age_qx_plot + ggtitle("Age Effect on Percent Mortality (qx)") + geom_point(aes(age, qx), colour="#3366FF", size=1)

# Graphing ax on age
age_ax_plot <- ggplot(life_table, aes(age, ax))
age_ax_plot + ggtitle("Age Effect on Annuity (ax)") + geom_point(aes(age, ax), colour="#3366FF", size=1)
