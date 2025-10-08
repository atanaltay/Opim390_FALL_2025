# 1. Define variables
price <- 20
quantity <- 1:5

# 2. Compute total revenue for each quantity
revenue <- price * quantity
revenue

# 3. Average and maximum revenue
avg_revenue <- mean(revenue)
max_revenue <- max(revenue)

avg_revenue
max_revenue

# 4. Create a matrix with observations in rows and variables in columns
sales_matrix <- matrix(c(quantity, revenue), 
                       ncol = 2, byrow = FALSE)
sales_matrix
# Add column names for clarity
colnames(sales_matrix) <- c("Quantity", "Revenue")
sales_matrix

# 5. Row sums (total per observation) and column sums (totals for each variable)
col_totals <- colSums(sales_matrix)

col_totals



