#E-commerce Sales Analysis

install.packages("readxl")
install.packages("dplyr")
install.packages("writexl")

library(readxl)
library(dplyr)
library(writexl)

#Importing 3 Data that is Customer, Product & Order in 1data 
customer_data <- read_excel("C:/DVOC/LECTURES/R Programming/Projects/Excel/multiple data/customer_data.xlsx")
product_data <- read_excel("C:/DVOC/LECTURES/R Programming/Projects/Excel/multiple data/product_data.xlsx")
order_data <- read_excel("C:/DVOC/LECTURES/R Programming/Projects/Excel/multiple data/order_data.xlsx")
customer_data
order_data
product_data

# Clean column names in all tables
colnames(order_data)    <- tolower(gsub(" ", "_", colnames(order_data)))
colnames(customer_data) <- tolower(gsub(" ", "_", colnames(customer_data)))
colnames(product_data)  <- tolower(gsub(" ", "_", colnames(product_data)))

colnames(order_data)
colnames(product_data)
colnames(customer_data)

#Merging Customer, Product, and Order Data into a Single Database 
ecommerce_data <- order_data %>%
  inner_join(customer_data, by = "order_id") %>%
  inner_join(product_data, by = "order_id")
ecommerce_data

#Formation of Combined Database Titled “E-commerce Data”
head(ecommerce_data)
colnames(ecommerce_data)
View(ecommerce_data)

ecommerce_data

#Removal of Unnecessary Columns from the Data set 
ecommerce_data <- ecommerce_data %>%
  select(-country)
View(ecommerce_data)

ecommerce_data <- ecommerce_data %>%
  select(-order_date.y,
         -ship_date.y,
         -payment_mode.y,
         -payment_type.y
         
         )
ecommerce_data <- ecommerce_data %>%
  select(-product_name.y,
         -product_id.y,
         -order_date,
         -ship_date)
View(ecommerce_data)


#Renaming the Columns name
ecommerce_data <- ecommerce_data %>%
  rename(
    order_date   = order_date.x,
    ship_date    = ship_date.x,
    product__id  = product_id.x,
    product_name = product_name.x,
    payment_code = payment_mode.x,
    payment_type = payment_type.x
  )

#Renaming the Column from payment_code to payment_mode
library(dplyr)

# Rename payment_code to payment_mode
ecommerce_data <- ecommerce_data %>%
  rename(payment_mode = payment_code)

# Check the column names to confirm
colnames(ecommerce_data)


#Creation of Sales Variable in the Data set
ecommerce_data <- ecommerce_data %>%
  mutate(sales = quantity * price)
View(ecommerce_data)

str(ecommerce_data)
any(is.na(ecommerce_data))

#Conversion of Order_Date and Ship_Date from Character to Date Format
#e-commerce_data$order_date <- as.Date(e-commerce_data$order_date)
ecommerce_data$ship_date <- as.Date(ecommerce_data$ship_date)
str(ecommerce_data)

#Data Cleaning of Payment_Type Column
ecommerce_data <- ecommerce_data %>%
  mutate(payment_type = ifelse(payment_type == "COD", 
                               "Cash on Delivery", payment_type))

#Addition of Age and Age Segmentation Fields
set.seed(123)
ecommerce_data <- ecommerce_data %>%
  mutate(age = sample(17:70,n(),replace = TRUE))



install.packages("dplyr")
library(dplyr)

ecommerce_data <- ecommerce_data %>%
  mutate(age_group = case_when(
    age < 18 ~ "Minor",
    age >= 18 & age < 60 ~ "Young",
    age >= 60 ~ "Senior Citizen"
  ))

View(ecommerce_data)

#Adding Another New Column i.e Net Sales
ecommerce_data$Net_sales <- ecommerce_data$price -
  (ecommerce_data$price * ecommerce_data$discount)
ecommerce_data$Net_sales



install.packages("dplyr")
library(dplyr)



# KPI 
total_sales <- sum(ecommerce_data$sales)
total_sales

total_Net_sales <- sum(ecommerce_data$Net_sales, na.rm = TRUE)
total_Net_sales

total_orders <- n_distinct(ecommerce_data$order_id)
total_orders

total_customers <- dplyr::n_distinct(ecommerce_data$customer_id)
total_customers


total_shipping_cost <- sum(ecommerce_data$shipping_cost, na.rm = TRUE)
total_shipping_cost

total_quantity_sold <- sum(ecommerce_data$quantity, na.rm = TRUE)
total_quantity_sold

avg_discount <- round(mean(ecommerce_data$discount, na.rm = TRUE) * 100, 2)
avg_discount


total_products <- ecommerce_data %>%
  summarise(total_products = dplyr::n_distinct(product_name))  # use exact column name
total_products


total_categories <- n_distinct(ecommerce_data$category)
total_categories

total_ecommerce_type <- n_distinct(ecommerce_data$ecommerce_type)
total_ecommerce_type




#E-commerce KPI Analysis

kpi_summary <- ecommerce_data %>%
  summarise(
    Total_Sales = round(sum(sales, na.rm = TRUE), 2),
    Total_Net_Sales = round(sum(Net_sales, na.rm = TRUE), 2),
    Total_Orders = n_distinct(order_id),
    Total_Customers = n_distinct(customer_id),
    Total_Quantity = sum(quantity, na.rm = TRUE),
    Total_Shipping_Cost = round(sum(shipping_cost, na.rm = TRUE), 2),
    Avg_Discount_Percent = round(mean(discount, na.rm = TRUE) * 100, 2),
    Total_Products = n_distinct(product_name),
    Total_Categories = n_distinct(category),
    Total_Ecommerce_Type = n_distinct(ecommerce_type)
  )

kpi_summary
View(kpi_summary)

kpi_summary


# Data Visualization
install.packages("ggplot2")
library(ggplot2)






# Region-wise Sales
region_sales <- ecommerce_data %>%
  group_by(region) %>%
  summarise(Total_Sales = sum(sales, na.rm = TRUE))

ggplot(region_sales, aes(region, Total_Sales, fill = region)) +
  geom_col() +
  geom_text(aes(label = paste0(round(Total_Sales/1000000, 1), "M")),
            vjust = -0.5) +
  labs(title = "Region-wise Sales") +
  theme_minimal() +
  theme(legend.position = "none")


#Top 5 Product Sales Analysis
top5_products <- ecommerce_data %>%
  group_by(product_name) %>%
  summarise(Total_Sales = sum(sales, na.rm = TRUE)) %>%
  arrange(desc(Total_Sales)) %>%
  slice_head(n = 5)

ggplot(top5_products, aes(reorder(product_name, Total_Sales), Total_Sales, fill = product_name)) +
  geom_col() +
  geom_text(aes(label = paste0(round(Total_Sales/1e6,1),"M")), hjust = 0.0) +
  coord_flip() +
  scale_y_continuous(labels = function(x) paste0(round(x/1e6,1),"M")) +
  labs(title = "Top 5 Product Sales Analysis", x = NULL, y = NULL) +
  theme_minimal() +
  theme(legend.position = "none")




# Category-wise Sales Analysis
ecommerce_data %>%
  group_by(category) %>%
  summarise(sales_m = sum(sales, na.rm = TRUE)/1e6) %>%
  mutate(perc = round(sales_m / sum(sales_m) * 100, 1),
         label = paste0(category, "\n", round(sales_m,1), "M (", perc, "%)")) %>%
  ggplot(aes(x=2, y=sales_m, fill=category)) +
  geom_col(width=1) +
  coord_polar(theta="y") +
  geom_text(aes(label=label), position=position_stack(vjust=0.5), size=4) +
  xlim(0.5,2.5) +
  theme_void() +
  theme(plot.title=element_text(hjust=0.5, face="bold"),
        legend.position = "none") +
  labs(title="Category-wise Sales Analysis")

#Year-on-Year Sales Analysis
yearly_sales <- ecommerce_data %>%
  mutate(year = format(as.Date(order_date), "%Y")) %>%
  group_by(year) %>%
  summarise(Total_Sales = sum(sales, na.rm = TRUE))

ggplot(yearly_sales, aes(year, Total_Sales, group = 1)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_text(aes(label = paste0(round(Total_Sales/1000000, 1), "M")),
            vjust = -0.4) +
  scale_y_continuous(labels = function(x) paste0(x/1000000, "M")) +
  labs(title = "Year-on-Year Sales Analysis") +
  theme_minimal()

#Analysis of E-commerce Sales by Platform
ecom_sales <- ecommerce_data %>%
  group_by(ecommerce_type) %>%
  summarise(Total_Sales = sum(sales, na.rm = TRUE))

ggplot(ecom_sales, 
       aes(reorder(ecommerce_type, Total_Sales), 
           Total_Sales, fill = ecommerce_type)) +
  geom_col() +
  geom_text(aes(label = paste0(round(Total_Sales/1e6, 1), "M")),
            hjust = -0.2) +
  coord_flip() +
  scale_y_continuous(labels = function(x) paste0(round(x/1e6,1), "M")) +
  labs(title = "Analysis of E-commerce Sales by Different Platform",
       y = "Total Sales (M)",
       x = "E-commerce Platform") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "none")



# E-commerce Sales Distribution by State
state_sales <- ecommerce_data %>%
group_by(state) %>%
summarise(Total_Sales = sum(sales, na.rm = TRUE))

ggplot(state_sales,
       aes(reorder(state, Total_Sales), Total_Sales, fill = state)) +
  geom_col() +
  geom_text(aes(label = paste0(round(Total_Sales/1e6, 1), "M")),
            hjust = -0.1, size = 3) +
  coord_flip() +
  scale_y_continuous(labels = function(x) paste0(x/1e6, "M")) +
  labs(title = "E-commerce Sales Across PAN India",
       x = "State",
       y = "Total Sales") +
  theme_minimal() +
  theme(legend.position = "none")


# Age-wise Sales Analysis
age_group <- ecommerce_data %>%
  group_by(age_group) %>%
  summarise(Total_Sales = sum(sales, na.rm = TRUE))

# Custom colors for age groups
age_sales <- ecommerce_data %>%
  group_by(age_group) %>%
  summarise(Total_Sales = sum(sales, na.rm = TRUE))


age_colors <- c("Minor" = "#fa8072", "Senior Citizen" = "#40e0d0", "Young" = "#32cd32")

# Age-Wise Sales Analysis
ggplot(age_sales, aes(x = reorder(age_group, Total_Sales), 
                      y = Total_Sales, 
                      fill = age_group)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(round(Total_Sales/1e6, 1), "M")),
            vjust = -0.4, size = 3) +
  scale_y_continuous(labels = function(x) paste0(x/1e6, "M")) +
  scale_fill_manual(values = age_colors) +
  labs(title = "Age-wise Sales Analysis",
       x = "Age Group",
       y = "Total Sales") +
  theme_minimal() +
  theme(legend.position = "none")


#Payment Method Analysis for Customers”
payment_sales <- ecommerce_data %>%
  group_by(payment_type) %>%
  summarise(Total_Sales = sum(sales, na.rm = TRUE)) %>%
  mutate(
    Percent = round(Total_Sales / sum(Total_Sales) * 100, 1),
    Label = paste0(round(Total_Sales/1e6, 1), "M\n(", Percent, "%)"),  # Both M and %
    ypos = cumsum(Total_Sales) - 0.5 * Total_Sales
  )

# Customer Payment Preferences by Sales
ggplot(payment_sales, aes(x = "", y = Total_Sales, fill = payment_type)) +
  geom_col(color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(y = ypos, label = Label), size = 4) +
  labs(title = "Customer Payment Preferences by Sales", fill = "Payment Type") +
  theme_void() +
  scale_fill_brewer(palette = "Set2")


# Total Sales Performance Across Ten Major Cities
city_sales <- ecommerce_data %>%
  group_by(city) %>%
  summarise(Total_Sales = sum(sales, na.rm = TRUE)) %>%
  slice_max(Total_Sales, n = 10)

ggplot(city_sales, aes(reorder(city, Total_Sales), Total_Sales, fill = city)) +
  geom_col() +
  geom_text(aes(label = paste0(round(Total_Sales/1e6,1),"M")),
            hjust = -0.1, size = 2.5) +
  coord_flip() +
  scale_y_continuous(labels = function(x) paste0(round(x/1e6,1),"M")) +
  scale_fill_manual(values = c("#1F77B4",  # Blue
                               "#2CA02C",  # Green
                               "#FFD700",  # Yellow
                               "#FF7F0E",  # Orange
                               "#D62728",  # Red
                               "#9467BD",  # Purple
                               "#17BECF",  # Cyan
                               "#8C564B",  # Brown
                               "#E377C2",  # Pink
                               "#7F7F7F")) + # Grey
  labs(title = "Total Sales Performance Across 10 Major Cities",
       x = "City", y = "Total Sales (M)") +
  theme_minimal() +
  theme(legend.position = "none")



#Total Sales Distribution by Payment Mode
payment_summary <- ecommerce_data %>%
  group_by(payment_mode) %>%
  summarise(Total_Sales = sum(Net_sales, na.rm = TRUE)) %>%
  mutate(
    Percentage = round(Total_Sales / sum(Total_Sales) * 100, 1),
    label = paste0(payment_mode, "\n", Percentage, "%")
  )

ggplot(payment_summary, aes(x = 1, y = Total_Sales, fill = payment_mode)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  xlim(0.5, 1.5) +   
  geom_text(aes(label = label),
            position = position_stack(vjust = 0.5),
            size = 4) +
  theme_void() +
  labs(title = "Analysis of Total Sales by Payment Mode") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.title = element_blank()
  )

install.packages("writexl")
library(writexl)
write_xlsx(ecommerce_data, path = "ecommerce_data.xlsx")

# Business Recommendation:

# 1.Promote top-selling products through 
#bundle offers and targeted campaigns to boost repeat purchases.

#2.Encourage digital payment methods by offering small discounts or 
#cash-back to reduce dependency on Cash on Delivery and improve cash flow.

#3. Target the Young age group segment with personalized promotions, 
#as they contribute the highest share of sales.


# CONCLUSION:-
#The E-commerce analysis shows that revenue is mainly driven by specific 
#regions, cities, top-selling products, and the young customer segment. 
#Sales are also influenced by platform type, payment methods, and discount
#strategies. By focusing on high-performing areas and improving operational 
#efficiency, the business can increase profitability and achieve sustainable 
#growth.




