USE Project
CREATE TABLE retail_sales
(Transaction_id INT PRIMARY KEY,
Sales_Date DATE,
Sale_Time TIME,
Customer_Id INT,
Gender VARCHAR(15),
Age INT,
Category VARCHAR(100),
Quantity INT,
Price_per_unit FLOAT,
Cogs FLOAT,
Total_sales FLOAT
)
SELECT * FRom retail_sales

--Display only 10 Rows
SELECT COUNT (*) FROM retail_sales

--Finding The Null Values
SELECT * FROM retail_sales
WHERE Transaction_id IS NULL;

SELECT * FROM retail_sales
WHERE Sales_Date IS NULL;

SELECT * FROM retail_sales
WHERE Transaction_id is NULL
OR
Sales_Date  IS NULL
OR
Sale_Time IS NULL
OR
Customer_Id IS NULL
OR
Gender IS NULL
OR 
Age IS Null	
OR
Category IS NULL
OR
Quantity IS Null
OR 
Quantity IS NULL
OR 
Price_per_unit IS NULL
OR
Cogs IS NULL
OR 
Total_sales IS Null

--Update The Data
SELECT Customer_Id,Gender, Age
FROM retail_sales

SELECT Customer_Id,Gender,Age
FROM retail_sales
WHERE Gender = 'Other'
ORDER BY Customer_Id;

SELECT COUNT(*) AS other_Gender
FROM retail_sales
WHERE Gender = 'Other'

SELECT DISTINCT Quantity,Gender
FROM retail_sales;

-- Check current distribution
SELECT Gender, COUNT(*) as Count
FROM RetailSales
GROUP BY Gender;

-- Update the records
UPDATE RetailSales
SET Gender = 'Female'
WHERE Gender = 'Other';

-- Verify the changes
UPDATE retail_sales
SET Gender = 'Female'
WHERE Gender = 'Other'

Select Customer_Id,Gender, Age
FROM retail_sales

SELECT DISTINCT Gender
FROM retail_sales

--How Many Total Sales We Have
Select Count(*) as Total_Sales
FROm retail_sales

------------------CONCLUSION-------------------------------------------
---Successfully created and analyzed the Retail Sales dataset by performing data 
----cleaning, handling missing values, and standardizing customer gender information. 
---The project provided valuable insights into sales records and ensured data accuracy
----for reliable reporting and future business analysis.




