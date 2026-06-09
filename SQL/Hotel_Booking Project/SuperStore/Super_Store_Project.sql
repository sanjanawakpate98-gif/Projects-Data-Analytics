Create DATABASE SuperStore
Create TABLE ProductData
(ProdId VARCHAR (50) PRIMARY KEY,
ProductName VARCHAR (100),
Category VARCHAR(100),
SubCategory VARCHAR(100),
ShippingCost DECIMAL(18,2),
Quantity INT,
Price Decimal (18,2),
)
SELECT * FROm ProductData

Create TABLE OrderData
(ProdId VARCHAR(50),
OrderId VARCHAR (50),
OrderDate DATE,
ShippingDate DATE,
ShippingMode VARCHAR(100),
OrderPriority VARCHAR(100),
PaymentMode Varchar(100)
CONSTRAINT PK_Orde_Product
PRIMARY KEY(OrderId,ProdId)
)

Create Table CustomerData(
CustomerID VARCHAR(50),
CustomerName VARCHAR (100),
ProdId VARCHAR(50),
Segment VARCHAR(100),
City VARCHAR(100),
State VARCHAR(100),
Country VARCHAR(100),
PostalCode INT,
MARKET VARCHAR(50),
Region VARCHAR(100),
CONSTRAINT PK_Customer_Product
PRIMARY KEY (CustomerID, ProdId)
)

--Inserting Values in the Product Data
INSERT INTO ProductData
VALUES
('P001','Laptop','Technology','Computers',50,10,60000),
('P002','Mouse','Technology','Accessories',10,50,500),
('P003','Keyboard','Technology','Accessories',15,40,1500),
('P004','Printer','Technology','Office',100,8,12000),
('P005','Tablet','Technology','Gadgets',40,12,30000),
('P006','Chair','Furniture','Office',200,20,5000),
('P007','Desk','Furniture','Office',300,15,12000),
('P008','Pen','Stationery','Writing',5,200,50),
('P009','Notebook','Stationery','Paper',10,150,100),
('P010','Monitor','Technology','Displays',80,10,15000),
('P011','Router','Technology','Networking',60,12,4000),
('P012','Headphones','Technology','Audio',30,20,2500),
('P013','Speaker','Technology','Audio',50,18,6000),
('P014','Webcam','Technology','Accessories',20,15,3000),
('P015','Scanner','Technology','Office',70,6,10000),
('P016','Sofa','Furniture','Home',500,4,40000),
('P017','Table','Furniture','Home',300,6,15000),
('P018','Fan','Electrical','Appliance',120,10,3500),
('P019','Light','Electrical','Appliance',20,30,800),
('P020','AC','Electrical','Cooling',600,5,45000),
('P021','TV','Electronics','Entertainment',400,7,55000),
('P022','Remote','Electronics','Accessories',10,30,700),
('P023','Camera','Electronics','Photography',90,9,25000),
('P024','Tripod','Electronics','Photography',25,14,3000),
('P025','Microwave','Electronics','Kitchen',350,6,18000),
('P026','Oven','Electronics','Kitchen',300,5,22000),
('P027','Mixer','Electronics','Kitchen',100,10,8000),
('P028','Bottle','Utility','Kitchen',15,40,600),
('P029','Bag','Utility','Travel',50,20,2000),
('P030','Shoes','Fashion','Footwear',60,25,3500),
('P031','TShirt','Fashion','Clothing',30,50,1200),
('P032','Jeans','Fashion','Clothing',40,35,2500),
('P033','Jacket','Fashion','Clothing',70,15,4500),
('P034','Cap','Fashion','Accessories',20,30,600),
('P035','Watch','Fashion','Accessories',25,20,8000),
('P036','Ring','Fashion','Jewellery',10,12,15000),
('P037','Necklace','Fashion','Jewellery',15,10,22000),
('P038','Perfume','Lifestyle','Beauty',20,18,3500),
('P039','Cream','Lifestyle','Beauty',10,25,800),
('P040','Shampoo','Lifestyle','Beauty',15,40,600),
('P041','Cycle','Sports','Outdoor',300,8,12000),
('P042','Ball','Sports','Indoor',25,30,900),
('P043','Bat','Sports','Outdoor',60,20,2500),
('P044','Helmet','Sports','Safety',80,12,4000),
('P045','Gloves','Sports','Safety',30,20,1200),
('P046','Book','Education','Study',10,100,500),
('P047','NotebookPro','Education','Study',20,60,200),
('P048','Calculator','Education','Exam',15,40,900),
('P049','BagPack','Education','Accessories',40,20,2500),
('P050','WaterBottle','Utility','DailyUse',10,50,700),
('P051','Product 51','Technology','Accessories',55.00,10,510.00),
('P052','Product 52','Furniture','Chairs',60.00,12,820.00),
('P053','Product 53','Office Supplies','Binders',20.00,30,120.00),
('P054','Product 54','Technology','Phones',75.00,8,900.00),
('P055','Product 55','Furniture','Tables',90.00,6,1500.00),
('P056','Product 56','Technology','Accessories',45.00,15,450.00),
('P057','Product 57','Office Supplies','Paper',10.00,50,60.00),
('P058','Product 58','Furniture','Bookcases',70.00,5,1100.00),
('P059','Product 59','Technology','Laptops',120.00,4,2500.00),
('P060','Product 60','Office Supplies','Pens',5.00,100,30.00),
('P061','Product 61','Technology','Accessories',50.00,12,520.00),
('P062','Product 62','Furniture','Chairs',58.00,10,780.00),
('P063','Product 63','Office Supplies','Envelopes',12.00,40,90.00),
('P064','Product 64','Technology','Tablets',85.00,7,1300.00),
('P065','Product 65','Furniture','Tables',95.00,5,1700.00),
('P066','Product 66','Technology','Accessories',42.00,18,430.00),
('P067','Product 67','Office Supplies','Files',18.00,35,150.00),
('P068','Product 68','Furniture','Bookcases',72.00,6,1150.00),
('P069','Product 69','Technology','Laptops',125.00,3,2800.00),
('P070','Product 70','Office Supplies','Markers',8.00,80,45.00),
('P071','Product 71','Technology','Accessories',48.00,14,510.00),
('P072','Product 72','Furniture','Chairs',60.00,9,800.00),
('P073','Product 73','Office Supplies','Paper',11.00,60,70.00),
('P074','Product 74','Technology','Phones',78.00,6,950.00),
('P075','Product 75','Furniture','Tables',98.00,4,1800.00),
('P076','Product 76','Technology','Accessories',43.00,16,460.00),
('P077','Product 77','Office Supplies','Binders',22.00,28,140.00),
('P078','Product 78','Furniture','Bookcases',74.00,5,1200.00),
('P079','Product 79','Technology','Laptops',130.00,3,3000.00),
('P080','Product 80','Office Supplies','Pens',6.00,120,35.00),
('P081','Product 81','Technology','Accessories',54.00,11,560.00),
('P082','Product 82','Furniture','Chairs',62.00,8,790.00),
('P083','Product 83','Office Supplies','Folders',16.00,45,110.00),
('P084','Product 84','Technology','Tablets',88.00,6,1350.00),
('P085','Product 85','Furniture','Tables',92.00,5,1650.00),
('P086','Product 86','Technology','Accessories',46.00,17,480.00),
('P087','Product 87','Office Supplies','Notebooks',14.00,70,95.00),
('P088','Product 88','Furniture','Bookcases',76.00,4,1250.00),
('P089','Product 89','Technology','Laptops',128.00,2,3100.00),
('P090','Product 90','Office Supplies','Highlighters',7.00,90,40.00),
('P091','Product 91','Technology','Accessories',49.00,13,500.00),
('P092','Product 92','Furniture','Chairs',59.00,9,780.00),
('P093','Product 93','Office Supplies','Sticky Notes',9.00,100,55.00),
('P094','Product 94','Technology','Phones',80.00,6,980.00),
('P095','Product 95','Furniture','Tables',100.00,4,1900.00),
('P096','Product 96','Technology','Accessories',44.00,18,470.00),
('P097','Product 97','Office Supplies','Files',19.00,32,155.00),
('P098','Product 98','Furniture','Bookcases',78.00,5,1300.00),
('P099','Product 99','Technology','Laptops',135.00,2,3200.00),
('P100','Product 100','Office Supplies','Pens',6.50,110,38.00),
('P101','Product 101','Technology','Accessories',50.00,15,520.00),
('P102','Product 102','Furniture','Chairs',63.00,8,810.00),
('P103','Product 103','Office Supplies','Envelopes',13.00,50,92.00),
('P104','Product 104','Technology','Tablets',90.00,6,1380.00),
('P105','Product 105','Furniture','Tables',97.00,4,1750.00),
('P106','Product 106','Technology','Accessories',47.00,17,495.00),
('P107','Product 107','Office Supplies','Paper',12.00,70,75.00),
('P108','Product 108','Furniture','Bookcases',80.00,4,1400.00),
('P109','Product 109','Technology','Laptops',140.00,2,3300.00),
('P110','Product 110','Office Supplies','Markers',9.00,85,50.00),
('P111','Product 111','Technology','Accessories',52.00,14,540.00),
('P112','Product 112','Furniture','Chairs',65.00,7,830.00),
('P113','Product 113','Office Supplies','Labels',14.00,55,85.00),
('P114','Product 114','Technology','Phones',82.00,5,1000.00),
('P115','Product 115','Furniture','Tables',102.00,3,1950.00),
('P116','Product 116','Technology','Accessories',48.00,16,510.00),
('P117','Product 117','Office Supplies','Binders',24.00,26,145.00),
('P118','Product 118','Furniture','Bookcases',82.00,4,1450.00),
('P119','Product 119','Technology','Laptops',145.00,2,3400.00),
('P120','Product 120','Office Supplies','Pens',7.00,130,42.00),
('P121','Product 121','Technology','Accessories',53.00,15,550.00),
('P122','Product 122','Furniture','Chairs',66.00,6,860.00),
('P123','Product 123','Office Supplies','Folders',18.00,40,120.00),
('P124','Product 124','Technology','Tablets',92.00,5,1450.00),
('P125','Product 125','Furniture','Tables',105.00,3,2000.00),
('P126','Product 126','Technology','Accessories',49.00,17,525.00),
('P127','Product 127','Office Supplies','Notebooks',15.00,60,100.00),
('P128','Product 128','Furniture','Bookcases',85.00,4,1500.00),
('P129','Product 129','Technology','Laptops',150.00,1,3600.00),
('P130','Product 130','Office Supplies','Highlighters',8.00,95,48.00),
('P131','Product 131','Technology','Accessories',54.00,14,560.00),
('P132','Product 132','Furniture','Chairs',68.00,6,880.00),
('P133','Product 133','Office Supplies','Files',20.00,34,160.00),
('P134','Product 134','Technology','Phones',85.00,5,1050.00),
('P135','Product 135','Furniture','Tables',108.00,3,2100.00),
('P136','Product 136','Technology','Accessories',51.00,16,530.00),
('P137','Product 137','Office Supplies','Paper',13.00,75,82.00),
('P138','Product 138','Furniture','Bookcases',88.00,3,1550.00),
('P139','Product 139','Technology','Laptops',155.00,1,3800.00),
('P140','Product 140','Office Supplies','Pens',7.50,140,45.00),
('P141','Product 141','Technology','Accessories',55.00,13,570.00),
('P142','Product 142','Furniture','Chairs',70.00,5,900.00),
('P143','Product 143','Office Supplies','Binders',26.00,24,150.00),
('P144','Product 144','Technology','Tablets',95.00,4,1500.00),
('P145','Product 145','Furniture','Tables',110.00,3,2200.00),
('P146','Product 146','Technology','Accessories',52.00,15,545.00),
('P147','Product 147','Office Supplies','Envelopes',15.00,45,100.00),
('P148','Product 148','Furniture','Bookcases',90.00,3,1600.00),
('P149','Product 149','Office Supplies','Labels',14.00,55,85.00),
('P150','Product 150','Technology','Accessories',52.00,13,540.00);


--Inserting Values in the Column of Order Data
INSERT INTO OrderData
VALUES
('P001','O001','2024-01-01','2024-01-03','Standard Class','High Priority','UPI'),
('P002','O002','2024-01-02','2024-01-04','Second Class','Medium','Card'),
('P003','O003','2024-01-03','2024-01-03','Same Day','Critical','UPI'),
('P004','O004','2024-01-04','2024-01-06','First Class','High Priority','NetBanking'),
('P005','O005','2024-01-05','2024-01-07','Standard Class','Medium','Cash'),
('P006','O006','2024-01-06','2024-01-08','Second Class','High Priority','UPI'),
('P007','O007','2024-01-07','2024-01-09','First Class','Critical','Card'),
('P008','O008','2024-01-08','2024-01-10','Standard Class','Low','Cash'),
('P009','O009','2024-01-09','2024-01-11','Second Class','Medium','UPI'),
('P010','O010','2024-01-10','2024-01-12','Same Day','High Priority','Card'),
('P011','O011','2024-01-11','2024-01-13','Standard Class','Medium','UPI'),
('P012','O012','2024-01-12','2024-01-14','First Class','High Priority','Card'),
('P013','O013','2024-01-13','2024-01-15','Second Class','Critical','NetBanking'),
('P014','O014','2024-01-14','2024-01-16','Standard Class','Low','UPI'),
('P015','O015','2024-01-15','2024-01-17','Same Day','High Priority','Cash'),
('P016','O016','2024-01-16','2024-01-18','First Class','Medium','Card'),
('P017','O017','2024-01-17','2024-01-19','Second Class','High Priority','UPI'),
('P018','O018','2024-01-18','2024-01-20','Standard Class','Low','Cash'),
('P019','O019','2024-01-19','2024-01-21','Same Day','Critical','UPI'),
('P020','O020','2024-01-20','2024-01-22','First Class','High Priority','Card'),
('P021','O021','2024-01-21','2024-01-23','Standard Class','Medium','UPI'),
('P022','O022','2024-01-22','2024-01-24','Second Class','Low','Cash'),
('P023','O023','2024-01-23','2024-01-25','First Class','High Priority','Card'),
('P024','O024','2024-01-24','2024-01-26','Standard Class','Medium','UPI'),
('P025','O025','2024-01-25','2024-01-27','Same Day','Critical','NetBanking'),
('P026','O026','2024-01-26','2024-01-28','Second Class','Medium','Cash'),
('P027','O027','2024-01-27','2024-01-29','First Class','High Priority','Card'),
('P028','O028','2024-01-28','2024-01-30','Standard Class','Low','UPI'),
('P029','O029','2024-01-29','2024-01-31','Second Class','Medium','Cash'),
('P030','O030','2024-01-30','2024-02-01','Same Day','High Priority','UPI'),
('P031','O031','2024-01-31','2024-02-02','First Class','Critical','Card'),
('P032','O032','2024-02-01','2024-02-03','Standard Class','Medium','Cash'),
('P033','O033','2024-02-02','2024-02-04','Second Class','High Priority','UPI'),
('P034','O034','2024-02-03','2024-02-05','Same Day','Low','Cash'),
('P035','O035','2024-02-04','2024-02-06','First Class','High Priority','Card'),
('P036','O036','2024-02-05','2024-02-07','Standard Class','Medium','UPI'),
('P037','O037','2024-02-06','2024-02-08','Second Class','Critical','NetBanking'),
('P038','O038','2024-02-07','2024-02-09','Standard Class','Low','Cash'),
('P039','O039','2024-02-08','2024-02-10','First Class','High Priority','UPI'),
('P040','O040','2024-02-09','2024-02-11','Same Day','Medium','Card'),
('P041','O041','2024-02-10','2024-02-12','Standard Class','High Priority','UPI'),
('P042','O042','2024-02-11','2024-02-13','Second Class','Low','Cash'),
('P043','O043','2024-02-12','2024-02-14','First Class','Critical','Card'),
('P044','O044','2024-02-13','2024-02-15','Standard Class','Medium','UPI'),
('P045','O045','2024-02-14','2024-02-16','Same Day','High Priority','NetBanking'),
('P046','O046','2024-02-15','2024-02-17','Second Class','Medium','Cash'),
('P047','O047','2024-02-16','2024-02-18','First Class','Low','UPI'),
('P048','O048','2024-02-17','2024-02-19','Standard Class','Medium','Card'),
('P049','O049','2024-02-18','2024-02-20','Second Class','High Priority','UPI'),
('P050','O050','2024-02-19','2024-02-21','Standard Class','Medium','UPI'),
('P051','O051','2024-03-01','2024-03-03','Standard Class','High Priority','UPI'),
('P052','O052','2024-03-02','2024-03-05','Second Class','Medium','Card'),
('P053','O053','2024-03-03','2024-03-03','Same Day','Critical','UPI'),
('P054','O054','2024-03-04','2024-03-06','First Class','High Priority','NetBanking'),
('P055','O055','2024-03-05','2024-03-07','Standard Class','Medium','Cash'),
('P056','O056','2024-03-06','2024-03-08','Second Class','High Priority','UPI'),
('P057','O057','2024-03-07','2024-03-09','First Class','Critical','Card'),
('P058','O058','2024-03-08','2024-03-10','Standard Class','Low','Cash'),
('P059','O059','2024-03-09','2024-03-11','Same Day','High Priority','UPI'),
('P060','O060','2024-03-10','2024-03-12','First Class','Medium','Card'),
('P061','O061','2024-03-11','2024-03-13','Standard Class','Medium','UPI'),
('P062','O062','2024-03-12','2024-03-14','Second Class','High Priority','NetBanking'),
('P063','O063','2024-03-13','2024-03-13','Same Day','Critical','UPI'),
('P064','O064','2024-03-14','2024-03-16','First Class','Medium','Card'),
('P065','O065','2024-03-15','2024-03-17','Standard Class','Low','Cash'),
('P066','O066','2024-03-16','2024-03-18','Second Class','High Priority','UPI'),
('P067','O067','2024-03-17','2024-03-19','First Class','Medium','Card'),
('P068','O068','2024-03-18','2024-03-20','Standard Class','Low','Cash'),
('P069','O069','2024-03-19','2024-03-21','Same Day','Critical','UPI'),
('P070','O070','2024-03-20','2024-03-22','First Class','High Priority','NetBanking'),
('P071','O071','2024-03-21','2024-03-23','Standard Class','Medium','UPI'),
('P072','O072','2024-03-22','2024-03-24','Second Class','Low','Cash'),
('P073','O073','2024-03-23','2024-03-25','First Class','High Priority','Card'),
('P074','O074','2024-03-24','2024-03-26','Same Day','Critical','UPI'),
('P075','O075','2024-03-25','2024-03-27','Standard Class','Medium','NetBanking'),
('P076','O076','2024-03-26','2024-03-28','Second Class','Low','Cash'),
('P077','O077','2024-03-27','2024-03-29','First Class','High Priority','UPI'),
('P078','O078','2024-03-28','2024-03-30','Standard Class','Medium','Card'),
('P079','O079','2024-03-29','2024-03-31','Same Day','Critical','UPI'),
('P080','O080','2024-03-30','2024-04-01','First Class','High Priority','NetBanking'),
('P081','O081','2024-04-01','2024-04-03','Standard Class','Medium','UPI'),
('P082','O082','2024-04-02','2024-04-04','Second Class','Low','Cash'),
('P083','O083','2024-04-03','2024-04-05','First Class','High Priority','Card'),
('P084','O084','2024-04-04','2024-04-06','Same Day','Critical','UPI'),
('P085','O085','2024-04-05','2024-04-07','Standard Class','Medium','NetBanking'),
('P086','O086','2024-04-06','2024-04-08','Second Class','High Priority','UPI'),
('P087','O087','2024-04-07','2024-04-09','First Class','Medium','Card'),
('P088','O088','2024-04-08','2024-04-10','Standard Class','Low','Cash'),
('P089','O089','2024-04-09','2024-04-11','Same Day','Critical','UPI'),
('P090','O090','2024-04-10','2024-04-12','First Class','High Priority','NetBanking'),
('P091','O091','2024-04-11','2024-04-13','Standard Class','Medium','UPI'),
('P092','O092','2024-04-12','2024-04-14','Second Class','Low','Cash'),
('P093','O093','2024-04-13','2024-04-15','First Class','High Priority','Card'),
('P094','O094','2024-04-14','2024-04-16','Same Day','Critical','UPI'),
('P095','O095','2024-04-15','2024-04-17','Standard Class','Medium','NetBanking'),
('P096','O096','2024-04-16','2024-04-18','Second Class','High Priority','UPI'),
('P097','O097','2024-04-17','2024-04-19','First Class','Medium','Card'),
('P098','O098','2024-04-18','2024-04-20','Standard Class','Low','Cash'),
('P099','O099','2024-04-19','2024-04-21','Same Day','Critical','UPI'),
('P100','O100','2024-04-20','2024-04-22','First Class','High Priority','NetBanking'),
('P101','O101','2024-04-21','2024-04-23','Standard Class','Medium','UPI'),
('P102','O102','2024-04-22','2024-04-24','Second Class','Low','Cash'),
('P103','O103','2024-04-23','2024-04-25','First Class','High Priority','Card'),
('P104','O104','2024-04-24','2024-04-26','Same Day','Critical','UPI'),
('P105','O105','2024-04-25','2024-04-27','Standard Class','Medium','NetBanking'),
('P106','O106','2024-04-26','2024-04-28','Second Class','High Priority','UPI'),
('P107','O107','2024-04-27','2024-04-29','First Class','Medium','Card'),
('P108','O108','2024-04-28','2024-04-30','Standard Class','Low','Cash'),
('P109','O109','2024-04-29','2024-05-01','Same Day','Critical','UPI'),
('P110','O110','2024-04-30','2024-05-02','First Class','High Priority','NetBanking'),
('P111','O111','2024-05-01','2024-05-03','Standard Class','Medium','UPI'),
('P112','O112','2024-05-02','2024-05-04','Second Class','Low','Cash'),
('P113','O113','2024-05-03','2024-05-05','First Class','High Priority','Card'),
('P114','O114','2024-05-04','2024-05-06','Same Day','Critical','UPI'),
('P115','O115','2024-05-05','2024-05-07','Standard Class','Medium','NetBanking'),
('P116','O116','2024-05-06','2024-05-08','Second Class','High Priority','UPI'),
('P117','O117','2024-05-07','2024-05-09','First Class','Medium','Card'),
('P118','O118','2024-05-08','2024-05-10','Standard Class','Low','Cash'),
('P119','O119','2024-05-09','2024-05-11','Same Day','Critical','UPI'),
('P120','O120','2024-05-10','2024-05-12','First Class','High Priority','NetBanking'),
('P121','O121','2024-05-11','2024-05-13','Standard Class','Medium','UPI'),
('P122','O122','2024-05-12','2024-05-14','Second Class','Low','Cash'),
('P123','O123','2024-05-13','2024-05-15','First Class','High Priority','Card'),
('P124','O124','2024-05-14','2024-05-16','Same Day','Critical','UPI'),
('P125','O125','2024-05-15','2024-05-17','Standard Class','Medium','NetBanking'),
('P126','O126','2024-05-16','2024-05-18','Second Class','High Priority','UPI'),
('P127','O127','2024-05-17','2024-05-19','First Class','Medium','Card'),
('P128','O128','2024-05-18','2024-05-20','Standard Class','Low','Cash'),
('P129','O129','2024-05-19','2024-05-21','Same Day','Critical','UPI'),
('P130','O130','2024-05-20','2024-05-22','First Class','High Priority','NetBanking'),
('P131','O131','2024-05-21','2024-05-23','Standard Class','Medium','UPI'),
('P132','O132','2024-05-22','2024-05-24','Second Class','Low','Cash'),
('P133','O133','2024-05-23','2024-05-25','First Class','High Priority','Card'),
('P134','O134','2024-05-24','2024-05-26','Same Day','Critical','UPI'),
('P135','O135','2024-05-25','2024-05-27','Standard Class','Medium','NetBanking'),
('P136','O136','2024-05-26','2024-05-28','Second Class','High Priority','UPI'),
('P137','O137','2024-05-27','2024-05-29','First Class','Medium','Card'),
('P138','O138','2024-05-28','2024-05-30','Standard Class','Low','Cash'),
('P139','O139','2024-05-29','2024-05-31','Same Day','Critical','UPI'),
('P140','O140','2024-05-30','2024-06-01','First Class','High Priority','NetBanking'),
('P141','O141','2024-05-31','2024-06-02','Standard Class','Medium','UPI'),
('P142','O142','2024-06-01','2024-06-03','Second Class','Low','Cash'),
('P143','O143','2024-06-02','2024-06-04','First Class','High Priority','Card'),
('P144','O144','2024-06-03','2024-06-05','Same Day','Critical','UPI'),
('P145','O145','2024-06-04','2024-06-06','Standard Class','Medium','NetBanking'),
('P146','O146','2024-06-05','2024-06-07','Second Class','High Priority','UPI'),
('P147','O147','2024-06-06','2024-06-08','First Class','Medium','Card'),
('P148','O148','2024-06-07','2024-06-09','Standard Class','Low','Cash'),
('P149','O149','2024-06-28','2024-06-30','Second Class','Medium','UPI'),
('P150','O150','2024-06-29','2024-07-01','Standard Class','High Priority','Card');


-- Inserting Values in the Customers Data
INSERT INTO CustomerData VALUES
('C001','Amit','P001','Consumer','Delhi','Delhi','India',110001,'APAC','South Asia'),
('C002','Neha','P002','Corporate','Mumbai','MH','India',400001,'APAC','South Asia'),
('C003','Ravi','P003','Home Office','Bengaluru','KA','India',560001,'APAC','South Asia'),
('C004','Sara','P004','Consumer','New York','NY','USA',10001,'US','East'),
('C005','John','P005','Corporate','Toronto','ON','Canada',20001,'Canada','North'),
('C006','Luis','P006','Consumer','Mexico City','NA','Mexico',30001,'LATAM','Central'),
('C007','Anna','P007','Home Office','Berlin','BE','Germany',10115,'EU','West'),
('C008','Paul','P008','Consumer','Paris','IDF','France',75001,'EU','West'),
('C009','Mia','P009','Corporate','Sydney','NSW','Australia',2000,'APAC','Oceania'),
('C010','Raj','P010','Consumer','Pune','MH','India',411001,'APAC','South Asia'),
('C011','Kunal','P011','Corporate','Ahmedabad','GJ','India',380001,'APAC','South Asia'),
('C012','Sophia','P012','Consumer','Los Angeles','CA','USA',90001,'US','West'),
('C013','Daniel','P013','Corporate','Chicago','IL','USA',60601,'US','Central'),
('C014','Emily','P014','Home Office','Seattle','WA','USA',98101,'US','West'),
('C015','Arjun','P015','Consumer','Hyderabad','TS','India',500001,'APAC','South Asia'),
('C016','Meera','P016','Corporate','Chennai','TN','India',600001,'APAC','South Asia'),
('C017','Noah','P017','Consumer','Dallas','TX','USA',75201,'US','South'),
('C018','Lucas','P018','Corporate','São Paulo','SP','Brazil',10001,'LATAM','South'),
('C019','Isabella','P019','Home Office','Rome','RM','Italy',00100,'EU','South'),
('C020','Oliver','P020','Consumer','Madrid','MD','Spain',28001,'EU','South'),
('C021','Ayaan','P021','Corporate','Jaipur','RJ','India',302001,'APAC','South Asia'),
('C022','Zara','P022','Consumer','London','LDN','UK',70001,'EU','West'),
('C023','Leo','P023','Corporate','Amsterdam','NH','Netherlands',1011,'EU','West'),
('C024','Eva','P024','Home Office','Vienna','VI','Austria',1010,'EU','Central'),
('C025','Omar','P025','Consumer','Dubai','DXB','UAE',00000,'APAC','Middle East'),
('C026','Fatima','P026','Corporate','Abu Dhabi','AUH','UAE',00000,'APAC','Middle East'),
('C027','Yuki','P027','Consumer','Tokyo','TK','Japan',1000001,'APAC','East Asia'),
('C028','Ken','P028','Home Office','Osaka','OS','Japan',5300001,'APAC','East Asia'),
('C029','Min','P029','Consumer','Seoul','SO','South Korea',04524,'APAC','East Asia'),
('C030','Jin','P030','Corporate','Busan','BS','South Korea',48920,'APAC','East Asia'),
('C031','Carlos','P031','Consumer','Buenos Aires','BA','Argentina',1001,'LATAM','South'),
('C032','Diego','P032','Corporate','Lima','LI','Peru',15001,'LATAM','West'),
('C033','Maria','P033','Home Office','Bogota','BO','Colombia',110111,'LATAM','North'),
('C034','Sofia','P034','Consumer','Santiago','SA','Chile',8320000,'LATAM','South'),
('C035','Nina','P035','Corporate','Zurich','ZH','Switzerland',8001,'EU','Central'),
('C036','Tom','P036','Consumer','Munich','BY','Germany',80331,'EU','Central'),
('C037','Henry','P037','Corporate','Oslo','OS','Norway',0101,'EU','North'),
('C038','Ella','P038','Home Office','Stockholm','ST','Sweden',10005,'EU','North'),
('C039','Jack','P039','Consumer','Copenhagen','CPH','Denmark',1050,'EU','North'),
('C040','Harry','P040','Corporate','Dublin','DN','Ireland',2,'EU','West'),
('C041','Ishaan','P041','Consumer','Nagpur','MH','India',440001,'APAC','South Asia'),
('C042','Pooja','P042','Corporate','Indore','MP','India',452001,'APAC','South Asia'),
('C043','Manoj','P043','Home Office','Patna','BR','India',800001,'APAC','South Asia'),
('C044','Ritika','P044','Consumer','Bhopal','MP','India',462001,'APAC','South Asia'),
('C045','Kevin','P045','Corporate','San Diego','CA','USA',92101,'US','West'),
('C046','Brian','P046','Consumer','Austin','TX','USA',73301,'US','South'),
('C047','Nancy','P047','Home Office','Boston','MA','USA',02101,'US','East'),
('C048','George','P048','Corporate','Atlanta','GA','USA',30301,'US','South'),
('C049','Victor','P049','Consumer','Vancouver','BC','Canada',30001,'Canada','West'),
('C050','David','P050','Corporate','London','LDN','UK',70001,'EU','West'),
('C051','Arun','P051','Consumer','Delhi','DL','India',110002,'APAC','South Asia'),
('C052','Nisha','P052','Corporate','Mumbai','MH','India',400002,'APAC','South Asia'),
('C053','Rahul','P053','Home Office','Bengaluru','KA','India',560002,'APAC','South Asia'),
('C054','Jessica','P054','Consumer','San Jose','CA','USA',95101,'US','West'),
('C055','Mark','P055','Corporate','Vancouver','BC','Canada',30002,'Canada','West'),
('C056','Carlos','P056','Consumer','Monterrey','NL','Mexico',64000,'LATAM','Central'),
('C057','Laura','P057','Home Office','Madrid','MD','Spain',28002,'EU','South'),
('C058','Peter','P058','Consumer','Hamburg','HH','Germany',20095,'EU','West'),
('C059','Emma','P059','Corporate','Melbourne','VIC','Australia',3000,'APAC','Oceania'),
('C060','Rohit','P060','Consumer','Nagpur','MH','India',440002,'APAC','South Asia'),
('C061','Karan','P061','Corporate','Surat','GJ','India',395003,'APAC','South Asia'),
('C062','Sneha','P062','Consumer','Indore','MP','India',452002,'APAC','South Asia'),
('C063','Anil','P063','Home Office','Patna','BR','India',800002,'APAC','South Asia'),
('C064','Kevin','P064','Consumer','San Diego','CA','USA',92101,'US','West'),
('C065','Nancy','P065','Corporate','Boston','MA','USA',2108,'US','East'),
('C066','Miguel','P066','Consumer','Guadalajara','JA','Mexico',44100,'LATAM','Central'),
('C067','Isabella','P067','Corporate','Milan','MI','Italy',20100,'EU','South'),
('C068','Jonas','P068','Home Office','Oslo','OS','Norway',0101,'EU','North'),
('C069','Ava','P069','Consumer','Auckland','AU','New Zealand',1010,'APAC','Oceania'),
('C070','Suresh','P070','Corporate','Coimbatore','TN','India',641001,'APAC','South Asia'),
('C071','Ramesh','P071','Consumer','Trichy','TN','India',620001,'APAC','South Asia'),
('C072','Pallavi','P072','Corporate','Kolhapur','MH','India',416003,'APAC','South Asia'),
('C073','Deepak','P073','Home Office','Udaipur','RJ','India',313001,'APAC','South Asia'),
('C074','Chris','P074','Consumer','Phoenix','AZ','USA',85001,'US','West'),
('C075','Sophia','P075','Corporate','Calgary','AB','Canada',50001,'Canada','West'),
('C076','Alejandro','P076','Consumer','Puebla','PB','Mexico',72000,'LATAM','Central'),
('C077','Charlotte','P077','Corporate','Lyon','LY','France',69001,'EU','West'),
('C078','Felix','P078','Home Office','Zurich','ZH','Switzerland',8002,'EU','Central'),
('C079','Liam','P079','Consumer','Perth','WA','Australia',6000,'APAC','Oceania'),
('C080','Manish','P080','Corporate','Noida','UP','India',201301,'APAC','South Asia'),
('C081','Harish','P081','Consumer','Jodhpur','RJ','India',342001,'APAC','South Asia'),
('C082','Neelam','P082','Corporate','Ajmer','RJ','India',305001,'APAC','South Asia'),
('C083','Sunil','P083','Home Office','Dhanbad','JH','India',826001,'APAC','South Asia'),
('C084','Andrew','P084','Consumer','Denver','CO','USA',80201,'US','Central'),
('C085','Laura','P085','Corporate','Ottawa','ON','Canada',60001,'Canada','East'),
('C086','Ricardo','P086','Consumer','Toluca','MX','Mexico',50000,'LATAM','Central'),
('C087','Marta','P087','Corporate','Valencia','VC','Spain',46001,'EU','South'),
('C088','Simon','P088','Home Office','Bern','BE','Switzerland',3001,'EU','Central'),
('C089','Olivia','P089','Consumer','Canberra','ACT','Australia',2600,'APAC','Oceania'),
('C090','Prakash','P090','Corporate','Belgaum','KA','India',590001,'APAC','South Asia'),
('C091','Nitin','P091','Consumer','Satara','MH','India',415001,'APAC','South Asia'),
('C092','Aarti','P092','Corporate','Sangli','MH','India',416416,'APAC','South Asia'),
('C093','Mahesh','P093','Home Office','Solapur','MH','India',413001,'APAC','South Asia'),
('C094','Brian','P094','Consumer','Tucson','AZ','USA',85701,'US','West'),
('C095','George','P095','Corporate','Winnipeg','MB','Canada',70001,'Canada','Central'),
('C096','Hector','P096','Consumer','Cancun','QR','Mexico',77500,'LATAM','Central'),
('C097','Elena','P097','Corporate','Seville','SV','Spain',41001,'EU','South'),
('C098','Thomas','P098','Home Office','Geneva','GE','Switzerland',1201,'EU','Central'),
('C099','Jack','P099','Consumer','Hobart','TS','Australia',7000,'APAC','Oceania'),
('C100','Ravi','P100','Corporate','Vijayawada','AP','India',520001,'APAC','South Asia'),
('C101','Henry','P101','Consumer','Edinburgh','SC','UK',70002,'EU','West'),
('C102','William','P102','Corporate','Bristol','BR','UK',70003,'EU','West'),
('C103','James','P103','Home Office','Leeds','LD','UK',70004,'EU','West'),
('C104','Noah','P104','Consumer','York','YK','UK',70005,'EU','West'),
('C105','Lucas','P105','Corporate','Reading','RD','UK',70006,'EU','West'),
('C106','Ethan','P106','Consumer','Cardiff','CF','UK',70007,'EU','West'),
('C107','Benjamin','P107','Corporate','Derby','DB','UK',70008,'EU','West'),
('C108','Elijah','P108','Home Office','Bath','BA','UK',70009,'EU','West'),
('C109','Logan','P109','Consumer','Oxford','OX','UK',70010,'EU','West'),
('C110','Mason','P110','Corporate','Cambridge','CB','UK',70011,'EU','West'),
('C111','Daniel','P111','Consumer','Plymouth','PL','UK',70012,'EU','West'),
('C112','Matthew','P112','Corporate','Exeter','EX','UK',70013,'EU','West'),
('C113','Joseph','P113','Home Office','Swindon','SN','UK',70014,'EU','West'),
('C114','Samuel','P114','Consumer','Woking','WK','UK',70015,'EU','West'),
('C115','David','P115','Corporate','Slough','SL','UK',70016,'EU','West'),
('C116','Luke','P116','Consumer','Croydon','CR','UK',70017,'EU','West'),
('C117','Andrew','P117','Corporate','Harrow','HR','UK',70018,'EU','West'),
('C118','Ryan','P118','Home Office','Watford','WD','UK',70019,'EU','West'),
('C119','Nathan','P119','Consumer','Ilford','IL','UK',70020,'EU','West'),
('C120','Aaron','P120','Corporate','Enfield','EN','UK',70021,'EU','West'),
('C121','Isaac','P121','Consumer','Epsom','EP','UK',70022,'EU','West'),
('C122','Caleb','P122','Corporate','Redhill','RH','UK',70023,'EU','West'),
('C123','Hunter','P123','Home Office','Dorking','DK','UK',70024,'EU','West'),
('C124','Julian','P124','Consumer','Sevenoaks','SE','UK',70025,'EU','West'),
('C125','Levi','P125','Corporate','Maidstone','MD','UK',70026,'EU','West'),
('C126','Sebastian','P126','Consumer','Ashford','AS','UK',70027,'EU','West'),
('C127','Connor','P127','Corporate','Canterbury','CT','UK',70028,'EU','West'),
('C128','Jason','P128','Home Office','Ramsgate','RM','UK',70029,'EU','West'),
('C129','Zachary','P129','Consumer','Margate','MG','UK',70030,'EU','West'),
('C130','Adrian','P130','Corporate','Deal','DL','UK',70031,'EU','West'),
('C131','Henry','P131','Consumer','Folkestone','FK','UK',70032,'EU','West'),
('C132','Patrick','P132','Corporate','Hythe','HT','UK',70033,'EU','West'),
('C133','Sean','P133','Home Office','Sittingbourne','SB','UK',70034,'EU','West'),
('C134','Kevin','P134','Consumer','Chatham','CH','UK',70035,'EU','West'),
('C135','Alan','P135','Corporate','Rochester','RC','UK',70036,'EU','West'),
('C136','Peter','P136','Consumer','Gravesend','GR','UK',70037,'EU','West'),
('C137','Frank','P137','Corporate','Dartford','DF','UK',70038,'EU','West'),
('C138','Scott','P138','Home Office','Bexley','BX','UK',70039,'EU','West'),
('C139','Bryan','P139','Consumer','Sidcup','SD','UK',70040,'EU','West'),
('C140','Adam','P140','Corporate','Erith','ER','UK',70041,'EU','West'),
('C141','Jordan','P141','Consumer','Blackheath','BH','UK',70042,'EU','West'),
('C142','Kyle','P142','Corporate','Lewisham','LW','UK',70043,'EU','West'),
('C143','Justin','P143','Home Office','Greenwich','GW','UK',70044,'EU','West'),
('C144','Aaron','P144','Consumer','Deptford','DP','UK',70045,'EU','West'),
('C145','Evan','P145','Corporate','Peckham','PK','UK',70046,'EU','West'),
('C146','Miles','P146','Consumer','Brixton','BR','UK',70047,'EU','West'),
('C147','Cole','P147','Corporate','Clapham','CP','UK',70048,'EU','West'),
('C148','Blake','P148','Home Office','Tooting','TT','UK',70049,'EU','West'),
('C149','Henry','P149','Consumer','Edinburgh','SC','UK',70002,'EU','West'),
('C150','William','P150','Corporate','Bristol','BR','UK',70003,'EU','West');

SELECT * From CustomerData
Select * From ProductData
Select * From OrderData

--Creating the Full State Name in the Data
ALTER TABLE CustomerData
ADD State_Name VARCHAR(100)
SElect * from CustomerData

--Insserting the Values in the New Columns
UPDATE CustomerData
SET State_Name =
CASE
  WHEN State = 'Delhi' AND Country = 'India' THEN 'Delhi'
  WHEN State = 'MH' AND Country = 'India' THEN 'Maharashtra'
  WHEN State = 'KA' AND Country = 'India' THEN 'Karnataka'
  WHEN State = 'GJ' AND Country = 'India' THEN 'Gujarat'
  WHEN State = 'TS' AND Country = 'India' THEN 'Telangana'
  WHEN State = 'TN' AND Country = 'India' THEN 'Tamil Nadu'
  WHEN State = 'NY' AND Country = 'USA' THEN 'New York'
  WHEN State = 'CA' AND Country = 'USA' THEN 'California'
  WHEN State = 'TX' AND Country = 'USA' THEN 'Texas'
  WHEN State = 'WA' AND Country = 'USA' THEN 'Washington'
  WHEN State = 'ON' AND Country = 'Canada' THEN 'Ontario'
  WHEN State = 'BC' AND Country = 'Canada' THEN 'British Columbia'
  WHEN State = 'NSW' AND Country = 'Australia' THEN 'New South Wales'
  WHEN State = 'VIC' AND Country = 'Australia' THEN 'Victoria'
  WHEN State = 'DXB' AND Country = 'UAE' THEN 'Dubai'
  WHEN State = 'AUH' AND Country = 'UAE' THEN 'Abu Dhabi'
  WHEN State = 'TK' AND Country = 'Japan' THEN 'Tokyo'
  WHEN State = 'OS' AND Country = 'Japan' THEN 'Osaka'
  WHEN State = 'OS' AND Country = 'Norway' THEN 'Oslo'
  WHEN State = 'BE' AND Country = 'Germany' THEN 'Berlin'
  WHEN State = 'BE' AND Country = 'Switzerland' THEN 'Bern'
  WHEN State = 'RM' AND Country = 'Italy' THEN 'Rome'
  WHEN State = 'SP' AND Country = 'Brazil' THEN 'São Paulo'
  WHEN State = 'BA' AND Country = 'Argentina' THEN 'Buenos Aires'
  WHEN State = 'LDN' AND Country = 'UK' THEN 'London'
  WHEN State = 'OX' AND Country = 'UK' THEN 'Oxfordshire'
  WHEN State = 'NH' AND Country = 'Netherlands' THEN 'North Holland'
  WHEN State = 'SO' AND Country = 'South Korea' THEN 'Seoul'
  WHEN State = 'ZH' AND Country = 'Switzerland' THEN 'Zurich'
  WHEN State = 'GE' AND Country = 'Switzerland' THEN 'Geneva'
  WHEN State = 'MP' AND Country = 'India' THEN 'Madhya Pradesh'
  WHEN State = 'DL' AND Country = 'India' THEN 'Delhi'
  WHEN State = 'RJ' AND Country = 'India' THEN 'Rajasthan'
  WHEN State = 'UP' AND Country = 'India' THEN 'Uttar Pradesh'
  WHEN State = 'JH' AND Country = 'India' THEN 'Jharkhand'
  WHEN State = 'MA' AND Country = 'USA' THEN 'Massachusetts'
  WHEN State = 'AZ' AND Country = 'USA' THEN 'Arizona'
  WHEN State = 'CO' AND Country = 'USA' THEN 'Colorado'
  WHEN State = 'NL' AND Country = 'Mexico' THEN 'Nuevo León'
  WHEN State = 'PB' AND Country = 'Mexico' THEN 'Puebla'
  WHEN State = 'MX' AND Country = 'Mexico' THEN 'Mexico City'
  WHEN State = 'LY' AND Country = 'France' THEN 'Lyon'
  WHEN State = 'WA' AND Country = 'Australia' THEN 'Western Australia'
  WHEN State = 'ACT' AND Country = 'Australia' THEN 'Australian Capital Territory'
  WHEN State = 'TS' AND Country = 'Australia' THEN 'Tasmania'
  WHEN State = 'MB' AND Country = 'Canada' THEN 'Manitoba'
  WHEN State = 'BR' AND Country = 'UK' THEN 'Bristol'
  WHEN State = 'BA' AND Country = 'UK' THEN 'Bath'
  WHEN State = 'CB' AND Country = 'UK' THEN 'Cambridge'
  WHEN State = 'OX' AND Country = 'UK' THEN 'Oxfordshire'
  WHEN State = 'WK' AND Country = 'UK' THEN 'Warwickshire'
  WHEN State = 'HR' AND Country = 'UK' THEN 'Herefordshire'
  WHEN State = 'IL' AND Country = 'UK' THEN 'Islington'
  WHEN State = 'MD' AND Country = 'UK' THEN 'Middlesex'
  WHEN State = 'CT' AND Country = 'UK' THEN 'Coventry'
  WHEN State = 'RM' AND Country = 'UK' THEN 'Romford'
  WHEN State = 'DL' AND Country = 'UK' THEN 'London'
  WHEN State = 'FK' AND Country = 'UK' THEN 'Falkirk'
  WHEN State = 'HT' AND Country = 'UK' THEN 'Hertfordshire'
  WHEN State = 'SB' AND Country = 'UK' THEN 'Southwark'
  WHEN State = 'CH' AND Country = 'UK' THEN 'Cheshire'
  WHEN State = 'GR' AND Country = 'UK' THEN 'Greenwich'
  WHEN State = 'BX' AND Country = 'UK' THEN 'Bexley'
  WHEN State = 'SD' AND Country = 'UK' THEN 'Southend'
  WHEN State = 'BH' AND Country = 'UK' THEN 'Brighton & Hove'
  WHEN State = 'CP' AND Country = 'UK' THEN 'Cambridgeshire'
  WHEN State = 'TT' AND Country = 'UK' THEN 'Teesside'
  WHEN State = 'NA'  AND Country = 'Mexico'       THEN 'Mexico City'
  WHEN State = 'IDF' AND Country = 'France'       THEN 'Paris'
  WHEN State = 'IL'  AND Country = 'USA'          THEN 'Illinois'
  WHEN State = 'MD'  AND Country = 'Spain'        THEN 'Madrid'
  WHEN State = 'VI'  AND Country = 'Austria'      THEN 'Vienna'
  WHEN State = 'BS'  AND Country = 'South Korea'  THEN 'Busan'
  WHEN State = 'LI'  AND Country = 'Peru'         THEN 'Lima'
  WHEN State = 'BO'  AND Country = 'Colombia'     THEN 'Bogotá'
  WHEN State = 'SA'  AND Country = 'Chile'        THEN 'Santiago'
  WHEN State = 'BY'  AND Country = 'Germany'      THEN 'Bavaria (Munich)'
  WHEN State = 'ST'  AND Country = 'Sweden'       THEN 'Stockholm'
  WHEN State = 'CPH' AND Country = 'Denmark'      THEN 'Copenhagen'
  WHEN State = 'DN'  AND Country = 'Ireland'      THEN 'Dublin'
  WHEN State = 'BR'  AND Country = 'India'        THEN 'Bihar (Patna)'
  WHEN State = 'GA'  AND Country = 'USA'          THEN 'Georgia'
  WHEN State = 'HH'  AND Country = 'Germany'      THEN 'Hamburg'
  WHEN State = 'JA'  AND Country = 'Mexico'       THEN 'Jalisco (Guadalajara)'
  WHEN State = 'MI'  AND Country = 'Italy'        THEN 'Milan'
  WHEN State = 'AU'  AND Country = 'New Zealand'  THEN 'Auckland'
  WHEN State = 'AB'  AND Country = 'Canada'       THEN 'Alberta (Calgary)'
  WHEN State = 'VC'  AND Country = 'Spain'        THEN 'Valencia'
  WHEN State = 'QR'  AND Country = 'Mexico'       THEN 'Quintana Roo (Cancún)'
  WHEN State = 'SV'  AND Country = 'Spain'        THEN 'Seville'
  WHEN State = 'AP'  AND Country = 'India'        THEN 'Andhra Pradesh (Vijayawada)'
  WHEN State = 'SC'  AND Country = 'UK'           THEN 'Scotland (Edinburgh)'
  WHEN State = 'LD'  AND Country = 'UK' THEN 'Leeds'
  WHEN State = 'YK'  AND Country = 'UK'THEN 'York'
  WHEN State = 'RD'  AND Country = 'UK'THEN 'Reading'
  WHEN State = 'CF'  AND Country = 'UK'THEN 'Cardiff'
  WHEN State = 'DB' AND Country = 'UK' THEN 'Derbyshire'
  WHEN State = 'PL' AND Country = 'UK' THEN 'Plymouth'
  WHEN State = 'EX' AND Country = 'UK' THEN 'Exeter'
  WHEN State = 'SN' AND Country = 'UK' THEN 'Swindon'
  WHEN State = 'SL' AND Country = 'UK' THEN 'Slough'
  WHEN State = 'CR' AND Country = 'UK' THEN 'Croydon'
  WHEN State = 'WD' AND Country = 'UK' THEN 'Watford'
  WHEN State = 'EN' AND Country = 'UK' THEN 'Enfield'
  WHEN State = 'EP' AND Country = 'UK' THEN 'Epping'
  WHEN State = 'RH' AND Country = 'UK' THEN 'Redhill'
  WHEN State = 'DK' AND Country = 'UK' THEN 'Doncaster'
  WHEN State = 'SE' AND Country = 'UK' THEN 'South East London'
  WHEN State = 'AS' AND Country = 'UK' THEN 'Aylesbury'
  WHEN State = 'MG' AND Country = 'UK' THEN 'Manchester'
  WHEN State = 'RC' AND Country = 'UK' THEN 'Rochdale'
  WHEN State = 'DF' AND Country = 'UK' THEN 'Dartford'
  WHEN State = 'ER' AND Country = 'UK' THEN 'Erdington'
  WHEN State = 'LW' AND Country = 'UK' THEN 'Lewisham'
  WHEN State = 'GW' AND Country = 'UK' THEN 'Greenwich'
  WHEN State = 'DP' AND Country = 'UK' THEN 'Dundee'
  WHEN State = 'PK' AND Country = 'UK' THEN 'Perth and Kinross'
  END

SELECT * FROM CustomerData
SELECT * FROM ProductData

--Creating Sales for Aggregate
ALTER TABLE ProductData
ADD Sales DECIMAL(18,2)

UPDATE ProductData
SET Sales = Quantity * Price

SELECT * FROM ProductData

-- Creating The Discount Column As per Sub-category
ALTER TABLE ProductData
ADD Discount DECIMAL(10,2)

--Inserting the Values in the Column of Discount
UPDATE ProductData
SET Discount = CASE SubCategory
    WHEN 'Computers'      THEN 0.00
    WHEN 'Accessories'    THEN 0.01
    WHEN 'Office'         THEN 0.02
    WHEN 'Gadgets'        THEN 0.03
    WHEN 'Writing'        THEN 0.04
    WHEN 'Paper'          THEN 0.05
    WHEN 'Displays'       THEN 0.06
    WHEN 'Networking'    THEN 0.07
    WHEN 'Audio'          THEN 0.08
    WHEN 'Home'           THEN 0.09
    ELSE 0.10
END;

---Creating the New Columns Such as Price_With_Discount, Revenue & Net_Sales
ALTER TABLE ProductData
ADD Price_with_discount DECIMAL(18,2),
    Net_Sales DECIMAL(18,2),
    

--Inserting the Values in the All 3 Column 
Update ProductData
SET Price_with_discount = Price - (Price * Discount)

UPDATE ProductData
SET Profit = Net_Sales - ShippingCost

SELECT * FROM ProductData

--Joining All 3 Table in One data
SELECT * From ProductData p INNER JOIN OrderData o
ON p.ProdId = o.ProdId
INNER JOIN CustomerData c
ON o.ProdId = c.ProdId

SELECT * From ProductData

--Find the Product Which do not Have Order 
SELECT * From ProductData p LEFT JOIN OrderData o 
ON p.ProdId = o.ProdId
WHERE o.ProdId IS NULL

-- Count the Number of Orders Per Product
SELECT p.ProductName,
       COUNT(o.ProdId) AS Order_Count
FROM ProductData p 
INNER  JOIN OrderData o
  ON o.ProdId = p.ProdId
GROUP BY p.ProductName;

SELECT ProductName  From ProductData

SELECT * From ProductData

---------------------CONCLUSION-----------------------------
---This project demonstrates the use of SQL for database creation, data transformation,
---and business analysis using Customer, Order, and Product data. By applying joins, 
--calculations, and aggregate functions, meaningful insights such as sales, discounts,
---profits, and order trends were generated to support data-driven decision-making.










  

