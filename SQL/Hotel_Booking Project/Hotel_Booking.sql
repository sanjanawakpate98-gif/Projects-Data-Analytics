USe Project
Create TABLE Hotel(
Hotel_Id Varchar(10),
Booking_ID VARCHAR(10) PRIMARY KEY,
Hotel_Name VARCHAR(100),
City VARCHAR(100),
State VARCHAR(100),
Total_Rooms INT
)
Select * From Hotel

INSERT INTO Hotel (Hotel_Id, Booking_ID, Hotel_Name, City, State, Total_Rooms) 
VALUES
('H001', 'B001', 'Royal Palace Hotel', 'Mumbai', 'Maharashtra', 120),
('H002', 'B002', 'Sunshine Inn', 'Pune', 'Maharashtra', 85),
('H003', 'B003', 'Ocean View Resort', 'Goa', 'Goa', 150),
('H004', 'B004', 'Heritage Stay', 'Jaipur', 'Rajasthan', 90),
('H005', 'B005', 'Mountain Retreat', 'Manali', 'Himachal Pradesh', 60),
('H006', 'B006', 'City Lights Hotel', 'Delhi', 'Delhi', 130),
('H007', 'B007', 'Lotus Palace', 'Bangalore', 'Karnataka', 95),
('H008', 'B008', 'Golden Sands', 'Chennai', 'Tamil Nadu', 110),
('H009', 'B009', 'Royal Comfort', 'Kolkata', 'West Bengal', 100),
('H010', 'B010', 'Silver Oak Hotel', 'Hyderabad', 'Telangana', 105),
('H011', 'B011', 'Emerald Stay', 'Ahmedabad', 'Gujarat', 80),
('H012', 'B012', 'Grand Horizon', 'Lucknow', 'Uttar Pradesh', 75),
('H013', 'B013', 'Blue Lagoon Resort', 'Goa', 'Goa', 140),
('H014', 'B014', 'Maple Inn', 'Shimla', 'Himachal Pradesh', 70),
('H015', 'B015', 'Royal Garden', 'Jaipur', 'Rajasthan', 85),
('H016', 'B016', 'City Comforts', 'Pune', 'Maharashtra', 90),
('H017', 'B017', 'Sunset Resort', 'Mumbai', 'Maharashtra', 120),
('H018', 'B018', 'Palm Grove Hotel', 'Bangalore', 'Karnataka', 95),
('H019', 'B019', 'Lotus Inn', 'Chennai', 'Tamil Nadu', 100),
('H020', 'B020', 'Silver Line Hotel', 'Delhi', 'Delhi', 130),
('H021', 'B021', 'Royal Horizon', 'Kolkata', 'West Bengal', 110),
('H022', 'B022', 'Golden Leaf Hotel', 'Hyderabad', 'Telangana', 90),
('H023', 'B023', 'Ocean Breeze Inn', 'Goa', 'Goa', 150),
('H024', 'B024', 'Sunflower Resort', 'Mumbai', 'Maharashtra', 80),
('H025', 'B025', 'Lotus Garden', 'Bangalore', 'Karnataka', 100),
('H026', 'B026', 'Maple Retreat', 'Shimla', 'Himachal Pradesh', 60),
('H027', 'B027', 'City Royale', 'Delhi', 'Delhi', 130),
('H028', 'B028', 'Emerald Palace', 'Jaipur', 'Rajasthan', 95),
('H029', 'B029', 'Silver Sands Hotel', 'Chennai', 'Tamil Nadu', 110),
('H030', 'B030', 'Golden Horizon', 'Pune', 'Maharashtra', 85),
('H031', 'B031', 'Sunset Inn', 'Goa', 'Goa', 120),
('H032', 'B032', 'Ocean Pearl Resort', 'Mumbai', 'Maharashtra', 140),
('H033', 'B033', 'Royal Lotus Hotel', 'Bangalore', 'Karnataka', 100),
('H034', 'B034', 'Maple Comfort', 'Shimla', 'Himachal Pradesh', 70),
('H035', 'B035', 'City View Inn', 'Delhi', 'Delhi', 125),
('H036', 'B036', 'Emerald Retreat', 'Jaipur', 'Rajasthan', 80),
('H037', 'B037', 'Silver Creek Hotel', 'Chennai', 'Tamil Nadu', 105),
('H038', 'B038', 'Golden Leaf Resort', 'Kolkata', 'West Bengal', 90),
('H039', 'B039', 'Ocean Mist Inn', 'Goa', 'Goa', 150),
('H040', 'B040', 'Sunrise Hotel', 'Mumbai', 'Maharashtra', 115),
('H041', 'B041', 'Lotus Horizon', 'Bangalore', 'Karnataka', 95),
('H042', 'B042', 'Maple Palace', 'Shimla', 'Himachal Pradesh', 70),
('H043', 'B043', 'City Comfort Inn', 'Delhi', 'Delhi', 130),
('H044', 'B044', 'Emerald Sands', 'Jaipur', 'Rajasthan', 85),
('H045', 'B045', 'Silver Horizon', 'Chennai', 'Tamil Nadu', 100),
('H046', 'B046', 'Golden Pearl Hotel', 'Hyderabad', 'Telangana', 110),
('H047', 'B047', 'Ocean View Inn', 'Goa', 'Goa', 140),
('H048', 'B048', 'Sunset Horizon', 'Mumbai', 'Maharashtra', 120),
('H049', 'B049', 'Lotus Comfort', 'Bangalore', 'Karnataka', 95),
('H050', 'B050', 'Maple View Hotel', 'Shimla', 'Himachal Pradesh', 65),
('H051', 'B051', 'City Royale Inn', 'Delhi', 'Delhi', 125),
('H052', 'B052', 'Emerald Horizon', 'Jaipur', 'Rajasthan', 90),
('H053', 'B053', 'Silver Sands Resort', 'Chennai', 'Tamil Nadu', 105),
('H054', 'B054', 'Golden Bay Hotel', 'Kolkata', 'West Bengal', 100),
('H055', 'B055', 'Ocean Breeze Resort', 'Goa', 'Goa', 150),
('H056', 'B056', 'Sunshine Palace', 'Mumbai', 'Maharashtra', 120),
('H057', 'B057', 'Lotus Bay Inn', 'Bangalore', 'Karnataka', 95),
('H058', 'B058', 'Maple Horizon', 'Shimla', 'Himachal Pradesh', 70),
('H059', 'B059', 'City Comforts Hotel', 'Delhi', 'Delhi', 130),
('H060', 'B060', 'Emerald Sands Inn', 'Jaipur', 'Rajasthan', 85),
('H061', 'B061', 'Silver Pearl Hotel', 'Chennai', 'Tamil Nadu', 110),
('H062', 'B062', 'Golden Crest Hotel', 'Hyderabad', 'Telangana', 100),
('H063', 'B063', 'Ocean Wave Resort', 'Goa', 'Goa', 150),
('H064', 'B064', 'Sunrise Palace', 'Mumbai', 'Maharashtra', 115),
('H065', 'B065', 'Lotus Retreat', 'Bangalore', 'Karnataka', 95),
('H066', 'B066', 'Maple Sands', 'Shimla', 'Himachal Pradesh', 70),
('H067', 'B067', 'City Horizon', 'Delhi', 'Delhi', 125),
('H068', 'B068', 'Emerald Bay Hotel', 'Jaipur', 'Rajasthan', 90),
('H069', 'B069', 'Silver Crest Inn', 'Chennai', 'Tamil Nadu', 105),
('H070', 'B070', 'Golden Ocean Hotel', 'Kolkata', 'West Bengal', 100),
('H071', 'B071', 'Ocean Pearl Inn', 'Goa', 'Goa', 150),
('H072', 'B072', 'Sunset Bay Hotel', 'Mumbai', 'Maharashtra', 120),
('H073', 'B073', 'Lotus Sands', 'Bangalore', 'Karnataka', 95),
('H074', 'B074', 'Maple Retreat Hotel', 'Shimla', 'Himachal Pradesh', 65),
('H075', 'B075', 'City View Palace', 'Delhi', 'Delhi', 130),
('H076', 'B076', 'Emerald Bay Inn', 'Jaipur', 'Rajasthan', 85),
('H077', 'B077', 'Silver Horizon Resort', 'Chennai', 'Tamil Nadu', 100),
('H078', 'B078', 'Golden Bay Inn', 'Hyderabad', 'Telangana', 110),
('H079', 'B079', 'Ocean Crest Hotel', 'Goa', 'Goa', 140),
('H080', 'B080', 'Sunrise Bay Inn', 'Mumbai', 'Maharashtra', 115),
('H081', 'B081', 'Lotus Pearl Hotel', 'Bangalore', 'Karnataka', 95),
('H082', 'B082', 'Maple Horizon Inn', 'Shimla', 'Himachal Pradesh', 70),
('H083', 'B083', 'City Comfort Palace', 'Delhi', 'Delhi', 125),
('H084', 'B084', 'Emerald Pearl Inn', 'Jaipur', 'Rajasthan', 90),
('H085', 'B085', 'Silver Bay Hotel', 'Chennai', 'Tamil Nadu', 105),
('H086', 'B086', 'Golden Sands Inn', 'Kolkata', 'West Bengal', 100),
('H087', 'B087', 'Ocean Bay Resort', 'Goa', 'Goa', 150),
('H088', 'B088', 'Sunset Pearl Hotel', 'Mumbai', 'Maharashtra', 120),
('H089', 'B089', 'Lotus Crest Inn', 'Bangalore', 'Karnataka', 95),
('H090', 'B090', 'Maple Bay Hotel', 'Shimla', 'Himachal Pradesh', 70),
('H091', 'B091', 'City Horizon Inn', 'Delhi', 'Delhi', 130),
('H092', 'B092', 'Emerald Crest Hotel', 'Jaipur', 'Rajasthan', 85),
('H093', 'B093', 'Silver Sands Inn', 'Chennai', 'Tamil Nadu', 105),
('H094', 'B094', 'Golden Pearl Resort', 'Hyderabad', 'Telangana', 100),
('H095', 'B095', 'Ocean Horizon Inn', 'Goa', 'Goa', 150),
('H096', 'B096', 'Sunrise Pearl Hotel', 'Mumbai', 'Maharashtra', 115),
('H097', 'B097', 'Lotus Bay Hotel', 'Bangalore', 'Karnataka', 95),
('H098', 'B098', 'Maple Crest Inn', 'Shimla', 'Himachal Pradesh', 70),
('H099', 'B099', 'City Pearl Inn', 'Delhi', 'Delhi', 125),
('H100', 'B100', 'Emerald Horizon Resort', 'Jaipur', 'Rajasthan', 90),
('H101', 'B101', 'Golden Horizon Palace', 'Kolkata', 'West Bengal', 110),
('H102', 'B102', 'Ocean Pearl Resort', 'Goa', 'Goa', 150),
('H103', 'B103', 'Sunset Bay Inn', 'Mumbai', 'Maharashtra', 120),
('H104', 'B104', 'Lotus Pearl Palace', 'Bangalore', 'Karnataka', 95),
('H105', 'B105', 'Maple Bay Resort', 'Shimla', 'Himachal Pradesh', 70),
('H106', 'B106', 'City View Horizon', 'Delhi', 'Delhi', 130),
('H107', 'B107', 'Emerald Bay Inn', 'Jaipur', 'Rajasthan', 85),
('H108', 'B108', 'Silver Crest Resort', 'Chennai', 'Tamil Nadu', 100),
('H109', 'B109', 'Golden Sands Palace', 'Hyderabad', 'Telangana', 110),
('H110', 'B110', 'Ocean Horizon Palace', 'Goa', 'Goa', 140),
('H111', 'B111', 'Sunrise Pearl Inn', 'Mumbai', 'Maharashtra', 115),
('H112', 'B112', 'Lotus Bay Resort', 'Bangalore', 'Karnataka', 95),
('H113', 'B113', 'Maple Crest Palace', 'Shimla', 'Himachal Pradesh', 70),
('H114', 'B114', 'City Pearl Resort', 'Delhi', 'Delhi', 125),
('H115', 'B115', 'Emerald Horizon Inn', 'Jaipur', 'Rajasthan', 90),
('H116', 'B116', 'Silver Sands Palace', 'Chennai', 'Tamil Nadu', 105),
('H117', 'B117', 'Golden Pearl Inn', 'Hyderabad', 'Telangana', 100),
('H118', 'B118', 'Ocean Bay Inn', 'Goa', 'Goa', 150),
('H119', 'B119', 'Sunset Pearl Resort', 'Mumbai', 'Maharashtra', 120),
('H120', 'B120', 'Lotus Crest Palace', 'Bangalore', 'Karnataka', 95),
('H121', 'B121', 'Maple Bay Inn', 'Shimla', 'Himachal Pradesh', 70),
('H122', 'B122', 'City Horizon Resort', 'Delhi', 'Delhi', 130),
('H123', 'B123', 'Emerald Crest Inn', 'Jaipur', 'Rajasthan', 85),
('H124', 'B124', 'Silver Sands Bay', 'Chennai', 'Tamil Nadu', 105),
('H125', 'B125', 'Golden Bay Inn', 'Hyderabad', 'Telangana', 100),
('H126', 'B126', 'Ocean Horizon Bay', 'Goa', 'Goa', 150),
('H127', 'B127', 'Sunrise Bay Resort', 'Mumbai', 'Maharashtra', 115),
('H128', 'B128', 'Lotus Pearl Inn', 'Bangalore', 'Karnataka', 95),
('H129', 'B129', 'Maple Horizon Bay', 'Shimla', 'Himachal Pradesh', 70),
('H130', 'B130', 'City Comfort Bay', 'Delhi', 'Delhi', 125),
('H131', 'B131', 'Emerald Pearl Bay', 'Jaipur', 'Rajasthan', 90),
('H132', 'B132', 'Silver Horizon Bay', 'Chennai', 'Tamil Nadu', 105),
('H133', 'B133', 'Golden Crest Resort', 'Hyderabad', 'Telangana', 100),
('H134', 'B134', 'Ocean Bay Palace', 'Goa', 'Goa', 150),
('H135', 'B135', 'Sunset Horizon Inn', 'Mumbai', 'Maharashtra', 120),
('H136', 'B136', 'Lotus Bay Palace', 'Bangalore', 'Karnataka', 95),
('H137', 'B137', 'Maple Pearl Inn', 'Shimla', 'Himachal Pradesh', 70),
('H138', 'B138', 'City Horizon Bay', 'Delhi', 'Delhi', 130),
('H139', 'B139', 'Emerald Crest Bay', 'Jaipur', 'Rajasthan', 85),
('H140', 'B140', 'Silver Sands Horizon', 'Chennai', 'Tamil Nadu', 100),
('H141', 'B141', 'Golden Pearl Bay', 'Hyderabad', 'Telangana', 110),
('H142', 'B142', 'Ocean Horizon Inn', 'Goa', 'Goa', 140),
('H143', 'B143', 'Sunrise Horizon Palace', 'Mumbai', 'Maharashtra', 115),
('H144', 'B144', 'Lotus Crest Bay', 'Bangalore', 'Karnataka', 95),
('H145', 'B145', 'Maple Bay Horizon', 'Shimla', 'Himachal Pradesh', 70),
('H146', 'B146', 'City Pearl Horizon', 'Delhi', 'Delhi', 125),
('H147', 'B147', 'Emerald Bay Horizon', 'Jaipur', 'Rajasthan', 90),
('H148', 'B148', 'Silver Crest Bay', 'Chennai', 'Tamil Nadu', 105),
('H149', 'B149', 'Golden Horizon Bay', 'Hyderabad', 'Telangana', 100),
('H150', 'B150', 'Ocean Pearl Bay', 'Goa', 'Goa', 150),
('H151', 'B151', 'Royal Kerala Resort', 'Kochi', 'Kerala', 120),
('H152', 'B152', 'Sunrise Palace', 'Trivandrum', 'Kerala', 90),
('H153', 'B153', 'Lotus Bay Inn', 'Kochi', 'Kerala', 100),
('H154', 'B154', 'Maple Horizon Hotel', 'Trivandrum', 'Kerala', 80),
('H155', 'B155', 'City Comforts', 'Chandigarh', 'Punjab', 110),
('H156', 'B156', 'Emerald Palace', 'Amritsar', 'Punjab', 95),
('H157', 'B157', 'Silver Sands Resort', 'Chandigarh', 'Punjab', 100),
('H158', 'B158', 'Golden Bay Inn', 'Amritsar', 'Punjab', 120),
('H159', 'B159', 'Ocean Horizon Hotel', 'Puri', 'Odisha', 130),
('H160', 'B160', 'Sunset Palace', 'Bhubaneswar', 'Odisha', 110),
('H161', 'B161', 'Lotus Pearl Resort', 'Puri', 'Odisha', 140),
('H162', 'B162', 'Maple Bay Hotel', 'Bhubaneswar', 'Odisha', 100),
('H163', 'B163', 'City Horizon Inn', 'Dehradun', 'Uttarakhand', 90),
('H164', 'B164', 'Emerald Bay Hotel', 'Mussoorie', 'Uttarakhand', 85),
('H165', 'B165', 'Silver Crest Inn', 'Dehradun', 'Uttarakhand', 100),
('H166', 'B166', 'Golden Sands Resort', 'Mussoorie', 'Uttarakhand', 120),
('H167', 'B167', 'Ocean Pearl Inn', 'Nagpur', 'Maharashtra', 115),
('H168', 'B168', 'Sunset Bay Hotel', 'Nashik', 'Maharashtra', 95),
('H169', 'B169', 'Lotus Horizon Palace', 'Nagpur', 'Maharashtra', 105),
('H170', 'B170', 'Maple Pearl Resort', 'Nashik', 'Maharashtra', 90),
('H171', 'B171', 'City Comfort Bay', 'Raipur', 'Chhattisgarh', 100),
('H172', 'B172', 'Emerald Horizon Inn', 'Bilaspur', 'Chhattisgarh', 85),
('H173', 'B173', 'Silver Bay Palace', 'Raipur', 'Chhattisgarh', 110),
('H174', 'B174', 'Golden Pearl Resort', 'Bilaspur', 'Chhattisgarh', 120),
('H175', 'B175', 'Ocean Crest Hotel', 'Ranchi', 'Jharkhand', 130),
('H176', 'B176', 'Sunrise Horizon Inn', 'Jamshedpur', 'Jharkhand', 115),
('H177', 'B177', 'Lotus Bay Resort', 'Ranchi', 'Jharkhand', 95),
('H178', 'B178', 'Maple Pearl Hotel', 'Jamshedpur', 'Jharkhand', 105),
('H179', 'B179', 'City Horizon Palace', 'Patna', 'Bihar', 120),
('H180', 'B180', 'Emerald Crest Resort', 'Gaya', 'Bihar', 100),
('H181', 'B181', 'Silver Sands Bay', 'Patna', 'Bihar', 110),
('H182', 'B182', 'Golden Horizon Inn', 'Gaya', 'Bihar', 95),
('H183', 'B183', 'Ocean Bay Resort', 'Kolkata', 'West Bengal', 140),
('H184', 'B184', 'Sunset Pearl Inn', 'Darjeeling', 'West Bengal', 100),
('H185', 'B185', 'Lotus Crest Bay', 'Kolkata', 'West Bengal', 115),
('H186', 'B186', 'Maple Bay Horizon', 'Darjeeling', 'West Bengal', 90),
('H187', 'B187', 'City Pearl Horizon', 'Hyderabad', 'Telangana', 120),
('H188', 'B188', 'Emerald Bay Horizon', 'Warangal', 'Telangana', 85),
('H189', 'B189', 'Silver Crest Bay', 'Hyderabad', 'Telangana', 100),
('H190', 'B190', 'Golden Horizon Bay', 'Warangal', 'Telangana', 110),
('H191', 'B191', 'Ocean Pearl Bay', 'Visakhapatnam', 'Andhra Pradesh', 150),
('H192', 'B192', 'Sunrise Horizon Palace', 'Vijayawada', 'Andhra Pradesh', 95),
('H193', 'B193', 'Lotus Crest Bay', 'Visakhapatnam', 'Andhra Pradesh', 105),
('H194', 'B194', 'Maple Bay Horizon', 'Vijayawada', 'Andhra Pradesh', 90),
('H195', 'B195', 'City Pearl Horizon', 'Shillong', 'Meghalaya', 80),
('H196', 'B196', 'Emerald Bay Horizon', 'Mawlynnong', 'Meghalaya', 70),
('H197', 'B197', 'Silver Crest Bay', 'Shillong', 'Meghalaya', 85),
('H198', 'B198', 'Golden Horizon Bay', 'Mawlynnong', 'Meghalaya', 75),
('H199', 'B199', 'Ocean Pearl Bay', 'Imphal', 'Manipur', 95),
('H200', 'B200', 'Sunset Horizon Inn', 'Kohima', 'Nagaland', 80),
('H201', 'B201', 'Royal Palm Hotel', 'Surat', 'Gujarat', 120),
('H202', 'B202', 'Sunshine Bay Inn', 'Vadodara', 'Gujarat', 95),
('H203', 'B203', 'Lotus Horizon Palace', 'Surat', 'Gujarat', 110),
('H204', 'B204', 'Maple Comfort Hotel', 'Vadodara', 'Gujarat', 85),
('H205', 'B205', 'City Pearl Resort', 'Udaipur', 'Rajasthan', 130),
('H206', 'B206', 'Emerald Bay Inn', 'Jodhpur', 'Rajasthan', 100),
('H207', 'B207', 'Silver Crest Palace', 'Udaipur', 'Rajasthan', 115),
('H208', 'B208', 'Golden Sands Hotel', 'Jodhpur', 'Rajasthan', 120),
('H209', 'B209', 'Ocean Horizon Inn', 'Mysore', 'Karnataka', 105),
('H210', 'B210', 'Sunset Bay Palace', 'Mangalore', 'Karnataka', 90),
('H211', 'B211', 'Lotus Pearl Resort', 'Mysore', 'Karnataka', 100),
('H212', 'B212', 'Maple Bay Inn', 'Mangalore', 'Karnataka', 85),
('H213', 'B213', 'City Horizon Palace', 'Kochi', 'Kerala', 120),
('H214', 'B214', 'Emerald Crest Hotel', 'Thiruvananthapuram', 'Kerala', 95),
('H215', 'B215', 'Silver Sands Bay', 'Kochi', 'Kerala', 110),
('H216', 'B216', 'Golden Horizon Bay', 'Thiruvananthapuram', 'Kerala', 100),
('H217', 'B217', 'Ocean Pearl Bay', 'Chandigarh', 'Punjab', 130),
('H218', 'B218', 'Sunrise Horizon Palace', 'Amritsar', 'Punjab', 105),
('H219', 'B219', 'Lotus Crest Bay', 'Chandigarh', 'Punjab', 110),
('H220', 'B220', 'Maple Bay Horizon', 'Amritsar', 'Punjab', 90),
('H221', 'B221', 'City Pearl Horizon', 'Puri', 'Odisha', 120),
('H222', 'B222', 'Emerald Bay Horizon', 'Bhubaneswar', 'Odisha', 95),
('H223', 'B223', 'Silver Crest Bay', 'Puri', 'Odisha', 100),
('H224', 'B224', 'Golden Horizon Bay', 'Bhubaneswar', 'Odisha', 110),
('H225', 'B225', 'Ocean Pearl Bay', 'Dehradun', 'Uttarakhand', 85),
('H226', 'B226', 'Sunset Horizon Inn', 'Mussoorie', 'Uttarakhand', 75),
('H227', 'B227', 'Lotus Bay Resort', 'Dehradun', 'Uttarakhand', 95),
('H228', 'B228', 'Maple Pearl Hotel', 'Mussoorie', 'Uttarakhand', 80),
('H229', 'B229', 'City Horizon Palace', 'Nagpur', 'Maharashtra', 120),
('H230', 'B230', 'Emerald Crest Resort', 'Nashik', 'Maharashtra', 100),
('H231', 'B231', 'Silver Sands Bay', 'Nagpur', 'Maharashtra', 110),
('H232', 'B232', 'Golden Horizon Inn', 'Nashik', 'Maharashtra', 95),
('H233', 'B233', 'Ocean Bay Resort', 'Ranchi', 'Jharkhand', 130),
('H234', 'B234', 'Sunrise Horizon Inn', 'Jamshedpur', 'Jharkhand', 105),
('H235', 'B235', 'Lotus Crest Bay', 'Ranchi', 'Jharkhand', 100),
('H236', 'B236', 'Maple Bay Horizon', 'Jamshedpur', 'Jharkhand', 115),
('H237', 'B237', 'City Pearl Horizon', 'Patna', 'Bihar', 120),
('H238', 'B238', 'Emerald Bay Horizon', 'Gaya', 'Bihar', 95),
('H239', 'B239', 'Silver Crest Bay', 'Patna', 'Bihar', 110),
('H240', 'B240', 'Golden Horizon Bay', 'Gaya', 'Bihar', 100),
('H241', 'B241', 'Ocean Pearl Bay', 'Shillong', 'Meghalaya', 85),
('H242', 'B242', 'Sunset Horizon Inn', 'Mawlynnong', 'Meghalaya', 70),
('H243', 'B243', 'Lotus Bay Resort', 'Shillong', 'Meghalaya', 95),
('H244', 'B244', 'Maple Pearl Hotel', 'Mawlynnong', 'Meghalaya', 80),
('H245', 'B245', 'City Horizon Palace', 'Imphal', 'Manipur', 100),
('H246', 'B246', 'Emerald Crest Resort', 'Kohima', 'Nagaland', 90),
('H247', 'B247', 'Silver Sands Bay', 'Imphal', 'Manipur', 95),
('H248', 'B248', 'Golden Horizon Inn', 'Kohima', 'Nagaland', 80),
('H249', 'B249', 'Ocean Bay Resort', 'Gangtok', 'Sikkim', 85),
('H250', 'B250', 'Sunrise Horizon Inn', 'Pelling', 'Sikkim', 70),
('H251', 'B251', 'Lotus Crest Bay', 'Gangtok', 'Sikkim', 90),
('H252', 'B252', 'Maple Bay Horizon', 'Pelling', 'Sikkim', 75),
('H253', 'B253', 'City Pearl Horizon', 'Raipur', 'Chhattisgarh', 110),
('H254', 'B254', 'Emerald Bay Horizon', 'Bilaspur', 'Chhattisgarh', 95),
('H255', 'B255', 'Silver Crest Bay', 'Raipur', 'Chhattisgarh', 100),
('H256', 'B256', 'Golden Horizon Bay', 'Bilaspur', 'Chhattisgarh', 105),
('H257', 'B257', 'Ocean Pearl Bay', 'Visakhapatnam', 'Andhra Pradesh', 140),
('H258', 'B258', 'Sunset Horizon Inn', 'Vijayawada', 'Andhra Pradesh', 95),
('H259', 'B259', 'Lotus Bay Resort', 'Visakhapatnam', 'Andhra Pradesh', 110),
('H260', 'B260', 'Maple Pearl Hotel', 'Vijayawada', 'Andhra Pradesh', 100),
('H261', 'B261', 'City Horizon Palace', 'Thane', 'Maharashtra', 120),
('H262', 'B262', 'Emerald Crest Resort', 'Navi Mumbai', 'Maharashtra', 105),
('H263', 'B263', 'Silver Sands Bay', 'Thane', 'Maharashtra', 110),
('H264', 'B264', 'Golden Horizon Inn', 'Navi Mumbai', 'Maharashtra', 95),
('H265', 'B265', 'Ocean Bay Resort', 'Aurangabad', 'Maharashtra', 130),
('H266', 'B266', 'Sunrise Horizon Inn', 'Nagpur', 'Maharashtra', 105),
('H267', 'B267', 'Lotus Crest Bay', 'Aurangabad', 'Maharashtra', 115),
('H268', 'B268', 'Maple Bay Horizon', 'Nagpur', 'Maharashtra', 90),
('H269', 'B269', 'City Pearl Horizon', 'Bhopal', 'Madhya Pradesh', 100),
('H270', 'B270', 'Emerald Bay Horizon', 'Indore', 'Madhya Pradesh', 95);

Select * FROM Hotel

CREATE TABLE Bookings (
    Booking_ID VARCHAR(100) PRIMARY KEY,
    Hotel_ID VARCHAR(10),
    Room_Type VARCHAR(20),
    Price_Per_Night DECIMAL(8,2),
    Payment_Type VARCHAR(100),
    Days_waiting INT,
    Check_In DATE,
    Check_Out DATE
    );

INSERT INTO Bookings VALUES
('B001','H001','Deluxe',2500,'Online',2,'2026-04-01','2026-04-03'),
('B002','H002','Standard',1800,'Cash',1,'2026-04-02','2026-04-04'),
('B003','H003','Suite',4000,'Card',3,'2026-04-03','2026-04-06'),
('B004','H004','Deluxe',2600,'Online',2,'2026-04-04','2026-04-06'),
('B005','H005','Standard',1700,'Cash',1,'2026-04-05','2026-04-07'),
('B006','H006','Suite',4200,'Card',4,'2026-04-06','2026-04-09'),
('B007','H007','Standard',1500,'Online',2,'2026-04-07','2026-04-09'),
('B008','H008','Deluxe',2800,'Card',3,'2026-04-08','2026-04-10'),
('B009','H009','Suite',4100,'Cash',2,'2026-04-09','2026-04-12'),
('B010','H010','Deluxe',2550,'Online',1,'2026-04-10','2026-04-12'),
('B011','H011','Standard',1750,'Cash',2,'2026-04-11','2026-04-13'),
('B012','H012','Suite',4300,'Card',3,'2026-04-12','2026-04-15'),
('B013','H013','Standard',1600,'Online',1,'2026-04-13','2026-04-15'),
('B014','H014','Deluxe',2900,'Card',2,'2026-04-14','2026-04-17'),
('B015','H015','Suite',4500,'Cash',4,'2026-04-15','2026-04-18'),
('B016','H016','Standard',1800,'Online',2,'2026-04-16','2026-04-18'),
('B017','H017','Deluxe',2700,'Card',3,'2026-04-17','2026-04-20'),
('B018','H018','Suite',4400,'Cash',2,'2026-04-18','2026-04-21'),
('B019','H019','Standard',1650,'Online',1,'2026-04-19','2026-04-21'),
('B020','H020','Deluxe',3000,'Card',2,'2026-04-20','2026-04-23'),
('B021','H021','Suite',4200,'Cash',3,'2026-04-21','2026-04-24'),
('B022','H022','Standard',1500,'Online',1,'2026-04-22','2026-04-24'),
('B023','H023','Deluxe',2800,'Card',2,'2026-04-23','2026-04-26'),
('B024','H024','Suite',4600,'Cash',4,'2026-04-24','2026-04-27'),
('B025','H025','Standard',1700,'Online',2,'2026-04-25','2026-04-27'),
('B026','H026','Deluxe',2900,'Card',3,'2026-04-26','2026-04-29'),
('B027','H027','Suite',4100,'Cash',2,'2026-04-27','2026-04-30'),
('B028','H028','Standard',1600,'Online',1,'2026-04-28','2026-04-30'),
('B029','H029','Deluxe',2750,'Card',2,'2026-04-29','2026-05-02'),
('B030','H030','Suite',4500,'Cash',3,'2026-04-30','2026-05-03'),
('B031','H031','Standard',1550,'Online',1,'2026-05-01','2026-05-03'),
('B032','H032','Deluxe',3000,'Card',2,'2026-05-02','2026-05-05'),
('B033','H033','Suite',4700,'Cash',4,'2026-05-03','2026-05-06'),
('B034','H034','Standard',1650,'Online',2,'2026-05-04','2026-05-06'),
('B035','H035','Deluxe',2800,'Card',3,'2026-05-05','2026-05-08'),
('B036','H036','Suite',4300,'Cash',2,'2026-05-06','2026-05-09'),
('B037','H037','Standard',1700,'Online',1,'2026-05-07','2026-05-09'),
('B038','H038','Deluxe',2600,'Card',2,'2026-05-08','2026-05-11'),
('B039','H039','Suite',4400,'Cash',3,'2026-05-09','2026-05-12'),
('B040','H040','Standard',1600,'Online',1,'2026-05-10','2026-05-12'),
('B041','H041','Deluxe',2900,'Card',2,'2026-05-11','2026-05-14'),
('B042','H042','Suite',4600,'Cash',4,'2026-05-12','2026-05-15'),
('B043','H043','Standard',1500,'Online',1,'2026-05-13','2026-05-15'),
('B044','H044','Deluxe',2750,'Card',2,'2026-05-14','2026-05-17'),
('B045','H045','Suite',4200,'Cash',3,'2026-05-15','2026-05-18'),
('B046','H046','Standard',1650,'Online',1,'2026-05-16','2026-05-18'),
('B047','H047','Deluxe',3000,'Card',2,'2026-05-17','2026-05-20'),
('B048','H048','Suite',4700,'Cash',4,'2026-05-18','2026-05-21'),
('B049','H049','Standard',1700,'Online',2,'2026-05-19','2026-05-21'),
('B050','H050','Deluxe',2800,'Card',3,'2026-05-20','2026-05-23'),
('B051','H051','Suite',4500,'Cash',3,'2026-05-21','2026-05-24'),
('B052','H052','Standard',1600,'Online',1,'2026-05-22','2026-05-24'),
('B053','H053','Deluxe',2900,'Card',2,'2026-05-23','2026-05-26'),
('B054','H054','Suite',4600,'Cash',4,'2026-05-24','2026-05-27'),
('B055','H055','Standard',1500,'Online',1,'2026-05-25','2026-05-27'),
('B056','H056','Deluxe',2750,'Card',2,'2026-05-26','2026-05-29'),
('B057','H057','Suite',4300,'Cash',3,'2026-05-27','2026-05-30'),
('B058','H058','Standard',1650,'Online',1,'2026-05-28','2026-05-30'),
('B059','H059','Deluxe',3000,'Card',2,'2026-05-29','2026-06-01'),
('B060','H060','Suite',4700,'Cash',4,'2026-05-30','2026-06-02'),
('B061','H061','Standard',1600,'Online',1,'2026-05-31','2026-06-02'),
('B062','H062','Deluxe',2800,'Card',2,'2026-06-01','2026-06-04'),
('B063','H063','Suite',4500,'Cash',3,'2026-06-02','2026-06-05'),
('B064','H064','Standard',1500,'Online',1,'2026-06-03','2026-06-05'),
('B065','H065','Deluxe',2900,'Card',2,'2026-06-04','2026-06-07'),
('B066','H066','Suite',4600,'Cash',4,'2026-06-05','2026-06-08'),
('B067','H067','Standard',1700,'Online',1,'2026-06-06','2026-06-08'),
('B068','H068','Deluxe',2750,'Card',2,'2026-06-07','2026-06-10'),
('B069','H069','Suite',4400,'Cash',3,'2026-06-08','2026-06-11'),
('B070','H070','Standard',1600,'Online',1,'2026-06-09','2026-06-11'),
('B071','H071','Deluxe',2800,'Card',2,'2026-06-10','2026-06-13'),
('B072','H072','Suite',4600,'Cash',3,'2026-06-11','2026-06-14'),
('B073','H073','Standard',1600,'Online',1,'2026-06-12','2026-06-14'),
('B074','H074','Deluxe',2900,'Card',2,'2026-06-13','2026-06-16'),
('B075','H075','Suite',4700,'Cash',4,'2026-06-14','2026-06-17'),
('B076','H076','Standard',1500,'Online',1,'2026-06-15','2026-06-17'),
('B077','H077','Deluxe',2750,'Card',2,'2026-06-16','2026-06-19'),
('B078','H078','Suite',4400,'Cash',3,'2026-06-17','2026-06-20'),
('B079','H079','Standard',1650,'Online',1,'2026-06-18','2026-06-20'),
('B080','H080','Deluxe',3000,'Card',2,'2026-06-19','2026-06-22'),
('B081','H081','Suite',4500,'Cash',3,'2026-06-20','2026-06-23'),
('B082','H082','Standard',1700,'Online',1,'2026-06-21','2026-06-23'),
('B083','H083','Deluxe',2800,'Card',2,'2026-06-22','2026-06-25'),
('B084','H084','Suite',4600,'Cash',4,'2026-06-23','2026-06-26'),
('B085','H085','Standard',1600,'Online',1,'2026-06-24','2026-06-26'),
('B086','H086','Deluxe',2900,'Card',2,'2026-06-25','2026-06-28'),
('B087','H087','Suite',4700,'Cash',3,'2026-06-26','2026-06-29'),
('B088','H088','Standard',1500,'Online',1,'2026-06-27','2026-06-29'),
('B089','H089','Deluxe',2750,'Card',2,'2026-06-28','2026-07-01'),
('B090','H090','Suite',4400,'Cash',3,'2026-06-29','2026-07-02'),
('B091','H091','Standard',1650,'Online',1,'2026-06-30','2026-07-02'),
('B092','H092','Deluxe',3000,'Card',2,'2026-07-01','2026-07-04'),
('B093','H093','Suite',4600,'Cash',4,'2026-07-02','2026-07-05'),
('B094','H094','Standard',1700,'Online',1,'2026-07-03','2026-07-05'),
('B095','H095','Deluxe',2800,'Card',2,'2026-07-04','2026-07-07'),
('B096','H096','Suite',4500,'Cash',3,'2026-07-05','2026-07-08'),
('B097','H097','Standard',1600,'Online',1,'2026-07-06','2026-07-08'),
('B098','H098','Deluxe',2900,'Card',2,'2026-07-07','2026-07-10'),
('B099','H099','Suite',4700,'Cash',4,'2026-07-08','2026-07-11'),
('B100','H100','Standard',1500,'Online',1,'2026-07-09','2026-07-11'),
('B101','H101','Deluxe',2750,'Card',2,'2026-07-10','2026-07-13'),
('B102','H102','Suite',4400,'Cash',3,'2026-07-11','2026-07-14'),
('B103','H103','Standard',1650,'Online',1,'2026-07-12','2026-07-14'),
('B104','H104','Deluxe',3000,'Card',2,'2026-07-13','2026-07-16'),
('B105','H105','Suite',4600,'Cash',4,'2026-07-14','2026-07-17'),
('B106','H106','Standard',1700,'Online',1,'2026-07-15','2026-07-17'),
('B107','H107','Deluxe',2800,'Card',2,'2026-07-16','2026-07-19'),
('B108','H108','Suite',4500,'Cash',3,'2026-07-17','2026-07-20'),
('B109','H109','Standard',1600,'Online',1,'2026-07-18','2026-07-20'),
('B110','H110','Deluxe',2900,'Card',2,'2026-07-19','2026-07-22'),
('B111','H111','Suite',4700,'Cash',4,'2026-07-20','2026-07-23'),
('B112','H112','Standard',1500,'Online',1,'2026-07-21','2026-07-23'),
('B113','H113','Deluxe',2750,'Card',2,'2026-07-22','2026-07-25'),
('B114','H114','Suite',4400,'Cash',3,'2026-07-23','2026-07-26'),
('B115','H115','Standard',1650,'Online',1,'2026-07-24','2026-07-26'),
('B116','H116','Deluxe',3000,'Card',2,'2026-07-25','2026-07-28'),
('B117','H117','Suite',4600,'Cash',4,'2026-07-26','2026-07-29'),
('B118','H118','Standard',1700,'Online',1,'2026-07-27','2026-07-29'),
('B119','H119','Deluxe',2800,'Card',2,'2026-07-28','2026-07-31'),
('B120','H120','Suite',4500,'Cash',3,'2026-07-29','2026-08-01'),
('B121','H121','Standard',1600,'Online',1,'2026-07-30','2026-08-01'),
('B122','H122','Deluxe',2900,'Card',2,'2026-07-31','2026-08-03'),
('B123','H123','Suite',4700,'Cash',4,'2026-08-01','2026-08-04'),
('B124','H124','Standard',1500,'Online',1,'2026-08-02','2026-08-04'),
('B125','H125','Deluxe',2750,'Card',2,'2026-08-03','2026-08-06'),
('B126','H126','Suite',4400,'Cash',3,'2026-08-04','2026-08-07'),
('B127','H127','Standard',1650,'Online',1,'2026-08-05','2026-08-07'),
('B128','H128','Deluxe',3000,'Card',2,'2026-08-06','2026-08-09'),
('B129','H129','Suite',4600,'Cash',4,'2026-08-07','2026-08-10'),
('B130','H130','Standard',1700,'Online',1,'2026-08-08','2026-08-10'),
('B131','H131','Deluxe',2800,'Card',2,'2026-08-09','2026-08-12'),
('B132','H132','Suite',4500,'Cash',3,'2026-08-10','2026-08-13'),
('B133','H133','Standard',1600,'Online',1,'2026-08-11','2026-08-13'),
('B134','H134','Deluxe',2900,'Card',2,'2026-08-12','2026-08-15'),
('B135','H135','Suite',4700,'Cash',4,'2026-08-13','2026-08-16'),
('B136','H136','Standard',1500,'Online',1,'2026-08-14','2026-08-16'),
('B137','H137','Deluxe',2750,'Card',2,'2026-08-15','2026-08-18'),
('B138','H138','Suite',4400,'Cash',3,'2026-08-16','2026-08-19'),
('B139','H139','Standard',1650,'Online',1,'2026-08-17','2026-08-19'),
('B140','H140','Deluxe',3000,'Card',2,'2026-08-18','2026-08-21'),
('B141','H141','Suite',4600,'Cash',3,'2026-08-19','2026-08-22'),
('B142','H142','Standard',1700,'Online',1,'2026-08-20','2026-08-22'),
('B143','H143','Deluxe',2800,'Card',2,'2026-08-21','2026-08-24'),
('B144','H144','Suite',4500,'Cash',4,'2026-08-22','2026-08-25'),
('B145','H145','Standard',1600,'Online',1,'2026-08-23','2026-08-25'),
('B146','H146','Deluxe',2900,'Card',2,'2026-08-24','2026-08-27'),
('B147','H147','Suite',4700,'Cash',3,'2026-08-25','2026-08-28'),
('B148','H148','Standard',1500,'Online',1,'2026-08-26','2026-08-28'),
('B149','H149','Deluxe',2750,'Card',2,'2026-08-27','2026-08-30'),
('B150','H150','Suite',4400,'Cash',3,'2026-08-28','2026-08-31'),
('B151','H151','Standard',1650,'Online',1,'2026-08-29','2026-08-31'),
('B152','H152','Deluxe',3000,'Card',2,'2026-08-30','2026-09-02'),
('B153','H153','Suite',4600,'Cash',4,'2026-08-31','2026-09-03'),
('B154','H154','Standard',1700,'Online',1,'2026-09-01','2026-09-03'),
('B155','H155','Deluxe',2800,'Card',2,'2026-09-02','2026-09-05'),
('B156','H156','Suite',4500,'Cash',3,'2026-09-03','2026-09-06'),
('B157','H157','Standard',1600,'Online',1,'2026-09-04','2026-09-06'),
('B158','H158','Deluxe',2900,'Card',2,'2026-09-05','2026-09-08'),
('B159','H159','Suite',4700,'Cash',4,'2026-09-06','2026-09-09'),
('B160','H160','Standard',1500,'Online',1,'2026-09-07','2026-09-09'),
('B161','H161','Deluxe',2750,'Card',2,'2026-09-08','2026-09-11'),
('B162','H162','Suite',4400,'Cash',3,'2026-09-09','2026-09-12'),
('B163','H163','Standard',1650,'Online',1,'2026-09-10','2026-09-12'),
('B164','H164','Deluxe',3000,'Card',2,'2026-09-11','2026-09-14'),
('B165','H165','Suite',4600,'Cash',4,'2026-09-12','2026-09-15'),
('B166','H166','Standard',1700,'Online',1,'2026-09-13','2026-09-15'),
('B167','H167','Deluxe',2800,'Card',2,'2026-09-14','2026-09-17'),
('B168','H168','Suite',4500,'Cash',3,'2026-09-15','2026-09-18'),
('B169','H169','Standard',1600,'Online',1,'2026-09-16','2026-09-18'),
('B170','H170','Deluxe',2900,'Card',2,'2026-09-17','2026-09-20'),
('B171','H171','Suite',4700,'Cash',4,'2026-09-18','2026-09-21'),
('B172','H172','Standard',1500,'Online',1,'2026-09-19','2026-09-21'),
('B173','H173','Deluxe',2750,'Card',2,'2026-09-20','2026-09-23'),
('B174','H174','Suite',4400,'Cash',3,'2026-09-21','2026-09-24'),
('B175','H175','Standard',1650,'Online',1,'2026-09-22','2026-09-24'),
('B176','H176','Deluxe',3000,'Card',2,'2026-09-23','2026-09-26'),
('B177','H177','Suite',4600,'Cash',4,'2026-09-24','2026-09-27'),
('B178','H178','Standard',1700,'Online',1,'2026-09-25','2026-09-27'),
('B179','H179','Deluxe',2800,'Card',2,'2026-09-26','2026-09-29'),
('B180','H180','Suite',4500,'Cash',3,'2026-09-27','2026-09-30'),
('B181','H181','Standard',1600,'Online',1,'2026-09-28','2026-09-30'),
('B182','H182','Deluxe',2900,'Card',2,'2026-09-29','2026-10-02'),
('B183','H183','Suite',4700,'Cash',4,'2026-09-30','2026-10-03'),
('B184','H184','Standard',1500,'Online',1,'2026-10-01','2026-10-03'),
('B185','H185','Deluxe',2750,'Card',2,'2026-10-02','2026-10-05'),
('B186','H186','Suite',4400,'Cash',3,'2026-10-03','2026-10-06'),
('B187','H187','Standard',1650,'Online',1,'2026-10-04','2026-10-06'),
('B188','H188','Deluxe',3000,'Card',2,'2026-10-05','2026-10-08'),
('B189','H189','Suite',4600,'Cash',4,'2026-10-06','2026-10-09'),
('B190','H190','Standard',1700,'Online',1,'2026-10-07','2026-10-09'),
('B191','H191','Deluxe',2800,'Card',2,'2026-10-08','2026-10-11'),
('B192','H192','Suite',4500,'Cash',3,'2026-10-09','2026-10-12'),
('B193','H193','Standard',1600,'Online',1,'2026-10-10','2026-10-12'),
('B194','H194','Deluxe',2900,'Card',2,'2026-10-11','2026-10-14'),
('B195','H195','Suite',4700,'Cash',4,'2026-10-12','2026-10-15'),
('B196','H196','Standard',1500,'Online',1,'2026-10-13','2026-10-15'),
('B197','H197','Deluxe',2750,'Card',2,'2026-10-14','2026-10-17'),
('B198','H198','Suite',4400,'Cash',3,'2026-10-15','2026-10-18'),
('B199','H199','Standard',1650,'Online',1,'2026-10-16','2026-10-18'),
('B200','H200','Deluxe',3000,'Card',2,'2026-10-17','2026-10-20'),
('B201','H201','Suite',4600,'Cash',4,'2026-10-18','2026-10-21'),
('B202','H202','Standard',1700,'Online',1,'2026-10-19','2026-10-21'),
('B203','H203','Deluxe',2800,'Card',2,'2026-10-20','2026-10-23'),
('B204','H204','Suite',4500,'Cash',3,'2026-10-21','2026-10-24'),
('B205','H205','Standard',1600,'Online',1,'2026-10-22','2026-10-24'),
('B206','H206','Deluxe',2900,'Card',2,'2026-10-23','2026-10-26'),
('B207','H207','Suite',4700,'Cash',4,'2026-10-24','2026-10-27'),
('B208','H208','Standard',1500,'Online',1,'2026-10-25','2026-10-27'),
('B209','H209','Deluxe',2750,'Card',2,'2026-10-26','2026-10-29'),
('B210','H210','Suite',4400,'Cash',3,'2026-10-27','2026-10-30'),
('B211','H211','Standard',1600,'Online',1,'2026-10-28','2026-10-30'),
('B212','H212','Deluxe',2800,'Card',2,'2026-10-29','2026-11-01'),
('B213','H213','Suite',4600,'Cash',3,'2026-10-30','2026-11-02'),
('B214','H214','Standard',1700,'Online',1,'2026-10-31','2026-11-02'),
('B215','H215','Deluxe',2900,'Card',2,'2026-11-01','2026-11-04'),
('B216','H216','Suite',4700,'Cash',4,'2026-11-02','2026-11-05'),
('B217','H217','Standard',1500,'Online',1,'2026-11-03','2026-11-05'),
('B218','H218','Deluxe',2750,'Card',2,'2026-11-04','2026-11-07'),
('B219','H219','Suite',4400,'Cash',3,'2026-11-05','2026-11-08'),
('B220','H220','Standard',1650,'Online',1,'2026-11-06','2026-11-08'),
('B221','H221','Deluxe',3000,'Card',2,'2026-11-07','2026-11-10'),
('B222','H222','Suite',4600,'Cash',4,'2026-11-08','2026-11-11'),
('B223','H223','Standard',1700,'Online',1,'2026-11-09','2026-11-11'),
('B224','H224','Deluxe',2800,'Card',2,'2026-11-10','2026-11-13'),
('B225','H225','Suite',4500,'Cash',3,'2026-11-11','2026-11-14'),
('B226','H226','Standard',1600,'Online',1,'2026-11-12','2026-11-14'),
('B227','H227','Deluxe',2900,'Card',2,'2026-11-13','2026-11-16'),
('B228','H228','Suite',4700,'Cash',4,'2026-11-14','2026-11-17'),
('B229','H229','Standard',1500,'Online',1,'2026-11-15','2026-11-17'),
('B230','H230','Deluxe',2750,'Card',2,'2026-11-16','2026-11-19'),
('B231','H231','Suite',4400,'Cash',3,'2026-11-17','2026-11-20'),
('B232','H232','Standard',1650,'Online',1,'2026-11-18','2026-11-20'),
('B233','H233','Deluxe',3000,'Card',2,'2026-11-19','2026-11-22'),
('B234','H234','Suite',4600,'Cash',4,'2026-11-20','2026-11-23'),
('B235','H235','Standard',1700,'Online',1,'2026-11-21','2026-11-23'),
('B236','H236','Deluxe',2800,'Card',2,'2026-11-22','2026-11-25'),
('B237','H237','Suite',4500,'Cash',3,'2026-11-23','2026-11-26'),
('B238','H238','Standard',1600,'Online',1,'2026-11-24','2026-11-26'),
('B239','H239','Deluxe',2900,'Card',2,'2026-11-25','2026-11-28'),
('B240','H240','Suite',4700,'Cash',4,'2026-11-26','2026-11-29'),
('B241','H241','Standard',1500,'Online',1,'2026-11-27','2026-11-29'),
('B242','H242','Deluxe',2750,'Card',2,'2026-11-28','2026-12-01'),
('B243','H243','Suite',4400,'Cash',3,'2026-11-29','2026-12-02'),
('B244','H244','Standard',1650,'Online',1,'2026-11-30','2026-12-02'),
('B245','H245','Deluxe',3000,'Card',2,'2026-12-01','2026-12-04'),
('B246','H246','Suite',4600,'Cash',4,'2026-12-02','2026-12-05'),
('B247','H247','Standard',1700,'Online',1,'2026-12-03','2026-12-05'),
('B248','H248','Deluxe',2800,'Card',2,'2026-12-04','2026-12-07'),
('B249','H249','Suite',4500,'Cash',3,'2026-12-05','2026-12-08'),
('B250','H250','Standard',1600,'Online',1,'2026-12-06','2026-12-08'),
('B251','H251','Deluxe',2900,'Card',2,'2026-12-07','2026-12-10'),
('B252','H252','Suite',4700,'Cash',4,'2026-12-08','2026-12-11'),
('B253','H253','Standard',1500,'Online',1,'2026-12-09','2026-12-11'),
('B254','H254','Deluxe',2750,'Card',2,'2026-12-10','2026-12-13'),
('B255','H255','Suite',4400,'Cash',3,'2026-12-11','2026-12-14'),
('B256','H256','Standard',1650,'Online',1,'2026-12-12','2026-12-14'),
('B257','H257','Deluxe',3000,'Card',2,'2026-12-13','2026-12-16'),
('B258','H258','Suite',4600,'Cash',4,'2026-12-14','2026-12-17'),
('B259','H259','Standard',1700,'Online',1,'2026-12-15','2026-12-17'),
('B260','H260','Deluxe',2800,'Card',2,'2026-12-16','2026-12-19');


SELECT * FRom Bookings

--Using Joins
SELECT h.Hotel_Id,h.Hotel_name,h.City,h.State,h.Total_Rooms,b.Booking_Id,
b.Room_Type,b.Price_Per_Night,b.Payment_Type,b.Days_waiting,b.Check_In,b.Check_Out
INTO Hotel_Booking FROM Hotel h LEFT JOIN Bookings b
ON h.Hotel_Id= b.Hotel_ID

Select * From Hotel_Booking

--Creating a New Column that is Region
ALTER TABLE Hotel_Booking
ADD Region VARCHAR(100)

select * FROM Hotel_Booking

--Inserting the Values in the Column of Region
UPDATE Hotel_Booking
SET Region = 
CASE
     WHEN State IN ('Maharashtra', 'Gujarat', 'Goa') THEN 'West'
     WHEN State IN ('Karnataka', 'Tamil Nadu', 'Telangana', 'Kerala', 'Andhra Pradesh') THEN 'South'
     WHEN State IN ('West Bengal', 'Odisha', 'Bihar', 'Jharkhand',
                   'Assam', 'Meghalaya', 'Manipur', 'Nagaland', 'Sikkim') THEN 'East'
    WHEN State IN ('Madhya Pradesh', 'Chhattisgarh') THEN 'East'
    WHEN State IN ('Himachal Pradesh','Punjab','Delhi','Uttar Pradesh','Uttarakhand')THEN 'North'
       
END;

SELECT State,Region FROM Hotel_Booking

SELECT * from Hotel_Booking
WHERE Region = 'NA'



UPDATE Hotel_Booking
SET Region =
CASE
     WHEN State IN ('Rajasthan','Himachal Pradesh','Delhi','Uttar Pradesh','Punjab',
     'Uttarakhand') THEN 'North'
     END

SELECT * FROM Hotel_Booking

--Creation a KPI 
--Total Booking 
SELECT COUNT(*) AS Total_Booking
FROM Hotel_Booking

--Total Revenue    
SELECT SUM(Price_Per_Night * DATEDIFF(DAY, Check_In, Check_Out)) AS Total_Revenue
FROM Hotel_Booking

--Average Room Price
SELECT AVG(Price_Per_Night) AS Avg_Room_Price
From Hotel_Booking

--Analysis of Average Stay Duration

SELECT AVG(DATEDIFF(Day, Check_In, Check_Out)) AS Avg_Stay_Duration
FROM Hotel_Booking

--Room Type Preference Analysis
SELECT Top 1 Room_Type,
COUNT(*) AS Total_Bookings
FROM Hotel_Booking
Group BY Room_Type
ORDER BY Total_Bookings DESC

--Most Commonly Used Payment Method Analysis
SELECT TOP 1 Payment_Type,
COUNT(*) AS Usage_Count
FROM Hotel_Booking
GROUP BY Payment_Type
ORDER BY Usage_Count DESC

--Creating Advanced Analysis
--Aggregate Function-Based Analysis
SELECT Region,Room_Type,
COUNT(*) AS Total_Booking,
SUM(Price_Per_Night * DATEDIFF(DAY, Check_In, Check_Out)) AS Total_Revenue,
AVG(Price_Per_Night * DATEDIFF(DAY, Check_In, Check_Out)) AS Average_Revenue
FROM Hotel_Booking
GROUP BY Region, Room_Type;

--Analytical Processing Using Window Functions
WITH Revenue_CTE AS (SELECT Region,Hotel_Id,
SUM(Price_Per_Night * DATEDIFF(DAY, Check_In, Check_Out)) AS Total_Revenue
FROM Hotel_Booking
GROUP BY Region, Hotel_Id)
SELECT Region, Hotel_Id,Total_Revenue,
RANK() OVER (PARTITION BY Region ORDER BY Total_Revenue DESC) AS Revenue_Rank
FROM Revenue_CTE;

SELECT * From Hotel_Booking

--Analysis Using PIVOT and UNPIVOT Functions
--Pivot
SELECT * FROM (SELECT Region,Room_Type,
(Price_Per_Night * DATEDIFF(DAY, Check_In, Check_Out)) AS Revenue
FROM Hotel_Booking) AS SourceTable
PIVOT
(SUM(Revenue)FOR Room_Type IN ([Standard],[Deluxe],[Suite])
) AS PivotTable;

--Unpivot Table
SELECT Region, Room_Type, Revenue
FROM (SELECT Region, 
[Standard], 
[Deluxe], 
[Suite]
FROM (SELECT Region,Room_Type,
(Price_Per_Night * DATEDIFF(DAY, Check_In, Check_Out)) AS Revenue
FROM Hotel_Booking) AS SourceData
PIVOT 
(SUM(Revenue)FOR Room_Type IN ([Standard], [Deluxe], [Suite])) AS Pvt
) AS PivotedData
UNPIVOT 
(Revenue FOR Room_Type IN ([Standard], [Deluxe], [Suite])) AS Unpvt;

--Trigger-Based Data Management and Automation
ADD Revenue DECIMAL(10,2)
UPDATE Hotel_Booking
SET Revenue = Price_Per_Night * DATEDIFF(DAY, Check_In, Check_Out);

CREATE TRIGGER trg_Calculate_Revenue
ON Hotel_Booking
AFTER INSERT, UPDATE
AS
BEGIN
UPDATE hb
SET hb.Revenue = hb.Price_Per_Night * DATEDIFF(DAY, hb.Check_In, hb.Check_Out)
FROM Hotel_Booking hb INNER JOIN inserted i
ON hb.Booking_ID = i.Booking_ID;
END;

--Use of CUBE for Advanced Data Summarization
SELECT Region, Room_Type,
SUM(Price_Per_Night * DATEDIFF(DAY, Check_In, Check_Out)) AS Total_Revenue,
COUNT(*) AS Total_Booking
FROM Hotel_Booking
GROUP BY CUBE(Region, Room_Type);

--Stored Procedure-Based Data Management
CREATE PROCEDURE sp_Region_Analysis
@RegionName VARCHAR(50)
AS
BEGIN
SELECT 
Hotel_Id,Room_Type,
COUNT(*) AS Total_Booking,
SUM(Price_Per_Night * DATEDIFF(DAY, Check_In, Check_Out)) AS Total_Revenue
FROM Hotel_Booking
WHERE Region = @RegionName
GROUP BY Hotel_Id, Room_Type
ORDER BY SUM(Price_Per_Night * DATEDIFF(DAY, Check_In, Check_Out)) DESC;
END;

EXEC sp_Region_Analysis 'East';

---Analysis of Structured vs. Unstructured Query Approaches
SELECT Region, Count(*) AS Total_Booking,
SUM(Revenue) AS Total_Revenue,
AVG(Revenue) AS Avg_Revenue,
MAX(Revenue) AS Max_Revenue,
MIN(Revenue) AS Min_Revenue
FROM Hotel_Booking
Group By Region
Order BY Total_Revenue DESC

--Application of System Functions in Data Processing
SELECT Booking_ID,
GETDATE() AS CurrentDate,
CONVERT(VARCHAR, Check_In, 106) AS Formatted_CheckIn,
DATEDIFF(DAY, Check_In, Check_Out) AS Stay_Days,
DATENAME(MONTH, Check_In) AS CheckIn_Month,
ISNULL(Payment_Type, 'Online') AS Payment_Status
FROM Hotel_Booking;

Select * From Hotel_Booking



