temp_data <- read.csv("Temperature.csv", header = FALSE)
temp_states <- read.csv("state_codes.csv")

colnames(temp_data) <- c("Division_Year","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

#Parse our the state, division and year from code
temp_data$State_code <- substr(temp_data$Division_Year, 1, nchar(temp_data$Division_Year)-8)
temp_data$Division <- substr(temp_data$Division_Year, nchar(temp_data$Division_Year)-7, nchar(temp_data$Division_Year)-6)
#temp_data$Unknown <- substr(temp_data$Division_Year, nchar(temp_data$Division_Year)-5, nchar(temp_data$Division_Year)-4)
temp_data$Year <- substr(temp_data$Division_Year, nchar(temp_data$Division_Year)-3, nchar(temp_data$Division_Year))

#Convert state code to name
temp_data <- merge(temp_data, temp_states)

#Remove unnecessary columns
temp_data <- temp_data[,-c(1:2)]

#Replace missing values (-99.99) with NA
temp_data[temp_data == '-99.99'] <- NA

#Generate yearly average
temp_data$Yearly_Avg <- rowMeans(temp_data[,1:12])

#Generate Index based on state abbreviation and year
temp_data$state_yr <- paste0(temp_data$Abbreviation, temp_data$Year)

#inventory_yr_state <- inventory %>% 
# mutate(state_yr = paste0(STATE_ABBR,EVAL_GRP_YEAR)) %>% 
# group_by(state_yr, STATE_ABBR, EVAL_GRP_YEAR) %>% 
# summarise(total_inventory = sum(SELECT_WHITE_OAK_INV_CUFT_GS, na.rm = TRUE))

#inventory_temp <- merge(inventory_yr_state, temp_data[,17:18])

colnames(temp_data)[14] <- "EVAL_GRP_YEAR"  
colnames(temp_data)[16] <- "STATE_ABBR"

oak_total <- merge(oak_total, temp_data, by = c("EVAL_GRP_YEAR", "STATE_ABBR"))
oak_total <- oak_total[,c(1:7, 22)]
colnames(oak_total)[8] <- "yearly_avg_temp"

#Average annual level by state
# temp_data %>% 
#         filter(Abbreviation %in% eastern_states) %>% 
#         filter(Year > 1968) %>% 
#         ggplot(aes(x = Year, y = Yearly_Avg, colour = Abbreviation)) +
#         geom_line() +
#         labs(title = "Annual Palmer Drought Serverity Index",
#              y = "Index",
#              x = "Year") +
#         theme_minimal()

oak_temp_summary <- oak_total %>% 
  group_by(EVAL_GRP_YEAR) %>% 
  summarise(total_removal = sum(total_removal, na.rm = TRUE),
            total_growth = sum(total_growth, na.rm = TRUE),
            total_mortality = sum(total_mortality, na.rm = TRUE),
            total_inventory = sum(total_inventory, na.rm = TRUE),
            avg_temp = mean(yearly_avg_temp, na.rm = TRUE))

temp_growth_mod <- lm(total_growth ~ avg_temp, data = oak_temp_summary)
plot(total_growth ~ avg_temp, data = oak_temp_summary)
summary(temp_growth_mod)
cor.test(oak_temp_summary$total_growth, oak_temp_summary$avg_temp)

temp_mortality_mod <- lm(total_mortality ~ avg_temp, data = oak_temp_summary)
plot(total_mortality ~ avg_temp, data = oak_temp_summary)
summary(temp_mortality_mod)
cor.test(oak_temp_summary$total_mortality, oak_temp_summary$avg_temp)
