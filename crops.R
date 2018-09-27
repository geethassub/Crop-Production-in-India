library(dplyr)
library(ggplot2)
options(scipen=999)
setwd("C:/Geetha/AV/Crop Production")

file <- read.csv(file.path(getwd(), "crop_production.csv"))
names(file)

#Explore the data 

str(file)

file$Crop_Year <- as.factor(file$Crop_Year)

summary(file)
file <- file[!file$Crop_Year == 1997,]#1997 doesnt have much data
file <- file[!file$Crop_Year == 2015,]#2015 doesnt have much data

#FInd the region with highest Production

ggplot(file, aes(State_Name, Production))+geom_bar(stat = "identity", fill = "blue")+
  theme(axis.text.x = element_text(angle = 90))+xlab("State")+ylab("Production in million")

#Kerala has highest production. Let us now analyse data for kerala. 


State <- function(data, state){
  file_s <- subset(data, State_Name == state)
  season_summary <- aggregate(Production~State_Name+District_Name+Season, data = file_s, mean)
  ggplot(file_s, aes(Crop, Production))+geom_bar(stat = "identity")+theme(axis.text.x = element_text(angle = 90))
  ggplot(file_s, aes(Season,Production))+geom_bar(stat = "identity")+theme(axis.text.x = element_text(angle = 90))
}
State(file, "Kerala" )

#From the above graphs, we could infer that coconut is produced maximum in kerala and Season "Whole Year" has the maximum production. Hence we will
#analyse Coconut and other crops seperately. We will also analyse Season Whole year and other seasons seperately.

###################################################################################################

#District - wise analysis for Kerala for Coconut crop

K_C <- subset(file, State_Name == "Kerala" & Crop == "Coconut ")
ggplot(K_C, aes(reorder(District_Name,-Production), Production))+geom_bar(stat = "identity")+
  theme(axis.text.x = element_text(angle = 90))+ xlab("District")+ylab("Production")+
  labs(title = "Coconut Production in Kerala")+facet_grid(~Season)


#select top 5 ditsricts where coconut is produced

K_D_C <- subset(K_C, District_Name %in% c("KOZHIKODE", "MALAPPURAM","THIRUVANANTHAPURAM", "THRISSUR", "KANNUR"))
ggplot(K_D_C, aes(reorder(District_Name,-Production), Production))+geom_bar(stat = "identity")+
  theme(axis.text.x = element_text(angle = 90))+ xlab("District")+ylab("Production")+
  labs(title = "Coconut Production in top 5 districst in Kerala")+facet_grid(~Crop_Year)+
  coord_flip()

#From the plot, we can see the trend in the coconut production. Kozhikkode was the highest number of coconut manufacturer initially,
#but later malapuram had more production than kozikkode and now they are at the same level.

ggplot(K_D_C, aes(reorder(District_Name,-Area), Area))+geom_bar(stat = "identity")+
  theme(axis.text.x = element_text(angle = 90))+ xlab("District")+ylab("Area")+
  labs(title = "Area under production")+facet_grid(~Crop_Year)


#The increase in production is due to increase in the area under production.

#####################################################################################################


#District - wise analysis for Kerala for crops other than coconut and not in whole year

K_O <- subset(file, State_Name == "Kerala" & Crop != "Coconut " & Season != "Whole Year ")
add <- setNames(aggregate(K_O$Production, by =list(K_O$District_Name), FUN =sum, na.rm = TRUE),c("District_Name", "Production"))

ggplot(add, aes(x = reorder(District_Name,-Production), y =Production))+
  geom_bar(stat = "identity")+
  theme(axis.text.x = element_text(angle = 90))+ xlab("District")+ylab("Production")+
  labs(title = "Crops (other than Coconut) Production in Kerala (Other than Whole Year)")



#Kollam, Palakkad, Thiruvananthapuram, Idukki, Malappuram are the hightes producers of crops other than coconut.

K_D_O <- subset(file, District_Name %in% c("ALAPPUZHA", "PALAKKAD","THRISSUR" ,"ERNAKULAM","KOTTAYAM"))
K_D_O_C <- subset(K_D_O, Crop != "Coconut "& Season != "Whole Year ")
add_1 <- K_D_O_C%>%
  group_by(Crop, Crop_Year)%>%
  filter(!is.na(Production))%>%
  mutate(perc = Production/sum(Production))

ggplot(add_1, aes(District_Name, perc, fill = Crop))+
  geom_bar(stat = "identity", position = "dodge")+
  theme(axis.text.x = element_text(angle = 90))+ xlab("District")+ylab("Production")+
  labs(title = "Other crops Production in top 5 districst in Kerala(Other than Whole Year)")+
  facet_grid(~Crop_Year)


K_D_O_R <- subset(K_D_O_C, Crop =="Rice")

ggplot(K_D_O_R, aes(reorder(District_Name,Production), Production, fill = Crop))+
  geom_bar(stat = "identity", position = "dodge")+
  theme(axis.text.x = element_text(angle = 90))+ xlab("District")+ylab("Production")+
  labs(title = "Rice Production in top 5 districst in Kerala")+
  facet_grid(~Crop_Year)

ggplot(K_D_O_R, aes(reorder(District_Name,Area), Area, fill = Crop))+
  geom_bar(stat = "identity", position = "dodge")+
  theme(axis.text.x = element_text(angle = 90))+ xlab("District")+ylab("Area")+
  labs(title = "Rice Production in top 5 districst in Kerala")+
  facet_grid(~Crop_Year)


########################################################################################################


#District wise analysis for Whole year for Kerala, other than coconut

K_W <- subset(file, State_Name == "Kerala" & Season == "Whole Year " & Crop != "Coconut ")
add_2 <- K_W%>%
  group_by(District_Name, Crop,Crop_Year)%>%
  filter(!is.na(Production))%>%
  mutate(perc = Production/sum(Production))


ggplot(add_2, aes(reorder(District_Name,-Production), Production))+
  geom_bar(stat = "identity", na.rm = TRUE, aes(y = Production/sum(Production)))+
  theme(axis.text.x = element_text(angle = 90))+ xlab("District")+ylab("Production")+
  labs(title = "Season - Whole Year in Kerala")+
  scale_y_continuous(labels = scales::percent)

K_D_W <- subset(file, District_Name %in% c("THIRUVANANTHAPURAM", "PALAKKAD","KOLLAM", "IDUKKI", "MALAPPURAM"))
K_D_W <- subset(K_D_W, Crop != "Coconut "& Season == "Whole Year ")
add_2 <- K_D_W%>%
  group_by(District_Name, Crop, Crop_Year)%>%
  filter(!is.na(Production))%>%
  mutate(perc = Production/sum(Production))

ggplot(add_2, aes(reorder(District_Name,Production), Production, fill = Crop))+
  geom_bar(stat = "identity", position = "dodge")+
  theme(axis.text.x = element_text(angle = 90))+ xlab("District")+ylab("Production")+
  labs(title = "Crops(other than coconut) Production in top 5 districst in Kerala(Whole Year)")+
  geom_text(aes(label = Crop), show.legend = F)

#############################################

#District wise analysis for Seasons other than Whole year for Kerala

K_W_O <- subset(file, State_Name == "Kerala" & Season != "Whole Year " & Crop != "Coconut ")
add_3 <- K_W_O%>%
  group_by(District_Name, Crop,Crop_Year)%>%
  filter(!is.na(Production))%>%
  mutate(perc = Production/sum(Production))


ggplot(add_3, aes(reorder(District_Name,Production), Production))+
  geom_bar(stat = "identity", na.rm = TRUE, aes(y = Production/sum(Production)))+
  theme(axis.text.x = element_text(angle = 90))+ xlab("District")+ylab("Production")+
  labs(title = "Season - Other than Whole Year in Kerala")+
  scale_y_continuous(labels = scales::percent)


K_W_O_D <- subset(K_W_O, District_Name %in% c("PALAKKAD", "ALAPPUZHA", "THRISSUR", "KOTTAYAM", "ERNAKULAM")) 

ggplot(K_W_O_D, aes(reorder(District_Name,Production), Production, fill = Crop))+
  geom_bar(stat = "identity", position = "dodge")+
  theme(axis.text.x = element_text(angle = 90))+ xlab("District")+ylab("Production")+
  labs(title = "Crops(other than coconut) Production in top 5 districst in Kerala(other than Whole Year)")+
  facet_grid(~Season)



##########################################################################################################
##########################################################################################################
##########################################################################################################

#After Kerala, Andra Prades has high crop production. 

A <- subset(file, State_Name = "Andhra Pradesh")
a_1 <- A%>%
  group_by(Crop)%>%
  filter(!is.na(Production)) %>% 
  summarise (Production = mean(Production))

ggplot(A, aes(Crop, Production))+geom_bar(stat = "identity", fill = "red")+
  theme(axis.text.x = element_text(angle = 90))+xlab("Cropr")+ylab("Production")+
  labs(title= "Crops Produced in Andra")

#Coconut is produced in maximum

ggplot(A, aes(Season, Production))+geom_bar(stat = "identity", fill = "red")+
  theme(axis.text.x = element_text(angle = 90))+xlab("Cropr")+ylab("Season")+
  labs(title= "Crops Produced in Andra")


#Whole year has maximum production

###################################################################################################
#District - wise analysis for Andra Pradesh for Coconut crop

A_C <- subset(file, State_Name == "Andhra Pradesh" & Crop == "Coconut ")
ggplot(A_C, aes(reorder(District_Name,-Production), Production))+geom_bar(stat = "identity")+
  theme(axis.text.x = element_text(angle = 90))+ xlab("District")+ylab("Production")+
  labs(title = "Coconut Production in Andra Pradesh")+facet_grid(~Season)


#select top 3 ditsricts where coconut is produced

A_D_C <- subset(A_C, District_Name %in% c("EAST GODAVARI", "WEST GODAVARI", "SRIKAKULAM"))
ggplot(A_D_C, aes(reorder(District_Name,-Production), Production))+geom_bar(stat = "identity")+
  theme(axis.text.x = element_text(angle = 90))+ xlab("District")+ylab("Production")+
  labs(title = "Coconut Production in top 5 districst in Kerala")+facet_grid(~Crop_Year)+
  coord_flip()

#From the plot, we can see the trend in the coconut production. Kozhikkode was the highest number of coconut manufacturer initially,
#but later malapuram had more production than kozikkode and now they are at the same level.

ggplot(K_D_C, aes(reorder(District_Name,-Area), Area))+geom_bar(stat = "identity")+
  theme(axis.text.x = element_text(angle = 90))+ xlab("District")+ylab("Area")+
  labs(title = "Area under production")+facet_grid(~Crop_Year)


#Increase in area for malapuram

#####################################################################################################


































