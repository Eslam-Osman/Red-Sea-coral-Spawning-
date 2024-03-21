

#### SST during Spawning seasons across studied sites ###


# Install and load necessary packages
library(ggplot2)
require(dplyr)


SST_Spawning_final <- read.csv("C:/Eslam/KAUST/Coral spwaning/SST/SST_Spawning_final.csv")

SST_Spawning_final$Site=factor(SST_Spawning_final$Site, levels = c("Thuwal", "Shushah Island", "Hurghada"))

SST_Spawning_final$Date=strptime(as.character(SST_Spawning_final$Date), format = "%d/%m/%Y")
SST_Spawning_final$Date=as.Date(SST_Spawning_final$Date)

# Plot the temperature time series


ggplot(SST_Spawning_final, aes(x=Date, y=AvgTemp, linetype=Source, color=Site)) +
  geom_line(size=1.5)+
  labs(x = "Month (2023)", y = "Temperature (°C)", title = "")+ 
  theme_bw()+
  scale_color_manual(values=c("#1b9e77", "#d95f02","#7570b3"))+
  annotate("rect",  xmin = as.Date("2023-04-4", "%Y-%m-%d"),
         xmax = as.Date("2023-5-7",  "%Y-%m-%d"), 
         ymin=22,
         ymax=32,
         alpha = .1,fill = "blue")+
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  theme(legend.position = c(0.15, 0.75))+
  theme(axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 12),   # Change x-axis label size
        axis.text.y = element_text(size = 12))+   # Change y-axis label size)
  theme(legend.title = element_text(size = 12),
        legend.text = element_text(size = 12))



#scale_linetype_manual(values=c( "dotted", "solid"))

###### SST and warming in 2023 ####

require(readxl)

Histrocal_spawning_record <- read_excel("C:/Eslam/KAUST/Coral spwaning/Histrocal spawning record.xlsx", 
                                        +     sheet = "Sheet1")


SST_23=Histrocal_spawning_record %>% filter(Site=="Hurghada" | Site=="Thuwal" | Site=="Shushah")

SST_23=Histrocal_spawning_record %>% filter(Year=="2023")



ggplot(SST_23, aes(Site, Absolute_SST, color=Site ))+
  geom_point(size=4)+ 
  geom_errorbar(aes(ymin=(SST_23$Absolute_SST - SST_23$SST_SD), ymax=(Absolute_SST+SST_SD), width=.2))+
  theme_bw()+
  labs(x = "Sites", y = "Absolute SST in 2023 (°C)", title = "")+
  scale_color_manual(values=rev(c("#1b9e77", "#d95f02","#7570b3")))+ 
  theme(plot.background = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank())+
  theme(legend.position = c(0.25, 0.75))+
  theme(axis.title = element_text(size = 16),
        axis.text.x = element_text(size = 14),   # Change x-axis label size
        axis.text.y = element_text(size = 14))+   # Change y-axis label size)
  theme(legend.title = element_text(size = 14),
        legend.text = element_text(size = 14))+
  theme(legend.position = "none")
  


### Warming rate in 2023 ## 

ggplot(SST_23, aes(Site, Rate, color=Site ))+
  geom_point(size=4)+ 
  theme_bw()+
  labs(x = "Sites", y = "Warming rate 2023 (°C)", title = "")+
  scale_color_manual(values=rev(c("#1b9e77", "#d95f02","#7570b3")))+ 
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  theme(legend.position = c(0.25, 0.75))+
  theme(axis.title = element_text(size = 16),
        axis.text.x = element_text(size = 14),   # Change x-axis label size
        axis.text.y = element_text(size = 14))+   # Change y-axis label size)
  theme(legend.title = element_text(size = 14),
        legend.text = element_text(size = 14))+
  theme(legend.position = "none")






#### Historcal SST and warming rate ######

SST_Var <- read.csv("C:/Eslam/KAUST/Coral spwaning/Histrocal spawning record_var.csv")
Hist_SST=SST_Var %>% filter(Site=="Hurghada" | Site=="Thuwal" | Site== "Shushah")



#### Historical absolute SST #####


Hist_SST_abs=Hist_SST %>% filter(Para=="Absolute SST")


ggplot(Hist_SST_abs, aes(x=Site, y=Var, fill=Site)) +
  geom_boxplot(alpha=0.7)+
  geom_point(size=5,alpha=0.3, position = position_jitterdodge(0.3))+
  theme_bw()+
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  scale_fill_manual(values=rev(c("#1b9e77", "#d95f02","#7570b3")))+ 
  theme(legend.key = element_blank(), strip.background = element_rect(colour="black", fill="white"))+
  theme(legend.position = "none")+
  labs(x = "", y = "Historical SST mean (°C)", title = "")+ 
  theme(axis.title = element_text(size = 18),
        axis.text.x = element_text(size = 16),   # Change x-axis label size
        axis.text.y = element_text(size = 16))+
  theme(strip.background = element_blank(), strip.text = element_blank())


anova(glm(Var~ Site, data = Hist_SST_abs), test = "F")



#### Historical SST ####


Hist_SST_WR=Hist_SST%>% filter(Para=="Warning rate")

ggplot(Hist_SST_WR, aes(x=Site, y=Var, fill=Site)) +
  geom_boxplot(alpha=0.7)+
  geom_point(size=5,alpha=0.3, position = position_jitterdodge(0.3))+
  theme_bw()+ 
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  scale_fill_manual(values=rev(c("#1b9e77", "#d95f02","#7570b3")))+
  theme(legend.key = element_blank(), strip.background = element_rect(colour="black", fill="white"))+
  theme(legend.position = "none")+
  labs(x = "Site", y = "Warming rate (°C per week)", title = "")+ 
  theme(axis.title = element_text(size = 18),
        axis.text.x = element_text(size = 16),   # Change x-axis label size
        axis.text.y = element_text(size = 16))+
  theme(strip.background = element_blank(), strip.text = element_blank())




anova(glm(Var~ Site, data = Hist_SST_WR), test = "F")



Hist_SST_DT=Hist_SST%>% filter(Para=="Delta T")
anova(glm(Var~ Site, data = Hist_SST_DT), test = "F")




#######
#######



### Heatmap for gonads maturity #######




HeatMap <- read.csv("C:/Eslam/KAUST/Coral spwaning/HeatMap.csv")

HeatMap$Date_1=factor(HeatMap$Date_1, levels = c("3rd Apr", "8th Apr", "15th Apr", "3rd May", "19th May", "13th June", "Observation "))
HeatMap$Notional_sp=factor(HeatMap$Notional_sp, levels = c("A. cf. hyacinthus",
                                                           "A. cf. arabensis",
                                                           "A. cf. secale", 
                                                           "A. maryae",
                                                           "A. hemprichi (brown)",
                                                           "A. pharaonis",
                                                           "A. secale",
                                                           "A. hemprichi (blue)",
                                                           "Acropora sp. 1",
                                                           "A. cf. humilis",
                                                           "A. cf. cytherea",
                                                           "A. hemprichi (beige)",
                                                           "A. eurystoma",
                                                           "A. cf. downingi",
                                                           "A. samoensis"))





# Plot the heatmap 

ggplot(HeatMap, aes(x = Date_1, y =Notional_sp , fill=Grav_perc)) +
  geom_tile(color = "black")+
  labs(x = "Month (2023)", y = "Coral species", title = "", fill="Gravidity (%)")+
  theme_bw()+
  scale_fill_gradient(low = "white", high = "red")+coord_fixed()+
  theme(axis.text.x=element_text(angle = -45, hjust = 0))+
  geom_text(aes(label = Text), color = "black", size = 3)+
  theme(axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12, face = "italic"))+
  theme(legend.title = element_text(size = 12),
        legend.text = element_text(size = 12))
  
  








################

#Donuts plot for corals observed spawning vs non spawning ###


# Create test data.
data <- data.frame(
  category=c("Observed", "Non observed"),
  count=c(11, 12))

# Compute percentages
data$fraction <- data$count / sum(data$count)*100
#[1] 73.91304% 26.08696%

# Compute the cumulative percentages (top of each rectangle)
data$ymax <- cumsum(data$fraction)

# Compute the bottom of each rectangle
data$ymin <- c(0, head(data$ymax, n=-1))

# Compute label position
data$labelPosition <- (data$ymax + data$ymin) / 2

# Compute a good label
data$label <- paste0(data$category, "\n value: ", data$count)


# Make the plot
ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
  geom_rect(alpha=0.9, color="black", linewidth=0.2) +
  geom_text( x=2, aes(y=labelPosition, label=label), size=6) + # x here controls label position (inner / outer)
  scale_fill_manual(values = c("#998ec3", "#542788")) +
  coord_polar(theta="y") +
  xlim(c(0, 4)) +
  theme_void() +
  theme(legend.position = "none")
  



##############

#### Venn diagram for coral species investigated across sites ######


library("ggvenn")



venn<- read.csv("C:/Eslam/KAUST/Coral spwaning/Venn diagram.csv")
venn=as.list(venn[,1:3])


ggvenn(venn)

ggvenn(venn, 
  fill_color = c("#0073C2FF", "#EFC000FF", "#CD534CFF"),
  stroke_size = 0.5, set_name_size = 6, show_percentage = T, show_elements=F)


unique.partition <- VennDiagram::get.venn.partitions(venn)

venn<- read.csv("C:/Eslam/KAUST/Coral spwaning/Venn diagram.csv")
venn_s=as.list(venn[,6:7])
ggvenn(venn_s)

ggvenn(venn_s, 
       fill_color = c("#0073C2FF", "#EFC000FF", "#CD534CFF"),
       stroke_size = 0.5, set_name_size = 6, show_percentage = T, show_elements=F)





#########################

#### correlation between SST vs +/-FM ###
require(ggplot2)
require(readxl)
require(ggpmisc)

Histrocal_spawning_record <- read_excel("Histrocal spawning record.xlsx",sheet = "Sheet1")


SST_23=Histrocal_spawning_record 


# Historical absolute SST vs +/-FM

ggplot(SST_23, aes(x=as.numeric (SST_23$FM), y=as.numeric(SST_23$Absolute_SST)))+ 
  geom_point(aes(color=Site, shape=Site), size=3)+
  theme_bw()+
  labs(x = "+/-FM", y = "Absoult SST mean (°C)", title ="")+
  scale_color_manual(values=c("#1b9e77", "#d95f02","#7570b3", "#b10026"))+geom_smooth(method = lm, se=F, na.rm = T)+
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "adj.R2", "f", "p", "n")))+  
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  theme(legend.position = c(0.85, 0.15))+
  theme(axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 12),   # Change x-axis label size
        axis.text.y = element_text(size = 12))+   # Change y-axis label size)
  theme(legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))





# Historical warming rate vs +/-FM
 
  
ggplot(Histrocal_spawning_record, aes(x=as.numeric (SST_23$FM), y=as.numeric(SST_23$Rate)))+ 
  geom_point(aes(shape=Site, color= Site),size=3)+geom_smooth(method = lm, se=F, na.rm = T)+
  theme_bw()+
  labs(x = "+/-FM", y = "Warming rate (°C per week)", title ="")+
  scale_color_manual(values=c("#1b9e77", "#d95f02","#7570b3", "#b10026")) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "adj.R2", "f", "p", "n")))+
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  theme(legend.position = "none")+
  theme(axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 12),   # Change x-axis label size
        axis.text.y = element_text(size = 12))+   # Change y-axis label size)
  theme(legend.title = element_text(size = 12),
        legend.text = element_text(size = 12))





