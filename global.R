library(readxl)
library(dplyr)


b_plan<- read_excel("B_plan_data.xlsx")%>%
  #mutate(Amount = round(Amount, 0))%>%
  as.data.frame()


rownames(b_plan) <- b_plan$Variable
b_plan[1] <- NULL


PMI_distrib <- 0.29
lost_vect <- 0.123
usage_vect <- 0.4308
insecticide_eff_vect <- 0.0291
wear_tear_vect <- 0.0276

b_plan[3,3] <- b_plan[1,3]*b_plan[2,3]


b_plan <- b_plan %>%
  mutate()
  
typeof(b_plan[2,3])

dollar_format()(c(-100, 0.23, 1.456565, 2e3))
dollar_format()(c(1:10 * 10))
dollar(c(100, 0.23, 1.456565, 2e3))
dollar(c(1:10 * 10))

b_plan[c(3:13,15,16,18),3] <- dollar(b_plan[c(3:13,15,16,18),3])

color1 <-  rep("'palegreen3,'", 8)
color2 <- rep("'skyblue3'", 8)
color3 <- paste(rep("'linen'", 7), collapse=",")
colors <- c(color1, color2, color3)    

library(DT)
library(formattable)

iris_new <- formattable(iris, list(
  Sepal.Width = color_tile("red", "green")
))

df <- b_plan
r1 <- df[19,]
r2 <- df[22,]
r3 <- df[25,]
r4 <- df[28,]
r5 <- df[30,]
r6 <- df[33,]
r7 <- df[36,]
df1 <- rbind(r1, r2, r3, r4, r5, r6, r7) %>%
  as.data.frame() 
df1$lab <- "Total lost value"
df1 <- df1 %>%
  mutate(axis = c("Baseline lost value (all nets)", "Lost value with loss reduction",
             "Lost value with usage improvement", "Lost value with bioefficacy improvement", "Lost value with attrition improvement",
            "Lost value with effectiveness improvement", "Lost value with coverage need reduction"))


p1 <- df1 %>%
  ggplot(aes(x = axis, y = Amount, text = paste("Lost value of LLINs", "$",Amount))) +
  geom_bar( stat = "identity", fill = "#92C5DE", width = 0.4)+
  facet_wrap(~lab)+
  scale_y_continuous(labels = scales::dollar_format())+
  #labs(x = "Improvement")+
  # scale_x_discrete("Improvement", breaks = c("1", "2", "3", "4", "5", "6", "7",
  #                                           labels= c("Baseline lost value (all nets)", "Lost value with loss reduction",
  #                                                      "Lost value with usage improvement", "Lost value with bioefficacy improvement", "Lost value with attrition improvement",
  #                                                      "Lost value with effectiveness improvement", "Lost value with coverage need reduction")))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 10), axis.text.x = element_text(angle = 45),  axis.text.y = element_text(size = 8),
        plot.title = element_text(size = 12, hjust = 0.5), axis.title.x = element_text(size = 10), strip.background =element_rect(fill="white"),
        strip.text = element_text(size = 10, colour = "red", face = "bold-italics"))

ply1 <- ggplotly(p1, tooltip = c("text"))
ply1

p1 <- df %>%
  filter(Variable %in% c("Market size", "Cost of Distribution", "Total Annual Cost"
  ))%>%
  ggplot(aes(x = Variable, y = Percentage*100, text = paste("Percentage:", Percentage*100, "%"))) +
  geom_bar(stat = "identity", fill = "rosybrown1")+
  #geom_text(aes(label = Percentage), position = position_dodge(width = 0.9), vjust= -1)+
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,100))+
  ggtitle("Global Market")+ labs(x = "Value", y = "")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 10), axis.text.x = element_text(angle = 45), axis.text.y = element_text(size = 6),
        plot.title = element_text(size = 12, hjust = 0.5), axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10))

ply1 <- ggplotly(p1, tooltip = c("text", "y", "x"))

p2 <- df %>%
  filter(Variable %in% c("Market size", "Cost of Distribution", "Total Annual Cost"
  ))%>%
  ggplot(aes(x = Variable, y = Amount)) +
  geom_bar(aes(text = paste("Value:", round(Amount, 0))),stat = "identity", fill = "rosybrown4")+
  ggtitle("Global Market")+ labs(x = "Percentage of Total budget", y = "")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 10), axis.text.x = element_text(angle = 45), axis.text.y = element_text(size = 6),
        plot.title = element_text(size = 12, hjust = 0.5), axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10))

ggplotly(p2, tooltip = c("text"))


plot <- subplot(p1, p2, titleX = TRUE, margin = 0.07)
dev.off()
plot


p1 <- ggplot(mtcars, aes(wt, mpg, text = row.names(mtcars))) + 
  geom_point()
ply1<- ggplotly(p1, tooltip = c("text"))

p2 <- ggplot(mtcars, aes(wt, cyl, text = paste("Disp:", disp))) + 
  geom_point()
ply2<- ggplotly(p2, tooltip = c("text"))

ply2

subp <- subplot(ply1, ply2)
subp






#------------------------------------------------------------------------
#------------------------------------------------------------------------


reduction_loss <- df[20,2]*100
improve_usage <- df[23,2]*100
df1 <- df[21:22,]
df1$Lost_value <- "After loss reduction"

total <- df[8,]
total$Lost_value <- "Total lost value"

df3 <- rbind(df1, total)
df3$lab <- paste("Reduce LLIN loss by:", reduction_loss, "%")
#-----------------------------------------------------------------
df2 <- df[24:25,]
df2$lab <- paste("Improve LLIN usage by:", improve_usage, "%")





p1 <- ggplot(df3, aes(x = Variable, y = Amount,  fill = factor(Lost_value), text = paste("Percentage:", Percentage*100, "%"))) +
  geom_bar(stat = "identity", position = position_dodge2(preserve = "single"))+
  facet_wrap(~lab)+
  labs(y = "Value ($)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 10), axis.text.x = element_text(angle = 45),  axis.text.y = element_text(size = 8),
        plot.title = element_text(size = 12, hjust = 0.5), axis.title.x = element_text(size = 10), strip.background =element_rect(fill="white"),
        strip.text = element_text(size = 12, face = "bold", hjust = 0.5))

ply1 <- ggplotly(p1, tooltip = c("text"))

p2 <- df2 %>%
  ggplot(aes(x = Variable, y = Amount, text = paste("Percentage:", Percentage*100, "%"))) +
  geom_bar( stat = "identity", fill = "#016C59", width = 0.5)+
  facet_wrap(~lab)+
  labs(y = "Value")+
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_text(angle = 45),  axis.text.y = element_text(size = 8),
        plot.title = element_text(size = 12, hjust = 0.5), axis.title.x = element_text(size = 10), strip.background =element_rect(fill="white"),
        strip.text = element_text(size = 12, face = "bold", hjust = 0.5))

ply2 <- ggplotly(p2, tooltip = c("text"))

plot <- subplot(ply1, ply2, titleY = TRUE, margin = 0.07)

plot



improve_effectiveness<- df[32,2]*100
coverage_need <- df[35,2]*100

df1 <- df[33:34,]
df1$Lost_value <- "After effectiveness improvement"

total <- df[8,]
total$Lost_value <- "Total lost value"
df3 <- rbind(df1, total)
df3$lab <- paste("Improve effectiveness by:", improve_effectiveness, "% \n (bioefficacy + wear and tear)")
#-----------------------------------------------------------------------------------------
df2 <- df[36,]
df2$Lost_value <- "After regional coverage improvement"

df4 <- rbind(df2, total)
df4$lab <- paste("% Reduction in coverage need:", coverage_need, "%")



p1 <- ggplot(df3, aes(x = Variable, y = Amount,  fill = Lost_value, text = paste("Percentage:", Percentage*100, "%"))) +
  geom_bar(stat = "identity", position = "dodge")+
  facet_wrap(~lab)+
  guides(fill = FALSE)+
  labs(y = "Value")+
  #scale_y_continuous(limits = c(0, 1000000000))+
  scale_fill_manual(values=c("#CAB2D6" ,"#D53E4F"))+        
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 10), axis.text.x = element_text(angle = 45),  axis.text.y = element_text(size = 8),
        plot.title = element_text(size = 12, hjust = 0.5), axis.title.x = element_text(size = 10), strip.background =element_rect(fill="white"),
        strip.text = element_text(size = 12, face = "italics", hjust = 0.5))

ply1 <- ggplotly(p1, tooltip = c("text"))

p2 <- ggplot(df4, aes(x = Variable, y = Amount,  fill = Lost_value, text = paste("Percentage:", Percentage*100, "%"))) +
  geom_bar(stat = "identity", position = "dodge")+
  facet_wrap(~lab)+
  labs( y = "Value")+
  guides(fill = FALSE)+
  #scale_y_continuous(limits = c(0, 1000000000))+
  scale_fill_manual(values=c("#92C5DE","#D53E4F"))+        
  scale_x_discrete(labels= "% Reduction in coverage need")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 10), axis.text.x = element_text(angle = 45),  axis.text.y = element_text(size = 8),
        plot.title = element_text(size = 12, hjust = 0.5), axis.title.x = element_text(size = 10), strip.background =element_rect(fill="white"),
        strip.text = element_text(size = 12, face = "bold", hjust = 0.5))

ply2 <- ggplotly(p2, tooltip = c("text"))

plot <- subplot(ply1, ply2, titleY = TRUE, titleX = TRUE, margin = 0.07)

plot

r1 <- df[19,]
r2 <- df[22,]
r3 <- df[25,]
r4 <- df[28,]
r5 <- df[30,]
r6 <- df[33,]
r7 <- df[36,]

df1 <- rbind(r1, r2, r3, r4, r5, r6, r7)

df1 <- df1 %>%
  mutate(axis = c("Baseline lost value (all nets)", "Gain from loss reduction",
                  "Gain from usage improvement", "Gain from bioefficacy improvement", "Lost value with wear and tear improvement",
                  "Gain from effectiveness improvement", "Gain from reduction in coverage need"),
         Amount = as.numeric(gsub(",", "", gsub("\\$", "", Amount))),
         gain = Amount[1]-Amount)
df1$lab <- "Gains due to improvements from baseline loss ($)"

df1[7,5] <- df1[7,3]

df2 <- rbind(df1, r7)%>%
  mutate(Amount = as.numeric(gsub(",", "", gsub("\\$", "", Amount))),
    gain = Amount)

