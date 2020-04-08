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
