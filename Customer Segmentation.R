library(data.table)
library(dplyr)
library(ggplot2)
library(tidyr)
library(knitr)
library(rmarkdown)

df_data<-read.csv("/Users/yaoyuxin/desktop/project/ecommerce_data.csv")

df_data <- df_data %>% 
  mutate(Quantity = replace(Quantity, Quantity<=0, NA),
         UnitPrice = replace(UnitPrice, UnitPrice<=0, NA))
df_data <- df_data %>%
  drop_na()

df_data <- df_data %>% 
  mutate(InvoiceNo=as.factor(InvoiceNo), StockCode=as.factor(StockCode), 
         InvoiceDate=as.Date(InvoiceDate, '%m/%d/%Y'), CustomerID=as.factor(CustomerID), 
         Country=as.factor(Country))

df_data <- df_data %>% 
  mutate(total_dollar = Quantity*UnitPrice)
df_RFM <- df_data %>% 
  group_by(CustomerID) %>% 
  summarise(recency=as.numeric(as.Date("2012-01-01")-max(InvoiceDate)),
            frequency=n_distinct(InvoiceNo), monitery= sum(total_dollar)/n_distinct(InvoiceNo))

hist(df_RFM$monitery)
df_RFM$monitery <- log(df_RFM$monitery)
hist(df_RFM$monitery)

Kdata<-df_RFM[,c(2,3,4)]
tot.withinss <- vector("numeric", length = 10)
for (i in 1:10){
  kDet <- kmeans(Kdata, i)
  tot.withinss[i] <- kDet$tot.withinss
}

ggplot(as.data.frame(tot.withinss), aes(x = seq(1,10), y = tot.withinss)) + 
  geom_point(col = "#F8766D") +    
  geom_line(col = "#F8766D") + 
  theme(axis.title.x.bottom = element_blank()) +
  ylab("Within-cluster Sum of Squares") +
  xlab("Number of Clusters") +
  ggtitle("Elbow K Estimation")

customerClusters <- kmeans(Kdata, 5)
customerClusters
ggplot(Kdata, aes(x = recency, y = monitery)) + 
  geom_point(stat = "identity", aes(color = as.factor(customerClusters$cluster))) +
  scale_color_discrete(name=" ",
                       breaks=c("1", "2", "3", "4", "5"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5")) +
  ggtitle("Mall Customer Segmens", subtitle = "K-means Clustering")


#########visualization
#Which product sell the most?
options(repr.plot.width=6, repr.plot.height=3)
df_data %>% group_by(StockCode, Description) %>% summarise(c= n()) %>% arrange(desc(c)) %>% head()%>%
  ggplot(aes(x=reorder (Description,c), y=c, fill = c)) + geom_bar(stat= "identity") + coord_flip() + 
  labs(y="Number of items purchased", x="Product")

#Sales trend by day/month
df_data$Year <- as.factor(year(df_data$InvoiceDate))
df_data$Month <- as.factor(month(df_data$InvoiceDate))
options(repr.plot.width=10, repr.plot.height=3)
df_data %>% group_by(Month) %>% summarise(revenue = sum(total_dollar)) %>%  
  ggplot(aes(x=Month, y=revenue/1000000)) + geom_bar(stat="identity") +
  labs(x="Month", y="Revenue in million")
df_data %>% group_by(InvoiceDate) %>% summarise(revenue = sum(total_dollar)) %>%  
  ggplot(aes(x=InvoiceDate, y=revenue/1000000)) + geom_line(stat="identity") +
  labs(x="Date", y="Revenue in million")
#What's the total sales amount per year per country?
df_data %>% 
  group_by(Year, Country) %>% summarise(revenue = sum(total_dollar)/1000000) %>%
  ggplot(aes(x=Country, y=revenue, fill=Year)) + geom_bar(stat="identity") + coord_flip() +
  labs(x= "Country", y="Total Sales", title = "Total sales per year")

df_data %>% filter(Country != "United Kingdom") %>% 
  group_by(Year, Country) %>% summarise(revenue = sum(total_dollar)/1000000) %>%
  ggplot(aes(x=Country, y=revenue, fill=Year)) + geom_bar(stat="identity") + coord_flip() +
  labs(x= "Country", y="Total Sales", title = "Total sales per year")
