library(data.table)
library(dplyr)
library(ggplot2)
library(readxl)
library(tidytext)
library(stringr)
library(DT)
library(shiny)
library(plotly)
library(magrittr)
library(knitr)

online <- read_excel("Online Retail.xlsx")
dim(online)
str(online)

colSums(is.na(online))

head(online)
View(online)
summary(online)


#online[which(online$Quantity < 0),]

# cleaning data
online$InvoiceNo[1]



online$cancel <- substring(online$InvoiceNo,1,1)=="C"

#Incorrect Description

#View(online[substring(online$Description,1,1) == "?", ])


#in = vector(mode = "character", length = nrow(online))


str_sub(online$Description, 1, 1) == "?"

Desc <- grep("\\?",online$Description)

online <- online[-Desc,]


# Removing null values in customer Id column
colSums(is.na(online))
online <- online[complete.cases(online$CustomerID),]
colSums(is.na(online))
dim(online)

summary(online)

# Quantity should be greater than 0

neg_online <- online %>% filter (Quantity <= 0)
online1 <- online %>% filter(Quantity > 0)


summary(online1)

# transforming variables

online1$InvoiceDate = as.Date(online1$InvoiceDate, "%Y-%m-%d")
online1$year_of_purchase = as.factor(format(online1$InvoiceDate, "%Y"))
online1$days_since       = as.numeric(difftime(time1 = "2012-01-01",
                                            time2 = online1$InvoiceDate,
                                            units = "days"))

# summary(online1$days_since )
# 
# online1 %>% group_by(CustomerID) %>%  summarize(count = n()) %>% arrange(desc(count))
# 
# length(unique(online1$CustomerID))

# Let us look at the structure of the table
online1$cancel <- NULL

data.frame(variable = names(online1),
           class = sapply(online1,class),
           first_values = sapply(online1, function(x) paste0(head(x,3), 
                                                             collapse = ", ")),
           row.names = NULL) %>% kable()



# Omitting outliers after 99.9th perecentile

quantile(online1$Quantity, 0.999)
quantile(online1$UnitPrice, 0.999)
str(online1)
summary(online1)
par(mfrow=c(1,1))
boxplot(online1[,4])
boxplot(online1[,6])


online1 %>% arrange(desc(Quantity)) %>% select(Quantity) %>% head(10)

online1 <- online1 %>% filter(Quantity < 504) %>% filter(UnitPrice < 42)

quantity <- online1$Quantity
unitprice <- online1$UnitPrice

f <- list(
  family = "Courier New, monospace",
  size = 18,
  color = "#7f7f7f"
)

plot_ly(y=quantity, type = "box",hoverinfo = 'y')%>%
  layout(title = "Box plot for Quantity", yaxis = list(
    title = "Quantity"
  ))

####Box plot using ggplot

p1 <- ggplot(data = online1, aes(x = " ", y = Quantity)) +
  geom_boxplot(outlier.alpha = .02) +
  scale_y_log10(
    labels = quantile(online1$Quantity),
    breaks = quantile(online1$Quantity))

p2 <- ggplot(data = online1, aes(x = " ", y = UnitPrice)) +
  geom_boxplot(outlier.alpha = .02) +
  scale_y_log10(
    labels = scales::dollar, 
    breaks = quantile(online1$UnitPrice))

gridExtra::grid.arrange(p1, p2, ncol = 2)
  

plot_ly(y=unitprice, type = "box",hoverinfo = 'y')%>%
  layout(title = "Box plot for Unit Price", yaxis = list(
    title = "Unit Price"
  ))

str(online1)
glimpse(online1)

str(online1)

online1 <- mutate(online1,Total_Price = UnitPrice * Quantity )

# ui <- fluidPage(
#   titlePanel("Cleaned dataset preview"),
#   numericInput("rows", "Data Snippet", 6),
#   tableOutput("mytable"))
# 
# server <- function(input, output) { output$mytable <- renderTable({
#   head(online1, input$rows)
# })}
# 
# shinyApp(ui, server)

# glimpse(online1)
str(online1)
require(scales)
options(scipen=10000)

online1 %>% group_by(Country) %>% summarise(Revenue = sum(Total_Price)) %>%
  arrange(desc(Revenue)) %>% top_n(8)%>%
  ggplot(aes(x=reorder(Country,Revenue), y=Revenue)) +
  geom_bar(stat="identity", aes(fill=factor(Country))) +
theme_classic()+
  labs(title =" Revenue by Country", 
       x = "Country",
       y = "Revenue",
       fill="Country") +
  theme(plot.title = element_text(hjust=0.5)) +
  coord_flip()

online1 %>% group_by(Country) %>% summarize(Num_invoices = n()) %>%
  arrange(desc(Num_invoices)) %>% top_n(8) %>%
  ggplot(aes(x=reorder(Country,Num_invoices), y=Num_invoices)) +
  geom_bar(stat="identity", aes(fill=factor(Country))) +
  theme_classic()+
  labs(title =" Number of Invoices by Country", 
       x = "Country",
       y = "Number of invoices",
       fill="Country") + 
  theme(plot.title = element_text(hjust=0.5)) +
  coord_flip()



p <- online1 %>% group_by(Country) %>% summarise(Revenue = sum(Total_Price),
                                            Num_invoices = n(),
                                            Average_Price = 
                                              Revenue/Num_invoices) %>%
  arrange(desc(Average_Price)) %>% top_n(8)

plot_ly(x=reorder(p$Country,p$Average_Price), 
        y=p$Average_Price, type="bar",
        mode="markers",hoverinfo="text", text = paste("Price:", p$Average_Price))


online1 %>% group_by(Country) %>% summarise(Revenue = sum(Total_Price),
                                            Num_invoices = n(),
                                            Average_Price = 
                                              Revenue/Num_invoices) %>%
  arrange(desc(Average_Price)) %>% top_n(8) %>%
  ggplot(aes(x=reorder(Country,Average_Price), y=Average_Price)) +
  geom_bar(stat="identity", aes(fill=factor(Country))) +
  theme_classic()+
  labs(title =" Average Total Price Per Transaction Countrywise", 
       x = "Country",
       y = "Average Total Price Per Transaction (in sterling pounds)",
       fill="Country") + 
  theme(plot.title = element_text(hjust=0.5)) +
  coord_flip()

ggplot(online1, aes(x = Total_Price))+
  geom_histogram(binwidth = 0.3, aes(fill = ..count..)) +
  scale_fill_gradient( name = "Frequency",
                       low = "green",
                       high = "red") +
  coord_cartesian(xlim = c(0,50)) +
  ggtitle("Histogram for Total Price of each transaction") +
  xlab("Total Price") + 
  ylab("Frequency of Total Price") + 
  theme(plot.title = element_text(hjust=0.5)) 


str(online1)
summary(online1)

########################

res <- lapply( online1[,c(4,6,7,10,11)] , function(x) rbind( mean = mean(x) ,
                                         sd = sd(x) ,
                                         median = median(x) ,
                                         minimum = min(x) ,
                                         maximum = max(x) ,
                                         s.size = length(x) ) )

View(data.frame( res ))



############## Data Preparation for RFM analysis and PCA
##### Analysis is restricted to UK
set.seed(12383328)
library(sqldf)

online2 <- sqldf("SELECT CustomerID,
                 MIN(days_since) AS 'Recency',
                 MAX(days_since) AS 'First_Purchase',
                 COUNT(*) AS 'Frequency',
                 sum(Total_Price) as 'Revenue_Contribution_Customer',
                 Min(Total_Price) as 'Minimum_Purchase_Value',
                 Max(Total_Price) as 'Maximum_Purchase_Value',
                 Avg(Total_Price) as 'Average_Purchase_Value'
                 FROM online1
                 where Country = 'United Kingdom'
                 GROUP BY 1")

online2$CustomerID <- as.character(online2$CustomerID)
str(online2)
summary(online2)

res1 <- lapply( online2[,-1] , function(x) rbind( mean = mean(x) ,
                                                             sd = sd(x) ,
                                                             median = median(x) ,
                                                             minimum = min(x) ,
                                                             maximum = max(x) ) )

View(data.frame( res1 ))


online3 <- online2[,c(1,2,4,5,8)]

########################## Kmeans without PCA

set.seed(12383328)
str(online2)

online_scale <- scale(online3[,c(2,3,4,5)])
str(online_scale)
summary(online_scale)


# K-Means Cluster Analysis
finding1 <- kmeans(online_scale,4 ) 
plotcluster(online_scale, finding1$cluster)
table(finding1$cluster)

View(aggregate(online3[,-1],by=list(finding1$cluster),FUN=mean))
finding$centers

# Determine number of clusters Elbow method
wss <- (nrow(online_scale)-1)*sum(apply(online_scale,2,var))
for (i in 2:12) wss[i] <- sum(kmeans(online_scale,
                                     centers=i)$withinss)
plot(1:12, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")

online_no_pca_kmeans <- data.frame(online3, segment = finding1$cluster)

## silhoute

d = dist(online_scale, method = "euclidean")
result = matrix(nrow = 14, ncol = 3)
for (i in 2:15){
  cluster_result = kmeans(online_scale, i)
  clusterstat=cluster.stats(d, cluster_result$cluster)
  result[i-1,1]=i
  result[i-1,2]=clusterstat$avg.silwidth
  result[i-1,3]=clusterstat$dunn   
}
plot(result[,c(1,2)], type="l", ylab = 'silhouette width',
     xlab = 'number of clusters')


################Kernel K means without PCA

sc <- kkmeans(as.matrix(online_scale), centers=4)

plotcluster(online_scale,sc@.Data)

# ############Hierarchical clustering without PCA
# 

#Wards Method or Hierarchical clustering
#Calculate the distance matrix
online_scale.dist=dist(online_scale)
#Obtain clusters using the Wards method
online_scale.dist.hclust=hclust(online_scale.dist, method="ward")

# Plot Dendogram
plot(online_scale.dist.hclust)
#Cut dendrogram at the 3 clusters level and obtain cluster membership
online_scale.dist.2clust = cutree(online_scale.dist.hclust,6)
length(online_scale.dist.2clust)

library(dplyr)
hier <- mutate(online3, cluster = online_scale.dist.2clust)
count(hier, cluster)
j <- hier[,-1] %>% group_by(cluster) %>% summarise_all(funs(mean(.)))

View(j)


############# Principal Component Analysis

PCA_online <- online2[,-1]
#str(PCA_online)

# Compute Variance for each variable
apply(PCA_online, 2, var)

# Scaling and performing PCA on data set
pca_online_result <- prcomp(PCA_online, scale = TRUE)

# means
# pca_result$center

# standard deviations
# pca_result$scale


# Principal component loading is provided by rotation matrix

pca_online_result$rotation <- -pca_online_result$rotation
pca_online_result$rotation

# Fetch the principal component scores

pca_online_result$x <- -pca_online_result$x
pca_online_result$x

# Compute Proportion of Variance (PVE) explained

var_online <- pca_online_result$sdev^2

PVE_online <- var_online/(sum(var_online))
round(PVE_online,2)

#  scree plot for PVE
PC_number <- c(1:7)
Var_percent <- round(PVE_online,2)
pc_df <- data.frame(PC_number,Var_percent)
  
d <- qplot(PC_number, Var_percent) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("PVE") +
  ggtitle("Scree Plot") +
  ylim(0, 1)

ggplot(pc_df,aes(PC_number, Var_percent)) + 
  geom_line() + 
  geom_point()+
  xlab("Principal Component") + 
  ylab("Proportion of Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 1) + 
  theme(plot.title = element_text(hjust=0.5)) 

ggplotly(d)


Var_Explained <- round(cumsum(PVE_online),2)

pc_df <- data.frame(pc_df, Var_Explained)

# Cumulative PVE plot
b <- qplot(PC_number, Var_Explained) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab(NULL) + 
  ggtitle("Cumulative Scree Plot") +
  ylim(0,1)

ggplotly(b)

ggplot(pc_df,aes(PC_number, Var_Explained)) + 
  geom_line() + 
  geom_point()+
  xlab("Principal Component") + 
  ylab("Proportion of Variance Explained") +
  ggtitle("Cumulative Scree Plot") +
  ylim(0, 1) + 
  theme(plot.title = element_text(hjust=0.5)) 

############## Plot Principal component for each customer ID



data.frame(online2[,1])

required_PC <- pca_online_result$x[,1:4]
rownames(required_PC) <- NULL
PC1 <- required_PC[,1]
PC2 <- required_PC[,2]
PC3 <- required_PC[,3]
PC4 <- required_PC[,4]

Principal_Components <- data.frame(online2[,"CustomerID"],PC1, PC2, PC3, PC4)

names(Principal_Components) <- c("Cust_ID", "PC1", "PC2", "PC3", "PC4")

View(head(Principal_Components))

# ggplot(Principal_Components, aes(PC1, PC2)) + 
#   modelr::geom_ref_line(h = 0) +
#   modelr::geom_ref_line(v = 0) +
#   geom_text(aes(label = Cust_ID), size = 3) +
#   xlab("First Principal Component") + 
#   ylab("Second Principal Component") + 
#   ggtitle("First Two Principal Components of Retail Data")

########################### K Means Clustering on PCA data set
library(fpc)
set.seed(12383328)
online_kmeans_pca <- online2

clust_online <- Principal_Components[,2:5]

finding <- kmeans(clust_online, 6)
table(finding$cluster)
plotcluster(clust_online, finding$cluster)
View(aggregate(online2,by=list(finding$cluster),FUN=mean))

# Determine number of clusters Elbow method
wss <- (nrow(clust_online)-1)*sum(apply(clust_online,2,var))
for (i in 2:12) wss[i] <- sum(kmeans(clust_online,
                                     centers=i)$withinss)
plot(1:12, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")

#prediction.strength(clust_online, Gmin=2, Gmax=15, M=10,cutoff=0.5)

online_kmeans_pca <- data.frame(online2, segment = finding$cluster )
View(online_kmeans_pca)
#### silhoute

d = dist(clust_online, method = "euclidean")
result = matrix(nrow = 14, ncol = 3)
for (i in 2:15){
  cluster_result = kmeans(clust_online, i)
  clusterstat=cluster.stats(d, cluster_result$cluster)
  result[i-1,1]=i
  result[i-1,2]=clusterstat$avg.silwidth
  result[i-1,3]=clusterstat$dunn   
}
plot(result[,c(1,2)], type="l", ylab = 'silhouette width', xlab = 'number of clusters')

########Kernel K means on PCA data set

sc <- kkmeans(as.matrix(clust_online), centers=4)

plotcluster(clust_online,sc@.Data)




###########################
### ##### Hierarchical clustering
#############################

#Wards Method or Hierarchical clustering
#Calculate the distance matrix
clust_online.dist=dist(clust_online)
#Obtain clusters using the Wards method
clust_online.hclust=hclust(clust_online.dist, method="ward")

# Plot Dendogram
plot(clust_online.hclust)
#Cut dendrogram at the 4 clusters level and obtain cluster membership
clust_online.2clust = cutree(clust_online.hclust,5)
summary(clust_online.2clust)

#library(dplyr)
hier <- mutate(clust_online, cluster = clust_online.2clust)
names(hier)
count(hier, cluster)
o <- hier %>% group_by(cluster) %>%   summarise_all(funs(mean(.)))
View(o)
#clust_online.3clust
plotcluster(clust_online, clust_online.2clust)

aggregate(online2[,-1],by=list(clust_online.2clust),FUN=mean)







############################################

summary(revenue_second_half)

online_kmeans_pca %>% select(segment, Revenue_Contribution_Customer) %>%
  group_by(segment) %>% summarise(Revenue_Contribution = sum(Revenue_Contribution_Customer))

online_kmeans_pca %>% filter(Recency <= 180,First_Purchase > 180 ) %>%
  select(segment, Revenue_Contribution_Customer) %>%
   group_by(segment) %>% summarise(Revenue_Contribution = sum(Revenue_Contribution_Customer))

online_kmeans_pca %>% filter(Recency > 180,First_Purchase > 180 ) %>%
  select(segment, Revenue_Contribution_Customer) %>%
  group_by(segment) %>% summarise(Revenue_Contribution = sum(Revenue_Contribution_Customer))

online_kmeans_pca %>% filter(First_Purchase <= 180 ) %>%
  select(segment, Revenue_Contribution_Customer) %>%
  group_by(segment) %>% summarise(Revenue_Contribution = sum(Revenue_Contribution_Customer))

########################

