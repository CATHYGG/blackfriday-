library(ggplot2)

black<- read.csv(file.choose(),na.strings=" ")
head(black)
str(black)
##scatter matrix 
plot(uniqueID[,1:12])
x <- black$Product_Category_1
y <- black$Product_Category_2
plot(x,y)

#duplicated removal by user's ID
clean <- na.omit(black$User_ID)
duplicated <- duplicated(clean)

uniqueID <- as.data.frame(black[which(!duplicated),])

#combine the purchase amount by one ID
pursum.ID <- aggregate(black$Purchase,list(black$User_ID),sum,na.rm=TRUE) #sum purchase of each id 
uniqueID$purchase.ID <- as.integer(pursum.ID$x)

head(uniqueID)

head(uniqueID)
head(black)

#combining the product category
product1.ID <- aggregate(black$Product_Category_1,list(black$User_ID),sum,na.rm=TRUE) #sum purchase of each id 
uniqueID$pro1.ID <- as.integer(product1.ID$x)

product2.ID <- aggregate(black$Product_Category_2,list(black$User_ID),sum,na.rm=TRUE) #sum purchase of each id 
uniqueID$pro2.ID <- as.integer(product2.ID$x)

product3.ID <- aggregate(black$Product_Category_3,list(black$User_ID),sum,na.rm=TRUE) #sum purchase of each id 
uniqueID$pro3.ID <- as.integer(product3.ID$x)

head(uniqueID)
uniqueID[,c(-9,-10,-11,-12)]

#gender 
uniqueID$Gender <- factor(uniqueID$Gender)
summary(uniqueID$Gender)
ggplot(uniqueID,aes(uniqueID$Gender)) + geom_bar()
summary(Gender)
aggregate(black$Purchase,list(black$Gender),sum,na.rm=TRUE) #average purchase of each gender

pur.gender<- aggregate(uniqueID$purchase.ID,list(uniqueID$Gender),mean,na.rm=TRUE) #average purchase of each gender
ggplot(pur.gender,aes(pur.gender)) + geom_bar()
x<- pur.gender$Group.1
y<- pur.gender$x
plot(x,y, type = "l")

pur.gender$x <- as.integer(pur.gender$x)

#age
uniqueID$Age <- factor(uniqueID$Age)
summary(uniqueID$Age)
ggplot(uniqueID,aes(Age)) + geom_bar()
ggplot(uniqueID,aes(x = Gender, fill = Age)) + geom_bar(position = "stack") #more young
ggplot(uniqueID,aes(x = Age, fill = Gender)) + geom_bar(position = "stack") #more male, no matter what group 

aggregate(uniqueID$purchase.ID,list(uniqueID$Age),mean,na.rm=TRUE)

#occupation
uniqueID$Occupation <- factor(uniqueID$Occupation)
summary(uniqueID$Occupation)
ggplot(uniqueID,aes(Occupation)) + geom_bar()
plot(uniqueID$Occupation,range=1,las=1)

ggplot(uniqueID,aes(x = Occupation, fill = Age)) + geom_bar(position = "stack")#young in 4 
ggplot(uniqueID,aes(x = Occupation, fill = Gender)) + geom_bar(position = "stack")#more male 

#######box relation
boxplot(Occupation~Age, data=uniqueID,1,2)
ggplot(uniqueID,aes(x = Occupation, fill = Age)) + geom_bar(position = "stack")
#proportion vs rate
ggplot(black,aes(Stay_In_Current_City_Years,purchase)) + geom_point(colour="blue") +
  geom_smooth(method=lm,size=1,colour="red")


#City_category 
uniqueID$City_Category <- factor(uniqueID$City_Category)
summary(uniqueID$City_Category)
Y1 <- ggplot(uniqueID,aes(City_Category)) + geom_bar(fill = c ("#56B4E9","#009E73","#E69F00")) + ylab("total consumer number") #c has the most people 
Y2 <- ggplot(black,aes(City_Category)) + geom_bar(fill = c ("#56B4E9","#009E73","#E69F00"))+ylab("total transaction number") ##more transaction in b

plot_grid(Y1,Y2,
          labels = c("Total number of consumer","Total transaction number"),
          ncol = 2, nrow = 1)
data.frame(Y1,Y2)
head(black)
pur <- aggregate(uniqueID$Purchase,list(uniqueID$City_Category),mean,na.rm=TRUE)


ggplot(black,aes(x = City_Category, fill = totalpur)) + geom_bar(position = "stack")

ggplot(uniqueID,aes(x = City_Category, fill = Purchase)) + geom_bar(position = "stack")
#why happened --demograph 
G1<- ggplot(uniqueID,aes(x = Gender, fill = City_Category)) + geom_bar(position = "stack")+scale_fill_manual(values = c ("#56B4E9","#009E73","#E69F00"))+ ylab("total consumer number")
G2 <- ggplot(black,aes(x = Gender, fill = City_Category)) + geom_bar(position = "stack")+scale_fill_manual(values = c ("#56B4E9","#009E73","#E69F00"))+ylab("total transaction number")

G3 <- ggplot(uniqueID,aes(x = Stay_In_Current_City_Years, fill = City_Category)) + geom_bar(position = "stack")+scale_fill_manual(values = c ("#56B4E9","#009E73","#E69F00"))+ ylab("total consumer number")
G4 <- ggplot(black,aes(x = Stay_In_Current_City_Years, fill = City_Category)) + geom_bar(position = "stack")+scale_fill_manual(values = c ("#56B4E9","#009E73","#E69F00"))+ylab("total transaction number")
plot_grid(G1,G2,G3,G4,
          labels = c("        Gender"," ", "Stay_In_Current_City_Years"),
          ncol = 2, nrow = 2)
data.frame(G1,G2)
#Stay in Current City Years 
uniqueID$Stay_In_Current_City_Years <- factor(uniqueID$Stay_In_Current_City_Years)
summary(uniqueID$Stay_In_Current_City_Years)
ggplot(uniqueID,aes(Stay_In_Current_City_Years)) + geom_bar()
#Martial Status
uniqueID$Marital_Status <- factor(uniqueID$Marital_Status)
ggplot(uniqueID,aes(Marital_Status)) + geom_bar(fill = c ("#009E73","#E69F00"))
#product category
plot(black$Product_Category_1,black$Product_Category_2,type = "p")
plot(black$Product_Category_2,black$Product_Category_1,type = "p")
ggplot(aes(black$Product_Category_1,black$Product_Category_2)) + 
  geom_point() + 
  geom_smooth(method = "lm")

plot(black[,9:11])
head(uniqueID)
ax <- black$Product_Category_1
y <- black$Product_Category_2
ggplot(black,aes(x,y)) + geom_point(alpha=0.5,colour="blue") +
  geom_smooth(method="loess",size=1,colour="red")
black$Age <- factor(black$Age)
summary(black$Age)
ggplot(black,aes(Age)) + geom_bar()

#muti plot

par(mfrow=c(3,2),mar=c(4,4,2,0.5))
x <- c("Female", "Male")
y <- table(uniqueID$Gender)
plot(x,y,xlab = "Gender", ylab = "numbers of consumer")
p6 <- ggplot(uniqueID,aes(Gender)) + geom_bar(fill = c("#D55E00","#0072B2"))

f6 <- p6 + ylab("number of consumer")
p1 <- ggplot(uniqueID,aes(Marital_Status)) + geom_bar(fill = c ("#009E73","#E69F00")) 
f1 <- p1 + ylab("number of consumer")
p2 <- ggplot(uniqueID,aes(x = Stay_In_Current_City_Years, fill = Stay_In_Current_City_Years)) + geom_bar()+
  scale_fill_brewer(palette = 15)
f2 <- p2 + ylab("number of consumer")
p3 <- ggplot(uniqueID,aes(City_Category)) + geom_bar(fill = c ("#56B4E9","#009E73","#E69F00"))
f3 <- p3 + ylab("number of consumer")
p4 <- ggplot(uniqueID,aes(x = Occupation)) + geom_bar() 
f4 <- p4 + ylab("number of consumer")
p5 <- ggplot(uniqueID,aes(x = Age,fill = Age)) + geom_bar() +
  scale_fill_brewer(palette = 'GnBu')
f5 <- p5 + ylab("number of consumer")

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#0072B2", "#D55E00", "#CC79A7")
head(black)
plot_grid(f6,f5,f4,f3,f2,f1,
          labels = c("A","B","C","D","E","F"),
          ncol = 3, nrow = 2)
data.frame(f1,f2,f3,f4,f5,f6)

#top selling 
top_product <- table(black$Product_ID)
top.prod <- sort(top_product,decreasing = TRUE))
top.prod

#buyer who spend the most and profile
most_spend<- sort(black$Purchase, decreasing = TRUE)
#group and sum the purchase amount foe each same id
totalpur <- aggregate(uniqueID$Purchase,list(uniqueID$User_ID),sum,na.rm=TRUE) #average purchase of each gender
class(totalpur)
black$totalpur <- as.factor(totalpur)
head(black$totalpur)
which.max(totalpur)
black[rank(totalpur),] #buyer with the most purchase 
class(uniqueID)
head(totalpur)
#regression 

lm(formula = black$Purchase ~ black$Product_Category_1+black$Product_Category_2 + black$Product_Category_3, data = black)
is.factor(black$Gender)
black$Age <- as.factor(black$Age)
black$Stay_In_Current_City_Years <- as.factor(black$Stay_In_Current_City_Years)
class(black$Age)
black$Gender<- as.factor(black$Gender)
summary(lm(purchase.ID ~ Age + Gender + City_Category + Marital_Status + Stay_In_Current_City_Years + Occupation
           + pro1.ID + pro2.ID + pro3.ID, data = uniqueID))
summary(lm(Purchase ~ Age + Gender + City_Category + Marital_Status + Stay_In_Current_City_Years + Occupation
           + Product_Category_1 + Product_Category_2 + Product_Category_3, data = black))
table(uniqueID$purchase.ID)
head(black)
head(uniqueID)
str(black)
summary(lm(Purchase ~ Age + Gender + City_Category, data = black))
y <- lm(Purchase ~ Age, data = black)
y
x <- black$Age
plot(x,y,type = "p")
head(black)
table(black$Age)
black$Age[rank(black$Age),]
