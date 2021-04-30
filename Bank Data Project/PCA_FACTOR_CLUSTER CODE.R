bank <- read.csv("bank-additional-full.csv", sep=";")
head(bank)
str(bank)

bank_pca_data=bank[,c(1,11:14,16:20)]
str(bank_pca_data)

summary(bank_pca_data)
cor(bank_pca_data)



bank_pca <- prcomp(bank_pca_data,scale=TRUE)
plot(bank_pca)
summary(bank_pca)
biplot(bank_pca)


(eigen_bank <- bank_pca$sdev^2)  
names(eigen_bank) <- paste("PC",1:10,sep="")
eigen_bank
sumlambdas <- sum(eigen_bank)
sumlambdas
propvar <- eigen_bank/sumlambdas
propvar
cumvar_bank <- cumsum(propvar)
cumvar_bank
matlambdas <- rbind(eigen_bank,propvar,cumvar_bank)
rownames(matlambdas) <- c("Eigenvalues","Prop. variance","Cum. prop. variance")
round(matlambdas,4)
summary(bank_pca)
bank_pca$rotation
print(bank_pca)

head(bank_pca$x)


banktyp_pca <- cbind(data.frame(bank$y),bank_pca$x)
head(banktyp_pca)



t.test(PC1~bank$y,data=banktyp_pca)
t.test(PC2~bank$y,data=banktyp_pca)
t.test(PC3~bank$y,data=banktyp_pca)
t.test(PC4~bank$y,data=banktyp_pca)
t.test(PC5~bank$y,data=banktyp_pca)
t.test(PC6~bank$y,data=banktyp_pca)
t.test(PC7~bank$y,data=banktyp_pca)
t.test(PC8~bank$y,data=banktyp_pca)
t.test(PC9~bank$y,data=banktyp_pca)
t.test(PC10~bank$y,data=banktyp_pca)

plot(eigen_bank, xlab = "Component number", ylab = "Component variance", type = "l", main = "Scree diagram")

#Can take till PC6 i.e. here 7 features reduced to 6
default_dummy=ifelse(bank$default=='yes',1,0) 
unique(bank$housing)
housing_dummy=ifelse(bank$housing=='yes',1,0) 
unique(bank$loan)
loan_dummy=ifelse(bank$loan=='yes',1,0) 
unique(bank$contact)
cell_dummy=ifelse(bank$contact=='cellular',1,0)
unique(bank$marital)
married_dummy=ifelse(bank$marital=='married',1,0)
divorced_dummy=ifelse(bank$marital=='divorced',1,0)
admin_dummy=ifelse(bank$job== 'admin.',1,0)
bluecollar_dummy=ifelse(bank$job== 'blue-collar',1,0)
technician_dummy=ifelse(bank$job== 'technician',1,0)
services_dummy=ifelse(bank$job=='services',1,0)
management_dummy=ifelse(bank$job=='management',1,0)
retired_dummy=ifelse(bank$job=='retired',1,0)
entrepreneur_dummy=ifelse(bank$job=='entrepreneur',1,0)
selfemployed_dummy=ifelse(bank$job=='self-employed',1,0)
housemaid_dummy=ifelse(bank$job=='housemaid',1,0)
unemployed_dummy=ifelse(bank$job=='unemployed',1,0)
student_dummy=ifelse(bank$job=='student',1,0)
student_dummy
bank1=data.frame(age,duration,campaign,pdays,previous,default_dummy,housing_dummy,loan_dummy,cell_dummy,married_dummy,divorced_dummy,
                 admin_dummy,bluecollar_dummy,technician_dummy,services_dummy,management_dummy,retired_dummy,
                 entrepreneur_dummy,selfemployed_dummy,housemaid_dummy,unemployed_dummy,student_dummy)

library(psych)
vss(bank1)
#therefore choosing 8 Factors 


fit.pc <- fa(bank1, nfactors=6)
fit.pc
fit.pc$loadings
fit.pc$loadings
fa.diagram(fit.pc) # Visualize the relationship

bank_additional <- read.csv("bank-additional-full.csv", sep=";")
library(cluster)
library(factoextra)
# Selecting observations to determine cluster parameters
BankAdditionalNum <- data.frame(as.numeric(as.factor(bank_additional$age)),
                                as.numeric(as.factor(bank_additional$job)),
                                as.numeric(as.factor(bank_additional$marital)),
                                as.numeric(as.factor(bank_additional$education)),
                                as.numeric(as.factor(bank_additional$housing)),
                                as.numeric(as.factor(bank_additional$loan)))

# Rename the columns
colnames(BankAdditionalNum) <- c("Age", "Job", "Marital", "Education","housing","loan")

# Reduce the amount of dataset records for legibility within clusters
BankAdditionalNum2 <- BankAdditionalNum[sample(nrow(BankAdditionalNum),100),]

# Kmeans clustering to create 5 clusters
set.seed(12345)
BankAdditionalNum_k5 <- kmeans(BankAdditionalNum2, centers=4)
BankAdditionalNum_k5

#scale
df <- scale(BankAdditionalNum2)
df

#distance
distance <- get_dist(df)
#fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
str(BankAdditionalNum_k5)
fviz_cluster(BankAdditionalNum_k5, data = df)


