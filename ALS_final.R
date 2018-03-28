---
html_document: default
pdf_document: default
---

##HW 5
##Winter 2018, DSPA (HS650) 
##Name : Prabhjot Singh
##SID: 82364840
##UMich E-mail: prabhj@umich.edu

####I certify that the following paper represents my own independent work and conforms with the guidelines of academic honesty described in the UMich student handbook.

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Problem 5.1
### Design, train, and optimize a generic neural network (NN) that can learn and predict the power-functionLinks to an external site. for a given power parameter (λ∈R).     Assess the accuracy of the NN prediction of the power function. 

#Gnereate Random numbers between -10 and 10 to train.
```{r}
rand_data = runif(1000, 0, 20)
#In order to take input from the user
#n = readline(prompt="Enter the power parameter : ")
#n = as.integer(n)
n = 2 # We take n=3 for the purpose of this homework.

pwr_df <- data.frame(rand_data, pwr_data = (rand_data) ^ (n)) 
plot(rand_data, pwr_df$pwr_data)
```

#### Train the neural net

```{r}
library(neuralnet)
set.seed(1234)
net.pwr <- neuralnet(pwr_data ~ rand_data,  pwr_df, hidden=50, threshold=0.1, stepmax = 1e06)
```

#### report the NN
```{r}
#print(net.pwr)
```

#### Generate testing data seq(from=10, to=20, step=0.1)
```{r}
test_data <- seq(0, 30, 0.1)
test_data_pwr <- (test_data) ^ n
```

#### Try to predict the power values
#### Compute or predict for test data, test_data
```{r}
pred_pwr <- compute(net.pwr, test_data)$net.result
```

#### compare real (test_data_pwr) and NN-predicted (pred_pwr) cube value of test_data

```{r}
plot(pred_pwr, test_data_pwr, xlim=c(0, 900), ylim=c(0,900))
abline(0,1, col="red", lty=2)
legend("bottomright",  c("Pred vs. Actual Power", "Pred=Actual Line"), cex=0.8, lty=c(1,2), lwd=c(2,2),col=c("black","red"))

```

#### compare_df
```{r}
compare_df <-data.frame(pred_pwr, test_data_pwr)

plot(test_data, test_data_pwr)
lines(test_data, pred_pwr, pch=22, col="red", lty=2)
legend("bottomright",  c("Actual Power","Predicted Power"), lty=c(1,2), lwd=c(2,2),col=c("black","red"))
```
#### We can obsever in the above plot that the actual power value is ver close to the predicted power, although upto some extent, after which it breaks. After trying it with diifrent number of hidden layers, this seems to the better solution. 

## Problem 5.2
#### Use the ALS dataset to study a rare but devastating progressive neurodegenerative disease, amyotrophic lateral sclerosis (ALS). Major clinically relevant questions include: What patient phenotypes can be automatically and reliably identified and used to predict the change of the ALSFRS slope over time?

```{r}
#setwd("~/Downloads")
als_train = read.csv("ALS_TrainingData_2223.csv")
head(als_train)

```

```{r}
str(als_train)
```

```{r}
plot(als_train$ALT.SGPT._median, als_train$ALSFRS_slope, pch = 19, col=ifelse(als_train$Gender_mean==1, "red", "blue"))
legend("topright", pch=c(19,19), col=c("red", "pink"), c("Male", "Female"), bty="o", cex=1.1, box.col="darkgreen")
```

```{r}
plot(als_train$AST.SGOT._median, als_train$ALSFRS_slope, pch = 19, col=ifelse(als_train$Gender_mean==1, "red", "blue"))
legend("topright", pch=c(19,19), col=c("red", "blue"), c("Male", "Female"), bty="o", cex=1.1, box.col="darkgreen")
```

```{r}
plot(als_train$Creatinine_median, als_train$ALSFRS_slope, pch = 19, col=ifelse(als_train$Gender_mean==1, "red", "blue"))
legend("topright", pch=c(19,19), col=c("red", "blue"), c("Male", "Female"), bty="o", cex=1.1, box.col="darkgreen")
```

```{r}
plot(als_train$Glucose_median, als_train$ALSFRS_slope, pch = 19, col=ifelse(als_train$Gender_mean==1, "red", "blue"))
legend("topright", pch=c(19,19), col=c("red", "blue"), c("Male", "Female"), bty="o", cex=1.1, box.col="darkgreen")
```

```{r}
plot(als_train$Hematocrit_median, als_train$ALSFRS_slope, pch = 19, col=ifelse(als_train$Gender_mean==1, "red", "blue"))
legend("bottomleft", pch=c(19,19), col=c("red", "blue"), c("Male", "Female"), bty="o", cex=1.1, box.col="darkgreen")
```

```{r}
plot(als_train$Platelets_median, als_train$ALSFRS_slope, pch = 19, col=ifelse(als_train$Gender_mean==1, "red", "blue"))
legend("topright", pch=c(19,19), col=c("red", "blue"), c("Male", "Female"), bty="o", cex=1.1, box.col="darkgreen")
```

```{r}
plot(als_train$Potassium_median , als_train$ALSFRS_slope, pch = 19, col=ifelse(als_train$Gender_mean==1, "red", "blue"))
legend("topright", pch=c(19,19), col=c("red", "blue"), c("Male", "Female"), bty="o", cex=1.1, box.col="darkgreen")
```

```{r}
plot(als_train$pulse_median, als_train$ALSFRS_slope, pch = 16, col=ifelse(als_train$Gender_mean==1, "red", "blue"))
legend("topright", pch=c(19,19), col=c("red", "blue"), c("Male", "Female"), bty="o", cex=1.1, box.col="darkgreen")
```

```{r}
plot(als_train$respiratory_median, als_train$ALSFRS_slope, pch = 19, col=ifelse(als_train$Gender_mean==1, "red", "blue"))
legend("topright", pch=c(19,19), col=c("red", "blue"), c("Male", "Female"), bty="o", cex=1.1, box.col="darkgreen")
```

#### It is observed from the above preliminary visualizations, that changes in these variables such as SGOT, SGPT, etc cause considerable variance in ALSFRS slope. Hence, we will consider only these 11 variable in our clustering analysis. 

```{r}
als_train = als_train[, c(7, 13, 17, 45, 48, 50, 58, 76, 78, 82, 87  )]
str(als_train)
```
####Train a k-Means model on the data, select k
####First, scale the data.
```{r}
als_train_scaled = as.data.frame(lapply(als_train, scale))
str(als_train_scaled)
```

####First, conside k = 4.
```{r}
library(stats)
set.seed(321)
als_clusters = kmeans(als_train_scaled, 4)
```
#Evaluate the model performance using bar and silhouette plots and summarize the results

```{r}
als_clusters$size
```

```{r}
require(cluster)
dis = dist(als_train_scaled)
sil = silhouette(als_clusters$cluster, dis)
summary(sil)
```

## Silouhette plots

```{r}
plot(sil, border = NA)
```

##Bar Plots

```{r}
par(mfrow=c(1, 1), mar=c(4, 4, 4, 2))
myColors <- c("darkblue", "red", "green", "brown", "pink", "purple", "yellow", "orange", "black", "grey", "violet")
barplot(t(als_clusters$centers), beside = TRUE, xlab="cluster", 
        ylab="value", col = myColors)
legend("top", ncol=2, legend = c("ALSFRS_Slope", "ALT.SGPT._median", "AST.SGOT._median", "Creatinine_median", "Glucose_median", "Hematocrit_median", "Platelets_median", "Potassium_median", "pulse_median", "respiratory_median"), fill = myColors)

```

## Model Improvement

```{r}
library(matrixStats)
kpp_init = function(dat, K) {
  x = as.matrix(dat)
  n = nrow(x)
  # Randomly choose a first center
  centers = matrix(NA, nrow=K, ncol=ncol(x))
  set.seed(123)
  centers[1,] = as.matrix(x[sample(1:n, 1),])
  for (k in 2:K) {
    # Calculate dist^2 to closest center for each point
    dists = matrix(NA, nrow=n, ncol=k-1)
    for (j in 1:(k-1)) {
      temp = sweep(x, 2, centers[j,], '-')
      dists[,j] = rowSums(temp^2)
    }
    dists = rowMins(dists)
    # Draw next center with probability proportional to dist^2
    cumdists = cumsum(dists)
    prop = runif(1, min=0, max=cumdists[n])
    centers[k,] = as.matrix(x[min(which(cumdists > prop)),])
  }
  return(centers)
}
```

```{r}
clust_kpp = kmeans(als_train_scaled, kpp_init(als_train_scaled, 4), iter.max=100, algorithm='Lloyd')
clust_kpp$centers

sil2 = silhouette(clust_kpp$cluster, dis)
summary(sil2)
```

## Silouhette plot after model improvememnt.
```{r}
plot(sil2, border = NA)
```

## The above sil plot has better S(i) values, however, average sil width remains same.
## Tuning the parameter k

```{r}
n_rows <- 15
mat = matrix(0,nrow = n_rows)
for (i in 2:n_rows){
  set.seed(321)
  clust_kpp = kmeans(als_train_scaled, kpp_init(als_train_scaled, i), iter.max=100, algorithm='Lloyd')
  sil = silhouette(clust_kpp$cluster, dis)
  mat[i] = mean(as.matrix(sil)[,3])
}
colnames(mat) <- c("Avg_Silhouette_Value")
mat
```

```{r}
library(ggplot2)
  ggplot(data.frame(k=2:n_rows,sil=mat[2:n_rows]),aes(x=k,y=sil))+
  geom_line()+
  scale_x_continuous(breaks = 2:n_rows)
```

## Considering the above sil plot, we must consider our model with k=3 as well.

```{r}
k = 3
set.seed(31)
clust_kpp = kmeans(als_train_scaled, kpp_init(als_train_scaled, k), iter.max=200, algorithm="MacQueen")
sil3 = silhouette(clust_kpp$cluster, dis)
summary(sil3)
```

```{r}
plot(sil3, border = NA)
```

###Comparing these sil plots, we realize that it is better with k=3 than it was with k=4, with greater silouhette average.
##Hierarchial Clustering

```{r}
library(cluster)
pitch_sing = agnes(als_train_scaled, diss=FALSE, method='single')
pitch_comp = agnes(als_train_scaled, diss=FALSE, method='complete')
pitch_ward = agnes(als_train_scaled, diss=FALSE, method='ward')
sil_sing = silhouette(cutree(pitch_sing, k=3), dis)
sil_comp = silhouette(cutree(pitch_comp, k=5), dis)
sil_ward = silhouette(cutree(pitch_ward, k=4), dis)
```

### Dendograms
```{r}
library(ggdendro)
ggdendrogram(as.dendrogram(pitch_ward), leaf_labels=FALSE, labels=FALSE)
```

```{r}
mean(sil_ward[,"sil_width"])
```

```{r}
ggdendrogram(as.dendrogram(pitch_ward), leaf_labels=TRUE, labels=T, size=10)
```

```{r}
summary(sil_ward)
```
```{r}
summary(sil_comp)
```

```{r}
plot(sil_ward, border = NA)
```
```{r}
plot(sil_comp, border = NA)
```

```{r}
library(mclust)
set.seed(1234)
gmm_clust <- Mclust(als_train)
summary(gmm_clust, parameters = TRUE)
```

```{r}
gmm_clust$modelName
```
#### Hence, we have ellipsoidal, equal shaped clusters in our model.
#### Following is the BIC plot.

```{r}
plot(gmm_clust$BIC, legendArgs = list(x = "bottom", ncol = 2, cex = 1))
```

# Density Plot
```{r}
plot(gmm_clust, what = "density")
```

## Classification Plot
```{r}
plot(gmm_clust, what = "classification")

```

```{r}
plot(gmm_clust, what = "uncertainty", dimens = c(2,1), main = "ALSFRS Slope vs. SGPT ")
```
```{r}
plot(gmm_clust, what = "uncertainty", dimens = c(4,1), main = "ALSFRS Slope vs. SGPT ")
```

# It can be observed that uncertainity plots do not provide a good estimate of dependence.

```{r}
gmm_clustDR <- MclustDR(gmm_clust, lambda=1)
summary(gmm_clustDR)
```

## Boundaries Plot
```{r}
plot(gmm_clustDR, what = "boundaries", ngrid = 200)
```
```{r}
plot(gmm_clustDR, what = "pairs")
plot(gmm_clustDR, what = "scatterplot")
```
### The above graph shows three different clusters in seperate gaussian planes which signify the diffrent patient phenotypes that can be automatically and reliably identified and used to predict the change of the ALSFRS slope over time.


