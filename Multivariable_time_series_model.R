library(MASS)
library(leaps)
library("ggplot2")
#library(caret)
library(nlme)
#library(urca)
library(vars)
#library(fUnitRoots)
library(aod)
library(zoo)
#library(tseries)
library(plyr)
library(forecast)
library('ncdf4')
library(raster)
library(plyr)
require(rgeos)
require(RColorBrewer)
library(rgdal)
library(maptools)
library(SpatialTools)
#library(PBSmapping)
library(geosphere)


setwd("~/Dropbox/TESIS/R_code/Theme 1.8")

Capture <- read.csv(file="fishery_2002_2015.csv",sep = ",",header=TRUE)
Capture <- Capture[,c(seq(-3,-29,-2))]

#str(Capture)
for(i in c(2:ncol(Capture))) {
  Capture[,i] <- as.numeric(as.character(Capture[,i]), na.pass=T)}
rm(i)


marine_areas <- data.frame("zone" = c(18,21,27,31,34,37,41,47,48,51,57,58,61,67,71,77,81,87,88),
                           "Ocean.Area" = c("Arctic Sea","Atlantic, Northwest","Atlantic, Northeast","Atlantic, Western Central","Atlantic, Eastern Central ","Mediterranean and Black Sea","Atlantic, Southwest ","Atlantic, Southeast ","Atlantic, Antarctic","Indian Ocean, Western ","Indian Ocean, Eastern ","Indian Ocean, Antarctic and Southern","Pacific, Northwest ","Pacific, Northeast ","Pacific, Western Central ","Pacific, Eastern Central","Pacific, Southwest","Pacific, Southeast ","Pacific, Antarctic"))


marine_areas <- marine_areas[order(marine_areas$Ocean.Area),]

marine_areas[,3:17] <- Capture




CRSWG84 <- CRS(" +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
setwd("~/Dropbox/data/FAO_AREAS")
fao <- readShapeSpatial("FAO_AREAS.shp",
                        proj4string = CRSWG84)

#plot(fao.union)
fao.union <- (unionSpatialPolygons(fao, fao$F_AREA))
zone <- row.names(fao.union)
fao.union <- SpatialPolygonsDataFrame(fao.union,
                                      data = data.frame("zone" = zone,
                                                        "area" = data.frame("area" = areaPolygon(fao.union))/10^6,
                                                        row.names=row.names(fao.union)))



FAO_cap <- merge(fao.union@data, marine_areas[,c(-2,-3)], "zone")


setwd("~/Dropbox/TESIS/R_code/Theme 1.8")

sst <- read.csv("sst4.csv", sep=" ")
chl <- read.csv("chlor_a.csv", sep=" ")
pic <- read.csv("pic.csv", sep=" ")
poc <- read.csv("poc.csv", sep=" ")


makedf <- function(fao_zone){
  #fao_zone = 18
  var<- cbind.data.frame(
    (colMeans(sst[sst$fao.zone %in% fao_zone,-1])),
    (colMeans(chl[chl$fao.zone %in% fao_zone,-1])),
    ((colMeans(pic[pic$fao.zone %in% fao_zone,-1])*100090)/(colMeans(poc[poc$fao.zone %in% fao_zone,-1]))))

  colnames(var) <- c("sst","chl","pic.poc")

  var2 <- cbind.data.frame((colSums(FAO_cap[FAO_cap$zone %in% fao_zone,-1:-2]))*19.78426/sum(FAO_cap$area[FAO_cap$zone %in% fao_zone]),
                           var[-15,] , 2002:2015)
  colnames(var2) <- c("tasa_FAO", "sst", "chl","pic.poc","year")

  return(var2)}


makedf_ts <- function(fao_zone){
  #fao_zone = 21
  var<- cbind.data.frame(
    (colMeans(sst[sst$fao.zone %in% fao_zone,-1])),
    (colMeans(chl[chl$fao.zone %in% fao_zone,-1])),
    ((colMeans(pic[pic$fao.zone %in% fao_zone,-1])*100090)/(colMeans(poc[poc$fao.zone %in% fao_zone,-1]))))

  colnames(var) <- c("sst","chl","pic.poc")

  var2 <- cbind.data.frame((colSums(FAO_cap[FAO_cap$zone %in% fao_zone,-1:-2]))*19.78426/sum(FAO_cap$area[FAO_cap$zone %in% fao_zone]),
                           var[-15,] , 2002:2015)
  colnames(var2) <- c("tasa_FAO", "sst", "chl","pic.poc","year")


  var2 <- ts(var2[,-5], start=c(2002), end=c(2015))
  var3 <- apply(var2[,-5], 2, function(c){
    c <- ts(c, start=c(2002), end=c(2015))
    #print(c)
    nd <-ndiffs(c)
    # cat(colnames(c), nd)
    if (nd > 0) {
      col.modi <-c(rep(NA, nd),diff(c,differences=nd))}
    else
      col.modi <- c
    return(col.modi)

  })
  var3 <-ts(as.data.frame(var3), start=c(2002), end=c(2015))
  var3 <- na.omit(var3)
  return(var3)}

makedf_ts_conNA <- function(fao_zone){
  #fao_zone = 27
  var<- cbind.data.frame(
    (colMeans(sst[sst$fao.zone %in% fao_zone,-1])),
    (colMeans(chl[chl$fao.zone %in% fao_zone,-1])),
    ((colMeans(pic[pic$fao.zone %in% fao_zone,-1])*100090)/(colMeans(poc[poc$fao.zone %in% fao_zone,-1]))))

  colnames(var) <- c("sst","chl","pic.poc")

  var2 <- cbind.data.frame((colSums(FAO_cap[FAO_cap$zone %in% fao_zone,-1:-2]))*19.78426/sum(FAO_cap$area[FAO_cap$zone %in% fao_zone]),
                           var[-15,] , 2002:2015)
  colnames(var2) <- c("tasa_FAO", "sst", "chl","pic.poc","year")
  var2 <- ts(var2[,-5], start=c(2002), end=c(2015))
  var3 <- apply(var2[,-5], 2, function(c){
    c <- ts(c, start=c(2002), end=c(2015))
    #print(c)
    nd <- ndiffs(c)
    #print(nd)
    if (nd > 0) {
      col.modi <-c(rep(NA, nd),diff(c,differences=nd))}
    else
      col.modi <- c
    return(col.modi)

  })
  var3 <-ts(as.data.frame(var3), start=c(2002), end=c(2015))
  # var3 <- na.omit(var3)
  return(var3)}



cointeg <- function(fao_zone){
  # cat("fao area is ",fao_zone, "\n")

  # fao_zone <-(81)
  dff <- makedf(fao_zone)
  # dff_ts <- makedf_ts(fao_zone)
  var_combi <- data.frame("id"=1:7)
  if(length(fao_zone) == 1) {
    var_combi$zone <- rep(fao_zone, nrow(var_combi))} else {
      var_combi$zone <- rep("Global", nrow(var_combi))
    }

  #var_combi <- var_combi[,-1]
  vv<- rbind(ldply(list(2,3,4,c(2,3),c(2,4),c(3,4),c(2,3,4)),function(l) {
    #cat(l, length(l),"\n")
    #l=2
    vv <- data.frame(matrix(c(colnames(dff)[c(l)],rep(NA,3-length(l))),ncol = 3),stringsAsFactors=FALSE)
    #print(vv)

    cajo <- ca.jo(dff[,c(1,l)], type="eigen",spec="transitory",ecdet="none",K=2)
    S <- summary(cajo)
    valo <- as.data.frame(cbind(S@teststat, S@cval))

    if(length(which(valo[,1]>valo[,4])) >0 ){
      r <- rownames(valo[which(valo[,1]>valo[,3]),])
      vv$rank <-max(as.numeric(substr(r, start=5, stop=6)))+1

    } else{
      vv$rank <- 0
    }
    return(vv)}))


  colnames(vv)[1:3]<-c("variables_1","variables_2","variables_3")

  var_combi <- cbind(var_combi,vv)

  var_level <- mdply(var_combi[,c(-1,-6)], function(zone, variables_1, variables_2, variables_3){
    i_d <-as.data.frame(t( apply(dff[,-5],2,  function(c){
      c <- ts(c, start=c(2002), end=c(2015))
      nd <-ndiffs(c)
    })))
    max_id <- max(i_d)
    vari <- c(na.omit(c(variables_1, variables_2, variables_3)))
    #cat(zone, vari, "\n")

    vv <-  VAR(dff[,c("tasa_FAO",vari)], lag.max = 2, ic = c("AIC"))
    ff <- tryCatch(summary(vv)$varresult$tasa_FAO$fstatistic,
                   error = function(e) as.numeric(NA))
    #cat("ok...\n")
    pvalue <-tryCatch(pf(ff[1],ff[2],ff[3],lower.tail=F),
                      error = function(e) as.numeric(NA))
    rmse_value <-  tryCatch(accuracy(vv$varresult[[1]])[,2],
                            error = function(e) as.numeric(NA))
    normality <-  tryCatch(serial.test(vv, lags.pt=7, type="PT.adjusted")$serial$p.value,
                           error = function(e) as.numeric(NA))
    bic_value <- CV(vv$varresult$tasa_FAO)[[c(4)]]
    r2 <- CV(vv$varresult$tasa_FAO)[[c(5)]]
    causal <- tryCatch(causality(vv, cause = colnames(vv$y)[-1])$Granger$p.value,
                       error = function(e) NA)

    lt <- data.frame("max_I(d)"=max_id,"lag"=vv$p,"p_value"=pvalue,"BIC"=bic_value,
                     "RMSE"=rmse_value, "normality"=normality, "R2"=r2,
                     "causal" = causal)
    # cat("MIAU\n")

    lt <- cbind(i_d,lt)
    return(lt)
  })



  var_level <- cbind(var_level,var_combi[6])



  return(var_level)}



# FAO area scale

test <- ldply(levels(FAO_cap$zone), function(z){
  #cat("FAO area:", z, "\n");
  selec <- cointeg(z);
  #  print(selec)
})

#test


test_selec <- test[test$normality > 0.05 &  test$causal < 0.05,]

test <- test_selec[!(is.na(test_selec$zone)),]

test[order(test$BIC),]


test_vecm <- test[test$rank != 0,]
#VECM (lag=1)
library(tsDyn)
selec_vecm <- mdply(test_vecm[,c(1:4,17)], function(zone, variables_1, variables_2, variables_3, rank){


  level <- makedf(zone)
  vari <- c(na.omit(c(variables_1, variables_2, variables_3)))

  cajo<- ca.jo(level[,c("tasa_FAO",vari)], type="eigen",spec="transitory",ecdet="none",K=2)
  cajorls_model <- cajorls(cajo, r = rank)
  vecm_model <- VECM(level[,c("tasa_FAO",vari)], lag=1, estim = c("ML"))
  vectovar <- (vec2var(cajo,r=1  ))

  ff <- summary(cajorls_model$rlm)$`Response tasa_FAO.d`$fstatistic
  pvalue <- pf(ff[1],ff[2],ff[3],lower.tail=F)

  rmse_value <- accuracy(cajorls_model$rlm)[,2]

  bic_value <- BIC(vecm_model)
  aic_value <- AIC(vecm_model)
  R2 <- summary(cajorls_model$rlm)$`Response tasa_FAO.d`$adj.r.squared
  normality_test <-  tryCatch(serial.test(vectovar, lags.pt=7, type="PT.adjusted")$serial$p.value,
                              error = function(e) as.numeric(NA))
  lt <- data.frame("p_value"=pvalue, "RMSE"=rmse_value,
                   "bic"=bic_value,"aic"=aic_value, "R2"=R2, "normality"=normality_test)
  return(lt)
})





selec_vecm2 <- subset((selec_vecm[selec_vecm$normality > 0.05 & selec_vecm$p_value < 0.05 ,]))

selec_vecm2 <- selec_vecm2[order(selec_vecm2$bic, selec_vecm2$aic),]

selec_vecm2 <- selec_vecm2[!(is.na(selec_vecm2$zone)),]

selec_vecm2

## Area 18 (VECM): NO
#```{r,echo=FALSE, warning=FALSE, message=FALSE}
level <- makedf_ts(18)
cajo<- ca.jo(level[,c("tasa_FAO","chl","pic.poc")], type="eigen",spec="transitory",ecdet="none",K=2)
cajorls_model <- cajorls(cajo, r = 1)
vecm_model <- VECM(level[,c("tasa_FAO","chl","pic.poc")], lag=1, estim = c("ML"))
vectovar <- (vec2var(cajo,r=1  ))

summary(cajorls_model$rlm)
#```
## Area 21 (VAR): both I(1), no integ.
#```{r,echo=FALSE, warning=FALSE, message=FALSE}
level <- makedf(21)
model_21 <-  VAR(level[,c("tasa_FAO","chl")], lag.max = 3, ic = c("AIC"))
summary(model_21)

serial.test(model_21, lags.pt=7)
causality(model_21, cause = colnames(model_21$y)[-1])
causality(model_21, cause = colnames(model_21$y)[-2])


cajo<- ca.jo(level[,c("tasa_FAO","chl")], type="eigen",spec="transitory",ecdet="none",K=2)

plot(predict(model_21,5))

#```

## Area 31 (VAR): different I(d), no cointeg.
#```{r,echo=FALSE, warning=FALSE, message=FALSE}
level <- makedf(31)
model_31 <-  VAR(level[,c("tasa_FAO","sst")], lag.max = 2, ic = c("AIC"))
summary(model_31)

serial.test(model_31, lags.pt=7)
causality(model_31, cause = colnames(model_31$y)[-1])
causality(model_31, cause = colnames(model_31$y)[-2])


cajo<- ca.jo(level[,c("tasa_FAO","sst")], type="eigen",spec="transitory",ecdet="none",K=2)
summary(cajo)
plot(predict(model_31,5))

#```


## Area 41 (VAR): cap & picpoc same I(1), cointeg. but no one-way causality -> VAR sst
#```{r,echo=FALSE, warning=FALSE, message=FALSE}
level <- makedf(41)
dff <- makedf_ts(41)
model_41 <-  VAR(level[,c("tasa_FAO","sst")], lag.max = 2, ic = c("AIC"))
causality(model_41, cause = colnames(model_41$y)[-1])
causality(model_41, cause = colnames(model_41$y)[-2])
summary(model_41)


#```



## Area 57 (VAR): no cointeg.
#```{r,echo=FALSE, warning=FALSE, message=FALSE}
level <- makedf(57)
dff <- makedf_ts(57)
model_57 <-  VAR(level[,c("tasa_FAO","chl")], lag.max = 2, ic = c("AIC"))
causality(model_57, cause = colnames(model_57$y)[-1])
causality(model_57, cause = colnames(model_57$y)[-2])
summary(model_57)


#```

## Area 58 (VAR): no cointeg.
#```{r,echo=FALSE, warning=FALSE, message=FALSE}
level <- makedf(58)
dff <- makedf_ts(58)
model_58 <-  VAR(level[,c("tasa_FAO","chl")], lag.max = 2, ic = c("AIC"))
causality(model_58, cause = colnames(model_57$y)[-1])
causality(model_58, cause = colnames(model_57$y)[-2])
summary(model_58)


#```

## Area 67 (VAR): same I(d), 1 cointeg
#```{r,echo=FALSE, warning=FALSE, message=FALSE}
level <- makedf(67)

model_67 <-  VAR(level[,c("tasa_FAO","chl","pic.poc")], lag.max = 2, ic = c("AIC"))
causality(model_67, cause = colnames(model_67$y)[-1])
causality(model_67, cause = colnames(model_67$y)[-2])
causality(model_67, cause = colnames(model_67$y)[-3])
summary(model_67)


#```

## Area 77 (VECM): same I(d), 1 cointeg.
#```{r,echo=FALSE, warning=FALSE, message=FALSE}
level <- makedf(77)
model_77 <-  VAR(level[,c("tasa_FAO","sst")], lag.max = 2, ic = c("AIC"))
summary(model_77)



cajo<- ca.jo(level[,c("tasa_FAO","sst")], type="eigen",spec="transitory",ecdet="none",K=2)
summary(cajo)

cajorls_model <- cajorls(cajo, r = 1)
vecm_model <- VECM(level[,c("tasa_FAO","chl","pic.poc")], lag=1, estim = c("ML"))
vectovar <- (vec2var(cajo,r=1  ))

masummary(cajorls_model$rlm)


#```


#Global scale

#```{r,echo=FALSE, warning=FALSE, message=FALSE}
global_test <- cointeg(levels(FAO_cap$zone))

global_test[ global_test$p_value < 0.05 ,]

global_test_vecm <- global_test[global_test$rank != 0,]
#VECM (lag=1)
library(tsDyn)
selec_vecm <- mdply(global_test_vecm[,c(1:4,17)], function(zone, variables_1, variables_2, variables_3, rank){

  if(zone == "Global"){
    zone <- levels(FAO_cap$zone)
  }
  level <- makedf(zone)
  vari <- c(na.omit(c(variables_1, variables_2, variables_3)))

  cajo<- ca.jo(level[,c("tasa_FAO",vari)], type="eigen",spec="transitory",ecdet="none",K=2)
  cajorls_model <- cajorls(cajo, r = rank)
  vecm_model <- VECM(level[,c("tasa_FAO",vari)], lag=1, estim = c("ML"))
  vectovar <- (vec2var(cajo,r=1  ))

  ff <- summary(cajorls_model$rlm)$`Response tasa_FAO.d`$fstatistic
  pvalue <- pf(ff[1],ff[2],ff[3],lower.tail=F)

  rmse_value <- accuracy(cajorls_model$rlm)[,2]

  bic_value <- BIC(vecm_model)
  aic_value <- AIC(vecm_model)
  R2 <- summary(cajorls_model$rlm)$`Response tasa_FAO.d`$adj.r.squared
  normality_test <-  tryCatch(serial.test(vectovar, lags.pt=7, type="PT.adjusted")$serial$p.value,
                              error = function(e) as.numeric(NA))
  lt <- data.frame("p_value"=pvalue, "RMSE"=rmse_value,
                   "bic"=bic_value,"aic"=aic_value, "R2"=R2, "normality"=normality_test)
  return(lt)
})





selec_vecm2 <- subset((selec_vecm[selec_vecm$normality > 0.05 & selec_vecm$p_value < 0.05 ,]))

selec_vecm2 <- selec_vecm2[order(selec_vecm2$bic, selec_vecm2$aic),]

selec_vecm2 <- selec_vecm2[!(is.na(selec_vecm2$zone)),]

selec_vecm2

#```
##VECM_Global no exist -> VAR
#```{r,echo=FALSE, warning=FALSE, message=FALSE}

global_level <- makedf(levels(FAO_cap$zone))
global_diff <- makedf_ts(levels(FAO_cap$zone))

library(tsDyn)

cajo<- ca.jo(global_level[,c("tasa_FAO","sst")], type="trace",spec="transitory",ecdet="none",K=2)
cajorls_model <- cajorls(cajo, r = 1)
vecm_model <- VECM(global_level[,c("tasa_FAO","sst")], lag=1, estim = c("ML"))
vectovar <- (vec2var(cajo,r=1  ))

summary(cajo)
summary(cajorls_model$rlm)

print("global_level")
global_model <-  VAR(global_level[,c("tasa_FAO","sst")], lag.max = 2, ic = c("AIC"))
summary(global_model)

serial.test(global_model, lags.pt=2)

#```
