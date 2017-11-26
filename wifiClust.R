
library(RODBC)

channel = odbcConnect("master", "dtauser","dtauser")

dataSet = sqlQuery(channel, "select * from [HAERBIN].[dbo].[tb_wifi_zc]")

dim(dataSet)

head(dataSet)

str(dataSet)

# about time period: 8/22 test segment is  10:15-10:19
# 8/26: test segment is 8:49-8:53
# 8/30: test segment is 11:53-12:00 and 14:23-14:28

dataSet$???? = format(dataSet$????, format = '%Y-%m-%d')

# start with the data in 8-22 and 8-26, which include six floors

dataSet6f = subset(dataSet, dataSet$???? != '2016-08-30') #  9314 obs, with 6355 in 0822
                                                          #  and 2959 in 0826
dataSet1f = subset(dataSet, dataSet$???? == '2016-08-30') #  17830 obs

str(dataSet6f)

# change varible levels

dataSet6f$SSID = factor(dataSet6f$SSID)

table(dataSet6f$SSID)

library(ggplot2)

ggplot(data = dataSet, aes(x = dataSet$wifi_id)) + 
  geom_histogram(stat = 'count', fill = 'lightblue') + 
  geom_hline(yintercept = 10, linetype="dotted", color = 'red') +
  xlab('wifi_id')

library(dplyr)

wifiGroups = group_by(dataSet, wifi_id)

wifiGroupsCount = summarize(wifiGroups, Count = n())

# wifiGroupsCount$wifi_id = as.vector(wifiGroupsCount$wifi_id)

# for (i in 1:nrow(wifiGroupsCount)) {
#   wifiGroupsCount$name[i] = dss[which(wifiGroupsCount[i, 'wifi_id'] == dss$alias)[1], 'WIFI????']
# }

boxplot(wifiGroupsCount$Count, col = 'yellow')

summary(dataSet$rssi)

# SSID Count
# <fctr> <int>
# 1  08:bd:43:a5:1f:4f   103
# 2  16:75:90:a5:9d:4e   101
# 3  30:fc:68:f0:3b:6f   136
# 4  70:a8:e3:5e:04:a4   106
# 5  70:f9:6d:2c:e0:f1   104
# 6  70:f9:6d:2c:e0:f2   105
# 7  8c:a6:df:ca:c9:3f   120
# 8  8c:be:be:29:27:d6   122
# 9  8e:be:be:29:27:d8   130
# 10 c0:61:18:0d:6d:e0   117
# 11 dc:9c:9f:c7:b2:fd   142
# 12 dc:9c:9f:c8:8c:53   112

# view the distributin of wifis

dis = subset(dataSet6f, dataSet6f$SSID == '30:fc:68:f0:3b:6f' | 
               dataSet6f$SSID == '8e:be:be:29:27:d8')

# wifiRssiGroups = group_by(dataSet6f, WIFI????, RSSI)
# 
# wifiRssiGroupsCount = summarise(wifiRssiGroups, Count = n())

p1 = ggplot(data = subset(dataSet, dataSet$wifi_id == 'wifi100'), aes(x = rssi)) + 
  geom_histogram()

p2 = ggplot(data = subset(dataSet, dataSet$wifi_id == 'wifi266'), aes(x = rssi)) + 
  geom_histogram() + 
  scale_x_continuous(breaks = seq(-100, -40, 2))

p3 = ggplot(data = subset(dataSet, dataSet$wifi_id == 'wifi221'), aes(x = rssi)) + 
  geom_histogram() # 50 counts as a boundary?

grid.arrange(p1, p2, p3)

ggplot(data = dis, aes(x = RSSI)) +
  geom_freqpoly(aes(color = SSID))

library(gridExtra)

g1 = ggplot(data = subset(dataSet6f, dataSet6f$SSID == '30:fc:68:f0:3b:6f'), aes(x = RSSI)) + 
  geom_histogram() +
  scale_x_continuous(breaks = seq(-100, -40, 2))

g2 = ggplot(data = subset(dataSet6f, dataSet6f$SSID == '8e:be:be:29:27:d8'), aes(x = RSSI)) + 
  geom_histogram() + 
  scale_x_continuous(breaks = seq(-100, -40, 2))

ggplot(data = subset(dataSet6f, dataSet6f$SSID == '44:94:fc:5e:c3:bd'), aes(x = RSSI)) + 
  geom_histogram() +
  scale_x_continuous(breaks = seq(-100, -50, 2))

ggplot(data = subset(dataSet6f, dataSet6f$SSID == '70:f9:6d:2c:e0:e0'), aes(x = RSSI)) + 
  geom_histogram() +
  scale_x_continuous(breaks = seq(-100, -30, 2))

grid.arrange(g1, g2)

# the distribution of RSSI is irregulate

summary(dataSet$rssi) # we could see that most values(75%) are gathered in -98 ~ -73

summary(wifiGroupsCount$Count)

quantile(dataSet$rssi, 5/6) # -68

quantile(dataSet$rssi, 4/5)

boundID = subset(wifiGroupsCount, wifiGroupsCount$Count > 40)

ss = subset(dataSet6f, dataSet6f$SSID %in% boundID$SSID)

# if six floor included

quantile(ss$RSSI, 5/6) # -66

# if five floor included

quantile(ss$RSSI, 4/5) # -69

boundID = subset(wifiGroupsCount, wifiGroupsCount$Count > 70) # -62, -64 # obs

#######################################################################
#######################################################################

channel = odbcConnect("master", "dtauser","dtauser")

dataSet = sqlQuery(channel, "select * from [HAERBIN].[dbo].[tb_wifi_zy]")

save(dataSet, file = 'dataSetFloor.Rdata')

########################################################################
########################################################################

ggplot(data = dataSet, aes(x = sample_id)) +
  geom_histogram(stat = 'count')

ggplot(data = subset(dataSet, dataSet$sample_id %in% c(170:180)), aes(x = sample_id,
                                                                      y = rssi)) + 
  geom_boxplot(fill = 'pink') + 
  geom_point(alpha = 1/3)

head(dataSet, 20)

uu = unique(dataSet$sample_id)

str(dataSet)

dataSet$sample_id = factor(dataSet$sample_id)

length(uu)

ggplot(data = dataSet, aes(x = wifi_id)) + 
  geom_histogram(stat = 'count', fill = 'orange')

length(unique(dataSet$wifi_id))

ggplot(data = dataSet, aes(x = sample_id)) + 
  geom_histogram(stat = 'count', fill = 'orange')

table(dataSet$sample_id)

# delete duplicated rows

dd = dataSet[, 1:2]

dataSet = dataSet[!duplicated(dd), ] # 189 duplicated rows are selected out # dataSet

# reshape the matrix

wifiIDs = unique(dataSet$wifi_id)
sampleIDs = unique(dataSet$sample_id)

wifiMat = matrix(rep(0, 237*181), nrow = 237, ncol = 181)

# for (i in 1:237) {
#   for (j in 1:181) {
#     l = length(dataSet[which(dataSet$wifi_id == wifiIDs[i] & 
#                                dataSet$sample_id == sampleIDs[j]), 'rssi'])
#     if (l == 1) {
#       wifiMat[i, j] = dataSet[which(dataSet$wifi_id == wifiIDs[i] & 
#                                       dataSet$sample_id == sampleIDs[j]), 'rssi']
#     }
#   }
# }
# use dcast !!!!


for (i in 1:237) {
  for (j in 1:181) {
    if (wifiMat[i, j] != 0) {
      wifiMat[i, j] = 1 / wifiMat[i, j] # if reasonable
    }
  }
}

# trans into sparse matrix

wifiMatSparse = Matrix(wifiMat, sparse = T)

wifiFrame = data.frame(wifiMat)

row.names(wifiFrame) = wifiIDs

colnames(wifiFrame) = sampleIDs

hc = hclust(dist(wifiFrame, method = 'euclidean'), method = 'ward')

plot(hc, cex = 0.6)

cc = cutree(hc, k = 6)

# five classes maybe better
cc2five = cutree(hc, k = 5)

table(cc2five)

write.csv(cc, 'cc.csv')

write.csv(cc2five, 'cc2five.csv')

for (i in 1:nrow(tt)) {
  tt$name[i] = dss[which(tt[i, 'X'] == dss$alias)[1], 'WIFI????'] # as.vector(factor)
}

######################################################################################
######################################################################################
# using centroid method

hc1 = hclust(dist(wifiFrame, method = 'euclidean'), method = 'centroid')

plot(hc1, cex = 0.5) # cex is the mutiple of text

cc1 = cutree(hc1, k = 6)

table(cc1)

write.csv(cc1, 'cc1.csv')

# using average method

hc2 = hclust(dist(wifiFrame, method = 'euclidean'), method = 'average')

plot(hc2, cex = 0.6)

plot(hc2$height)

cc2 = cutree(hc2, k = 6)

# try 5 classes

cc2to5 = cutree(hc2, k = 5)

table(cc2)

write.csv(cc2, 'cc2.csv')

tt = read.csv('cc2.csv')

tt$X = as.vector(tt$X)

write.csv(tt, 'cc2.csv') # this seems much better

# delete those low frequency values and change the dist. low -> -120

# FreqNames = wifiGroupsCount[which(wifiGroupsCount$Count >= 10),'wifi_id']
# 
# dataSet2 = subset(dataSet, as.vector(dataSet$wifi_id) %in% as.vector(FreqNames))


boundID = subset(wifiGroupsCount, wifiGroupsCount$Count > 10)

ss = subset(dataSet, dataSet$wifi_id %in% boundID$wifi_id)

ggplot(data = ss, aes(x = ss$wifi_id)) + 
  geom_histogram(stat = 'count') + 
  geom_hline(yintercept = 10)

dds = dcast(data = ss, wifi_id ~ sample_id, value.var = 'rssi') # dds

# fill NAs with -120

dds[is.na(dds)] = -120

ddsScaled = scale(dds[, 2:182])

ddsScaled = data.frame(ddsScaled, row.names = dds[,1])

fit.average = hclust(dist(ddsScaled, method = 'euclidean'), method = 'average')

plot(fit.average)

# plot(fit.average$height)

nc = NbClust(ddsScaled, distance = 'eucludean', method = 'average')

cc3 = cutree(fit.average, k = 6)

table(cc3)

write.csv(cc3, 'cc3.csv')

################################ ward ##########################

fit = hclust(dist(ddsScaled, method = 'euclidean'), method = 'ward')

cc4 = cutree(fit, k = 6)

table(cc4)

write.csv(cc4, 'cc4.csv')

# use single

fit.single = hclust(dist(wifiFrame, method = 'euclidean'), method = 'single')

plot(fit.single$height)

cc.single = cutree(fit.single, k = 50)

table(cc.single)

write.csv(cc.single, 'ccSingle.csv')

# DBSCAN

install.packages('fpc')

library(fpc)

##################################################################
##################################################################

dds = dcast(data = dataSet, wifi_id ~ sample_id, value.var = 'rssi')

wifiGroups = group_by(dataSet, wifi_id)

wifiGroupsSumm = summarise(wifiGroups, maxRssi = max(rssi), count = n())

# wifiGroupsSumm$sample_id = dataSet[which(dataSet$wifi_id == wifiGroupsSumm$wifi_id &
#                                            dataSet$rssi == wifiGroupsSumm$maxRssi)[1], 'sample_id']

# a new matrix to store wifi relationship

wifiIDs = wifiGroupsSumm$wifi_id

wifiIDs = as.vector(wifiIDs)

seqq = 1:237

retMat = matrix(data = NA, nrow = 237, ncol = 237)

for (i in seqq) {
  wifiID = wifiIDs[i]
  rssi = wifiGroupsSumm[which(wifiGroupsSumm$wifi_id == wifiID), "maxRssi"]
  rssi = unlist(rssi)
  sampleID = dataSet[which(dataSet$wifi_id == wifiID & dataSet$rssi == rssi)[1], 'sample_id']
  if (length(sampleID) == 0) {
    print('sample does not exist!')
  } else {
    for (j in seqq) {
      leftWifi = wifiIDs[j]
      leftRssi = dataSet[which(dataSet$wifi_id == leftWifi & 
                                 dataSet$sample_id == sampleID), 'rssi']
      if (length(leftRssi != 0)) {
        retMat[i, j] = leftRssi
      }
    }
  }
}

##################################################################

dds = retMat

dds[is.na(dds)] = -100

dds = data.frame(dds, row.names = wifiIDs)

colnames(dds) = wifiIDs

# using layer clusters / average method

fit.average = hclust(dist(dds, method = 'euclidean'), method = 'single')

plot(fit.average)

plot(fit.average$height)

dd = data.frame(fit.average$merge, fit.average$height)

# nc = NbClust(dds, distance = 'eucludean', method = 'average')

ccDist1 = cutree(fit.average, k = 30)

table(ccDist1)

write.csv(ccDist1, 'ccDist1.csv')

# delete those weak wifis

weakWifiInd = which(wifiGroupsSumm$maxRssi < -70)

fit.average = hclust(dist(dds[-weakWifiInd, ], method = 'euclidean'), method = 'average')

plot(fit.average)

plot(fit.average$height)

ccDist2 = cutree(fit.average, k = 6)

table(ccDist2)

write.csv(ccDist2, 'ccDist2.csv')

# find the strongest wifi per sample
# 
# dbscan(data = dds, eps = 0.3, MinPts = 10, scale = T)

sampleGroups = group_by(dataSet, sample_id)

sampleSum = summarise(sampleGroups, maxRssi = max(rssi), n = n())

# create matrix
ww = NULL

dd = dataSet

dd$wifi_id = as.vector(dd$wifi_id)

# wifiFr = data.frame(wifi_id = NULL, sample_id = NULL, rssi = NULL)

for (i in 1:nrow(sampleSum)) {
  rssi = sampleSum[i, "maxRssi"]
  rssi = unlist(rssi)
  # wifi = subset(dataSet, dataSet$sample_id == i & dataSet$rssi == rssi)$wifi_id
  wifi = dd[which(dataSet$sample_id == i & dataSet$rssi == rssi), "wifi_id"]
  # wifiFr = rbind(wifiFr, list(wifi, i, rssi))
}

dds2 = sqlQuery(channel, "select * from [HAERBIN].[dbo].[tb_wifi_zy6] order by sample_id")

dds2[is.na(dds2)] = -100

# fit.average = hclust(dist(dds[-weakWifiInd, ], method = 'euclidean'), method = 'average')

dframe = data.frame(dds2[, 3:239], row.names = 1:245)

dframe[1,1:5]

ffit = hclust(dist(dframe, method = 'euclidean'), method = 'average')

ccDist3 = cutree(ffit, k = 6)

table(ccDist3)

write.csv(ccDist3, 'ccDist3.csv')

write.csv(dds2[, 2], 'aa.csv') # only 29 distinct wifis !!

###################### new DataSet ########################
###########################################################

test21 = sqlQuery(channel, "select * from [HAERBIN].[dbo].[tb_wifi_21]")

test21$class = 1

head(test)

str(test)

time2samp = function(ds) {
  tn = ds$time[1]
  s = 1
  for (i in 1:nrow(ds)) {
    if (ds$time[i] == tn) {
      ds$sample[i] = s
    } else {
      s = s + 1
      ds$sample[i] = s
      tn = ds$time[i]
    }
  }
  return(ds)
}

mac2wifi = function(ds) {
  l = as.vector(unique(ds$mac))
  ds$wifi_id = vector(length = nrow(ds))
  for (i in 1:nrow(ds)) {
    ww = which(l == ds[i, 'mac'])
    ds$wifi_id[i] = paste('wifi', ww, sep = '')
  }
  ds$wifi_id = factor(ds$wifi_id)
  return(ds)
}


# numStr = function(n) {
#   if (n >= 1000) {
#     return(0)
#   } else {
#     l = NULL
#     for (i in 1:n) {
#       if (n < 10) {
#         d = paste(0, 0, i, sep = '')
#         l = c(l, d)
#       } else if (n < 100) {
#         d = paste(0, i, sep = '')
#         l = c(l, d)
#       } else {
#         d = paste(i)
#         l = c(l, d)
#       }
#     }
#   }
#   return(l)
# }

testAll = rbind(test21, test22, test23, test24, test25, test26)

testAll = time2samp(testAll)

testAll = mac2wifi(testAll) # 386 samples and 316 wifis

testAll$sample = factor(testAll$sample)

testAllClean = data.frame(testAll[, 7], testAll[, 8], testAll[, 4]) # flag!

colnames(testAllClean) = c('Sample_id', 'Wifi_id', 'Rssi')

# store wifiNames, wifi_id and classLabel

dd = testAllClean[, 1:2]

testAllClean = testAllClean[!duplicated(dd), ] # 4814 duplicated rows are selected out

wifiIDclass = data.frame(testAll[, 2], testAll[, 8], testAll[, 6])

colnames(wifiIDclass) = c('Wifi_name', 'Wifi_id', 'Classlabel')

testFrame = dcast(testAllClean, Wifi_id ~ Sample_id, value.var = 'Rssi')

testFrame = data.frame(testFrame[, 2:ncol(testFrame)], row.names = testFrame[, 1])

# 21 -> 43wifi; 22 -> 53wifi; 23 -> 79wifi; 24 -> 84wifi; 25 -> 144wifi; 26 -> 13wifi
# different methods can be separated here

# method 1: scale the frame and fill NAs with 0. originValue = 1/originValue

tf1 = testFrame

tf1[!is.na(tf1)] = 1 / tf1[!is.na(tf1)]

tf1[is.na(tf1)] = 0

fit1 = hclust(dist(tf1, method = 'euclidean'), method = 'average')

plot(fit1)

plot(fit1$height)

##########################

fit1 = hclust(dist(tf1, method = 'euclidean'), method = 'complete')

cc3 = cutree(fit1, k = 7)

table(cc3)

write.csv(cc3, 'a.csv')

# method 2: fill NAs with -120, no scale

tf2 = testFrame

tf2[is.na(tf2)] = -120

fit2 = hclust(dist(tf2, method = 'euclidean'), method = 'average')

plot(fit2$height)

plot(fit2)

cc3 = cutree(fit2, k = 7)

table(cc3)

rect.hclust(fit2, k = 7)

# method3: try dbscan, fill NAs with -120

tf3 = testFrame

tf3[is.na(tf3)] = -120

fit3 = dbscan(data = tf3, eps = 1.2, MinPts = 2, scale = T)

d = data.frame(wifi_name = rownames(tf3), classLabel = fit3$cluster)

# dbscan Pts=316 MinPts=6 eps=5
#          0 1 2   3  4  5 6 7
# border 136 5 5   0  4  4 1 4
# seed     0 1 1 129 12  6 6 2
# total  136 6 6 129 16 10 7 6

# dbscan Pts=316 MinPts=9 eps=9
#         0  1  2  3  4   5  6
# border 75  6  2 16  0   0 10
# seed    0 31 19 16 10 129  2
# total  75 37 21 32 10 129 12

# method 4: use pamk to clust and define the number of classes

pp = pamk(tf4) # require(fpc)

table(pp$pamobject$clustering, wifiIDclass$Classlabel)

p = data.frame(rownames(tf3), pp$pamobject$clustering)

write.csv(p, 'p.csv')

plot(pp$pamobject, na.omit(T))

# classify

library(vegan)

# (SSB/(K-1))/(SSW/(n-K))
# based on KMeans

fit = cascadeKM(tf4, 2, 12)

# method 5: mclust

library(mclust)

fit = Mclust(data = tf3, G = 2:20) # bad

# method 6: based on wifi-relationships

tf4 = testAllClean

# sift out wifis in 26 and rssi < -80
# wifi177
# wifi304
# wifi307
# wifi308
# wifi309
# wifi310
# wifi311
# wifi312
# wifi313
# wifi314
# wifi315
# wifi316

wifi26IDs = c('wifi177', 'wifi304', 'wifi307', 'wifi308', 'wifi309', 'wifi310', 'wifi311',
              'wifi312', 'wifi313', 'wifi314', 'wifi315', 'wifi316')

wifiGroups = group_by(tf4, Wifi_id)

wifiGroupsSumm = summarise(wifiGroups, maxRssi = max(Rssi), count = n())

tf4 = wifiGroupsSumm[-which(wifiGroupsSumm$Wifi_id %in% wifi26IDs | wifiGroupsSumm$maxRssi < -65), ]

# a new matrix to store wifi relationship, 259 wifis left

wifiIDs = tf4$Wifi_id

wifiIDs = as.vector(wifiIDs)

seqq = 1:103

retMat = matrix(data = NA, nrow = 103, ncol = 103)

for (i in seqq) {
  wifiID = wifiIDs[i]
  rssi = tf4[which(tf4$Wifi_id == wifiID), "maxRssi"]
  rssi = unlist(rssi)
  sampleID = testAllClean[which(testAllClean$Wifi_id == wifiID & testAllClean$Rssi == rssi)[1],
                          'Sample_id']
  if (length(sampleID) == 0) {
    print('sample does not exist!')
  } else {
    for (j in seqq) {
      leftWifi = wifiIDs[j]
      leftRssi = testAllClean[which(testAllClean$Wifi_id == leftWifi & 
                                 testAllClean$Sample_id == sampleID), 'Rssi']
      if (length(leftRssi != 0)) {
        retMat[i, j] = leftRssi
      }
    }
  }
}

# add rownames wifiID

tf4 = retMat

tf4[is.na(tf4)] = -120

# define the number of clusters

pp = pamk(tf4)

fit4 = hclust(dist(tf4, method = 'euclidean'), method = 'centroid')

plot(fit4, cex = 0.7)

rect.hclust(fit4, k = 5)

plot(fit4$height, xlab = 'Step', ylab = 'Height')

wifiCorrelation = cutree(fit4, k = 5)

table(wifiCorrelation)

write.csv(data.frame(wifi_id = wifiIDs, class_label = wifiCorrelation), 
          'tt.csv')

# method 7: dbscan classify

fit5 = dbscan(data = tf4, eps = 1.2, MinPts = 2, scale = T)

write.csv(data.frame(wifi_id = wifiIDs, class_label = fit5$cluster), 
          'dbscan.csv')

# function to calculate the precision
# rownames: wifi_id, classLabel, floor

clusterPrec = function(file, k) {
  correctCount = 0
  rowNum = nrow(file)
  for (i in 1:k) {
    currentDF = subset(file, file[, 2] == i)
    freqTable = as.data.frame(table(currentDF[, 3]))
    correctCount = correctCount + max(freqTable$Freq)
  }
  sprintf('Precision is: %.2f', correctCount/rowNum)
  return(correctCount/rowNum)
}

# method 8: use samples -- wifis, fill NAs with -120

tf6 = testFrame[which(rownames(testFrame) %in% tf4$Wifi_id), ]

tf6[is.na(tf6)] = -120

fit6 = hclust(dist(tf6, method = 'euclidean'), method = 'average')

plot(fit6$height)

plot(fit6, cex = .8)

rect.hclust(fit6, k = 4)

cc = cutree(fit6, k = 5)

table(cc)

write.csv(cc, 'sample_wifi.csv')

# require:
# wifi_id classLabel actualLabel
# wifi1    1     21
# wifi10   1     21
# wifi104  2     21
# wifi105  2     21
# wifi11   1     21

c.clust = function(ds, k, ...) {
  fit = hclust(dist(ds, method = 'euclidean'))
  cc = cutree(fit, k = k)
  d = data.frame(wifi_id = wifiIDs, classLabel = cc, actual_label = label) # label
  return(clusterPrec(d, k))
}

# test in all floors(1-26)

ds26 = sqlQuery(channel, "SELECT *FROM [HAERBIN].[dbo].[tb_wifi_zy1]
  ORDER BY [wifi_id_service]")

ds26 = ds26[!duplicated(ds26[,1:2]), ]

dt = dcast(data = ds26, wifi_id_service ~ wifi_id_neighbour, value.var = 'rssi')

dt[is.na(dt)] = -120

fit = hclust(dist(dt, method = 'euclidean'), method = 'single')

plot(fit$height)

cc = cutree(fit, k = 26)

table(cc)

write.csv(data.frame(wifi_id = dt$wifi_id_service, class = cc), '26_wifi_clust.csv')

aa = data.frame(wifi_id = rownames(dt), classLabel = cc)

# get the wifi -> floor

ddd = sqlQuery(channel, "SELECT * FROM [HAERBIN].[dbo].[tb_wifi_zy2]
  ORDER BY [sample_floor]")

ddd = data.frame(ddd[, c(3, 4, 2)])

colnames(ddd) = c('rssi', 'class', 'wifi_id')

ddp = addLabels(ddd)

# addLabels = function(df) {
#   rn = c('rssi', 'class', 'wifi_id')
#   if (!identical(colnames(df), rn)) {
#     print('error! Rownames must equal to rssi, class, wifi_id.')
#   } else {
#     r = aggregate(cbind(df$rssi, df$class), by = list(df$wifi_id), max)
#     r = r[order(r[, 1]), ]
#     colnames(r) = c('Wifi_id', 'ActualLabel')
#     return(r)
#   }
# }

colnames(ddp) = c('wifi_id', 'rssi', 'actual_label')

ddpp = subset(ddp, ddp$wifi_id %in% aa$wifi_id)

aa$floor = ddpp$actual_label

clusterPrec(aa, 26)

mac_wifi_id = sqlQuery(channel, "select * from [HAERBIN].[dbo].[tb_wifi_zc_mac_wifi_id]")

mac_floor = sqlQuery(channel, "select * from [HAERBIN].[dbo].[tb_wifi_bak_floor]")

for (i in 1:nrow(mac_floor)) {
  mac_floor$wifi_id[i] = mac_wifi_id[which(mac_wifi_id$mac == mac_floor$mac[i]), 'wifi_id']
}

write.csv(data.frame(mac_floor[, 4], mac_floor[, 1], mac_floor[, 3]), 'mac_floor.csv')

# 55%

########################################################################
# distance to set floors

tf7 = tf6

wifi_class = read.csv('wifiAgreggate/distanceTest.csv', header = T)

for (i in 1:nrow(tf7)) {
  tf7$class[i] = wifi_class[which(wifi_class$wifi_id == rownames(tf7)[i]), 'floor']
}

aa = aggregate(tf7, by = list(tf7$class), mean)

dist(aa[, 2:387])


#       1        2        3        4
# 2 481.6554                           
# 3 463.0934 471.3556                  
# 4 528.4624 587.8078 482.7240         
# 5 648.1415 703.6085 642.6123 625.9861

tt = tf7[order(tf7$class), ]

table(tt$class)

tt$class = factor(tt$class)

# minDist = function(tt) {
#   ta = as.vector(table(tt$class))
#   cum = c(0, cumsum(ta))
#   n = ncol(tt)
#   for (i in 1:4) {
#     l = vector()
#     for (k in (cum[i] + 1):cum[i + 1]) {
#       curVec = tt[k, 1:(n - 1)]
#       for (j in (cum[i + 1] + 1):cum[i + 2]) {
#         dist = sum((curVec - tt[j, 1:(n - 1)]) ^ 2)
#         l = c(l, dist)
#       }
#       print(min(l))
#     }
#   }
# }

m = as.matrix(dist(tt))

l = c()

for (i in 1:3) { # the first class length
  l = c(l, min(m[4:10, i])) # 4:10 the second class length to traverse
}

min(l)

# tt is ordered by class, 1,2,... are rssi, last column is class
#############################################################

minDist = function(tt) {
  m = as.matrix(dist(tt[, 1:(ncol(tt) - 1)]))
  ta = as.vector(table(tt$class))
  cum = c(0, cumsum(ta))
  for (i in 1:(length(ta) - 1)) {
    # print('/n')# control which cluster
    col = (cum[i] + 1):(cum[i + 1])
    for (j in (i + 1):length(ta)) { # control the cluster to compare
      row = (cum[j] + 1):(cum[j + 1])
      print(min(m[row, col]))
    }
  }
}

#############################################################

# match the most wifiIDs in the floor

ggplot(data = aa, aes(x = wifi_id, y = rssi)) + 
  geom_boxplot(aes(fill = wifi_id)) + 
  scale_x_discrete(labels = c()) + 
  ggtitle('Rssi Range by Wifi_ID') + 
  theme(legend.position = 'none', plot.title = element_text(hjust = 0.5, vjust= 2.12)) 
  
uu = sqlQuery(channel, "SELECT [time]
                   ,[ssid] as wifi_name
                   ,[mac]
                   ,[rssi]
                   ,[freq]
                   ,[sample_floor]
                   FROM [HAERBIN].[dbo].[tb_wifi_bak]
                   order by time")

# easier way to trans to wifi 2 wifi_neighbor
# require sample ordered

wifi2wifiUp = function(dataSet, threshold = -65) {
  # sampleNums = max(dataSet$sample_id)
  df = data.frame()
  sampleCount = as.vector(table(dataSet$sample_id))
  sampleOrder = c(0, cumsum(sampleCount))
  for (i in 1:nrow(dataSet)) {
    currVec = dataSet[i, ]
    currSampleId = currVec[, 'sample_id']
    if (currVec[, 'rssi'] >= threshold) {
      dd = data.frame(currVec[, 'wifi_id'], 
                      dataSet[(sampleOrder[currSampleId] + 1):sampleOrder[currSampleId + 1], 
                              c('wifi_id', 'rssi')])
      df = rbind(df, dd)
    }
  }
  colnames(df) = c('wifi_id', 'wifi_neighbor_id', 'rssi')
  return(df)
}

#################################################################

df = wifi2wifiUp(dataSet, -65) # too slow

df = sqlQuery(channel, "SELECT * FROM [HAERBIN].[dbo].[tb_wifi_zy5]")

df = df[, c(1, 2, 4)]

dddGroup = group_by(dataSet, )

dd1 = aggregate(df[, 3], 
                         by = list(df$wifi_id_service, df$wifi_id_neighbour), mean)

colnames(dd1) = c('wifi_id', 'wifi_neighbor_id', 'rssi')

wifiRelat = dcast(dd1, wifi_id ~ wifi_neighbor_id, value.var = 'rssi')

# wifiRelat[is.na(wifiRelat)] = -200 # -120 choosable

# wifiClust() function is enough

#  start function here

medianClust = function(wifiRelat, floorLabel, fillNA, distMethod, method, k, write = F) {
  wifiRelat[is.na(wifiRelat)] = fillNA
  fit = hclust(dist(wifiRelat[, 2:ncol(wifiRelat)], method = distMethod), 
               method = method)
  cc = cutree(fit, k)
  dd = data.frame(wifi_id = wifiRelat[, 1], class = cc, floor = floorLabel)
  if (write) {
    write.csv(dd, 'clustResult6.csv')
  }
  clusterPrec(dd, k)
}

medianClust(wifiRelat, floorLabel, -400, 'euclidean', 'ward.D2', 30, write = T)

# fit = hclust(dist(wifiRelat[, 2:ncol(wifiRelat)], method = 'eclidean'), method = 'ward')
# 
# plot(fit$height)
# 
# cc = cutree(fit, 26)
# 
# table(cc)
# 
# write.csv(data.frame(wifi_id = wifiRelat[, 1], class = cc), 'wifiMedianWard200.csv')

t = table(re[,2], re[,3])

correctCount = 0

for (i in 1:24) {
  correctCount = correctCount + max(t[i, ])
}

# pp = pamk(tf4) # require(fpc)
# 
# table(pp$pamobject$clustering, wifiIDclass$Classlabel)

errorWifi = c('wifi_399',
              'wifi_400',
              'wifi_338',
              'wifi_348',
              'wifi_355',
              'wifi_371',
              'wifi_388',
              'wifi_390',
              'wifi_579',
              'wifi_625',
              'wifi_627',
              'wifi_628',
              'wifi_1003',
              'wifi_1016',
              'wifi_1061',
              'wifi_1062',
              'wifi_977',
              'wifi_995',
              'wifi_1001',
              'wifi_994',
              'wifi_327',
              'wifi_2',
              'wifi_3',
              'wifi_7'
)

ee = subset(wifiRelat, wifiRelat$wifi_id %in% errorWifi)

floorLabel = c(17,16,16,16,16,26,26,20,2,2,2,2,2,2,1,1,7,8,8,8,
               26,16,17,16) # 8 classes
l = c()

for (i in 1:618) {
  l = c(l, sum(!is.na(wifiRelat[i, ])))
}

##################################################

medianClust = function(wifiRelat, floorLabel, fillNA, distMethod, method, k, write = F) {
  wifiRelat[is.na(wifiRelat)] = fillNA
  fit = hclust(dist(wifiRelat[, 2:ncol(wifiRelat)], method = distMethod), 
               method = method)
  cc = cutree(fit, k)
  dd = data.frame(wifi_id = wifiRelat[, 1], class = cc, floor = floorLabel)
  if (write) {
    write.csv(dd, 'clustResult2.csv')
  }
  clusterPrec(dd, k)
}

ww = read.csv('wifiID.csv', header = T)

floorLabel = ww$floor

medianClust(wifiRelat, floorLabel, -120, 'euclidean', 'ward', 26) # 76%

medianClust(wifiRelat, floorLabel, -300, 'euclidean', 'ward', 26) # 80%

medianClust(wifiRelat, floorLabel, -300, 'euclidean', 'average', 26) # 71%

medianClust(wifiRelat, floorLabel, -1000, 'euclidean', 'ward', 26)

#####################################################################
# subset !NAs between 50 and 150

NACount = function(vec) {
  sum(!is.na(vec))
}

for (i in 1:618) {
  l = c(l, sum(!is.na(wifiRelat[i, ])))
}

wifiR = wifiRelat[which(l > 50 & l < 150), ]

floorLabel = ww$floor

medianClust(wifiRelat, floorLabel, -500, 'euclidean', 'ward', 100)

# take a closer look at the overlaps and the relatiobships

overlapRelat = wifiRelat

overlapRelat$floor = floorLabel

overlapRelat = overlapRelat[order(overlapRelat$floor), ]

sum(!is.na(overlapRelat[1, ]) & !is.na(overlapRelat[4, ]))























# wifi-wifi distance

ds = sqlQuery(channel, 'select * from [HAERBIN].[dbo].[tb_wifi_zy5]')

wifiR2 = dcast(ds, wifi_id_1 ~ wifi_id_2, value.var = 'rssi3')



nname = wifiR2$wifi_id1

ww = wifiR2

ww[is.na(ww)] = 2
# 
# for (i in 1:length(a)) {
#   for (j in 1:length(b)) {
#     if (a[i] != b[j]) {
#       print(a[i])
#     }
#   }
# }

medianClust = function(wifiRelat, floorLabel, fillNA, distMethod, method, k, write = F) {
  wifiRelat[is.na(wifiRelat)] = fillNA
  fit = hclust(dist(wifiRelat[, 2:ncol(wifiRelat)], method = distMethod), 
               method = method)
  cc = cutree(fit, k)
  dd = data.frame(wifi_id = wifiRelat[, 1], class = cc, floor = floorLabel)
  if (write) {
    write.csv(dd, 'clustResult3.csv')
  }
  clusterPrec(dd, k)
}

dd = wifiR2
 
dd[is.na(dd)] = 100
#
fit = hclust(as.dist(dd[, 2:ncol(dd)]), method = 'ward')
# 
cc = cutree(fit, 30)

plot(fit$height)

rect.hclust(fit, k = 100)
 
floorLabel = ww$floor

dd$class = cc

# dd = scale(dd[, 2:ncol(dd)])

# calculate distance

for (i in 1:618) {
  dd[i, i+1] = 0
}

ddg = aggregate(dd[, 2:(ncol(dd) - 1)], by = list(dd$class), mean)

a = dist(ddg[, 2:ncol(ddg)])

write.csv(as.matrix(a), 'ssDistance.csv')

# 
ss = data.frame(wifi_id = dd[, 1], class = cc, floor = floorLabel)

write.csv(ss, 'ss.csv')
# 
clusterPrec(ss, 30)
# 
write.csv(ss, 'ss.csv')

# using df, for some wifi merely ocurr as neigbors in a specific wifi, 
# these wifis may be taken as bad datas.

wifi_service_neighbor_group = group_by(ddd, wifi_id, wifi_neighbor_id) 

# df got 618 distinct wifis

wifi_service_neighbor = summarise(wifi_service_neighbor_group, rssi = mean(rssi),
                                  n = n())
# with many samples, just take the median

wifi_service_group = group_by(df, wifi_id_service)

wifi_service = summarise(wifi_service_group, n = n())

# after viewing, take 60% as the threshold, require wifi_service_neigbor is a dataFrame

wifi_service_neighbor = data.frame(wifi_service_neighbor)

outlierSift = function(wifi_service_neighbor, threshold = 0.6) {
  wifiCount = as.vector(table(wifi_service_neighbor[, 1]))
  wifiCountSum = c(0, cumsum(wifiCount))
  len = length(wifiCountSum) - 1
  df = data.frame()
  # browser()
  # colnames(df) = names(wifi_service_neighbor)
  for (i in 1:len) {
    maxCount = max(wifi_service_neighbor$n[(wifiCountSum[i] + 1):wifiCountSum[i + 1]])
    for (j in (wifiCountSum[i] + 1):wifiCountSum[i + 1]) {
      if (wifi_service_neighbor[j, 'n'] > threshold * maxCount) {
        df = rbind(df, wifi_service_neighbor[j, ])
      }
    }
  }
  return(df)
}

######

dfClean = outlierSift(wifi_service_neighbor)

dfWifi2Wifi = dcast(dfClean, wifi_id_service ~ wifi_id_neighbour, value.var = 'rssi')

dd = read.csv('floor.csv', header = T)

floorLabel = dd$floor

# if sift those ocurr freq too low wifis

l = c()

for (i in 1:618) {
  l = c(l, sum(!is.na(dfWifi2Wifi[i, ])))
}

wifiR = dfWifi2Wifi[which(l > 50), ]

floorLabel = floorLabel[which(l > 50)]

medianClust(wifiR, floorLabel, -200, 'euclidean', 'ward.D2', 13, T)

# define floor(using distance)

rr = read.csv('clustResult3.csv', header = T)

ddd = wifiR

ddd$class = rr$class

ddd[is.na(ddd)] = -200

aa = aggregate(ddd[, 2:1186], by = list(ddd$class), mean)

d = dist(aa[, 2:1186])

m = as.matrix(d)

# function to give floor in order, m is a distance matrix, with 1-26 classes,
# startFloor is the first floor-class
# assume there are 26 floors

floorOrder = function(m, startFloor) {
  l = startFloor
  range = 1:30
  minusRange = c()
  currFloor = startFloor
  for (i in range) {
    minusRange = c(minusRange, currFloor)
    minDist = min(m[range[-minusRange], currFloor])
    row = which(m[, currFloor] == minDist)
    l = c(l, row)
    currFloor = row
  }
  return(l)
}

for (i in 1:618) {
  l = c(l, sum(!is.na(wifiRelat[i, ])))
}

wifiR = wifiRelat[which(l > 50 & l < 150), ]

floorLabel = ww$floor

# calculate minDistance

ddd = ddd[order(ddd$class), ]

minDist(ddd)

# this minDist is for wifi2wifi

minDist = function(tt) {
  m = as.matrix(dist(tt[, 1:(ncol(tt) - 1)]))
  ta = as.vector(table(tt$class))
  cum = c(0, cumsum(ta))
  for (i in 1:(length(ta) - 1)) {
    # print('/n')# control which cluster
    col = (cum[i] + 1):(cum[i + 1])
    for (j in (i + 1):length(ta)) { # control the cluster to compare
      row = (cum[j] + 1):(cum[j + 1])
      print(min(m[row, col]))
    }
  }
}

##################################
# 
# mm = wifiR
# 
# mm[is.na(mm)] = -200
# 
# db = dbscan(mm[, -1], eps = 8, MinPts = 3)
# 
# dbClust = function(ds, floorLabel, fillNA, eps, MinPts) {
#   ds[is.na(ds)] = fillNA
#   db = dbscan(ds[, -1], eps = eps, MinPts = MinPts)
#   classes = max(db$cluster)
#   df = data.frame(wifi_id = ds[, 1], class = db$cluster, floor = floorLabel)
#   df = subset(df, df$class == 0)
#   clusterPrec(df, classes)
# }

# simiar to hclust

# continue to minDist, make some change

minDist = function(tt) {
  m = as.matrix(dist(tt[, 2:(ncol(tt) - 1)]))
  ta = as.vector(table(tt$class))
  cum = c(0, cumsum(ta))
  for (i in 1:(length(ta) - 1)) {
    # print('/n')# control which cluster
    col = (cum[i] + 1):(cum[i + 1])
    for (j in (i + 1):length(ta)) { # control the cluster to compare
      row = (cum[j] + 1):(cum[j + 1])
      print(min(m[row, col]))
    }
  }
}

l = minDist(ddd)

mm = matrix(nrow = 25, ncol = 25)

for (i in 1:25) {
  
}

xx = dfWifi2Wifi

xx[is.na(xx)] = -300

dbscan(xx[, -1], eps = 20, MinPts = 2)

errorWifi = c('wifi_393',
              'wifi_485',
              'wifi_513',
              'wifi_507',
              'wifi_510',
              'wifi_522',
              'wifi_544',
              'wifi_545',
              'wifi_549',
              'wifi_553',
              'wifi_573',
              'wifi_587',
              'wifi_594',
              'wifi_627',
              'wifi_645',
              'wifi_1003',
              'wifi_1062'
)

errorWifi2 = c('wifi_906',
               'wifi_1030'
)

ee = subset(dfWifi2Wifi, dfWifi2Wifi$wifi_id_service %in% errorWifi2)

ee [is.na(ee)] = -200

# distance between 1-5, and wifi_393 -- wifi_485
# floor 5-6, wifi_485 -- wifi_507

dist(ee[3, -1], ee[4, -1])

sum(abs(ee[3, -1] - ee[4, -1]))

sum(abs(ee[5, -1] - ee[4, -1]))

sum(abs(ee[1, -1] - ee[2, -1]))

# divide into 3 classes

dafr = dfWifi2Wifi

medianClust(dfWifi2Wifi, floorLabel, -200, 'euclidean', 'ward.D2', 3, T)

tem = function(df, k, floorLabel) {
  fit = hclust(as.dist(df[, 2:(ncol(df) - 1)]), method = 'ward.D2') # !! ncol(df) - 1
  cc = cutree(fit, k = k)
  dd = data.frame(wifi_id = df[, 1], class = cc, floor = floorLabel)
  # write.csv(dd, '4_25Clust.csv')
  clusterPrec(dd, k)
}

# trying k-means

kmd = dfWifi2Wifi

kmd[is.na(kmd)] = -200

km = kmeans(x = kmd[, -1], centers = 30)

clusterPrec(data.frame(wifi_id = kmd[, 1], class = km$cluster, floor = floorLabel), k = 30)

centerDist = km$centers

write.csv(as.matrix(dist(centerDist)), 'kmeansDist.csv')

# # #

ds = sqlQuery(channel, "select * from [HAERBIN].[dbo].[tb_wifi_zy9]") # save ds

# ds2 = sqlQuery(channel, "select * from [HAERBIN].[dbo].[tb_wifi_zy9]")

dsWifi2Wifi = dcast(ds, wifi_id1 ~ wifi_id2, value.var = 'rssi3')

dd = dsWifi2Wifi

# dd[is.na(dd)] = 4000
# 
# fit = hclust(as.dist(dd[, 2:ncol(dd)]), method = 'ward.D')

ccd = function(ds, floorLabel, fillNA, method = 'ward.D2', k, write = F) {
  ds[is.na(ds)] = fillNA
  fit = hclust(as.dist(ds[, 2:ncol(ds)]), method = method)
  cc = cutree(fit, k = k)
  dd = data.frame(wifi_id = ds[, 1], class = cc, floor = floorLabel)
  if (write) {
    write.csv(dd, 'clustResult5.csv')
  }
  clusterPrec(dd, k)
}

dd[is.na(dd)] = 4000

d1 = as.dist(dd[, 2:ncol(dd)])

fit = hclust(d1, method = 'ward')

d2 = cophenetic(fit)

cor(d1, d2)

ccd(dd, floorLabel, 5000, method = 'single', k = 30, write = T)

for (i in 1:618) {
  dd[i, i + 1] = 0
}

write.csv(dd, 'finalDistance.csv')


# 
# d_USArrests <- dist(USArrests)
# hc <- hclust(d_USArrests, "ave")
# 
# plot(hc$height)
# plot(cophenetic(hc) ~ d_USArrests)

# m is a distance-matrix, last column is class and is ordered by class

floorOrder = function(m, firstFloorLabel) {
  classLabels = m[, ncol(m)]
  m[is.na(m)] = 4000
  # browser()
  currLabel = firstFloorLabel
  ord = currLabel
  minusIndex = c()
  for (i in 1:(length(unique(classLabels)) - 1)) {
    rowIndex = which(m[, ncol(m)] == currLabel)
    minusIndex = c(minusIndex, rowIndex)
    map = list()
    flag = TRUE # judge if all the distance is over 4000
    for (j in rowIndex) {
      minValue = min(m[-minusIndex, j + 1])
      if (minValue == 4000) {
        next()
      } #
      flag = FALSE # change here 
      minRowIndex = which.min(m[-minusIndex, j + 1])
      label = m[-minusIndex, ncol(m)][minRowIndex]
      ###############################
      if (label %in% map$keys) { 
        index = which(map$keys == label)
        map$values[index] = map$values[index] + 1
      } else {
        map$keys = c(map$keys, label)
        map$values = c(map$values, 1)
      }
      ##########################
    } # add here
    # if (flag) {
    #   currLabel = ord[length(ord) - 1]
    #   rowIndex = which(m[, ncol(m)] == currLabel)
    #   map = list()
    #   for (j in rowIndex) {
    #     minValue = min(m[-minusIndex, j + 1])
    #     if (minValue == 4000) {
    #       next()
    #     }
    #     minRowIndex = which.min(m[-minusIndex, j + 1])
    #     label = m[-minusIndex, ncol(m)][minRowIndex]
    #     ###############################
    #     if (label %in% map$keys) { 
    #       index = which(map$keys == label)
    #       map$values[index] = map$values[index] + 1
    #     } else {
    #       map$keys = c(map$keys, label)
    #       map$values = c(map$values, 1)
    #     }
    #     ##########################
    #   }
    # } # add end here
    maxValueInd = which.max(map$values)
    currLabel = map$keys[maxValueInd]
    ord = c(ord, currLabel)
  }
  return(ord)
} # too complicated

aa = read.csv('clustResult5.csv', header = T)

mm = dd

mm$class = aa$class

floorOrder = function(m, firstFloorLabel) {
  classLabels = m[, ncol(m)]
  m[is.na(m)] = 4000
  # browser()
  currLabel = firstFloorLabel
  ord = currLabel
  minusIndex = c()
  for (i in 1:(length(unique(classLabels)) - 1)) {
    rowIndex = which(m[, ncol(m)] == currLabel)
    minusIndex = c(minusIndex, rowIndex)
    map = vote(m, rowIndex, minusIndex)
    cycleCount = 1
    while (length(map) == 0) {
      if (cycleCount == 1) {
        print('Inside cycle, starting from: ')
        print(ord[length(ord)])
      }
      currLabel = ord[length(ord) - cycleCount]
      rowIndex = which(m[, ncol(m)] == currLabel)
      map = vote(m, rowIndex, minusIndex)
      cycleCount = cycleCount + 1
      print(currLabel)
    }
    maxValueInd = which.max(map$values)
    currLabel = map$keys[maxValueInd]
    ord = c(ord, currLabel)
  }
  return(ord)
}


vote = function(m, rowIndex, minusIndex) {
  map = list()
  for (j in rowIndex) {
    minValue = min(m[-minusIndex, j])
    if (minValue == 4000) {
      next()
    } 
    minRowIndex = which.min(m[-minusIndex, j])
    label = m[-minusIndex, ncol(m)][minRowIndex]
    ###############################
    if (label %in% map$keys) { 
      index = which(map$keys == label)
      map$values[index] = map$values[index] + 1
    } else {
      map$keys = c(map$keys, label)
      map$values = c(map$values, 1)
    } 
  }
  return(map)
}

# calculate overlap, vectors with NAs
###############################################
# 
# similarity = function(v1, v2) {
#   if (length(v1) != length(v2)) {
#     return(-1)
#   } else {
#     bothHave = !is.na(v1) & !is.na(v2)
#     lapNo = sum(bothHave)
#     if (sum(lapNo) == 0) {
#       return(0)
#     }
#     detaRssi = abs(v1[bothHave] - v2[bothHave])
#     Rssi = mean(detaRssi)
#     if (Rssi == 0) {
#       Rssi = 1
#     }
#     atLeastOneHas = !is.na(v1) | !is.na(v2)
#     fullLength = sum(atLeastOneHas)
#   }
#   # return result * 100
#   return(100 * (lapNo / (fullLength * Rssi)))
# }
# 
# ############################################
# 
# # use similarity as distance matrix
# # 1. wifi2wifi get the neighbor over 60% boundary
# # 2. wifi2wifi fill with the max value correspondent sample
# # 3. calculate similarity, and form dist Matrix
# 
# wifi2wifi = sqlQuery(channel, "select * from [HAERBIN].[dbo].[tb_wifi_zy7]")
# 
# wifi2wifi = wifi2wifi[!duplicated(wifi2wifi[, 1:2]), ]
# 
# wifiR3 = dcast(wifi2wifi, wifi_id ~ wifi_neighbor, value.var = 'rssi')
# 
# # wifiR3 is wifi2wifiRelat matrix, with the first col is wifiIDs
# 
# wifiIDs = wifiR3[, 1]
# 
# floorLabel = floorLabel
# 
# similarityMat = function(wifiR3) {
#   len = nrow(wifiR3)
#   m = matrix(nrow = len, ncol = len)
#   for (i in 1:len) {
#     for (j in i:len) {
#       m[i, j] = similarity(wifiR3[i, -1], wifiR3[j, -1])
#     }
#   }
#   return(t(m))
# }
# 
# simiMat = similarityMat(wifiR3)
# 
# similarity(wifiR3[1, -1], wifiR3[2, -1])
# 
# # system.time(similarity(wifiR3[1, -1], wifiR3[2, -1]))
# 
# #######################################################
# 
# wifi2wifi2 = sqlQuery(channel, "select * from [HAERBIN].[dbo].[tb_wifi_zy6]
#                       order by wifi_id_service, wifi_id_neighbour")
# 
# 
# simi = function(wifi2wifi2) {
#   count = as.vector(table(wifi2wifi2[, 1]))
#   count = c(0, cumsum(count))
#   len = length(count) - 1
#   m = matrix(nrow = len, ncol = len)
#   # browser()
#   for (i in 1:len) {
#     currVec = wifi2wifi2[(count[i] + 1):count[i + 1], 2]
#     currVecLen = length(currVec)
#     for (j in (i + 1):len) {
#       compVec = wifi2wifi2[(count[j] + 1):count[j + 1], 2]
#       compVecLen = length(compVec)
#       p = q = 1 # p stands currVec, q stands compVec
#       storeRssi = c()
#       lapCount = 0
#       while (TRUE) {
#         if (p > currVecLen | q > compVecLen) {
#           break()
#         }
#         if (currVec[p] < compVec[q]){
#           p = p + 1
#         } else if (currVec[p] > compVec[q]) {
#           q = q + 1
#         } else {
#           storeRssi = c(storeRssi, wifi2wifi2[count[i] + p, 3] - wifi2wifi2[count[j] + q, 3])
#           lapCount = lapCount + 1
#         }
#       }
#       fullLength = currVecLen + compVecLen - lapCount
#       meanRssi = mean(storeRssi)
#       m[i, j] = 100 * (lapCount / (meanRssi * fullLength))
#     }
#   }
#   return(t(m))
# }
# 
# wifiIDs = uniqTsparse(wifi2wifi2[, 1])
# 
# wifi2wifi2 = sqlQuery(channel, "SELECT [wifi_id1]
#                       ,[wifi_id2]
#                       ,[distance]
#                       FROM [HAERBIN].[dbo].[tb_wifi_zy8]")
# 
# wifi2wifi = dcast(wifi2wifi2, wifi_id1 ~ wifi_id2, value.var = 'distance')
# 
# ccd(wifi2wifi, floorLabel, 20, k = 30)

# last column of m is class

corrDist = function(m) {
  n = length(unique(m$class))
  map = list()
  for (i in 1:n) {
    currLabel = i
    rowIndex = which(m[, ncol(m)] == currLabel)
    firstCycle = TRUE
    map[[i]] = list()
    for (j in rowIndex) {
      minValue = min(m[-rowIndex, j + 1])
      if (minValue == 4000) {
        next()
      } 
      #browser()
      minRowIndex = which.min(m[-rowIndex, j + 1])
      label = m[-rowIndex, ncol(m)][minRowIndex]
      ###############################
      if (firstCycle) {
        map[[i]]$label = label
        map[[i]]$distance = minValue
        firstCycle = FALSE
      } else if (!(label %in% map[[i]]$label)) { 
        map[[i]]$label = c(map[[i]]$label, label)
        map[[i]]$distance = c(map[[i]]$distance, minValue)
      } else {
        labelIndex = which(map[[i]]$label == label)
        if (minValue < map[[i]]$distance[labelIndex]) {
          map[[i]]$distance[labelIndex] = minValue
        }
      }
    }
  }
  return(map)
}

# steps testing

dataSet = sqlQuery(channel, "select * from [HAERBIN].[dbo].[tb_wifi_21]")

dataSet = time2samp(dataSet)

dataSet = mac2wifi(dataSet)


###########################################################################
########################New DataSet########################################
###########################################################################

ds = sqlQuery(channel, "select * from [HAERBIN].[dbo].[tb_wifi_195708_2]")

ggplot(data = ds, aes(x = sample_id, y = rssi)) + 
  geom_point(aes(color = sample_id))

ds = ds[!duplicated(ds[, 1:2]), ]
# Select major wifi by threshold(default is greater than -65), the original
# data is reshaped into a major wifi, which is greater than -65, to its neigbor wifi
# And it is supposed to order by wifi_id
df = wifi2wifiUp(ds)
# get each wifi to its neigbor's count and mean rssi
# at the third step wifi_service_neighbor should be turn into data frame first
wifi_service_neighbor_group = group_by(df, wifi_id, wifi_neighbor_id) 
wifi_service_neighbor = summarise(wifi_service_neighbor_group, rssi = mean(rssi),
                                  n = n())
wifi_service_neighbor = data.frame(wifi_service_neighbor)
# if one's neighbor wifi occurs less
# than 60% percent, then it's dropped.(samples require ordered)
wifi_service_neighbor[, 1] = factor(wifi_service_neighbor[, 1])
dfClean = outlierSift(wifi_service_neighbor) # too many rows are sifted out, with 20000 rows
# dropped to 8592 rows(since samples count per wifi_id is too small )
# Calculate distance matrix. 
# Formula is: min(overlappedMean + 120) * min(overlappedCount) / sum(count * meanRssi), 
# and its reciprocal (firstly dfClean's wifi should be refactored, since there are less
# wifis than before) # 700 distinct wifis
dfClean[, 1] = factor(dfClean[, 1])
# then we have the distance matrix, with row and colnames are wifiIDs
distMat = wifiDist(dfClean)
# dddi = sqlQuery(channel, "select * from [HAERBIN].[dbo].[tb_wifi_zy5]")
# use cluster function to clust, and the default method is ward.D2
# classNum is the setted floors of building
fit = hclust(as.dist(distMat), method = 'ward.D2')
cc = cutree(fit, k = 6)
# add class labels to the last column of the distance matrix
distMat$class = as.vector(cc)
# store wifi_id to class labels
wifiID_class = data.frame(wifi_id = rownames(distMat), classLabel = distMat[, ncol(distMat)])
# then we get the floor order using floorOrder function, requiring the
# first floor is fixed and return will be a list, which includes the floor
# and corresponding classes
floor_classes = floorOrder(distMat, firstFloorLabel = firstFloorLabel)
# the last step we trace back by using floor to classLabel to wifi_id and to wifi_name
# finally return a floor to wifi_name result
classLabel_floor = floorClass2df(floor_classes)
# by using merge, we get a classLabel to wifi_id to floor data frame
class_wifi_id_floor = merge(wifiID_class, classLabel_floor, by = 'classLabel')
# back to the top, using wifiName_mac_wifiID to find the corresponding wifi_name
res = merge(class_wifi_id_floor, wifiName_mac_wifiID, by = 'wifi_id')

wifi_gps = sqlQuery(channel, 'SELECT distinct [wifi_id] FROM [HAERBIN].[dbo].[tb_wifi_149221_gps]')

dd = data.frame(cc)

colnames(dd) = c('classLabel')

# find which classLabel stands for the first floor. enter is a dataframe with wifi_id to classLabel
# two columns and wifi_id to gps(the first column is wifi_id), which means the first floor

findFirstFloor = function(df, wifi_gps) {
  # browser()
  class = as.vector(unique(df[, 2]))
  wifiIDs = df[, 1]
  l = list()
  for (i in class) {
    classIndex = which(df[, 2] == i)
    wifis = wifiIDs[classIndex]
    firstFloorCount = length(intersect(wifis, wifi_gps[, 1]))
    l[[i]] = firstFloorCount
  }
  return(l)
}

ds = sqlQuery(channel, "select * from [HAERBIN].[dbo].[tb_wifi_bak_distance]")

ds = sqlQuery(channel, "select * from [HAERBIN].[dbo].[tb_wifi_149222_distance]")

library(reshape2)

cclust = function(ds, classNo) {
  distMat = dcast(ds, wifi_id1 ~ wifi_id2, value.var = 'distance')
  distMat = data.frame(distMat[, 2:ncol(distMat)], row.names = distMat[, 1])
  distMat[is.na(distMat)] = 4000
  fit = hclust(as.dist(distMat), method = 'ward.D2')
  cc = cutree(fit, k = 50) # classNo
  distMat$class = as.vector(cc)
  
  wifiID_class_1 = data.frame(wifi_id = rownames(distMat), classLabel = distMat[, ncol(distMat)])
  write.csv(wifiID_class_1, 'wifiID_class_1.csv')
  # require a distMatrix, with n(rows) * n(cols), without wifi_id column
  # floor_classes = floorOrder(distMat, firstFloorLabel = 25)
  
  # now use tsp method to find the shortest path, and no need to give
  # the first floor label
  tour = tspFloorOrder(distMat, start = classNo + 1)

}

# use tsp to find the shortest path, requiring distMat be labeled

tspFloorOrder = function(distMat, start) {
  tsp = classMeanDist(distMat)
  tsp = (tsp/1000)^4
  tsp = cbind(tsp, 0)
  tsp = rbind(tsp, 0)
  # add extra row
  for (i in 1:nrow(tsp)) {
    for (j in 1:ncol(tsp)) {
      if (tsp[i, j] == 256) {
        tsp[i, j] = 100000
      }
    }
  }
  # drop those class which has no relation to others
  labels = 1:40
  delIndex = c()
  for (j in 1:nrow(tsp)) {
    if (sum(tsp[j, ]) == 49 * 100000) { # 30 = start - 1
      delIndex = c(delIndex, j)
    }
  }
  tsp = tsp[-delIndex, -delIndex]
  labels = labels[-delIndex]
  etsp = TSP(tsp)
  
  while (TRUE) {
    tour = solve_TSP(etsp, method = "nearest_insertion", start = nrow(tsp), 
                     labels = labels) # nrow(tsp)
    if (tour_length(tour) < 100000) break
  }
  
  return(tour)
}

# add one more row to as.dist(distMat), whose distance to all the other
# class is 0(shortest length)
# 
# tsp = classMeanDist(distMat)
# 
# tsp = (tsp/1000)^4
# 
# # add one row and column which is all zero, so the added class is 27(26classes + 1)
# 
# tsp = cbind(tsp, 0)
# 
# tsp = rbind(tsp, 0)
# 
# for (i in 1:nrow(tsp)) {
#   for (j in 1:ncol(tsp)) {
#     if (tsp[i, j] == 256) {
#       tsp[i, j] = 100000
#     }
#   }
# }
# 
# ##############################
# 
# tsp = cbind(tsp, c(rep(10000, 24), 0, 10000))
# 
# tsp = rbind(tsp, c(rep(10000, 24), 0, 10000, 0))
# 
# etsp = TSP(tsp)
# 
# tour = solve_TSP(etsp, method = "nearest_insertion", start = 27L)
# 
# iter = 0
# 
# while (tour_length(tour) > 100000) {
#   tour = solve_TSP(etsp, method = "nearest_insertion", start = 29L)
# }

# new problem. Afer classification, some classes contain wifis only have relationships
# with inner wifi, which means its distance to other classes is the maximum
# so if one class is not related to others, this class will be dropped

# use floorOrder function, but to drop those no relation classes first, which
# should be stored in delIndex

orderMatrix = function(distMat, delIndex) {
  # browser()
  if (is.null(delIndex)) return(distMat)
  dropIndex = c()
  for (i in delIndex) {
    del = which(distMat[, ncol(distMat)] == i)
    dropIndex = c(dropIndex, del)
  }
  return(distMat[-dropIndex, -dropIndex])
}

# delIndex = c()
# for (j in 1:nrow(tsp)) {
#   if (sum(tsp[j, ]) == 39 * 4000) { # 30 = start - 1
#     delIndex = c(delIndex, j)
#   }
# }


#####################################################
ds = sqlQuery(channel, "select * from [HAERBIN].[dbo].[tb_wifi_nx_149224_distance2]")

distMat = dcast(ds, wifi_id1 ~ wifi_id2, value.var = 'distance')
distMat = data.frame(distMat[, 2:ncol(distMat)], row.names = distMat[, 1])
distMat[is.na(distMat)] = 4000
fit = hclust(as.dist(distMat), method = 'ward.D2')
cc = cutree(fit, k = 30) # classNo
distMat$class = as.vector(cc)

tsp = classMeanDist(distMat)
tsp = (tsp/1000)^4

for (i in 1:nrow(tsp)) {
  for (j in 1:ncol(tsp)) {
    if (tsp[i, j] == 256) {
      tsp[i, j] = 100000
    }
  }
}

delIndex = c()
for (j in 1:nrow(tsp)) {
  if (sum(tsp[j, ]) == 29 * 100000) { # 30 = start - 1
    delIndex = c(delIndex, j)
  }
}

dd = orderMatrix(distMat, delIndex)

wifi_gps = sqlQuery(channel, 'SELECT * FROM 
                    [HAERBIN].[dbo].[tb_wifi_149224_gps_1]')

wifi_id_class = data.frame(wifi_id = rownames(dd), class = dd$class)

findFirstFloor(wifi_id_class, wifi_gps)

floorList = floorOrder(dd, 8)

result = class2floor(wifi_id_class, floorList)

write.csv(result, '149224upGrade4.csv')

###################################################
# classification problem
# use wifiRelat

wifi_floor = read.csv('wifiID_floor.csv', header = T)

wifiR = merge(wifiRelat, wifi_floor[, c(1, 3)], by = 'wifi_id')

# take floor 1 and 2

wifiR1 = subset(wifiR, wifiR$floor %in% c(1, 2))

wifiR2 = subset(wifiR, wifiR$floor %in% c(3, 4))

simi = function(wifiR) {
  row = nrow(wifiR)
  col = ncol(wifiR)
  
  intersection = c()
  completeLength = c()
  meanRssi = c()
  label = c()
  for (i in 1:(row - 1)) {
    currV = wifiR[i, 2:col]
    for (j in (i + 1):row) {
      compV = wifiR[j, 2:col]
      classLabel = ifelse(currV$floor == compV$floor, 1, 0)
      
      overlap = 0
      fullLength = 0
      detaRssi = c()
    
      for (k in 1:(ncol(currV) - 1)) {
        # if (k == col - 1) browser()
        if (!is.na(currV[k]) & !is.na(compV[k])) {
          overlap = overlap + 1
          detaRssi = c(detaRssi, abs(currV[k] - compV[k]))
          rssi = mean(unlist(detaRssi))
        } else if (!is.na(currV[k]) | !is.na(compV[k])) {
          fullLength = fullLength + 1
        }
      }
      # browser()
      intersection = c(intersection, overlap)
      completeLength = c(completeLength, fullLength)
      meanRssi = c(meanRssi, rssi)
      label = c(label, classLabel)
    }
  }
  return(data.frame(intersection = intersection, completeLength = completeLength, meanRssi = 
                      meanRssi, label = label))
}

trainSet = simi(wifiR1)

testSet = simi(wifiR2)

trainSet$label = factor(trainSet$label)

testSet$label = factor(testSet$label)

# use svm to predict

library(e1071)

sv = svm(label ~ ., trainSet, gamma = 0.001, cost = 100)

s.preds = predict(sv, testSet)

#######

errorCount = 0

for (i in 1:nrow(testSet)) {
  if (testSet$label[i] != s.preds[i]) {
    errorCount = errorCount + 1
  }
}

errorCount / nrow(testSet)

#########################

errorCount = 0

for (i in 1:nrow(testSet)) {
  if (testSet$label[i] != s.preds[i] & s.preds[i] == 1) {
    errorCount = errorCount + 1
  }
}

errorCount / sum(s.preds == 1)

# inspect problems, cluster disorder

ii = sapply(c('wifi_140',
              'wifi_484',
              'wifi_486',
              'wifi_570',
              'wifi_607',
              'wifi_134',
              'wifi_135',
              'wifi_194',
              'wifi_195',
              'wifi_250',
              'wifi_264',
              'wifi_361',
              'wifi_362'
), function(x) which(colnames(distMat) == x))

aa = distMat[ii, ii]

write.csv(aa, 'a.csv')

# an wifi should not correlate with more than 5 classes

# function to sort those wifis, require distMat ordered by class

distMat = distMat[order(distMat$class, decreasing = F), ]

delWifi = function(distMat) {
  wifiCount = as.vector(table(distMat$class))
  cumWifiCount = c(0, cumsum(wifiCount))
  
  for (i in 1:length(wifiCount)) {
    for (j in (cumWifiCount[i] + 1):cumWifiCount[i + 1]) {
      # browser()
      ind = which(distMat[, j] < 4000)
      count = 0
      for (k in ind) {
        currClass = 0
        for (kk in 1:length(wifiCount)) {
          if (k < cumWifiCount[kk + 1] & cumWifiCount[kk + 1] != currClass) {
            count = count + 1
            currClass = cumWifiCount[kk + 1]
            break
            if (count > 5) {
              print(j)
              break
            }
          }
        }
      }
    }
  }
}


