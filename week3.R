library(readr)        # to import tabular data (e.g. csv)
library(dplyr)        # to manipulate (tabular) data
library(ggplot2)      # to visualize data
library(sf)           # to handle spatial vector data
library(terra)        # To handle raster data
library(lubridate)    # To handle dates and times
library(SimilarityMeasures)



#____________________________________________________________

caro <- read_delim("caro60.txt",",")
caro


#Step a)
#Specify a temporal window  

#Step b)
#Measure the distance from every point to every other point within this temporal window  

caro <- caro %>%
  mutate(
    nMinus2 = sqrt((lag(E,2)-E)^2+(lag(N,2)-N)^2),   # distance to pos -30 minutes
    nMinus1 = sqrt((lag(E,1)-E)^2+(lag(N,1)-N)^2),   # distance to pos -15 minutes
    nPlus1  = sqrt((E-lead(E,1))^2+(N-lead(N,1))^2), # distance to pos +15 mintues
    nPlus2  = sqrt((E-lead(E,2))^2+(N-lead(N,2))^2)  # distance to pos +30 minutes
  )

caro <- caro %>%
  rowwise() %>%
  mutate(
    stepMean = mean(c(nMinus2, nMinus1,nPlus1,nPlus2))
  ) %>%
  ungroup() 

caro

#Step c)
# Remove “static points”
caro <- caro %>% 
  ungroup() %>%
  mutate(static = stepMean < mean(stepMean, na.rm = TRUE))


caro_filter <- caro %>%
  filter(!static)

caro_filter%>%
  ggplot(aes(E, N))  +
  geom_path() +
  geom_point() +
  coord_fixed() +
  theme(legend.position = "bottom")


#___________________________________________________
#Task 1: Segmentation

caro <- caro %>%
  mutate(
    nMinus3 = sqrt((lag(E,3)-E)^2+(lag(N,3)-N)^2),   # distance to pos -3 minutes
    nMinus2 = sqrt((lag(E,2)-E)^2+(lag(N,2)-N)^2),   # distance to pos -2 minutes
    nMinus1 = sqrt((lag(E,1)-E)^2+(lag(N,1)-N)^2),   # distance to pos -1 minutes
    nPlus1  = sqrt((E-lead(E,1))^2+(N-lead(N,1))^2), # distance to pos +1 mintues
    nPlus2  = sqrt((E-lead(E,2))^2+(N-lead(N,2))^2),  # distance to pos +2 minutes
    nPlus3  = sqrt((E-lead(E,3))^2+(N-lead(N,3))^2)  # distance to pos +3 minutes
  )


#Task 2: Specify and apply threshold d
caro <- caro %>%
  rowwise() %>%
  mutate(
    stepMean = mean(c(nMinus3, nMinus2, nMinus1,nPlus1,nPlus2, nPlus2))
  ) %>%
  ungroup() 

caro <- caro %>% 
  ungroup() %>%
  mutate(static = stepMean > mean(stepMean, na.rm = TRUE))
caro


#Task 3: Visualize segmented trajectories
caro%>%
  ggplot(aes(E, N))  +
  geom_path() +
  geom_point(aes(colour=static)) +
  scale_color_manual(values=c("red", "green"))+
  coord_fixed() +
  theme(legend.position = "bottom")


#Task 4: Segment-based analysis
#defining ID's for trajectories
rle_id <- function(vec){
  x <- rle(vec)$lengths
  as.factor(rep(seq_along(x), times=x))
}

caro60 <- caro %>%
  mutate(segment_id = rle_id(static))

caro60

caro60%>%
  ggplot(aes(E, N))  +
  geom_path() +
  geom_point(aes(colour=segment_id)) +
  coord_fixed() +
  theme(legend.position = "bottom")

caro60 <- group_by(caro60,segment_id)
caro60<- mutate(caro60,timelag = as.integer(difftime(tail(DatetimeUTC,1), head(DatetimeUTC,1), units = "secs")))

caro_filter <- caro60%>%
  filter(timelag >= "300")

caro_filter%>%
  ggplot(aes(E, N))  +
  geom_path() +
  geom_point(aes(colour=segment_id)) +
  coord_fixed() +
  theme(legend.position = "bottom")


#Task 5: Similarity measures
pedestrian <- read_delim("pedestrian.txt",",")
pedestrian

pedestrian%>%
  ggplot(aes(E, N))  +
  geom_path() +
  geom_point(aes(colour=TrajID)) +
  coord_fixed() +
  theme(legend.position = "bottom")+
  facet_wrap(pedestrian$TrajID, ncol = 3)


#Task 6: Calculate similarity 

#????????

pedestrian$DatetimeUTC <- as.POSIXct(as.character(pedestrian$DatetimeUTC), format = "%Y-%m-%d%H:%m:%s",tz = "UTC")
pedestrian$DatetimeUTC[1:10]
pedestrian <- read_delim("pedestrian.txt",",")

ped_1 <- pedestrian%>%filter(TrajID == "1")
ped_1 <- ped_1[,!grepl("Traj",names(ped_1))]
ped_1_matr <- data.matrix(ped_1, rownames.force = NA)

ped_2 <- pedestrian%>%filter(TrajID == "2")
ped_2 <- ped_2[,!grepl("Traj",names(ped_2))]
ped_2_matr <- data.matrix(ped_2, rownames.force = NA)

ped_3 <- pedestrian%>%filter(TrajID == "3")
ped_3 <- ped_3[,!grepl("Traj",names(ped_3))]
ped_3_matr <- data.matrix(ped_3, rownames.force = NA)

ped_4 <- pedestrian%>%filter(TrajID == "4")
ped_4 <- ped_4[,!grepl("Traj",names(ped_2))]
ped_4_matr <- data.matrix(ped_4, rownames.force = NA)

ped_5 <- pedestrian%>%filter(TrajID == "5")
ped_5 <- ped_5[,!grepl("Traj",names(ped_5))]
ped_5_matr <- data.matrix(ped_5, rownames.force = NA)

ped_6 <- pedestrian%>%filter(TrajID == "6")
ped_6 <- ped_6[,!grepl("Traj",names(ped_6))]
ped_6_matr <- data.matrix(ped_6, rownames.force = NA)

EditDist(ped_1_matr, ped_2_matr, pointDistance=2)
EditDist(ped_1_matr, ped_3_matr, pointDistance=2)
EditDist(ped_1_matr, ped_4_matr, pointDistance=2)
EditDist(ped_1_matr, ped_5_matr, pointDistance=2)
EditDist(ped_1_matr, ped_6_matr, pointDistance=2)


