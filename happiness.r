library(tidyverse)
index <- read.table("index.txt", header=T)
age <- read.table("age.txt", header=T)
AQI <- read.table("AQI.txt", header=T)
crime <- read.table("crime.txt", header=T)
depend <- read.table("dependency.txt", header=T)
eq <- read.table("earthquake.txt", header=T)
energy <- read.table("energy.txt", header=T)
edu <- read.table("education.txt", header=T)
cost <- read.table("expense.txt", header=T)
trash <- read.table("garbage.txt", header=T)
green <- read.table("green.txt", header=T)
house <- read.table("house_price.txt", header=T)
examine <- read.table("med_examine.txt", header=T)
med <- read.table("medical.txt", header=T)
popu <- read.table("population.txt", header=T)
popu_rate <- read.table("population_rate.txt", header=T)
recycle <- read.table("recycle.txt", header=T)
temp <- read.table("temperature.txt", header=T)
typhoon <- read.table("typhoon.txt", header=T)
job <- read.table("unemployment.txt", header=T)
elec <- read.table("use_ele.txt", header=T)
water <- read.table("usewater.txt", header=T)

#計算年度房貸負擔率
house_price <- mutate(house, hou_total=(s1+s2+s3+s4)/4) 
house_total <- select(house_price, city, hou_total)

#計算收支比例
payment <- mutate(cost, pay_per_income=(non_payment+payment)/income)
expense <- select(payment, city, pay_per_income)

#計算總電量
elec_city <- group_by(elec, city)
total_elec <- summarise(elec_city, sum_elec = sum(use_electricity, na.rm = TRUE))

#計算總電量中再生能源的比例
pre.data <- inner_join(energy, total_elec, by="city")
new_energy <- mutate(pre.data, energy_ratio=energy_use/sum_elec)
energy_rate <- select(new_energy, city, energy_ratio)

#製作新temp表格
new_temp <- select(temp, city, month, t_ave=temperature, rain=Precp, humid=RelHumidity)
temp_city <- group_by(new_temp, city)
total_dist_temp <- summarise(temp_city, dist_temp = sum(abs(t_ave-21), na.rm = TRUE))
total_rain <- summarise(temp_city, sum_rain = sum(rain, na.rm = TRUE))
total_dist_humid <- summarise(temp_city, dist_humid = sum(abs(humid-45), na.rm = TRUE))

data.1 <- inner_join(age, crime, by="city")
data.2 <- inner_join(eq, depend, by="city")
data.3 <- inner_join(expense, edu, by="city")
data.4 <- inner_join(trash, green, by="city")
data.5 <- inner_join(recycle, house_total, by="city")
data.6 <- inner_join(AQI, examine, by="city")
data.7 <- inner_join(typhoon, med, by="city")
data.8 <- inner_join(energy_rate, popu, by="city")
data.9 <- inner_join(water, job, by="city")
data.10 <- inner_join(total_dist_temp, total_dist_humid, by="city")
data.11 <- inner_join(popu_rate, total_rain, by="city")
data <- inner_join(data.1, data.2, by="city")
data <- inner_join(data, data.3, by="city")
data <- inner_join(data, data.4, by="city")
data <- inner_join(data, data.5, by="city")
data <- inner_join(data, data.6, by="city")
data <- inner_join(data, data.7, by="city")
data <- inner_join(data, data.8, by="city")
data <- inner_join(data, data.9, by="city")
data <- inner_join(data, data.10, by="city")
data <- inner_join(data, data.11, by="city")

#計算每人可用的水量
data <- mutate(data, water_got = (sum_rain*0.2*31.42-usewater_ave/1000)/popu)
data <- data[,-18:-21]
data <- data[,-22]

data <- inner_join(index, data, by="city")
write.table(data, file="D:/RStudio/Happiness/data.txt", row.names = FALSE)

#score = 標準化 (將每項最大值設在1，最小值設在0)
score <- numeric(length(data[,1]))
file <- data.frame(data$city)
for(i in 3:length(data[1,])){
  rank = abs(data[,i]-min(data[,i]))/(max(data[,i])-min(data[,i]))
  file <- cbind(file,rank)
}

colnames(file) <- c("city","age_score","crime_score","eq_score",
                    "depend_score","pay_score","edu_score",
                    "garbage_score","green_score","recycle_score","hp_score",
                    "AQI.over100_score","med_examine_score","typhoon_score",
                    "people_per_bed_score","people_per_doctor_score",
                    "energy_score","unemploy_score","temp_score",
                    "humid_score","popu_score","water_score")

#若數值越低越好(即每項最小值為1，最小值為0)，則score = 1-標準化
for(i in c(3:6,8,11:16,18:21)){
  file[i] = 1-file[,i]
}

#將people_per_bed_score和people_per_doctor_score換成醫療資源的分數
new_file <- mutate(file, med_resource_score = (people_per_bed_score+people_per_doctor_score)/2)
med_score <- new_file[,c(1,15,16)]
med_score <- inner_join(index, med_score, by="city")
new_file <- new_file[,-15:-16]

#總分
score_soc <- mutate(new_file, society=(age_score + crime_score + depend_score +
                                     pay_score + edu_score + hp_score +
                                     med_examine_score + popu_score +
                                     unemploy_score + med_resource_score))

score_env <- mutate(score_soc, environment=(eq_score + garbage_score + 
                                              green_score + recycle_score +
                                              AQI.over100_score + typhoon_score +
                                              energy_score + temp_score +
                                              humid_score + water_score))

score <- mutate(score_env, final= society + environment)
score <- inner_join(index, score, by="city")

write.table(score, file="D:/RStudio/Happiness/score.txt", row.names = FALSE)

#社會與環境因素分數
soc_list <- score[,c(1,2,3,4,6,7,8,12,14,17,20,22,23)]
env_list <- score[,c(1,2,5,9,10,11,13,15,16,18,19,21,24)]

write.table(soc_list, file="D:/RStudio/Happiness/soc_list.txt", row.names = FALSE)
write.table(env_list, file="D:/RStudio/Happiness/env_list.txt", row.names = FALSE)


##社會因素##

#平均壽命
ggplot(score, aes(x = reorder(index, age_score), y = age_score, alpha = age_score))+
  geom_bar(fill = "brown", color ="brown", stat = "identity")+
  xlab("city")+
  theme(axis.text.x = element_text(angle = 65, hjust = 1))

#醫療資源中病床數和醫療人員數的關係
ggplot(data, aes(x = reorder(index, people_per_bed), y = people_per_bed, size = people_per_doctor))+
  geom_point(col = "#ff9900")+
  xlab("city")+
  theme(axis.text.x = element_text(angle = 65, hjust = 1))

#醫療資源總分
bed <- select(med_score, city, index, people_per_bed_score)
bed_name <- numeric(19)
for (i in 1:19){
  bed_name[i] <- "bed"
}
bed <- cbind(bed,bed_name)
colnames(bed)[c(3,4)] <- c("new_score","type")

doc <- select(med_score, city, index, people_per_doctor_score)
doc_name <- numeric(19)
for (i in 1:19){
  doc_name[i] <- "doc"
}
doc <- cbind(doc,doc_name)
colnames(doc)[c(3,4)] <- c("new_score","type")

med_plot <- rbind(bed,doc)

ggplot(med_plot, aes(x = reorder(index, new_score/2), y = new_score/2, fill = type))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values=c("#bab1d7", "#f2937a")) +
  xlab("city")+
  ylab("medical_resource total")+
  theme(axis.text.x = element_text(angle = 65, hjust = 1))

#就診率
ggplot(score, aes(x = reorder(index, med_examine_score), y = med_examine_score, alpha = med_examine_score))+
  geom_bar(fill = "brown", color ="brown", stat = "identity")+
  xlab("city")+
  theme(axis.text.x = element_text(angle = 65, hjust = 1))

#平均壽命和就診率的關係
ggplot(score, aes(x = reorder(index, age_score), y = age_score, alpha = med_examine_score))+
  geom_point(color = "brown", stat = "identity", size = 4)+
  xlab("city")+
  theme(axis.text.x = element_text(angle = 65, hjust = 1))

#刑案發生率
ggplot(score, aes(x = reorder(index, crime_score), y = crime_score, alpha = crime_score))+
  geom_bar(fill = "brown", color ="brown", stat = "identity")+
  xlab("city")+
  theme(axis.text.x = element_text(angle = 65, hjust = 1))

#扶養比
ggplot(score, aes(x = reorder(index, depend_score), y = depend_score, alpha = depend_score))+
  geom_bar(fill = "brown", color ="brown", stat = "identity")+
  xlab("city")+
  theme(axis.text.x = element_text(angle = 65, hjust = 1))

#生活費用
ggplot(score, aes(x = reorder(index, pay_score), y = pay_score, alpha = pay_score))+
  geom_bar(fill = "brown", color ="brown", stat = "identity")+
  xlab("city")+
  theme(axis.text.x = element_text(angle = 65, hjust = 1))

#房貸負擔率
ggplot(score, aes(x = reorder(index, hp_score), y = hp_score, alpha = hp_score))+
  geom_bar(fill = "brown", color = "brown", stat = "identity")+
  xlab("city")+
  theme(axis.text.x = element_text(angle = 65, hjust = 1))

#人口密度
plot_popD <- select(data, city, population_density)
plot_popD <- inner_join(index, plot_popD, by="city")
ggplot(plot_popD, aes(x = reorder(index, population_density), y = population_density, alpha = population_density))+
  geom_bar(fill = "#B766AD",color = "#B766AD",stat = "identity")+
  xlab("city")+
  theme(axis.text.x = element_text(angle = 65, hjust=1))+
  ggtitle('population density')

ggplot(score, aes(x = reorder(index, popu_score), y = popu_score, alpha = popu_score))+
  geom_bar(fill = "#B766AD", color = "#B766AD", stat = "identity")+
  xlab("city")+
  theme(axis.text.x = element_text(angle=65, hjust = 1))+
  ggtitle('population score')

#教育程度
ggplot(score, aes(x = reorder(index, edu_score), y = edu_score, alpha = edu_score))+
  geom_bar(fill = "#B766AD", color = "#B766AD", stat = "identity")+
  xlab("city")+
  theme(axis.text.x = element_text(angle = 65, hjust = 1))+
  ggtitle('education score')

#失業率
ggplot(score, aes(x = reorder(index, unemploy_score), y = unemploy_score, alpha = unemploy_score))+
  geom_bar(fill = "#B766AD", color = "#B766AD", stat = "identity")+
  xlab("city")+
  theme(axis.text.x = element_text(angle = 65, hjust = 1))+
  ggtitle('unemployment score')

#失業率跟教育程度-->無關
unemploy <- select(score, city, index, unemploy_score)
unemploy_name <- numeric(19)
for (i in 1:19){
  unemploy_name[i] <- "unemploy"
}
unemploy <- cbind(unemploy, unemploy_name)
colnames(unemploy)[c(3,4)] <- c("score","type")

education <- select(score, city, index, edu_score)

edu_name <- numeric(19)
for (i in 1:19){
  edu_name[i] <- "education"
}
education <- cbind(education,edu_name)
colnames(education)[c(3,4)] <- c("score","type")

plot_unem.edu <- rbind(unemploy, education)

ggplot(plot_unem.edu, aes(x = reorder(city, score), y = score, group = type, color = type))+
  geom_line(linetype = "solid",size = 1)+
  geom_point(fill = "white", size = 1.5)+
  xlab("city")+
  theme(axis.text.x = element_text(angle = 65, hjust = 1))


##環境因素##

#綠地面積
ggplot(score, aes(x = reorder(index, green_score), y = green_score, alpha = green_score))+
  geom_bar(fill = "#B766AD", color = "#B766AD", stat = "identity")+
  xlab("city")+
  theme(axis.text.x = element_text(angle = 65, hjust = 1))+
  ggtitle('green ground score')

#再生能源使用率
ggplot(score, aes(x = reorder(index, energy_score), y = energy_score, alpha = energy_score))+
  geom_bar(fill = "#B766AD", color = "#B766AD", stat = "identity")+
  xlab("city")+
  theme(axis.text.x = element_text(angle = 65, hjust = 1))+
  ggtitle('energy score')

#資源回收率
ggplot(score, aes(x = reorder(index, recycle_score),y = recycle_score, alpha = recycle_score))+
  geom_bar(fill = "#B766AD", color = "#B766AD", stat = "identity")+
  xlab("city")+
  theme(axis.text.x = element_text(angle = 65, hjust = 1))+
  ggtitle('recycle score')

#環保程度
green <- select(score, city, index, green_score)
green_name <- numeric(19)
for (i in 1:19){
  green_name[i] <- "green"
}
green <- cbind(green, green_name)
colnames(green)[c(3,4)] <- c("eco_score","type")

recycle <- select(score, city, index, recycle_score)
recycle_name <- numeric(19)
for (i in 1:19){
  recycle_name[i] <- "recycle"
}
recycle <- cbind(recycle, recycle_name)
colnames(recycle)[c(3,4)] <- c("eco_score","type")

energy <- select(score, city, index, energy_score)
energy_name <- numeric(19)
for (i in 1:19){
  energy_name[i] <- "energy"
}
energy <- cbind(energy, energy_name)
colnames(energy)[c(3,4)] <- c("eco_score","type")

plot_eco <- rbind(green, recycle)
plot_eco <- rbind(plot_eco, energy)

ggplot(plot_eco, aes(x = reorder(index, eco_score), y = eco_score, fill = type))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values = c("#FF99CC","#FFCC99","#CCCCFF")) +
  xlab("city")+
  theme(axis.text.x = element_text(angle = 65, hjust = 1))+
  ggtitle('eco-friendly score')

#水資源
ggplot(score, aes(x = reorder(index, water_score), y = water_score, alpha = water_score))+
  geom_bar(fill = "#B766AD", color = "#B766AD", stat = "identity")+
  xlab("city")+
  theme(axis.text.x = element_text(angle = 65, hjust = 1))+
  ggtitle('water score')

#AQI(空氣品質指標) score
ggplot(data = score, aes(x = reorder(index, AQI.over100_score), y = AQI.over100_score, alpha = AQI.over100_score)) +
  geom_bar(stat = "identity", fill = "brown", color = "brown") +
  xlab("city") +
  theme(axis.text.x = element_text(angle = 65, hjust = 1))

#AQI(空氣品質指標) data
ggplot(data = data, aes(x = reorder(index, AQI.over100rate.), y = AQI.over100rate., alpha = AQI.over100rate.)) +
  geom_bar(stat = "identity", fill = "red", color = "red") +
  xlab("city") +
  theme(axis.text.x = element_text(angle = 65, hjust = 1))

#地震次數 score
ggplot(data = score, aes(x = reorder(index, eq_score), y = eq_score, alpha = eq_score)) +
  geom_bar(stat = "identity", fill = "brown", color = "brown") +
  xlab("city") +
  theme(axis.text.x = element_text(angle = 65, hjust = 1))

#地震次數 data
ggplot(data = data, aes(x = reorder(index, earthquake), y = earthquake, alpha = earthquake)) +
  geom_bar(stat = "identity", fill = "red", color = "red") +
  xlab("city") +
  theme(axis.text.x = element_text(angle = 65, hjust = 1))

#垃圾製造量 score
ggplot(data = score, aes(x = reorder(index, garbage_score), y = garbage_score, alpha = garbage_score)) +
  geom_bar(stat = "identity", fill = "brown", color = "brown") +
  xlab("city") +
  theme(axis.text.x = element_text(angle = 65, hjust = 1))

#垃圾製造量 data
ggplot(data = data, aes(x = reorder(index, garbagegenerated), y = garbagegenerated, alpha = garbagegenerated)) +
  geom_bar(stat = "identity", fill = "red", color = "red") +
  xlab("city") +
  theme(axis.text.x = element_text(angle = 65, hjust = 1))

#平均溫度 score
ggplot(data = score, aes(x = reorder(index, temp_score), y = temp_score, alpha = temp_score)) +
  geom_bar(stat = "identity", fill = "brown", color = "brown") +
  xlab("city") +
  theme(axis.text.x = element_text(angle = 65, hjust = 1))

#平均溫度 data
new_temp <- inner_join(index, temp, by = "city")
ggplot(data = new_temp, aes(x = reorder(index, temperature, FUN = median), y = temperature)) +
  geom_boxplot(fill = "pink") +
  xlab("city") +
  theme(axis.text.x = element_text(angle = 65, hjust = 1))

#平均濕度 score
ggplot(data = score, aes(x = reorder(index, humid_score), y = humid_score, alpha = humid_score)) +
  geom_bar(stat = "identity", fill = "brown", color = "brown") +
  xlab("city") +
  theme(axis.text.x = element_text(angle = 65, hjust = 1))

#平均濕度 data
ggplot(data = new_temp, aes(x = reorder(index, RelHumidity, FUN = median), y = RelHumidity)) +
  geom_boxplot(fill = "pink") +
  xlab("city") +
  theme(axis.text.x = element_text(angle = 65, hjust = 1))


##最終結果##

#社會因素總分(滿分10分)
ggplot(score, aes(x = reorder(index, society), y = society, alpha = society))+
  geom_bar(fill = "#b5c0e3", color = "#b5cde3", stat = "identity")+
  xlab("city")+
  theme(axis.text.x = element_text(angle = 65, hjust = 1))

#環境因素總分(滿分10分)
ggplot(score, aes(x = reorder(index, environment), y = environment, alpha = environment))+
  geom_bar(fill = "#fbb4ae",color = "#fbb4ae" , stat = "identity")+
  xlab("city")+
  theme(axis.text.x = element_text(angle = 65, hjust = 1))

#整體總分(滿分20分)
soc <- select(score, city, index, society)
soc_name <- numeric(19)
for (i in 1:19){
  soc_name[i] <- "soc"
}
soc <- cbind(soc, soc_name)
colnames(soc)[c(3,4)] <- c("final_score","type")

env <- select(score, city, index, environment)
env_name <- numeric(19)
for (i in 1:19){
  env_name[i] <- "env"
}
env <- cbind(env,env_name)
colnames(env)[c(3,4)] <- c("final_score","type")

plot <- rbind(soc, env)

ggplot(plot, aes(x = reorder(index, final_score), y = final_score, fill = type))+
  geom_bar(stat = "identity")+
  scale_fill_brewer(palette = "Pastel1") +
  xlab("city")+
  theme(axis.text.x = element_text(angle = 65, hjust = 1))


