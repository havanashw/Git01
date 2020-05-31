# R语言小记录

## 提取字符串指定位置数据

```R
data <- read.csv('/Users/shihongwei/Documents/应用统计专硕_2020_2021/比赛_项目/防疫项目/covid_data/countrydata.csv')
data$date <- paste(substring(data$dateId,1,4),'-',
                   substring(data$dateId,5,6),'-',substring(data$dateId,7,8),sep = "")
data$date <- as.Date(data$date)
```

