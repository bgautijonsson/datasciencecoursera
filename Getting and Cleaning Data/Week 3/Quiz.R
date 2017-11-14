
# 1) 

url <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv'
download.file(url, destfile = 'AAC.csv')

df <- read.csv('AAC.csv')

tibble <- as_data_frame(df)

agricultureLogical1 <- filter(tibble, df$AGS==6, df$ACR==3)
agricultureLogical1
agricultureLogical2 <- df[df$AGS %in% 6 & df$ACR %in% 3, ]

# 2) 

url <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg'
download.file(url, destfile = 'instr.jpg')
image <- readJPEG('instr.jpg', native = TRUE)
quantile(image, probs=c(0.3,0.8))

# 3)

url <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv'
download.file(url, destfile = 'gdp.csv')
url <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv'
download.file(url, destfile = 'edu.csv')
gdp <- read.csv('gdp.csv')
gdp_tb <- as_data_frame(gdp)
edu <- read.csv('edu.csv')
edu_tb <- as_data_frame(edu)
gdp_tb <- gdp_tb[gdp_tb$Gross.domestic.product.2012 %in% 1:190, ]
nrow(gdp_tb)
edu_tb
unique(gdp_tb$Gross.domestic.product.2012)
nrow(merged[merged$X %in% intersect(gdp_tb$X, edu_tb$CountryCode),])

nrow(merged)
merged <- merge(gdp_tb, edu_tb,by.x = 'X', by.y  = 'CountryCode')
edu$match <- match(edu$CountryCode, gdp$X, nomatch=0)
nrow(edu[edu$match!=0, ])
merged_tb <- as_data_frame(merged)
merged_tb$Gross.domestic.product.2012 <- as.numeric(as.character(merged_tb$Gross.domestic.product.2012))

merged_tb <- arrange(merged_tb, desc(Gross.domestic.product.2012))
head(merged_tb, 13)
nrow(edu)

# 4)
unique(merged_tb$Income.Group)
merged_highincome1 <- filter(merged_tb, merged_tb$Income.Group=='High income: OECD')
merged_highincome2 <- filter(merged_tb, merged_tb$Income.Group=='High income: nonOECD')
mean(merged_highincome1$Gross.domestic.product.2012)
mean(merged_highincome2$Gross.domestic.product.2012, na.rm = TRUE)


# 5)

merged_tb$GDPQuant <- cut(merged_tb$Gross.domestic.product.2012,
                          breaks = 5)
table(merged_tb$Income.Group, merged_tb$GDPQuant)
table(merged_tb$GDPQuant)
arrange(merged_tb, desc(Gross.domestic.product.2012))
