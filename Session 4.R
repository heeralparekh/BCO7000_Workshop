library(tidyverse)

install.packages("skimr")
library(skimr)

install.packages("lubridate")
library(lubridate)

brewing_materials <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/brewing_materials.csv')

brewer_size <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/brewer_size.csv')

Skim(brewing_materials)

#Lubridate- mutate will create a new variable- named date -ymd is function - it will paste data from columns that is year month and we dont have date so we put 1
#1st line will make changes in the original data- here added new column to the data at the end 
#filter will filter the column

brewing_materials <- brewing_materials %>%
  mutate(date = ymd(paste(year, month, 1))) %>%
  filter(year < 2016)

#Remove total values
#filter str detect will detect "total" word from material type column- "Word" is case sensitive 

brewing_materials %>%
  filter(str_detect(material_type, "Total"))

# adding exclamation will show data other than the filter-here other than total..

#ggplot to make a graph, fill will color the bars, color instead of fill will online color the border in bar chart

brewing_materials %>%
  filter(!str_detect(material_type, "Total")) %>%
  ggplot(aes(type, month_current, fill = material_type)) +
  geom_col()

#coordflip will flip x axis and y axis, scale is a whole function used as it is to change labels 

brewing_materials %>%
  filter(!str_detect(material_type, "Total")) %>%
  ggplot(aes(type, month_current, fill = material_type)) +
  geom_col()+
  coord_flip()+
  scale_y_continuous(labels = scales::comma)

#reorder- use mutate, first write name of x axis that is the one u want to reorder and in bracket both the axis 

brewing_materials %>%
  filter(!str_detect(material_type, "Total")) %>%
  mutate(type = fct_reorder(type, month_current)) %>%
  ggplot(aes(type, month_current, fill = material_type)) +
  geom_col()+
  coord_flip()+
  scale_y_continuous(labels = scales::comma)+
  labs(x ="Integredients", y = "Total Production", fill = "Material Type", title = "Bar Chart")

# knowing the type of varaiable -use class, $ sign is used to seperate table name and variable together

class(brewing_materials$type)

#as factor to change the class 

brewing_materials$type <- as_factor(brewing_materials$type)


brewer_size %>%
  count(brewer_size) %>%
  arrange(desc(n))

#if you want to mix all tiny data -here smaller than 10

brewer_size %>%
  mutate(brewer_size = fct_lump(brewer_size, n=10)) %>%
  count(brewer_size)



new_data <- brewing_materials %>%
  mutate(production_level = case_when(month_current <= 100000000 - "Low", T -"High"))







