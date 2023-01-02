#This is titantic dataset downloaded from Kaggle.  I took this dataset and clean it to make the information more useful


#installing and loading tidyverse
install.packages("tidyverse")
library(tidyverse)

#installing and loading readxl package to import excel files
install.packages("readxl")
library(readxl)

#importing the titanic3 excel file and assigning the file to dataset called titantic
titanic1 <- read_excel("C:/Users/Darnell W/Desktop/Excel/titanic3.xls")  #Use / in file path name


#Created a new dataframe called titanic_clean to work with
titanic_clean <- titanic1 %>% 
  
#USED Mutate and string function TO ADD A CITY NAME TO "embarked" column values
  mutate(embarked = str_replace(embarked, "S", "Southhampton")) %>% 
  mutate(embarked = str_replace(embarked, "C", "Cherbourg")) %>%
  mutate(embarked = str_replace(embarked, "Q", "Queenstown")) %>% 
  
#Changed "survived" column values to Yes or No
  mutate(survived = str_replace(survived, "1", "Yes")) %>% 
  mutate(survived = str_replace(survived, "0", "No")) %>% 
  
#Changed "pclass" column values to first,second, or third
  mutate(pclass = str_replace(pclass, "1", "First")) %>%
  mutate(pclass = str_replace(pclass, "2", "Second")) %>% 
  mutate(pclass = str_replace(pclass, "3", "Third")) %>%
  
#Renamed "plcass" column to "Class"
  rename(Class=pclass) %>% 
  
#Extracted the string in parentheses from the "name" column which has some female passenger's maiden/first name
  mutate(mname = str_extract(name, "(?<=\\()[^()]*(?=\\))"))%>%
  
#Rounded the number in the age column to remove uncessary decimal places.
  mutate(age = round(titanic1$age,0)) %>% 
  
#Separated the name column into 3 columns
  separate(name, c('lastname', 'title', 'firstname' ), sep = " ") %>%
  
#Took the first name of the female mname column and created new column called maiden
  mutate(maiden = word(mname, start = 1, end = 1)) %>%
  
##Created an ifelse statement to use the maiden name in place of the first name
##in the firstname column instead of husband's firstname
  mutate(firstname = ifelse(!is.na(maiden), maiden, firstname)) %>% 
  
#Deleted 3 columns that were no longer needed (maiden, mname,title)
  select(-one_of('maiden', 'title','mname'))

#Removes ',' from the "lastname" column
titanic_clean$lastname <- gsub('[,]', '', titanic_clean$lastname) 

#Removes '?' from the "home.dest" column
titanic_clean$HOME.DEST <- gsub('[?]', '', titanic_clean$HOME.DEST)

#Capitalized column names
names(titanic_clean) <- toupper(names(titanic_clean))

