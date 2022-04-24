library(tidyverse)
library(rvest)
library(httr) # httr: Tools for Working with URLs and HTTP 
library(curl)
library(dplyr)
library(stringr)
library(ggplot2)
library(rvest)
library(jsonlite)
library(readxl)

Kaggle_master_data <- read_excel("Kaggle_data.xlsx")

swimmingscraping_master_data <- read_excel("SwimmingScraping.xlsx")

## Medal Summary for swim sports by year
Swimming_Medal_Summary <- swimmingscraping_master_data %>% filter(Measure == "Medal Summary") %>% arrange(URL)
#Swimming_Medal_Summary[3]
for( i in rownames(Swimming_Medal_Summary) ){
  URL <- Swimming_Medal_Summary[i, "URL"]
  Year = Swimming_Medal_Summary[i, "Year"]
  if (Year == '1896'){
    Medal_Summary_by_Year_1896 <- URL[[1]] %>% read_html() %>% html_table(fill = TRUE) 
    print('1896 Medal Summary') 
    Medal_Summary_by_Year_1896[[3]] <- cbind(Medal_Summary_by_Year_1896[[3]], new_col = Year) 
    Medal_Summary_by_Year_1896[[3]] <-Medal_Summary_by_Year_1896[[3]] %>% slice(1:(n()-1))
    print(Medal_Summary_by_Year_1896[[3]])  
    } #done
  else if (Year == '1900') {
    Medal_Summary_by_Year_1900 <- URL[[1]] %>% read_html() %>% html_table(fill = TRUE)
    print('1900 Medal Summary')
    Medal_Summary_by_Year_1900[[3]]<- cbind(Medal_Summary_by_Year_1900[[3]], new_col = Year)
    Medal_Summary_by_Year_1900[[3]] <- Medal_Summary_by_Year_1900[[3]] %>% slice(1:(n()-1))
    print(Medal_Summary_by_Year_1900[[3]]) } #done
  else if (Year == '1904'){
    Medal_Summary_by_Year_1904 <- URL[[1]] %>% read_html() %>% html_table(fill = TRUE)
    print('1904 Medal Summary')
    Medal_Summary_by_Year_1904[[3]]<- cbind(Medal_Summary_by_Year_1904[[3]], new_col = Year)
    Medal_Summary_by_Year_1904[[3]] <- Medal_Summary_by_Year_1904[[3]] %>% slice(1:(n()-1))
    print(Medal_Summary_by_Year_1904[[3]]) #done
  }
  else if (Year == '1906'){
    Medal_Summary_by_Year_1906 <- URL[[1]] %>% read_html() %>% html_table(fill = TRUE)
    print('1906 Medal Summary')
    Medal_Summary_by_Year_1906[[2]]<- cbind(Medal_Summary_by_Year_1906[[2]], new_col = Year)
    Medal_Summary_by_Year_1906[[2]] <- Medal_Summary_by_Year_1906[[2]] %>% slice(1:(n()-1))
    print(Medal_Summary_by_Year_1906[[2]]) #done
  }
  else if (Year == '1908'){
    Medal_Summary_by_Year_1908 <- URL[[1]] %>% read_html() %>% html_table(fill = TRUE)
    print('1908 Medal Summary')
    Medal_Summary_by_Year_1908[[4]]<- cbind(Medal_Summary_by_Year_1908[[4]], new_col = Year)
    Medal_Summary_by_Year_1908[[4]] <- Medal_Summary_by_Year_1908[[4]] %>% slice(1:(n()-1))
    print(Medal_Summary_by_Year_1908[[4]])  #done
  }
  else if (Year == '1912'){
    Medal_Summary_by_Year_1912 <- URL[[1]] %>% read_html() %>% html_table(fill = TRUE)
    print('1912 Medal Summary')
    Medal_Summary_by_Year_1912[[3]]<- cbind(Medal_Summary_by_Year_1912[[3]], new_col = Year)
    Medal_Summary_by_Year_1912[[3]] <- Medal_Summary_by_Year_1912[[3]] %>% slice(1:(n()-1))
    print(Medal_Summary_by_Year_1912[[3]]) #done
  }
  else if (Year == '1920'){
    Medal_Summary_by_Year_1920 <- URL[[1]] %>% read_html() %>% html_table(fill = TRUE)
    print('1920 Medal Summary')
    Medal_Summary_by_Year_1920[[3]]<- cbind(Medal_Summary_by_Year_1920[[3]], new_col = Year)
    Medal_Summary_by_Year_1920[[3]] <- Medal_Summary_by_Year_1920[[3]] %>% slice(1:(n()-1))
    print(Medal_Summary_by_Year_1920[[3]]) #done
  }
  else if (Year == '1924'){
    Medal_Summary_by_Year_1924 <- URL[[1]] %>% read_html() %>% html_table(fill = TRUE)
    print('1924 Medal Summary')
    Medal_Summary_by_Year_1924[[3]]<- cbind(Medal_Summary_by_Year_1924[[3]], new_col = Year)
    Medal_Summary_by_Year_1924[[3]] <- Medal_Summary_by_Year_1924[[3]] %>% slice(1:(n()-1))
    print(Medal_Summary_by_Year_1924[[3]]) #done
  }
  else if (Year == '1928'){
    Medal_Summary_by_Year_1928 <- URL[[1]] %>% read_html() %>% html_table(fill = TRUE)
    print('1928 Medal Summary')
    Medal_Summary_by_Year_1928[[3]]<- cbind(Medal_Summary_by_Year_1928[[3]], new_col = Year)
    Medal_Summary_by_Year_1928[[3]] <- Medal_Summary_by_Year_1928[[3]] %>% slice(1:(n()-1))
    print(Medal_Summary_by_Year_1928[[3]]) #done
  }
  else if (Year == '1932'){
    Medal_Summary_by_Year_1932 <- URL[[1]] %>% read_html() %>% html_table(fill = TRUE)
    print('1932 Medal Summary')
    Medal_Summary_by_Year_1932[[3]]<- cbind(Medal_Summary_by_Year_1932[[3]], new_col = Year)
    Medal_Summary_by_Year_1932[[3]] <- Medal_Summary_by_Year_1932[[3]] %>% slice(1:(n()-1))
    print(Medal_Summary_by_Year_1932[[3]])#done
  }
  else if (Year == '1936'){
    Medal_Summary_by_Year_1936 <- URL[[1]] %>% read_html() %>% html_table(fill = TRUE)
    print('1936 Medal Summary')
    Medal_Summary_by_Year_1936[[3]]<- cbind(Medal_Summary_by_Year_1936[[3]], new_col = Year)
    Medal_Summary_by_Year_1936[[3]] <- Medal_Summary_by_Year_1936[[3]] %>% slice(1:(n()-1))
    print(Medal_Summary_by_Year_1936[[3]]) #done
  }
  else if (Year == '1948'){
    Medal_Summary_by_Year_1948 <- URL[[1]] %>% read_html() %>% html_table(fill = TRUE)
    print('1948 Medal Summary')
    Medal_Summary_by_Year_1948[[3]]<- cbind(Medal_Summary_by_Year_1948[[3]], new_col = Year)
    Medal_Summary_by_Year_1948[[3]] <- Medal_Summary_by_Year_1948[[3]] %>% slice(1:(n()-1))
    print(Medal_Summary_by_Year_1948[[3]]) #done
  }
  else if (Year == '1952'){
    Medal_Summary_by_Year_1952 <- URL[[1]] %>% read_html() %>% html_table(fill = TRUE)
    print('1952 Medal Summary')
    Medal_Summary_by_Year_1952[[3]]<- cbind(Medal_Summary_by_Year_1952[[3]], new_col = Year)
    Medal_Summary_by_Year_1952[[3]] <- Medal_Summary_by_Year_1952[[3]] %>% slice(1:(n()-1))
    print(Medal_Summary_by_Year_1952[[3]]) #done
  }
  #else if (Year == '1956'){
   # Medal_Summary_by_Year_1956 <- URL[[1]] %>% read_html() %>% html_table(fill = TRUE)
    #print('1956 Medal Summary')
    #print(Medal_Summary_by_Year_1956[[3]])
  #}
  else if (Year == '1960'){
    Medal_Summary_by_Year_1960 <- URL[[1]] %>% read_html() %>% html_table(fill = TRUE)
    print('1960 Medal Summary')
    Medal_Summary_by_Year_1960[[3]]<- cbind(Medal_Summary_by_Year_1960[[3]], new_col = Year)
    Medal_Summary_by_Year_1960[[3]] <- Medal_Summary_by_Year_1960[[3]] %>% slice(1:(n()-1))
    print(Medal_Summary_by_Year_1960[[3]])  #done
  }
  else if (Year == '1964'){
    Medal_Summary_by_Year_1964 <- URL[[1]] %>% read_html() %>% html_table(fill = TRUE)
    print('1964 Medal Summary')
    Medal_Summary_by_Year_1964[[5]]<- cbind(Medal_Summary_by_Year_1964[[5]], new_col = Year)
    Medal_Summary_by_Year_1964[[5]] <- Medal_Summary_by_Year_1964[[5]] %>% slice(1:(n()-1))
    print(Medal_Summary_by_Year_1964[[5]])  #done
  }
  else if (Year == '1968'){
    Medal_Summary_by_Year_1968 <- URL[[1]] %>% read_html() %>% html_table(fill = TRUE)
    print('1968 Medal Summary')
    Medal_Summary_by_Year_1968[[4]]<- cbind(Medal_Summary_by_Year_1968[[4]], new_col = Year)
    Medal_Summary_by_Year_1968[[4]] <- Medal_Summary_by_Year_1968[[4]] %>% slice(1:(n()-1))
    print(Medal_Summary_by_Year_1968[[4]])#done
  }
  else if (Year == '1972'){
    Medal_Summary_by_Year_1972 <- URL[[1]] %>% read_html() %>% html_table(fill = TRUE)
    print('1972 Medal Summary')
    Medal_Summary_by_Year_1972[[3]]<- cbind(Medal_Summary_by_Year_1972[[3]], new_col = Year)
    Medal_Summary_by_Year_1972[[3]] <- Medal_Summary_by_Year_1972[[3]] %>% slice(1:(n()-1))
    print(Medal_Summary_by_Year_1972[[3]])#done
  }
  else if (Year == '1976'){
    Medal_Summary_by_Year_1976 <- URL[[1]] %>% read_html() %>% html_table(fill = TRUE)
    print('1976 Medal Summary')
    Medal_Summary_by_Year_1976[[5]]<- cbind(Medal_Summary_by_Year_1976[[5]], new_col = Year)
    Medal_Summary_by_Year_1976[[5]] <- Medal_Summary_by_Year_1976[[5]] %>% slice(1:(n()-1))
    print(Medal_Summary_by_Year_1976[[5]])#done
  }
  else if (Year == '1980'){
    Medal_Summary_by_Year_1980 <- URL[[1]] %>% read_html() %>% html_table(fill = TRUE)
    print('1980 Medal Summary')
    Medal_Summary_by_Year_1980[[4]]<- cbind(Medal_Summary_by_Year_1980[[4]], new_col = Year)
    Medal_Summary_by_Year_1980[[4]] <- Medal_Summary_by_Year_1980[[4]] %>% slice(1:(n()-1))
    print(Medal_Summary_by_Year_1980[[4]])#done
  }
  else if (Year == '1984'){
    Medal_Summary_by_Year_1984 <- URL[[1]] %>% read_html() %>% html_table(fill = TRUE)
    print('1984 Medal Summary')
    Medal_Summary_by_Year_1984[[3]]<- cbind(Medal_Summary_by_Year_1984[[3]], new_col = Year)
    Medal_Summary_by_Year_1984[[3]] <- Medal_Summary_by_Year_1984[[3]] %>% slice(1:(n()-1))
    print(Medal_Summary_by_Year_1984[[3]])#done
  }
  else if (Year == '1988'){
    Medal_Summary_by_Year_1988 <- URL[[1]] %>% read_html() %>% html_table(fill = TRUE)
    print('1988 Medal Summary')
    Medal_Summary_by_Year_1988[[3]]<- cbind(Medal_Summary_by_Year_1988[[3]], new_col = Year)
    Medal_Summary_by_Year_1988[[3]] <- Medal_Summary_by_Year_1988[[3]] %>% slice(1:(n()-1))
    print(Medal_Summary_by_Year_1988[[3]])#done
  }
  else if (Year == '1992'){
    Medal_Summary_by_Year_1992 <- URL[[1]] %>% read_html() %>% html_table(fill = TRUE)
    print('1992 Medal Summary')
    Medal_Summary_by_Year_1992[[3]]<- cbind(Medal_Summary_by_Year_1992[[3]], new_col = Year)
    Medal_Summary_by_Year_1992[[3]] <- Medal_Summary_by_Year_1992[[3]] %>% slice(1:(n()-1))
    print(Medal_Summary_by_Year_1992[[3]])#done
  }
  else if (Year == '1996'){
    Medal_Summary_by_Year_1996 <- URL[[1]] %>% read_html() %>% html_table(fill = TRUE)
    print('1996 Medal Summary')
    Medal_Summary_by_Year_1996[[3]]<- cbind(Medal_Summary_by_Year_1996[[3]], new_col = Year)
    Medal_Summary_by_Year_1996[[3]] <- Medal_Summary_by_Year_1996[[3]] %>% slice(1:(n()-1))
    print(Medal_Summary_by_Year_1996[[3]])#done
  }
  else if (Year == '2000'){
    Medal_Summary_by_Year_2000 <- URL[[1]] %>% read_html() %>% html_table(fill = TRUE)
    print('2000 Medal Summary')
    Medal_Summary_by_Year_2000[[6]] <- cbind(Medal_Summary_by_Year_2000[[6]], new_col = Year)
    Medal_Summary_by_Year_2000[[6]] <- Medal_Summary_by_Year_2000[[6]] %>% slice(1:(n()-1))
    print(Medal_Summary_by_Year_2000[[6]])#done
  }
  else if (Year == '2004'){
    Medal_Summary_by_Year_2004 <- URL[[1]] %>% read_html() %>% html_table(fill = TRUE)
    print('2004 Medal Summary')
    Medal_Summary_by_Year_2004[[6]] <- cbind(Medal_Summary_by_Year_2004[[6]], new_col = Year)
    Medal_Summary_by_Year_2004[[6]] <- Medal_Summary_by_Year_2004[[6]] %>% slice(1:(n()-1))
    print(Medal_Summary_by_Year_2004[[6]])#done
  }
  else if (Year == '2008'){
    Medal_Summary_by_Year_2008 <- URL[[1]] %>% read_html() %>% html_table(fill = TRUE)
    print('2008 Medal Summary')
    Medal_Summary_by_Year_2008[[6]] <- cbind(Medal_Summary_by_Year_2008[[6]], new_col = Year)
    Medal_Summary_by_Year_2008[[6]] <- Medal_Summary_by_Year_2008[[6]] %>% slice(1:(n()-1))
    print(Medal_Summary_by_Year_2008[[6]])#done
  }
  else if (Year == '2012'){
    Medal_Summary_by_Year_2012 <- URL[[1]] %>% read_html() %>% html_table(fill = TRUE)
    print('2012 Medal Summary')
    Medal_Summary_by_Year_2012[[6]] <- cbind(Medal_Summary_by_Year_2012[[6]], new_col = Year)
    Medal_Summary_by_Year_2012[[6]] <- Medal_Summary_by_Year_2012[[6]] %>% slice(1:(n()-1))
    print(Medal_Summary_by_Year_2012[[6]])#done
  }
  else if (Year == '2016'){
    Medal_Summary_by_Year_2016 <- URL[[1]] %>% read_html() %>% html_table(fill = TRUE)
    print('2016 Medal Summary')
    Medal_Summary_by_Year_2016[[6]] <- cbind(Medal_Summary_by_Year_2016[[6]], new_col = Year)
    Medal_Summary_by_Year_2016[[6]] <- Medal_Summary_by_Year_2016[[6]] %>% slice(1:(n()-1))
    print(Medal_Summary_by_Year_2016[[6]])#done
  }
  else if (Year == '2020'){
    Medal_Summary_by_Year_2020 <- URL[[1]] %>% read_html() %>% html_table(fill = TRUE)
    print('2020 Medal Summary')
    Medal_Summary_by_Year_2020[[7]] <- cbind(Medal_Summary_by_Year_2020[[7]], new_col = Year)
    Medal_Summary_by_Year_2020[[7]] <- Medal_Summary_by_Year_2020[[7]] %>% slice(1:(n()-1))
    colnames(Medal_Summary_by_Year_2020[[7]])[2] <- "Nation"
    print(Medal_Summary_by_Year_2020[[7]])#done
  }
}

options(max.print = 10000) 
medal_summary_1 <- rbind(Medal_Summary_by_Year_1896[[3]],Medal_Summary_by_Year_1900[[3]],Medal_Summary_by_Year_1904[[3]],Medal_Summary_by_Year_1906[[2]],
                    Medal_Summary_by_Year_1908[[4]],Medal_Summary_by_Year_1912[[3]],Medal_Summary_by_Year_1920[[3]],Medal_Summary_by_Year_1924[[3]],
                    Medal_Summary_by_Year_1928[[3]],Medal_Summary_by_Year_1932[[3]],Medal_Summary_by_Year_1936[[3]],Medal_Summary_by_Year_1948[[3]],
                    Medal_Summary_by_Year_1952[[3]],Medal_Summary_by_Year_1960[[3]],Medal_Summary_by_Year_1964[[5]],Medal_Summary_by_Year_1968[[4]])
medal_summary_2 <- rbind(Medal_Summary_by_Year_1972[[3]],Medal_Summary_by_Year_1976[[5]],Medal_Summary_by_Year_1980[[4]],Medal_Summary_by_Year_1984[[3]],
                    Medal_Summary_by_Year_1988[[3]],Medal_Summary_by_Year_1992[[3]],Medal_Summary_by_Year_1996[[3]],Medal_Summary_by_Year_2000[[6]],
                    Medal_Summary_by_Year_2004[[6]],Medal_Summary_by_Year_2008[[6]],Medal_Summary_by_Year_2012[[6]],Medal_Summary_by_Year_2016[[6]],
                    Medal_Summary_by_Year_2020[[7]])

final_medal_summary_swimming_df <- rbind(medal_summary_1,medal_summary_2)
final_medal_summary_swimming_df

