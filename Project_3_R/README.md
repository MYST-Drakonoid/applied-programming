# Overview

as a software engineer my goal is to make peoples lives more pleasent. whether that is through games that peopl emay enjoy or through making machines or programs to make life easier it doesnt matter.
i figured that with my love of computers that i could more easily make that difference in the digital space.

the software that i wrote i designed to take a large table and provide as much useful data information that i could extract from json files. things like growth over time and highest and lowest profits etc

to learn R and to realize that R is actually kind of cool

[Software Demo Video](https://youtu.be/INzYt4SyweI)

# Development Environment

i used visual studio code and the default R tools that you need to actually test the code

| Package       | Purpose / What It Does                                                                                      |
| ------------- | ----------------------------------------------------------------------------------------------------------- |
| **dplyr**     | Core tidyverse package for data manipulation (`mutate`, `group_by`, `summarise`, `arrange`, etc.)           |
| **jsonlite**  | For reading and writing JSON files (`fromJSON`, `toJSON`)                                                   |
| **lubridate** | Handles dates and times (`ymd_hms`, `year`, `month`, `quarter`, `wday`, etc.)                               |
| **stringr**   | String/text processing (`str_trim`, `str_replace_all`, `str_to_title`, `str_to_sentence`)                   |
| **magrittr**  | Enables the `%>%` pipe operator                                                 |
| **R6**        | Allows defining R6 classes for object-oriented programming  |


# Useful Websites

- [R-manual](https://stat.ethz.ch/R-manual)
- [R for Data Science](https://r4ds.hadley.nz)

# Future Work

- i want to change the floating point values to restrinct them to 2 decimal points
- the dates in some of the csv files are showing NA and that should be fixed
- i want the input to be more user friendly