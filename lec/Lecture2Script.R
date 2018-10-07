
##############################
## Lecture 2 Class R Script ##
##############################


## R Markdown Structure for Homeworks and Labs ##


##############################################
## Factors and Tables
##############################################

##

data <- rep(c("Control","Treatment"),c(3,4))
data # A character vector
group <- factor(data)
group


##


str(group)
mode(group) # Numeric?
summary(group)


##


group
ages <- c(20, 30, 40, 35, 35, 35, 35)
sex <- c("M", "M", "F", "M", "F", "F", "F")

##

split(ages, list(group, sex))

##


group
table(group)

##


table(sex, group)


##


new_table <- table(sex, group)
new_table[, "Control"]
round(new_table/length(group), 3) # Gives proportions


##############################################
## Dataframes
##############################################




Name <- c("John", "Jill", "Jacob", "Jenny")
Year <- c(1,1,2,4)
Grade <- c("B", "A+", "B-", "A")
student_data <- data.frame(Name, Year, Grade,stringsAsFactors = FALSE)

##

student_data
dim(student_data)

##

str(student_data)
summary(student_data)


##

student_data
student_data[3:4,]

##

student_data
student_data$Grade

##############################################
## Importing data
##############################################


## Working Directory ##

getwd()

list.files()

setwd("file_path")

getwd()

# Session -> Set Working Directory -> Choose Directory

getwd()

# Files -> More -> Set as Working Directory

getwd()


install.packages("datasets")
library(datasets)
states <- data.frame(state.x77, Region = state.region, 
                     Abbr = state.abb)
head(states, 2)



##

states["New York", ] # Can also use rownames



##

student_data[student_data$Grade == "A+", ]
student_data[student_data$Year <= 2, ]
states[states$Region == "Northeast", "Population"]


##


new_stu <- c("Bobby", 3, "A")
student_data <- rbind(student_data, new_stu)
student_data

##


student_data$School <- "Columbia"
student_data


##


Name <- c("John", "Bobby")
Age <- c(29, 23)
student_data2 <- data.frame(Name, Age, stringsAsFactors = FALSE)

student_data2
student_data
##

student_data
merge(student_data, student_data2)


##

plot(Illiteracy ~ Frost, data = states)


##############################################
#Text Example and Functions
##############################################


HC <- scan("HonorCode.txt", what = "")
head(HC, 20)
str(HC)


##


HC <- scan("HonorCode.txt", what = "")
head(HC, 15)

##


square_it <- function(x){
  out <- x*x
  return(out)
}

##


square_it(2); square_it(-4); square_it(146)


##


HC <- factor(HC, levels = unique(HC))  


##

# Simple example
split(1:10, c(rep("a",3),rep("b",7)))

## 

findwords <- function(text_vec){
  words <- split(1:length(text_vec), text_vec)
  return(words)
}


##


findwords(HC)[1:3]


##


HC <- as.character(HC)

HC[c(1, 48, 142, 204, 232, 310, 331)] # students

HC[c(2, 206)] # should


##


alphabetized_list <- function(wordlist) {
  nms <- names(wordlist) # The names are the words
  sorted <- sort(nms) # The words, but now in ABC order
  return(wordlist[sorted]) # Returns the sorted version
}

##

wl <- findwords(HC)
alphabetized_list(wl)[1:3]


##############################################
#Controls Statements 
##############################################


x <- c(5, 12, -3)
for (i in x) {
  print(i^2)
}

##


i <- 1
while (i <= 10) i <- i + 4



##


  for (i in seq(4)) {
    if (i %% 2 == 0) {print(log(i))}
    else {print("Odd")}
  }


##


library(matlab)    
total <- 0
for (i in 1:10) {
  if(isprime(i)) {
    total <- total + i
  }
}

total


##

u <- c(1,2,3)
v <- c(10,-20,30)
c <- vector(mode = "numeric", length = length(u))

for (i in 1:length(u)) {
  c[i] <- u[i] + v[i]
}
c

##

c <- u + v
c

##


for (i in seq(4)) {
  if (i %% 2 == 0) {print(log(i))}
  else {print("Odd")}
}




##

ifelse(seq(4) %% 2 == 0, log(seq(4)), "Odd")



##


mat <- matrix(1:12, ncol = 6)
mat
colSums(mat) # Recall colSums() from lab.

##



colSums(mat) 
apply(mat, 2, sum)


##

apply(mat, 1, sum) # Calculates the row sums





##

vec1 <- c(1.1,3.4,2.4,3.5)
vec2 <- c(1.1,3.4,2.4,10.8)
not_robust <- list(vec1, vec2)  
lapply(not_robust, mean)
# The sample mean is not rubust!


##


lapply(not_robust, median)
sapply(not_robust, median)
unlist(lapply(not_robust,median))


##


wl[1:3] # wl for word list


##


freq_list <- function(wordlist) {
  freqs <- sapply(wordlist, length) # The frequencies
  return(wordlist[order(freqs)])
}


##

head(freq_list(wl), 3)

##


tail(freq_list(wl), 3)

##


group
ages <- c(20, 30, 40, 35, 35, 35, 35)
tapply(ages, group, mean)






