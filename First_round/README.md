2020-04-11
================

Vowel Count
-----------

Return the number (count) of vowels in the given string.

We will consider a, e, i, o, and u as vowels for this Kata.

The input string will only consist of lower case letters and/or spaces.

``` r
get_count <- function(input_str){
str_count<- c()
  for (i in 1:nchar(input_str)){
    str_count[i]<- ifelse(strsplit(input_str, split = NULL)[[1]][i] %in% c("a", "e", "i", "o", "u"), 1, 0)
  }
  total_count<- sum(str_count)
  return(total_count)
}
```

Get the Middle Character
------------------------

You are going to be given a word. Your job is to return the middle character of the word. If the word's length is odd, return the middle character. If the word's length is even, return the middle 2 characters.

``` r
get_middle <- function(s){
if (nchar(s) %% 2 == 0) {
return(paste(strsplit(s, split=NULL)[[1]][nchar(s)/2], strsplit(s, split=NULL)[[1]][nchar(s)/2+1],  sep=""))
} else {
return(strsplit(s, split=NULL)[[1]][nchar(s)/2 + 1])
}
}
```

Mumbling
--------

Examples:

`accum("abcd") -> "A-Bb-Ccc-Dddd" accum("RqaEzty") -> "R-Qq-Aaa-Eeee-Zzzzz-Tttttt-Yyyyyyy" accum("cwAt") -> "C-Ww-Aaa-Tttt"`

``` r
accum <- function(s){
  
  string_div<- list()
  for (i in 1:nchar(s)){
    string_div[i]<- tolower(paste(rep(strsplit(s, split=NULL)[[1]][i], i), collapse = ""))
  }
  
  for (i in 1:length(string_div)){
    string_div[[i]]<- paste(toupper(substr(string_div[[i]], 1,1)), substr(string_div[[i]], 2, nchar(string_div[[i]])), sep = "")
  }
  
  paste(string_div, collapse = "-")
}
```

Shortest word
-------------

Simple, given a string of words, return the length of the shortest word(s).

String will never be empty and you do not need to account for different data types.

``` r
find_short <- function(s){
df_s<- data.frame(splits= as.character(unlist(strsplit(s, split = " "))))
df_s$num_char<- apply(df_s,1, nchar)
return(min(df_s$num_char))
}
```

Exes and Ohs
------------

Check to see if a string has the same amount of 'x's and 'o's. The method must return a boolean and be case insensitive. The string can contain any char.

``` r
xo <- function(s){
count_x<- 0
count_o<- 0
if (nchar(s)>0) {
for (i in 1:length(strsplit(s, split = NULL)[[1]])){
  count_x<-  ifelse(strsplit(s, split = NULL)[[1]][[i]] %in% c("x", "X"), count_x +1, count_x)
  count_o<-  ifelse(strsplit(s, split = NULL)[[1]][[i]] %in% c("o", "O"), count_o +1, count_o)
  }
  print(count_x == count_o)
}
else{
  print(TRUE)
}
}
```

Growth of a population
----------------------

``` r
nbYear <- function (p0, percent, aug, p) {

year_n<- 0

while (p0 < p){
  p0<- p0 + p0*percent/100 + aug 
  year_n<- year_n + 1
}
return(year_n)  
}
```
