# zadanie 1
fun_1 <- function(number_one, number_two){
    if (number_one %% number_two == 0){
        print("Podzielna")
    }
    else{
        print("Niepodzielna")
    }
}

fun_1(1,10)
fun_1(5,10)
fun_1(25,5)

# zadanie 2
fun_2 <- function(first_mean_velocity, second_mean_velocity){
    return((first_mean_velocity + second_mean_velocity)/2 )
}

fun_2(120,90)

library(data.table)
?data.table
data <- read.csv2("dane.csv")

data

wzrost <- data[["wzrost"]]
wzrost
class(wzrost)
typeof(wzrost)

waga <- data[["waga"]]
waga
class(waga)
typeof(waga)

# zadanie 3
fun_3 <- function(first_vector, second_vector){
    std_first_vector <- sd(first_vector)
    std_second_vector <- sd(second_vector)
    covariance <- cov(first_vector, second_vector)
    return(covariance/(std_first_vector * std_second_vector))
}


fun_3(wzrost, waga)
# Uzyskany wynik korelacj r-Pearsona można zinterpretować jako istotny statystycznie związek
# korelacji pomiędzy wzreostem a wagą.

# zadanie 4
stworzDataFrame<-function(x=1){
    headers<-readline(prompt="Podaj naglowki kolumn oddzielajac je spacjami: ")
    columns<-(strsplit(headers," "))
    as.vector(columns)
    matx<-matrix(NA, nrow=x, ncol=lengths(columns))
    df<-data.frame(matx)
    names(df)<-t(unlist(columns))
    for(column in colnames(df)){
        for(i in 1:x){
            input<-readline(prompt = "Podaj wartosc wiersza i nacisnij enter:")
            df[i, column]<-input
        }}
    View(df)
}
stworzDataFrame()

# zadanie 5
liczZplikow <- function(sciezka,jakaFunkcja="mean",DlaIluPlikow=1){ 
    if (!(jakaFunkcja %in% c('mean', 'max', 'min', 'median')))
        stop("Blednie uzupelniony parametr 'jakaFunkcja'", call. = TRUE)
    
        packages <- 'stringr'
        for(package in packages){
            if(!require(package, character.only = TRUE, quietly = TRUE)){
                install.packages(package, repos="http://cran.us.r-project.org")
                library(package, character.only = TRUE)
            }
            library(package, character.only = TRUE)
        }
    
        files <- list.files(sciezka, full.names = TRUE)[1: DlaIluPlikow]
    
        my_dt <- lapply(files, fread)
        my_dt <- rbindlist(my_dt)
        colnames(my_dt)
    
        cols <- sapply(my_dt, function (k) all(!is.na(k)))
        my_dt2 <- my_dt[, ..cols]
    
        print(colnames(my_dt2))
        nazwaKolumny <-  as.vector(readline(paste0('Select the columns: ')))
    
        nn <- as.vector(unlist(strsplit(nazwaKolumny, '"')))
        nn <- str_split(nn, ' ')
        nn <- nn[nn != '']
        nn <- unlist(nn[seq(1, length(nn), 2)])
    
        new_dt22 <- my_dt2[, ..nn]
        results <- data.table(zmienna = paste0(nn, '_mean'),arythmetic=lapply(new_dt22, jakaFunkcja))
    
        return(results)
}


liczZplikow(sciezka = './smogKrakow/', jakaFunkcja="mean", DlaIluPlikow=2)
