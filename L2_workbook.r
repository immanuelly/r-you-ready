# Vector of numbers
x <- c(7,8,4,2,4,1,6,8,-7,-10)
x

# Vector of numbers generated as a sequence
y <- seq(7)
y

# Access vector elements
x[5]
x[c(6,7)]
x[1:4]

# Vector of strings
z <- c("Bristol", "London", "Manchester")
z

# Vector of booleans (must be in CAPS)
w <- c(TRUE, FALSE, FALSE, TRUE)
w

# Vector of mixed type not allowed allegedly...
a <- c(TRUE, 5, "Manchester")
a

# to check type:
mode(a) # character
mode(z) # character
mode(w) # logical
mode(y) # numeric

# Matrices
# generate 2x5 matrix
# populated by filling each row before moving to next column
M <- matrix(seq(10), 2, 5)
M

# access third element of second row
M[2,3]

# inspect entire column four
M[,4]

# check if selected row/column is a vector
is.vector(M[,2])

# Lists
# can be of mixed type
first_list <- list(TRUE, 2, "Georgia")
first_list
mode(first_list) # type will be "list"

# can name list elements like a dictionary
second_list <- list(bool_value=TRUE, num_value=2, state="Georgia")
second_list$bool_value
second_list$state

# Data frames
# first generate vector(s)
city_name <- c("Lagos", "Kano", "Port Harcourt", "Akure")
population <- c(15.3, 4.1, 1.8, 0.4)

# generate data frame
first_data_frame <- data.frame(city_name, population)
first_data_frame

# different columns can be of different types, BUT cells within column must be 
# of same type
str(first_data_frame) # booky notation *facepalm*

# can also create data frame directly
second_data_frame <- data.frame(city_name= c("Ibadan", "Abuja", "Illorin", "Oyo"),
                                population= c(3.5, 1.2, 0.8, 0.7)
)
second_data_frame

# Arithmetic operations
# addition, subtraction, multiplication, division, exponentiation
(((4+2-1)*4)/2)^2

# using matrices for element-wise multiplication
# first generate two random 2x3 matrices
a <- matrix(sample(1:10, 6, replace = T),2,3)
b <- matrix(sample(1:10, 6, replace = T),2,3)
a
b
a*b

#transpose matrix
t(b)
# transpose multiplication
a%*%t(b)


# Functions
# Define and call

is_prime <- function(num) {
  
  # Function which takes in positive integer as input and returns Boolean stating
  # if input is a prime number
  
  stopifnot(is.numeric(num), num%%1==0, num>=0) # stop if "num" not a positive int
  
  t_val <- TRUE # initialise Boolean with default TRUE assignment
  
  if(num<2) {
    
    t_val <- FALSE # if "num" is 0 or 1
  } else if(num>2) {
    
    for (i in 2:sqrt(num)) { # check for possible divisors, i, between 2 and sqrt(num)
      
      if(num%%i==0) {
        t_val <- FALSE
        break # if i divides num then num is not prime
      }
    }
  }
  
  return(t_val) # return bool operator
}

# call function
is_prime(39)
is_prime(13)

sieve <- function(n) {
  
  if (n<2) return(NULL) # for inputs 1 and 0
  
  a <- rep(TRUE, n) # create vector/list of "TRUE"s of 1xn dimension
  
  a[1] <- FALSE # set vector element to represent 1 to "FALSE"
  
  for (i in seq(n)) { # for each i in sequence 1 to "n"...
    
    if (a[i]) { # ignore the "FALSE"s, thus saving computational resources
      
      j <- i^2 # assign variable, j, as square of i
      
      if (j>n) return(which(a)) # return list of primes between 2 and input, n, once j exceeds n
      
      a[seq(j, n, by=i)] <- FALSE # otherwise set each element from j to n, per i steps, as FALSE,
      
    }
    
  }
  
}
sieve(10)
sieve(2000)
