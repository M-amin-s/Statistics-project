N = 624
M = 397

int_to_unit <- function (x, adjustment=2^32) {
  x <- as.numeric(x)
  signs <- sign(x)
  x[signs < 0] <- x[signs < 0] + adjustment
  x
}

MATRIX_A = int_to_unit(0x9908b0df)
UPPER_MASK = int_to_unit(0x80000000)
LOWER_MASK = int_to_unit(0x7fffffff)


TEMPERING_MASK_B = int_to_unit(0x9d2c5680)
TEMPERING_MASK_C = int_to_unit(0xefc60000)

TEMPERING_SHIFT_U <- function(y) {
  y = y / (2 ** 11)
  return(y)
}

TEMPERING_SHIFT_S <- function(y) {
  y = y * (2 ** 7)
  return(y)
}

TEMPERING_SHIFT_T <- function(y) {
  y = y * (2 ** 15)
  return(y)
}

TEMPERING_SHIFT_L <- function(y) {
  y = y / (2 ** 18)
  return(y)
}

NUM_RNG = 100
MAX_DESC = 100

mt = matrix(, NUM_RNG, N)
mti = vector(, NUM_RNG)

init_twister <- function() {
  for(i in 0:NUM_RNG - 1){
    mti[i] = N + 1
  }
}

sgenrand <- function(rng_num, seed) {
  mt[rng_num, 0] = seed & int_to_unit(0xffffffff)
  mti[rng_num] = 1
  while(mti[rng_num] < N){
    mt[rng_num, mti[rng_num]] = (69069 * mt[rng_num, mti[rng_num] - 1]) & int_to_unit(0xffffffff)
    mti[rng_num] = mti[rng_num] + 1
  }
}

genrand <- function(rng_num){
  
  y = 0
  first = 1
  
  mag01 = matrix(, NUM_RNG, 2)
  
  if (first == 1) {
    for(i in 0:NUM_RNG - 1){
      mag01[i, 0] = 0x0
      mag01[i, 1] = MATRIX_A
    }
    
    first = 0
  }
  
  if (mti[rng_num] >= N){
    kk = 0
    
    if (mti[rng_num] == N + 1)
      sgenrand(rng_num, 4357)
    
    for(kk in 0: N - M){
      y = (mt[rng_num, kk] & UPPER_MASK)|(mt[rng_num, kk + 1] & LOWER_MASK)
      mt[rng_num, kk] = mt[rng_num, kk + M] ** (y * 2) ** mag01[rng_num, (y & 0x1)]
    }
    
    for(kk in kk:N - 1){
      y = (mt[rng_num, kk] & UPPER_MASK)|(mt[rng_num, kk + 1] & LOWER_MASK)
      mt[rng_num, kk] = mt[rng_num, kk + (M - N)] ** (y * 2) ** mag01[rng_num, (y & 0x1)]
    }
    
    y = (mt[rng_num, N - 1] & UPPER_MASK)|(mt[rng_num, 0] & LOWER_MASK)
    mt[rng_num, N - 1] = mt[rng_num, M - 1] ** (y * 2) ** mag01[rng_num, (y & 0x1)]
    
    mti[rng_num] = 0
  }
  
  y = mt[rng_num, mti[rng_num]]
  mti[rng_num] = mti[rng_num] + 1
  
  y = y ** TEMPERING_SHIFT_U(y)
  y = y ** TEMPERING_SHIFT_S(y) & TEMPERING_MASK_B
  y = y ** TEMPERING_SHIFT_T(y) & TEMPERING_MASK_C
  y = y ** TEMPERING_SHIFT_L(y)
  
  return(y)
}
