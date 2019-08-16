
# This script generates features from raw data

#
# The arguments
# raw_data: the raw signal data
# sidx: [a list of] the start index of each moving window
# eidx: [a list of] the end index of each moving window
# N: the size of window
Feature_Generator <- function(raw_data, sidx, eidx, N = 100) {
  
  # Features generated for PAEE estimation
  #
  # The size of the signal window ranges from 0.25 to 60 seconds
  # 
  
  #
  # In time-domain, divide raw sensor signal into consecutive small windows
  # Then, the features are calculated for each window
  # The size of window ranges from 0.25 to 60 seconds, i.e. either every one-fourth of a second or every second
  #
  #
  # Time domain
  #
  # 1) Data points of raw sensor signals: {si, i = 1,…,N}
  # s: raw sensor signal, N: number of the sensor data points within a certain period of time
   
  
  # 2) Accelerometer counts (integral/sum of the signals over a period of time): .
  # sum(si) {i = 1,...,N}
  print("Features group 1 ...")
  sum_x <- sapply(1:length(sidx), function(i) sum(raw_data[sidx[i]:eidx[i],1]))
  sum_y <- sapply(1:length(sidx), function(i) sum(raw_data[sidx[i]:eidx[i],2]))
  sum_z <- sapply(1:length(sidx), function(i) sum(raw_data[sidx[i]:eidx[i],3]))
  
  features <- data.frame(sum_x = sum_x, sum_y = sum_y, sum_z = sum_z)
  
  
  # 3) Mean
  # sum(si) / N {i = 1,...,N}
  print("Features group 2 ...")
  features["mean_x"] <- features[,"sum_x"] / N
  features["mean_y"] <- features[,"sum_y"] / N
  features["mean_z"] <- features[,"sum_z"] / N
  
  # features["mean_x"] <- mean_x
  # features["mean_y"] <- mean_y
  # features["mean_z"] <- mean_z
  
  
  # 4) Standard deviation
  # sd  {i = 1,...,N}
  print("Features group 3 ...")
  features["sd_x"] <- sapply(1:length(sidx), function(i) sd(raw_data[sidx[i]:eidx[i],1]) )
  features["sd_y"] <- sapply(1:length(sidx), function(i) sd(raw_data[sidx[i]:eidx[i],2]) )
  features["sd_z"] <- sapply(1:length(sidx), function(i) sd(raw_data[sidx[i]:eidx[i],3]) )
 
  
  # 5) Coefficients of variation (CV)
  # sd / |mean| {i = 1,...,N}
  print("Features group 4 ...")
  features["cv_x"] <- features[,"sd_x"] / abs(features[,"mean_x"])
  features["cv_y"] <- features[,"sd_y"] / abs(features[,"mean_y"])
  features["cv_z"] <- features[,"sd_z"] / abs(features[,"mean_z"])
  
  
  # 6) Peak-to-peak amplitude. Peak amplitude is the maximum value of signal at each window
  # max(si) - min(si) {i = 1,...,N}
  print("Features group 5 ...")
  features["ppa_x"] <- sapply(1:length(sidx), function(i) max(raw_data[sidx[i]:eidx[i],1]) - min(raw_data[sidx[i]:eidx[i],1]) )
  features["ppa_y"] <- sapply(1:length(sidx), function(i) max(raw_data[sidx[i]:eidx[i],2]) - min(raw_data[sidx[i]:eidx[i],2]) )
  features["ppa_z"] <- sapply(1:length(sidx), function(i) max(raw_data[sidx[i]:eidx[i],3]) - min(raw_data[sidx[i]:eidx[i],3]) )
  

  # 7) Percentiles
  # Moved to the end
  
  
  # 8) IQR
  # 
  print("Features group 7 ...")
  features["iqr_x"] <- sapply(1:length(sidx), function(i) IQR(raw_data[sidx[i]:eidx[i],1], na.rm = TRUE) )
  features["iqr_y"] <- sapply(1:length(sidx), function(i) IQR(raw_data[sidx[i]:eidx[i],2], na.rm = TRUE) )
  features["iqr_z"] <- sapply(1:length(sidx), function(i) IQR(raw_data[sidx[i]:eidx[i],3], na.rm = TRUE) )
  
  
  # 9) Correlation between accelerometer axes
  # 
  print("Features group 8 ...")
  features["cr_xy"] <- sapply( 1:length(sidx), function(i) cor(raw_data[sidx[i]:eidx[i],1], raw_data[sidx[i]:eidx[i],2]) )
  features["cr_xz"] <- sapply( 1:length(sidx), function(i) cor(raw_data[sidx[i]:eidx[i],1], raw_data[sidx[i]:eidx[i],3]) )
  features["cr_yz"] <- sapply( 1:length(sidx), function(i) cor(raw_data[sidx[i]:eidx[i],2], raw_data[sidx[i]:eidx[i],3]) )                  
  
  
  # 10) Autocorrelation
  # ?acf()
  print("Features group 9 ...")
  features["acf_x"] <- sapply(1:length(sidx), function(i) acf( raw_data[sidx[i]:eidx[i],1], lag.max = 1, plot = FALSE, na.action = na.pass)$acf[,,1][2] )
  features["acf_y"] <- sapply(1:length(sidx), function(i) acf( raw_data[sidx[i]:eidx[i],2], lag.max = 1, plot = FALSE, na.action = na.pass)$acf[,,1][2] )
  features["acf_z"] <- sapply(1:length(sidx), function(i) acf( raw_data[sidx[i]:eidx[i],3], lag.max = 1, plot = FALSE, na.action = na.pass)$acf[,,1][2] )
  
  
  # 11) Skewness, measure of asymmetry of the singal probabilty distribution
  #
  print("Features group 10 ...")
  features["skw_x"] <- sapply(1:length(sidx), function(i) skewness(raw_data[sidx[i]:eidx[i],1], na.rm = TRUE) )
  features["skw_y"] <- sapply(1:length(sidx), function(i) skewness(raw_data[sidx[i]:eidx[i],2], na.rm = TRUE) )
  features["skw_z"] <- sapply(1:length(sidx), function(i) skewness(raw_data[sidx[i]:eidx[i],3], na.rm = TRUE) )

  
  # 12) Kurtosis, degree of the peakedness of the signal probability distribution
  #
  print("Features group 11 ...")
  features["krt_x"] <- sapply(1:length(sidx), function(i) kurtosis(raw_data[sidx[i]:eidx[i],1], na.rm = TRUE) )
  features["krt_y"] <- sapply(1:length(sidx), function(i) kurtosis(raw_data[sidx[i]:eidx[i],2], na.rm = TRUE) )
  features["krt_z"] <- sapply(1:length(sidx), function(i) kurtosis(raw_data[sidx[i]:eidx[i],3], na.rm = TRUE) )

  
  # 13) Signal power
  # 
  print("Features group 12 ...")
  features["snp_x"] <- sapply(1:length(sidx), function(i) sum( raw_data[sidx[i]:eidx[i],1] ^ 2 ))
  features["snp_y"] <- sapply(1:length(sidx), function(i) sum( raw_data[sidx[i]:eidx[i],2] ^ 2 ))
  features["snp_z"] <- sapply(1:length(sidx), function(i) sum( raw_data[sidx[i]:eidx[i],3] ^ 2 ))

  
  # 14) Sum Log-energy
  #
  print("Features group 13 ...")
  features["sle_x"] <- sapply(1:length(sidx), function(i) sum( log( raw_data[sidx[i]:eidx[i],1] ^ 2 + 1 ) ) )
  features["sle_y"] <- sapply(1:length(sidx), function(i) sum( log( raw_data[sidx[i]:eidx[i],2] ^ 2 + 1 ) ) )
  features["sle_z"] <- sapply(1:length(sidx), function(i) sum( log( raw_data[sidx[i]:eidx[i],3] ^ 2 + 1 ) ) )
  
  # which(features[,"sle_x"] == -Inf)
  # features[5921,"sle_x"]
  # i <- 5921
  # sum( log( raw_data[sidx[i]:eidx[i],1] ^ 2 + 1 ) )
  # log
  # raw_data[i + 48,1]
  
  # 15) Peak intensity: number of the signal peak apperances within a certain period of time
  #
  print("Features group 14 ...")
  features["pin_x"] <- sapply(1:length(sidx), function(i) length( which( raw_data[sidx[i]:eidx[i],1] == max(raw_data[sidx[i]:eidx[i],1]) ) ) )
  features["pin_y"] <- sapply(1:length(sidx), function(i) length( which( raw_data[sidx[i]:eidx[i],2] == max(raw_data[sidx[i]:eidx[i],2]) ) ) )
  features["pin_z"] <- sapply(1:length(sidx), function(i) length( which( raw_data[sidx[i]:eidx[i],3] == max(raw_data[sidx[i]:eidx[i],3]) ) ) )
  
  
  # 16) Zero crossings: number of times the signal crosses its median
  #
  print("Features group 15 ...")
  features["zer_x"] <- sapply(1:length(sidx), function(i) length( which( raw_data[sidx[i]:eidx[i],1] == median(raw_data[sidx[i]:eidx[i],1]) ) ) )
  features["zer_y"] <- sapply(1:length(sidx), function(i) length( which( raw_data[sidx[i]:eidx[i],2] == median(raw_data[sidx[i]:eidx[i],2]) ) ) )
  features["zer_z"] <- sapply(1:length(sidx), function(i) length( which( raw_data[sidx[i]:eidx[i],3] == median(raw_data[sidx[i]:eidx[i],3]) ) ) )
  
  
  #
  # Frequency domain
  #
  # 17) & 18) Dominant frequency & Amplitude of dominant frequency
  #
  print("Features group 16 & 17 ...")
  DomFreq <- function(i, col) {
    FT <- fft(raw_data[sidx[i]:eidx[i], col])
    return(max(Re(FT ^ 2)))
  }
  AmplDomFreq <- function(i, col) {
    FT <- fft(raw_data[sidx[i]:eidx[i], col])
    idx <- which.max(Re(FT ^ 2))
    return(Re(FT[idx]))
  }
  # for (b in 1:length(sidx)) {
  #   print(paste("Doing for ", b))
  #   DomFreq(b, 1)
  #   
  # }
  #features <- data.frame(ID = 1:length(sidx))
  
  features["dmf_x"] <- sapply(1:length(sidx), DomFreq, col = 1)
  features["dmf_y"] <- sapply(1:length(sidx), DomFreq, col = 2)
  features["dmf_z"] <- sapply(1:length(sidx), DomFreq, col = 3)
  
  features["adf_x"] <- sapply(1:length(sidx), AmplDomFreq, col = 1)
  features["adf_y"] <- sapply(1:length(sidx), AmplDomFreq, col = 2)
  features["adf_z"] <- sapply(1:length(sidx), AmplDomFreq, col = 3)

  
  # 19) Entropy
  #  
  print("Features group 18 ...")
  features["ent_x"] <- sapply(1:length(sidx), function(i) entropy(raw_data[sidx[i]:eidx[i],1]))
  features["ent_y"] <- sapply(1:length(sidx), function(i) entropy(raw_data[sidx[i]:eidx[i],2]))
  features["ent_z"] <- sapply(1:length(sidx), function(i) entropy(raw_data[sidx[i]:eidx[i],3]))
  
  
  # 20) Vector magnitude features
  #
  print("Features group 19 ...")
  features["vector_mag"]   <- sapply(1:length(sidx), function(i) sqrt( sum (raw_data[sidx[i]:eidx[i],1]^2 + raw_data[sidx[i]:eidx[i],2]^2 + raw_data[sidx[i]:eidx[i],3]^2) ) )
  features["vector_mag_g"] <- sapply(1:length(sidx), function(i) sqrt( sum (raw_data[sidx[i]:eidx[i],1]^2 + raw_data[sidx[i]:eidx[i],2]^2 + raw_data[sidx[i]:eidx[i],3]^2) )  -9.81 )
  features["vector_mean"] <- sqrt(features$mean_x^2 + features$mean_y^2 + features$mean_z^2)

  # 7) Percentiles  
  print("Features group 20 ...")
  features["percentile"] <- ntile(features$vector_mag, 5)
  
  
  # Demographic and anthropometric features
  # Age, height, weight, BMI, gender, genetics, occupation, education 
  # Body composition: fat mass, fat-free mass
  
  
  return(features)
}

#
#
# N <- 100
# time_period <- 1 # 1 second
# SUB <- seq.int(1,100,25) # choosing four signals per second
# RND <- sample(c(1:24), size = 4) # choosing a random signal at each quantile
# SUB <- SUB + RND
# raw_data <- df_raw
# i <- 1
# data_window <- raw_data[sidx[i]:eidx[i],]
# data_window <- data_window[SUB,]
#
#
# top_1 <- sum( (data_window[,1] - mean_1) * (data_window[,2] - mean_2) )
# dwn_1 <- sqrt( sum( (data_window[,1] - mean_1)^2 ) * sum ( (data_window[,2] - mean_2)^2 ) )
# cor_1 <- top_1 / dwn_1
# CorrAccel <- function(i, col1, col2) {
#   top <- sum( (raw_data[sidx[i]:eidx[i], col1] - mean(raw_data[sidx[i]:eidx[i], col1])) *  (raw_data[sidx[i]:eidx[i], col2] - mean(raw_data[sidx[i]:eidx[i], col2])) )
#   dwn <- sqrt( sum( (raw_data[sidx[i]:eidx[i], col1] - mean(raw_data[sidx[i]:eidx[i], col1]))^2) * sum( (raw_data[sidx[i]:eidx[i], col2] - mean(raw_data[sidx[i]:eidx[i], col2]))^2) )
#   return(top/dwn)
# }
# 
# cr_12 <- sapply(1:length(sidx), CorrAccel, col1 = 1, col2 = 2)
# cr_13 <- sapply(1:length(sidx), CorrAccel, col1 = 1, col2 = 3)
# cr_23 <- sapply(1:length(sidx), CorrAccel, col1 = 2, col2 = 3)
# 
#
#
# Skew <- function(i, col) {
#   D <- raw_data[sidx[i]:eidx[i], col]
#   top <- sum( (D - mean(D) ) ^ 4 ) / length(D)
#   dwn <- ( sum( (D - mean(D) ) ^ 2 ) / length(D) ) ^ 3 
#   return(top/dwn)
# }
#
#
# pin_1 <- length( which( data_window[,1] == max(data_window[,1]) ) ) / nrow(data_window[,1])
# pin_2 <- length( which( data_window[,2] == max(data_window[,2]) ) ) / nrow(data_window[,2])
# pin_3 <- length( which( data_window[,3] == max(data_window[,3]) ) ) / nrow(data_window[,3])
#
# write.table(pin_1, file = "pin1.txt")
# pin_4 <- sapply(1:length(sidx), function(i) length( which( abs(raw_data[sidx[i]:eidx[i],1]) == max(abs(raw_data[sidx[i]:eidx[i],1])) ) ) )
# write.table(pin_4, file = "pin4.txt")
# plot(pin_1)
# plot(pin_4)
# length(which(pin_1 != pin_4))
# ??sinus
#
#
# 
#FT <- FT^2
#idx1 <- which.max(Re(FT[,1]))
#dom_freq_1 <- Re(FT[idx1,1])
#dom_ampl_1 <- Im(FT[idx1,1])
#head(FT)
#head(raw_power_spectrum)
# 
# FT <- sapply(data_window, fft)
# raw_power_spectrum <- FT ^ 2
# idx1 <- which.max(Re(raw_power_spectrum[,1]))
# dominant_frequency_1 <- Re(raw_power_spectrum[idx1,1])
# amplitude_dominant_frequency_1 <- Re(FT[idx1,1])
# 
# idx1 <- which.max(Re(raw_power_spectrum[,2]))
# dominant_frequency_2 <- Re(raw_power_spectrum[idx1,2])
# amplitude_dominant_frequency_2 <- Re(FT[idx1,2])
# 
# idx1 <- which.max(Re(raw_power_spectrum[,3]))
# dominant_frequency_3 <- Re(raw_power_spectrum[idx1,3])
# amplitude_dominant_frequency_3 <- Re(FT[idx1,3])
#
#
#
# 17) Dominant frequency: applying Fast Fourier Transform (FFT), on the original sensor signals
# values of the coefficients represent the amplitudes of the corresponding frequency components
# the coefficient with the largest amplitude corresponds to the dominant frequency of the signal  
# 
# Firstly, the power spectral density (P) of the acceleration was used to
# define the harmonic content of the signal. P was calculated using the fast Fourier
# transform algorithm on the acceleration signal of each segment. 
#
# The Fourier transform maps the data into the complex number domain. 
# We can look at its real part and imaginary part, the amplitudes of frequencies and the phases of frequencies.
# The squares of amplitudes are called the power spectrum or periodogram.
# The power spectrum of this time series has one pronounced peak clearly showing the dominant frequency
#

