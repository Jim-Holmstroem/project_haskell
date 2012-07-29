import Digit

n = 99
main = print . maximum . map (sum.digits) $ [a^b|a<-[1..n],b<-[1..n]]

