

rm(list = ls())

args <- commandArgs(TRUE)

args<-c("F", "nivel1", "Retiro Programado", "Renta Vitalicia Inmediata simple", "b")

gender<-args[1]   ## Género
econ<-args[2]    ## SES
mode1Q<-args[3] ## primera selección modalidad
mode2Q<-args[4] ## segunda selección modalidad
pg<-args[5]

source("Treatment-Script.R")
