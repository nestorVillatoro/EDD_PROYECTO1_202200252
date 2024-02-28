gfortran -c s/linkedList.f90 -o executable/linkedList.o
gfortran -c s/cliente.f90 -o executable/cliente.o
gfortran -c main.f90 -o executable/main.o

gfortran -o executable/myapp executable/cliente.o executable/linkedList.o executable/main.o

executable\myapp