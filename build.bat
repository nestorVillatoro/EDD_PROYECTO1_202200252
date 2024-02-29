gfortran -c s/linkedList.f90 -o executable/linkedList.o
gfortran -c s/listaVentanas.f90 -o executable/listaVentanas.o
gfortran -c s/cliente.f90 -o executable/cliente.o
gfortran -c s/ventanilla.f90 -o executable/ventanilla.o
gfortran -c main.f90 -o executable/main.o

gfortran -o executable/myapp executable/cliente.o executable/linkedList.o executable/ventanilla.o executable/listaVentanas.o executable/main.o

executable\myapp