gfortran -c s/linkedList.f90 -o executable/linkedList.o
gfortran -c s/linkedList2.f90 -o executable/linkedList2.o
gfortran -c s/circleList.f90 -o executable/circleList.o
gfortran -c s/listaVentanas.f90 -o executable/listaVentanas.o
gfortran -c s/cliente.f90 -o executable/cliente.o
gfortran -c s/ventanilla.f90 -o executable/ventanilla.o
gfortran -c s/objetoRelacion.f90 -o executable/objetoRelacion.o
gfortran -c s/listaAtendidos.f90 -o executable/listaAtendidos.o
gfortran -c s/listaImgGrande.f90 -o executable/listaImgGrandes.o
gfortran -c s/listaImgPequena.f90 -o executable/listaImgPequena.o
gfortran -c s/objetoIMG.f90 -o executable/objetoIMG.o
gfortran -c s/listaVentanasMovimientos.f90 -o executable/listaVentanasMovimientos.o
gfortran -c main.f90 -o executable/main.o

gfortran -o executable/myapp executable/cliente.o executable/objetoRelacion.o executable/linkedList2.o executable/circleList.o executable/objetoIMG.o executable/listaImgGrandes.o executable/listaImgPequena.o executable/listaAtendidos.o executable/listaVentanasMovimientos.o executable/linkedList.o executable/ventanilla.o executable/listaVentanas.o executable/main.o

executable\myapp