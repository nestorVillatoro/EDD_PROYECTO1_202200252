program menu
    
    implicit none
    integer :: ops
    
    do while(.true.)
    print*, "--------------------MENU--------------------"
    print*, "|  1. Parametros iniciales                 |"
    print*, "|  2. Ejecutar paso                        |"
    print*, "|  3. Estado en memoria de las estructuras |"
    print*, "|  4. Reportes                             |"
    print*, "|  5. Acerca de                            |"
    print*, "|  6. Salir                                |"
    print*, "--------------------------------------------"
    print*, "Ingrese una opcion: "
    read(*,*) ops
    if(ops.eq.1)then
        call parametrosIniciales()
    else if(ops.eq.2) then 
        call ejecutarPaso()
    else if(ops.eq.3) then
        call estadoDeMemoria()
    else if(ops.eq.4) then
        call reportes()
    else if(ops.eq.5) then 
        call acercaDe() 
    else if(ops.eq.6) then
        exit
    else 
        print*, "Opcion no valida"
    end if
    enddo
end program menu

subroutine parametrosIniciales()
use cliente 
use linkedList
implicit none
type(Clientes):: nuevoCliente 
type(linked_list):: mylist
integer :: opsPI
print*, "------------PARAMETROS INICIALES------------"
print*, "|  1. Regresar                             |"
print*, "|  2. Carga masiva de clientes             |"
print*, "|  3. Cantidad de ventanillas              |" 
print*, "--------------------------------------------"
print*, "Ingrese una opcion: " 
read(*,*) opsPI
select case(opsPI)
case(1)
case(2)
    call inicializar(nuevoCliente, 1, "Nestor Villatoro", 3, 2)
    call mylist%push(nuevoCliente) 
    call mylist%print()
case(3)
end select
call siguiente()
end subroutine parametrosIniciales
subroutine ejecutarPaso()
implicit none
integer :: opsEP
print*, "----------------EJECUTAR PASO----------------"
print*, "|  1. Regresar                              |"
print*, "---------------------------------------------"
print*, "Ingrese una opcion: "
read(*,*) opsEP
select case(opsEP)
case(1)
case(2)
case(3)
end select
call siguiente()
end subroutine ejecutarPaso 

subroutine estadoDeMemoria()
implicit none
integer :: opsEM
print*, "--------------ESTADO DE MEMORIA--------------"
print*, "|  1. Regresar                              |"
print*, "---------------------------------------------"
print*, "Ingrese una opcion: "
read(*,*) opsEM
select case(opsEM)
case(1)
case(2)
case(3)
end select
call siguiente()
end subroutine

subroutine reportes()
implicit none
integer :: opsR
print*, "-------------------REPORTE-------------------"
print*, "|  1. Regresar                              |"
print*, "---------------------------------------------"
print*, "Ingrese una opcion: "
read(*,*) opsR
select case(opsR)
case(1)
case(2)
case(3)
end select
call siguiente() 
end subroutine

subroutine acercaDe()
print*, "-----------------ACERCA DE-----------------"
print*, "|  Nombres: Nestor Enrique                |"
print*, "|  Apellidos: Villatoro Avendanio         |"
print*, "|  Carnet: 202200252                      |"
print*, "|  CUI: 3014408320101                     |"
print*, "|  Carrera: Ing en ciencias y sistemas    |"
print*, "|  Curso: Lab estructura de datos         |"
print*, "|  Semestre: 5to semestre                 |"
print*, "-------------------------------------------"
call siguiente()
end subroutine

subroutine siguiente()
print*, "Presione enter para continuar"
read(*,*) 
end subroutine