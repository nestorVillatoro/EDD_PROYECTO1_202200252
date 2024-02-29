program menu
    use cliente 
    use linkedList
    use listaVentanas
    use ventanilla
    implicit none
    integer :: ops
    integer cant
    integer i
    type(Clientes):: nuevoCliente 
    type(Ventanillas):: nuevaVentanilla
    type(linked_list):: mylist
    type(lista_ventanas):: listaVentanillas
    integer :: opsPI
    character(len=1):: pasar
    integer :: opsEP
    logical:: ver
    logical:: verVenLib
    integer:: contPaso
    contPaso = 0


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



        print*, "------------PARAMETROS INICIALES------------"
        print*, "|  1. Regresar                             |"
        print*, "|  2. Carga masiva de clientes             |" 
        print*, "|  3. Cantidad de ventanillas              |" 
        print*, "|  4. Imprimir clientes                    |" 
        print*, "|  5. Imprimir ventanillas                 |" 
        print*, "--------------------------------------------"
        print*, "Ingrese una opcion: " 
        read(*,*) opsPI
        select case(opsPI)
        case(1)
        case(2)
            call Cinicializar(nuevoCliente, 1, "Nestor Villatoro", 3, 2)
            call mylist%push(nuevoCliente)
            call Cinicializar(nuevoCliente, 2, "Enrique Avendanio", 4, 3)
            call mylist%push(nuevoCliente)
            call Cinicializar(nuevoCliente, 3, "Nestor Avendanio", 1, 1)
            call mylist%push(nuevoCliente)
            call Cinicializar(nuevoCliente, 4, "Enrique Villatoro", 3, 0)
            call mylist%push(nuevoCliente)
        case(3)
            print*, "Ingrese la cantidad de ventanillas: "
            read(*,*) cant
            do i = 1, cant
                call Vinicializar(nuevaVentanilla, i, .false., 0, 0) 
                call listaVentanillas%pushV(nuevaVentanilla)
            enddo
        case(4)
            call  mylist%print()
        case(5)
            call listaVentanillas%printV()
        end select
        call siguiente()



    else if(ops.eq.2) then 
        
        

        print*, "----------------EJECUTAR PASO----------------"
        print*, "|  1. Regresar                              |"
        print*, "|  2. Ejecutar pasos                        |"
        print*, "---------------------------------------------"
        print*, "Ingrese una opcion: "
        read(*,*) opsEP
        select case(opsEP)
        case(1)
        case(2)

            do while(.true.)
                print*, "Desea continuar con los pasos? Y/N "
                read(*,*) pasar
                if(pasar.eq."Y") then
                    contPaso = contPaso+1
                    print*, "=============Paso ", contPaso, "============="
                    call verificarElementos(mylist, ver)
                    if(ver .eqv. .true.) then
                        call verificarEspacio(listaVentanillas, verVenLib) 
                        if(verVenLib.eqv..true.)then 
                        end if    
                    else  
                        print*, "Ya no hay clientes en espera"
                    end if
                else if(pasar.eq."N") then
                    exit
                else 
                    print*, "Opcion no valida"
                end if
                print*, "==============================="
            enddo

        end select
        call siguiente()



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
end subroutine estadoDeMemoria

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
end subroutine reportes

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
end subroutine acercaDe

subroutine siguiente()
print*, "Presione enter para continuar"
read(*,*) 
end subroutine siguiente

subroutine filaVentanilla()
    use listaVentanas
    use linkedList
implicit none
type(linked_list):: ver
logical:: TorF
    call verificarElementos(ver, TorF)
    if(TorF .eqv. .true.) then
    else 
        print*, "Ya no hay clientes en espera"
    end if
end subroutine filaVentanilla