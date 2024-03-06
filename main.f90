program menu
    use cliente  
    use linkedList
    use listaVentanas
    use listaVentanasMovimientos
    use ventanilla
    use objetoRelacion
    use listaAtendidos  
    use objetoIMG
    use listaImgPequena
    use listaImgGrande
    use circleList
    implicit none
    character(len=1000)::str_num 
    type(Clientes):: nuevoCliente 
    type(Ventanillas):: nuevaVentanilla
    type(linked_list):: mylist  
    type(linked_list_atendidos):: lista_atendidos
    type(lista_ventanas):: listaVentanillas
    type(lista_ven_mov):: listaVentanillasMov
    type(objRelacion):: relacion
    type(IMGs):: objImg
    type(linked_list_img_pequena):: listaDeImgPequenas
    type(linked_list_img_grande):: listaDeImgGrandes
    type(circle_list):: cl
    integer :: opsPI, contPaso, NoVen, idClieCam, imgG, imgP, i, cant, Timg, ops, numV, IdC, ig, ip, j, k
    integer:: contImgG, contImgP, IDLE
    character(len=1):: pasar
    logical:: ver, verVenLib, ver2, verG, verP, verG2, verP2
    character(len=40):: ClieCam, name, tipo
    contPaso = 0
    contImgG = 0
    contImgP = 0

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
            call Cinicializar(nuevoCliente, .false., 1, "Cliente 1", 3, 2)
            call mylist%push(nuevoCliente)
            call Cinicializar(nuevoCliente, .false.,  2, "Cliente 2", 4, 3) 
            call mylist%push(nuevoCliente)
            call Cinicializar(nuevoCliente, .false., 3, "cliente 3", 1, 2)
            call mylist%push(nuevoCliente)
            call Cinicializar(nuevoCliente, .false., 4, "cliente 4", 3, 0)
            call mylist%push(nuevoCliente)
            call Cinicializar(nuevoCliente, .false., 5, "Cliente 5", 3, 2)
            call mylist%push(nuevoCliente)
            call Cinicializar(nuevoCliente, .false.,  6, "Cliente 6", 4, 3) 
            call mylist%push(nuevoCliente)
            call Cinicializar(nuevoCliente, .false., 7, "cliente7", 1, 2)
            call mylist%push(nuevoCliente)
            call Cinicializar(nuevoCliente, .false., 8, "cliente 8", 3, 0)
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

 
            do while(.true.)
                print*, "Desea continuar con los pasos? Y/N "
                read(*,*) pasar

                if(pasar.eq."Y") then
                    contPaso = contPaso+1


                    
                    write(str_num, '(I0)') contPaso

                    print*, "===============Paso ", trim(str_num), "==============="

                    IDLE = 0 
                    tipo="Null"
                    call verificarElementosG(listaDeImgGrandes, verG)
                    if(verG .eqv. .true.) then
                    call verificarEstadoDeImgG(listaDeImgGrandes, verG2)
                        if(verG2 .eqv. .false.) then
                         call imprimirG(listaDeImgGrandes, IDLE, tipo) 
                         if((.not.(IDLE.eq.0)) .and. (.not.(tipo.eq."Null"))) then
                            call cl%add(IDLE, tipo)
                            call atender(lista_atendidos, IDLE, tipo)
                         end if
                        end if
                    end if

                    IDLE = 0
                    tipo="Null"
                call verificarElementosP(listaDeImgPequenas, verP)
                    if(verP .eqv. .true.) then
                        call verificarEstadoDeImgP(listaDeImgPequenas, verP2)
                        if(verP2 .eqv. .false.) then
                        call imprimirP(listaDeImgPequenas, IDLE, tipo) 
                         if((.not.(IDLE.eq.0)) .and. (.not.(tipo.eq."Null"))) then
                            call cl%add(IDLE, tipo)
                            call atender(lista_atendidos, IDLE, tipo)
                            print*, "Se agrego una imagen pequena al usuario No. ", IDLE
                         end if
                        end if
                    end if



                    call verificarElementos(mylist, ver)
                    if(ver .eqv. .true.) then
                    call verificarEstadoDeElementos(mylist, ver2)
                    if(ver2 .eqv. .false.) then
                        call verificarEspacio(listaVentanillas, verVenLib, NoVen)
                        if(verVenLib.eqv..true.)then 
                            call cambiarEstadoCliente(mylist, ClieCam, idClieCam, imgP, imgG)
                            print*, ""
                            write(str_num, '(I0)') NoVen
                            print*, "Cliente ", trim(ClieCam), " agregado a ventanilla No. ", trim(str_num)
                            Timg = imgP + imgG 
                            write(str_num, '(I0)') Timg
                            print*, "El cual tiene ", trim(str_num), " imagenes en total"
                            print*, ""
                            call Oinicializar(relacion, idClieCam, ClieCam, imgG, imgP, NoVen)
                            call listaVentanillasMov%push(relacion)

                        else 
                            print*, "No hay ventanillas disponibles"
                            print*, ""
                    end if    
                    else
                        print*, "Ya no hay clientes en espera"
                end if
                end if
                numV = 0
                IdC = 0
                call moverImg(listaVentanillasMov)
                do i = 1, cant
                call liberarVentanilla(listaVentanillasMov, numV, IdC, name, ig, ip) 
                if((.not.(numV .eq. 0)) .and. (.not.(IdC.eq.0))) then
                call cambiarEstadoVentanilla(listaVentanillas, numV)
                call verificarContadores(mylist, IdC)
                call Cinicializar(nuevoCliente, .false., IdC, name, ig, ip)
                call pushA(lista_atendidos, nuevoCliente)
                do j = 1, ig
                    contImgG = contImgG+1
                    call IMGinicializar(objImg, contImgG, IdC, "grande", 2)
                    call pushImgG(listaDeImgGrandes, objImg)
                enddo

                do k = 1, ip
                    contImgP = contImgP+1
                    call IMGinicializar(objImg, contImgP, IdC, "pequena", 1)
                    call pushImgP(listaDeImgPequenas, objImg)
                enddo
                end if

                numV = 0
                IdC = 0
                enddo
                

                else if(pasar.eq."N") then
                    exit
                
                else if(pasar.eq."P") then
                    print*, "=======================LISTA DE VENTANILLAS======================="
                    call listaVentanillasMov%print()
                    print*, "====================LISTA DE CLIENTES ATENDIDOS==================="
                    call lista_atendidos%printA()
                    print*, "=====================LISTA DE IMPRESORA GRANDE===================="
                    call listaDeImgGrandes%printImgG()
                    print*, "===================LISTA DE IMPRESORA PEQUENIA===================="
                    call listaDeImgPequenas%printImgP()
                    print*, "=========================LISTA DE ESPERA=========================="
                    call cl%print_cl()
                else 
                    print*, "Opcion no valida"

                end if
                print*, "========================================="


            enddo

        call siguiente()



    else if(ops.eq.3) then
        call print_dot(cl, "listaEspera.dot")
    else if(ops.eq.4) then
        call mostrarClientesConMas(lista_atendidos) 
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