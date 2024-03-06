module listaVentanasMovimientos
    use objetoRelacion
    implicit none 
  
    type :: lista_ven_mov 
      type(node), pointer :: head => null() ! head of the list
  
      contains
        procedure :: push
        procedure :: print
    end type lista_ven_mov
  
    type :: node
      type(objRelacion) :: value
      type(node), pointer :: next
    end type node
  
    contains
  
    subroutine push(self, value)
      class(lista_ven_mov), intent(inout) :: self
      type(objRelacion), intent(in) :: value
  
      type(node), pointer :: newNode
      allocate(newNode)
  
      newNode%value = value
      newNode%next => null()
  
      if (.not. associated(self%head)) then
        self%head => newNode
      else
        newNode%next => self%head
        self%head => newNode
      end if
    end subroutine push
  
    subroutine print(self)
      class(lista_ven_mov), intent(in) :: self
  
      type(node), pointer :: current
  
      current => self%head
  
      do while (associated(current))
        print *, "ID: ", current%value%id_cliente
        print*, "Nombre: ", current%value%nombre
        print*, "img_g: ", current%value%img_g_cliente
        print*, "img_p: ", current%value%img_p_cliente
        print*, "total ", current%value%totalImg
        print *, "ID Ventanilla: ", current%value%id_ventanilla
        print*, "____________________________________________________"
        current => current%next
      end do
    end subroutine print

    subroutine moverImg(self)
        class(lista_ven_mov), intent(in) :: self
  
        type(node), pointer :: current
        character(len=100)::str_num
  
      current => self%head
  
      do while (associated(current))
        if(current%value%totalImg .gt. 0) then
            if(current%value%verify.eqv. .true.) then
            current%value%totalImg = current%value%totalImg-1
            write(str_num, '(I0)') current%value%id_ventanilla
            print*, "Se ha movido una imagen del cliente ", trim(current%value%nombre), " a la ventanilla No. ", trim(str_num)
            write(str_num, '(I0)') current%value%totalImg
            print*, "Ahora tiene ", trim(str_num), " imagenes pendientes"
            print*, ""
            else 
                current%value%verify = .true.
            end if
            
        end if
        current => current%next
      end do
    end subroutine moverImg



    subroutine liberarVentanilla(self, numV, IdC, name, ig, ip)
        class(lista_ven_mov), intent(in) :: self
  
        type(node), pointer :: current
        integer:: numV, IdC, ig, ip 
        character(len=*):: name
  
      current => self%head
  
      do while (associated(current))
        if((current%value%totalImg .eq. 0) .and. (current%value%verify2 .eqv. .true.))then
            current%value%verify2 = .false.
            numV = current%value%id_ventanilla 
            IdC = current%value%id_cliente
            name = current%value%nombre
            ig = current%value%img_g_cliente
            ip = current%value%img_p_cliente
            exit
        end if
        current => current%next
      end do
    end subroutine liberarVentanilla
      
  end module listaVentanasMovimientos
  
    