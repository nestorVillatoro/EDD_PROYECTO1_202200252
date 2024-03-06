module listaImgGrande
    use objetoIMG
    implicit none 
    
    type :: linked_list_img_grande 
    
      type(node), pointer :: head => null() ! head of the list
  
      contains
        procedure :: pushImgG
        procedure :: printImgG 

    end type linked_list_img_grande 
       
  
    type :: node
      type(IMGs) :: value
      type(node), pointer :: next
    end type node


    contains
  
    subroutine pushImgG(self, value) 
      class(linked_list_img_grande), intent(inout) :: self
      type(IMGs), intent(in) :: value
  
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

    end subroutine pushImgG
  
    subroutine printImgG(self)
      class(linked_list_img_grande), intent(in) :: self
  
      type(node), pointer :: current
  
      current => self%head
  
      do while (associated(current))
        print*, "ID", current%value%ID
        print *, "ID Cliente: ", current%value%id_cliente
        print*, "Tipo: ", current%value%tipo
        print*, ""

        current => current%next
      end do
    end subroutine printImgG
  

    subroutine verificarElementosG(self, verificarEstado)
      class(linked_list_img_grande), intent(inout) :: self
      logical::verificarEstado
      if (.not. associated(self%head)) then
        verificarEstado = .false.
      else
        verificarEstado = .true.
      end if
      end subroutine verificarElementosG

      subroutine verificarEstadoDeImgG(self, verificarEstadoElementos)
        class(linked_list_img_grande), intent(in) :: self
          type(node), pointer :: current
          logical:: verificarEstadoElementos
  
        current => self%head
    
        do while (associated(current))
          if(current%value%impreso.eqv..false.) then
              verificarEstadoElementos = .false.
              exit 
          else
              verificarEstadoElementos = .true.
          end if
          current => current%next
        end do
        end subroutine verificarEstadoDeImgG

      
    subroutine imprimirG(self, ID, tipo)
      class(linked_list_img_grande), intent(in) :: self
      integer::contadorDeOrden = 1
      integer:: ID
      character(len=*):: tipo
      type(node), pointer :: current
  
      current => self%head
  
      do while (associated(current))
        if((current%value%impreso.eqv..false.).and.(current%value%ID.eq.contadorDeOrden))then
          current%value%pasos = current%value%pasos-1
          if(current%value%pasos.eq.0) then
            current%value%impreso=.true.
            ID = current%value%id_cliente
            tipo = current%value%tipo
            print*, "La imagen grande del cliente numero ", current%value%id_cliente, "ha sido impresa"
            contadorDeOrden = contadorDeOrden+1
            
          end if
          exit
        end if
        current => current%next
      end do
    end subroutine imprimirG
  
    
  end module listaImgGrande
  
    