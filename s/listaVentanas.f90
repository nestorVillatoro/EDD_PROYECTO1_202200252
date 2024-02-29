module listaVentanas
    use ventanilla
    implicit none
  
    type :: lista_ventanas
      type(node), pointer :: head => null() ! head of the list
  
      contains
        procedure :: pushV
        procedure :: printV
        procedure :: verificarEspacio
    end type lista_ventanas
  
    type :: node
      type(Ventanillas) :: value
      type(node), pointer :: next
    end type node
  
    contains
  
    subroutine pushV(self, value)
      class(lista_ventanas), intent(inout) :: self
      type(Ventanillas), intent(in) :: value
  
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
  
      print *, 'pushed:: ', value%id
    end subroutine pushV
  
    subroutine printV(self)
      class(lista_ventanas), intent(in) :: self
  
      type(node), pointer :: current
  
      current => self%head
  
      do while (associated(current))
        print *, "ID: ", current%value%id 
        print*, "Estado: ", current%value%verificador
        print*, "img_g: ", current%value%img_g
        print*, "img_p: ", current%value%img_p
        current => current%next
      end do
    end subroutine printV

    subroutine verificarEspacio(self, verificarVentanillaLibre)
        class(lista_ventanas), intent(in) :: self
        logical:: verificarVentanillaLibre
        type(node), pointer :: current
  
      current => self%head
  
      do while (associated(current))
        if(current%value%verificador .eqv. .false.) then
            verificarVentanillaLibre = .true.
            current%value%verificador = .true.
            exit
        else
            verificarVentanillaLibre = .false.
            print*, "No hay ventanillas disponibles"
        end if
        current => current%next
      end do
    end subroutine
  end module listaVentanas