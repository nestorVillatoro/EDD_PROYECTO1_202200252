module linkedList
  use cliente 
  implicit none

  type :: linked_list
    type(node), pointer :: head => null() ! head of the list

    contains
      procedure :: push
      procedure :: print
      procedure :: verificarElementos
  end type linked_list

  type :: node
    type(Clientes) :: value
    type(node), pointer :: next
  end type node

  contains

  subroutine push(self, value)
    class(linked_list), intent(inout) :: self
    type(Clientes), intent(in) :: value

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

    print *, 'pushed:: ', value%nombre
  end subroutine push

  subroutine print(self)
    class(linked_list), intent(in) :: self

    type(node), pointer :: current

    current => self%head

    do while (associated(current))
      print *, "ID: ", current%value%id 
      print*, "Nombre: ", current%value%nombre
      print*, "img_g: ", current%value%img_g
      print*, "img_p: ", current%value%img_p
      current => current%next
    end do
  end subroutine print

  subroutine verificarElementos(self, verificarEstado)
    class(linked_list), intent(inout) :: self
    logical::verificarEstado
    if (.not. associated(self%head)) then
      verificarEstado = .false.
    else
      verificarEstado = .true.
    end if
    end subroutine verificarElementos
    
end module linkedList

  