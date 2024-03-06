module linkedList2
    implicit none
    private
  
    ! Definición del tipo nodo
    type node
        private
        character(len=100) :: value ! Cambio a cadena
        type(node), pointer :: next => null()
    end type node
  
    ! Definición del tipo lista
    type, public :: linked_list2
        private
        type(node), pointer :: head => null()
        
        contains
          procedure :: pushS
          procedure :: delete_by_position
          procedure :: delete_by_value
          procedure :: printS
    end type linked_list2
  
    contains
  
    ! Funciones
    subroutine pushS(self, value)
      class(linked_list2), intent(inout) :: self
      character(len=*), intent(in) :: value ! Cambio a cadena
    
      type(node), pointer :: newNode
      allocate(newNode)
      newNode%value = value
      newNode%next => null()
  
      if(.not. associated(self%head)) then
          self%head => newNode
      else
           newNode%next => self%head
          self%head => newNode
      end if      
  
    end subroutine pushS
      
    subroutine delete_by_position(self, position)
      class(linked_list2), intent(inout) :: self
      integer, intent(in) :: position
      type(node), pointer :: current
      type(node), pointer :: previous
      integer :: i
  
      current => self%head
      previous => null()
  
      if(.not. associated(current)) then
          print *, "La lista está vacía"
          return
      end if
  
      if(position == 1) then
          self%head => current%next
          deallocate(current)
          return
      end if
  
      i = 1
      do while(associated(current) .and. i < position)
          previous => current
          current => current%next
          i = i + 1
      end do
  
      if(.not. associated(current)) then
          print *, "La posicion ", position, " no existe"
          return
      end if
  
      previous%next => current%next
      deallocate(current)
    end subroutine delete_by_position
  
    subroutine delete_by_value(self, value)
      class(linked_list2), intent(inout) :: self
      character(len=*), intent(in) :: value
      type(node), pointer :: current
      type(node), pointer :: previous
  
      current => self%head
      previous => null()
  
      if(.not. associated(current)) then
          print *, "La lista está vacía"
          return
      end if
  
      do while(associated(current) .and. current%value /= value)
          previous => current
          current => current%next
      end do
  
      if(.not. associated(current)) then
          print *, "El valor ", value
          return
      end if
  
      if(.not. associated(previous)) then
          self%head => current%next
      else
          previous%next => current%next
      end if
  
      deallocate(current)
    end subroutine delete_by_value
    
    subroutine printS(self)
        class(linked_list2), intent(inout) :: self
        type(node), pointer :: current
        character(len=:), allocatable :: result
        result = "["
        current => self%head
  
        do while(associated(current))
          result = result // trim(current%value) // ", "
          current => current%next
        end do
  
        result = result // "]"
        print *, trim(adjustl(result))
    end subroutine
  
  end module linkedList2
  