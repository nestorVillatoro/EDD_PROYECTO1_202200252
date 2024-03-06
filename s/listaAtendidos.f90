module listaAtendidos 
    use cliente
    implicit none 
  
    type :: linked_list_atendidos
      type(node), pointer :: head => null() ! head of the list
  
      contains
        procedure :: pushA
        procedure :: printA
    end type linked_list_atendidos
  
    type :: node
      type(Clientes) :: value
      type(node), pointer :: next
    end type node
  
    contains
  
    subroutine pushA(self, value)
      class(linked_list_atendidos), intent(inout) :: self
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
  
    end subroutine pushA
  
    subroutine printA(self)
      class(linked_list_atendidos), intent(in) :: self
  
      type(node), pointer :: current
  
      current => self%head
  
      do while (associated(current))
        print *, "ID: ", current%value%id 
        print*, "Nombre: ", current%value%nombre
        print*, "img_g: ", current%value%img_g
        print*, "img_p: ", current%value%img_p
        print*, "Estado: ", current%value%estado_atendido
        current => current%next
      end do
    end subroutine printA
  
    subroutine atender(self, ID, tipo)
      class(linked_list_atendidos), intent(in) :: self
      integer:: ID
      character(len=*):: tipo
      type(node), pointer :: current
  
      current => self%head
  
      do while (associated(current))
        if((ID.eq.current%value%id) .and. (current%value%estado_atendido.eqv..false.)) then
          if(tipo.eq."pequena") then
            current%value%cont_p = current%value%cont_p-1
          end if

          if(tipo.eq."grande") then
            current%value%cont_g = current%value%cont_g-1
          end if

          if((current%value%cont_p.eq.0).and.(current%value%cont_g.eq.0)) then
            current%value%estado_atendido = .true.
          end if
        end if
        current => current%next
      end do
    end subroutine atender


 



    subroutine mostrarClientesConMas(self)
      class(linked_list_atendidos), intent(in) :: self
  type(Clientes), dimension(:), allocatable :: temp_clients
  type(node), pointer :: current
  integer :: i, j, max_index
  integer, dimension(5) :: top_clients_imgg

  ! Inicializar el arreglo temporal sin elementos
  allocate(temp_clients(0))
  
  current => self%head
  
  ! Recorrer la lista y agregar clientes que cumplen con los criterios al arreglo temporal
  do while (associated(current))
    if (current%value%estado_atendido .and. current%value%img_g > 0) then
      allocate(temp_clients(size(temp_clients) + 1))
      temp_clients(size(temp_clients)) = current%value
    end if
    current => current%next
  end do

  ! Ordenar el arreglo temporal según la cantidad de imágenes grandes
  do i = 1, min(5, size(temp_clients))
    max_index = i
    do j = i+1, size(temp_clients)
      if (temp_clients(j)%img_g > temp_clients(max_index)%img_g) then
        max_index = j
      end if
    end do
    ! Intercambiar los elementos para ordenar
    temp_clients([i, max_index]) = temp_clients([max_index, i])
    top_clients_imgg(i) = temp_clients(i)%img_g
  end do

  ! Imprimir los primeros 5 elementos del arreglo ordenado
  print *, "Los 5 clientes con más imágenes grandes y estado_atendido en true son:"
  do i = 1, min(5, size(temp_clients))
    print *, "ID = ", temp_clients(i)%id, ", Nombre = ", temp_clients(i)%nombre, ", Imágenes grandes = ", top_clients_imgg(i)
  end do

  deallocate(temp_clients)
    end subroutine mostrarClientesConMas

  end module listaAtendidos
  
    