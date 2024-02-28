module cliente
    implicit none 

    type:: Clientes
        integer:: id
        character(len=:), allocatable:: nombre
        integer:: img_g
        integer:: img_p
    end type Clientes

    contains

    subroutine inicializar(self, id, nombre, img_g, img_p)
        class(Clientes), intent(inout):: self
        integer, intent(in):: id, img_g, img_p
        character(len=*), intent(in):: nombre

        self%id = id
        self%nombre = nombre
        self%img_g = img_g
        self%img_p = img_p 
    end subroutine inicializar 

end module cliente