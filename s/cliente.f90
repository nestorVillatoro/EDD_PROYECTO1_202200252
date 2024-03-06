module cliente
    implicit none 

    type:: Clientes
        integer:: id
        character(len=:), allocatable:: nombre
        integer:: img_g
        integer:: img_p 
        logical:: estado_atendido, atendido
        integer:: cont_p
        integer:: cont_g

    end type Clientes
 
    contains

    subroutine Cinicializar(self, estado_atendido, id, nombre, img_g, img_p)
        class(Clientes), intent(inout):: self
        logical, intent(in):: estado_atendido
        integer, intent(in):: id, img_g, img_p
        character(len=*), intent(in):: nombre
        logical::atendido 
        integer:: cont_g, cont_p

        self%id = id
        self%nombre = nombre
        self%estado_atendido = estado_atendido
        self%img_g = img_g
        self%img_p = img_p 
        self%atendido = .false.
        self%cont_g = img_g 
        self%cont_p = img_p
    end subroutine Cinicializar 

end module cliente