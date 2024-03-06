module objetoIMG
    implicit none 
 
    type:: IMGs
        integer:: id_cliente, pasos, ID
        character(len=:), allocatable:: tipo
        logical:: impreso

    end type IMGs
 
    contains

    subroutine IMGinicializar(self, ID,  id_cliente, tipo, pasos)
        class(IMGs), intent(inout):: self
        integer, intent(in):: id_cliente, pasos, ID
        character(len=*), intent(in):: tipo
        logical:: impreso

        self%id_cliente = id_cliente
        self%tipo = tipo
        self%impreso = .false.
        self%pasos = pasos
        self%ID = ID
    end subroutine IMGinicializar 

end module  objetoIMG