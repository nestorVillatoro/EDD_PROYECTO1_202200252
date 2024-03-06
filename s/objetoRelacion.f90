module objetoRelacion
    implicit none 

    type:: objRelacion
        integer:: id_cliente
        character(len=:), allocatable:: nombre
        integer:: img_g_cliente
        integer:: img_p_cliente
        integer:: id_ventanilla
        integer:: totalImg
        logical:: verify, verify2
    end type objRelacion
 
    contains

    subroutine Oinicializar(self, id_cliente, nombre, img_g_cliente, img_p_cliente, id_ventanilla)
        class(objRelacion), intent(inout):: self
        integer, intent(in):: id_cliente, img_g_cliente, img_p_cliente, id_ventanilla
        character(len=*), intent(in):: nombre
        logical:: verify, verify2

        self%id_cliente = id_cliente
        self%id_ventanilla = id_ventanilla
        self%nombre = nombre
        self%img_g_cliente = img_g_cliente
        self%img_p_cliente = img_p_cliente 
        self%totalImg = img_p_cliente + img_g_cliente
        self%verify = .false.
        self%verify2 = .true.
    end subroutine Oinicializar 

end module objetoRelacion
