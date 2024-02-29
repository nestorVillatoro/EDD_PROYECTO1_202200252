module ventanilla
    implicit none 

    type:: Ventanillas
        integer:: id
        logical:: verificador
        integer:: img_g
        integer:: img_p
    end type Ventanillas

    contains

    subroutine Vinicializar(self, id, verificador, img_g, img_p)
        class(Ventanillas), intent(inout):: self
        integer, intent(in):: id, img_g, img_p
        logical, intent(in):: verificador

        self%id = id
        self%verificador = verificador
        self%img_g = img_g
        self%img_p = img_p 
    end subroutine Vinicializar 

end module ventanilla 