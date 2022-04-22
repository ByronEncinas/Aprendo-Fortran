function F(x)

  implicit none

  real,intent(in) :: x !! Función con raíces desconocidas en I=[a,b]
  real::F

  F = sin(3*x-0.1)

  end function
