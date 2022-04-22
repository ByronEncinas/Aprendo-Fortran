subroutine falsapos(a,b,ET,x)
  
  implicit none
!!!!!!!!!!!!!!!!!
  !              AUTOR: BYRON J. ENCINAS
  !
  !                    PROPÓSITO:
  !      APROXIMAR RAÍCES REALES PARA FUNCIONES
  !      MEDIANTE MÉTODOS DE FALSA POSICIÓN
  !
!!!!!!!!!!!!!!!!!!

  integer,parameter::q=selected_real_kind(32,600)
  real,external:: F
  real,intent(INOUT)::a,b,ET,x
  real(kind=q)::G,ET2
  integer::i,k,j

  G = F(b)*F(a)	!!! Variable G nos comunica si hay cambio de signo en el primer intervalo
	
IF(G==0 .OR. G>0)THEN		!!!verificación
WRITE(*,*)'No hay cambio de signo'
STOP

ELSE
WRITE(*,*)'     X_n         F(X_n)           a_n           b_n'

Do 

x = a - (F(a)*(b-a))/(F(b)-F(a))

PRINT*, x, F(x),a,b

ET2 = abs((x-a)*100/x)

IF(ET2 < ET)EXIT

!!=================================================================

	IF(F(x) /= 0)THEN

		IF(F(x)*F(a)<0)then
		!Si F(x)*F(a_n) <0 entonces hay cambio de signo
		!h<d por lo tanto daremos el nuevo valor de x a d (el 
			b = x !! I=[a,x]
                			
		ELSE IF(F(x)*F(b)<0)then
		a = x  !!I=[x,b]
		END IF

	ELSE
	PRINT*,"EUREKA TU SOLUCIÓN ES", x
STOP
	!============================
	END IF
	
END DO
END IF
END SUBROUTINE
