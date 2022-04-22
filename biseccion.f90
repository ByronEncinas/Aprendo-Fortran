subroutine bis(a,b,ET,x)
  
  implicit none

!!!!!!!!!!!!!!!!!
  !              AUTOR: BYRON J. ENCINAS
  !
  !                    PROPÓSITO:
  !      APROXIMAR RAÍCES REALES PARA FUNCIONES
  !      MEDIANTE MÉTODOS DE BISECCIÓN 
  !
!!!!!!!!!!!!!!!!!!

  integer,parameter::q=selected_real_kind(32,600)
  real,external::F
  real,intent(INOUT)::a,b,ET,x
  real(kind=q)::G,j
  integer::i,k
	
!======================================================

G = F(b)*F(a)		!!! Variable G nos comunica si hay cambio de signo en el primer intervalo
	
IF(G==0 .OR. G>0)THEN		!!!verificación
WRITE(*,*)'No hay cambio de signo'
STOP

ELSE !!!cambio de signo, LISTOS PARA CONTINUAR CON APROXIMACION
	
!!!!!===============CALCULO DE ERRORES: !!!BISECCION============================
	
  
!=====================================================================	
	IF(ET==0)then
	k=200			!!! condicionamos el numero de iteraciones, si es otorgado por el usuario
	ELSE 			!!! si no, se realizan 200 iteraciones
	j= log((b-a)/ET)/0.69314718056 -1
	k=NINT(j)+1
	END IF
!=======================================================================
	
WRITE(*,*)'X_n                     F(x_n)                a_n                b_n'
	!=============================================================
	Do i=1,k
x = (a + b)/2
	!====================
	WRITE(*,*) x,F(x),a,b

	IF(F(x) /= 0)THEN
	
	
	
	IF(F(x)*F(a)<0)then
	!Si F(x)*F(a_n) <0 entonces hay cambio de signo
	!h<d por lo tanto daremos el nuevo valor de x a d (el limite superior)
	b = x
	else 
	a = x
	END IF

	END IF

	!============================


	
	END DO

END IF


END Subroutine
