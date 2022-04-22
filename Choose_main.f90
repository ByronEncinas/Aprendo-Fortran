program Metodos
  implicit none

!!!!!!!!!!!!!!!!!
  !              AUTOR: BYRON J. ENCINAS
  !
  !                    PROPÓSITO:
  !      APROXIMAR RAÍCES REALES PARA FUNCIONES
  !      MEDIANTE MÉTODOS DE BISECCIÓN Y FALSA POSICIÓN
  !
!!!!!!!!!!!!!!!!!!
  real::a,b,ET,x
  CHARACTER::m,O
  INTEGER::i
  !!son los limtes

  write(*,*)'Introduce un intervalo para realizar una busqueda de la raiz' !! I = [a,b]
 READ*,a
READ*, b
O = "b"

DO
	IF(O=="a")EXIT
	WRITE(*,*)'¿Qué método prefieres? (responde "a" o "b")' !! Eleccion del metodo

	WRITE(*,*)'        a) Bisección'
	
	WRITE(*,*)'        b) Falsa posición'
	READ*, m

		IF(m=="a") then
		WRITE(*,*)'Puedes introducir un Error de Truncamiento en caso 			necesario. &
		&Si no, presiona 0 y el programa se detendra en la iteracion #200'
		READ*,ET	
		O="a"
		ELSE IF(m == "b") then
	
		WRITE(*,*)'Puedes introducir porcentaje de error, en caso 			necesario. &
		&Si no, presiona 0 y el programa se detendra en la iteracion #200'
	
		READ*,ET
		O="a"
		ELSE
		
		WRITE(*,*)'Tienes que introducir un método, trata de nuevo o salte'
		
		WRITE(*,*)'a) SALIR'
		WRITE(*,*)'b) OTRA VEZ'
		READ*, O
		END IF

END DO

	IF(m=="a")THEN
	call bis(a,b,ET,x)
	ELSE 
	IF (m=="b")THEN
	CALL falsapos(a,b,ET,x)
	ELSE
	WRITE(*,*)'ESE NO ES UN MÉTODO'
	END IF

END IF

END PROGRAM Metodos
