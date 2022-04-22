program principal
  implicit none

  real,allocatable::num(:)
  integer::N,error,i
  character(len=100)::ORDEN,NOMB,ORDEN1
 WRITE(*,*)'  '
 WRITE(*,*)'  '
  WRITE(*,*)'             ORDENAREMOS NUMEROS             '
  WRITE(*,*)'                  ¿CUANTOS? '
  READ*,N

  allocate(num(1:N))
error=1
  DO
IF(error == 0)exit
     IF(ORDEN /="ARCH" .OR. ORDEN /="TECL")THEN
  WRITE(*,*)'             ¿TE GUSTARÍA DARMELOS AQUÍ O POR'
  WRITE(*,*)'             UN ARCHIVO DE TEXTO? (ARCH/TECL)'
  READ*,ORDEN
  
  IF(ORDEN=="ARCH")THEN
     
     WRITE(*,*)'          ¿CÓMO SE LLAMA EL ARCHIVO DE TEXTO CON FORMATO .dat?'
     READ*,NOMB

     OPEN(1,FILE=NOMB,STATUS="OLD",IOSTAT=error)
     IF(error/=0)exit

     DO i=1,N,1
        READ(1,*)num(i)
                
     END DO
          close(1)  
       ELSE IF (ORDEN=="TECL")THEN
          DO i=1,N,1
             READ*, num(i)
          END DO
          
       ELSE
          WRITE(*,*)'              PRUEBA OTRA VEZ'         
       END IF
       END IF

    END DO

  WRITE(*,*)'              ¿ESTOS SON TUS DATOS? SPOLIER ALERT!!! SI LO SON xP'

    DO i=1,N,1
       WRITE(*,'(F12.4)')  num(i)
    END DO
    

 CALL Ordena(N,num)


WRITE(*,*)'PERDONA CORTAR LA EMOCIÓN, PERO ¿QUIERES TUS DATOS EN ASCENDENCIA O DESCENDENCIA?(AS/DS)'
READ*, ORDEN1
SELECT CASE(ORDEN1)
CASE ("AS")
       DO i=1,N,1
WRITE(*,*)num(i)
  END DO
CASE ("DS")
DO i=N,1,-1
WRITE(*,*)num(i)
END DO
CASE DEFAULT
WRITE(*,*)'NO JUEGUES CONMIGO'
END SELECT
END PROGRAM







