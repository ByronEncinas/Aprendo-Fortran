 subroutine Ordena(N,num)
   implicit none
   
  REAL,INTENT(INOUT)::num(1:N)
  INTEGER,INTENT(in)::N
  INTEGER::i,j,p
  REAL::aux

 !!n es el total de numeros a ordenar
 !!i = orden en que se encuentra un numero desde la derecha
 !!j= orden del numero desde la izquierda 
 !!p= pivote
 
DO i=1,N,1
DO j=1,N,1
IF(num(i)<num(j))THEN
aux=num(i)
num(i)=num(j)
num(j)=aux
END IF
END DO

IF(num(j)>num(i))then
aux=num(j)
num(j)=num(i)
num(i)=aux
END IF
END DO

end subroutine Ordena
