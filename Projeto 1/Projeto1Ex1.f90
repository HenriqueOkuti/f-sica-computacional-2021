program bhaskara

! Projeto 1 - Introdução à Programação
! Nome: Henrique Krastins Okuti
! Contato: henrique.okuti@usp.br

    implicit none
	
    real*8 a,b,c,delta,raizd,x1,x2
  
    write(*,*) "Quais serao os coeficientes a,b,c? Escreva nessa ordem!"
    read (*,*) a,b,c
	write(*,*) "Os valores dos coeficientes foram definidos como:"
    write(*,*) "a = ",a
    write(*,*) "b = ",b
    write(*,*) "c = ",c	

    delta=(b*b)-(4.d0*a*c)
	

	if (a.EQ.0.d0 .AND. b.EQ.0.d0) then ! Condicional para polinômio de grau 0
		write(*,*) "Os coeficientes fornecidos nao servem para o calculo de raizes"
	
    else if (delta<0.d0) then	! Delta negativo indica raízes complexas
        write (*,*) "Como Delta tem valor negativo, nao ha raizes reais"
	
	else if (a.EQ.0.d0) then ! Condicional para polinômio de grau 1
				x1 = -c/b
				write(*,*) "Raiz = ",x1
			
    else				! Delta positivo/nulo indica raízes reais

	
		raizd=sqrt(delta)
       write (*,*) "O valor de Delta: ", delta
	   write (*,*) "Raiz delta: ",raizd
	   write (*,*) "Como Delta tem valor nulo ou positivo, ha raizes reais"
	   
	   
			if (b<0.d0) then
				x1=(-b)/(2.d0*a)+raizd/(2.d0*a)
			
            else
				x1=(-b)/(2.d0*a)-raizd/(2.d0*a)
			
			end if
			
		x2=c/(a*x1)
		
			if (x1.NE.x2) then
				write(*,*) "Raiz 1 = ",x1
				write(*,*) "Raiz 2 = ",x2
			else
				write(*,*) "Raiz dupla = ",x1
			end if
    end if
 end program bhaskara
 
 