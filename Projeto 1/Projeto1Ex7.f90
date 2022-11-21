program autovalor

! Projeto 1 - Introdução à Programação
! Nome: Henrique Krastins Okuti
! Contato: henrique.okuti@usp.br

	implicit none

		real*8, dimension(:), allocatable	::	x,xaux
		real*8	r,vepsilon
		real*8	lambda,term
		real*8 raiz2, raiz7, pi, um_menosraiz5, um_maisraiz5, raiz10, raiz3
		integer	n, i, j, k, contador, m
		logical				   ::	cont= .True.
		real*8, dimension(3,3) :: matriz1
		real*8, dimension(4,4) :: matriz2
		real*8, dimension(5,5) :: matriz3
		
		! Algumas operações úteis para definirmos os elementos das matrizes
		raiz2 = sqrt(2.d0)
		raiz3 = sqrt(3.d0)
		raiz7 = sqrt(7.d0)
		raiz10 = sqrt(10.d0)
		pi = 4*atan(1.d0)
		um_menosraiz5 = 1.d0 - sqrt(5.d0)
		um_maisraiz5 = 1 + sqrt(5.d0)
		
		! Definimos a matriz1:
		matriz1 = reshape((/ (raiz2), 8.d0, 13.d0 ,8.d0, 3.d0, 5.d0, 13.d0, 5.d0, (raiz7) /),(/3,3/))
		
		
		! Agora confirmamos ela escrevendo ela na tela
		write(*,*)"Matriz 1:"
		
		i = 1
		m = 3
		n = 3
		do i=1,m
			write(*,*)(matriz1(i,j),j=1,3)
		enddo
		
		
		! Definimos a matriz2 membro a membro pois tive problemas com o tamanho da expressão no compilador
		matriz2(1,1) = raiz10
		matriz2(1,2) = -2
		matriz2(1,3) = 3
		matriz2(1,4) = 2
		matriz2(2,1) = -2
		matriz2(2,2) = 11
		matriz2(2,3) = -3
		matriz2(2,4) = pi
		matriz2(3,1) = 3
		matriz2(3,2) = -3
		matriz2(3,3) = 6
		matriz2(3,4) = 3.5
		matriz2(4,1) = 2
		matriz2(4,2) = pi
		matriz2(4,3) = 3.5
		matriz2(4,4) = 6
		
		! Confirmamos os seus elementos escrevendo eles na tela
		write(*,*)"Matriz 2:"
		
		i = 1
		m = 4
		n = 4
		do i=1,m
			write(*,*)(matriz2(i,j),j=1,4)
		enddo
		
		! Definimos a matriz3 membro a membro pelo mesmo motivo que a matriz2
		matriz3(1,1) = -10.d0
		matriz3(1,2) = um_maisraiz5
		matriz3(1,3) = -3.d0
		matriz3(1,4) = (-13.d0/15.d0)
		matriz3(1,5) = -1.d0
		matriz3(2,1) = um_maisraiz5
		matriz3(2,2) = -10.d0
		matriz3(2,3) = raiz3
		matriz3(2,4) = -4.d0
		matriz3(2,5) = -27.d0
		matriz3(3,1) = -3.d0
		matriz3(3,2) = raiz3
		matriz3(3,3) = um_menosraiz5
		matriz3(3,4) = -3.d0
		matriz3(3,5) = -3.d0
		matriz3(4,1) = -(13.d0/15.d0)
		matriz3(4,2) = -4.d0
		matriz3(4,3) = -3.d0
		matriz3(4,4) = -6.d0
		matriz3(4,5) = -4.d0
		matriz3(5,1) = -1.d0
		matriz3(5,2) = -27.d0
		matriz3(5,3) = -3.d0
		matriz3(5,4) = -4.d0
		matriz3(5,5) = -13.d0
		
		! Confirmamos os elementos escrevendo a matriz na tela
		write(*,*)"Matriz 3:"
		i = 1
		m = 5
		n = 5
		do i=1,m
			write(*,*)(matriz3(i,j),j=1,5)
		enddo
	
		write(*,*)"Dimensao da matriz:"
		read(*,*)n
		allocate(x(n))
		allocate(xaux(n))	! Leio a dimensão n da matriz que se deseja calcular o autovalor e gero
							! os vetores x(n) e xaux(n), que serão utilizados para multiplicar a matriz

		write(*,*)"Numero de interacoes:"
		read(*,*)k			! Número máximo de loops de múltiplicação que serão realizados antes de finalizar o programa
		write(*,*)"Precisao desejada (casas decimais):"
		read(*,*)vepsilon	! Número da precisao desejada

		i = 1
		do while (i.LE.n)
			call random_number(r)
			x(i) = r			! Gero um vetor aleatório x com n elementos
			i = i+1
		end do

		
		write(*,*)"Vetor utilizado para o chute inicial:"
		write(*,*)(x(i),i=1,n)
		write(*,*) "CALCULANDO"

		contador = 0
		do while(cont)
			contador = contador +1	! 	O contador será usado para ser comparado com o número máximo de loops, "K", 
									!	lido anteriormente. Ele será incrementado em 1 a cada loop
			do i= 1, n
				term = 0.0d0	! 'term' representa cada linha resultante da multiplicação entre a matriz e x, para cada uma das i linhas
				
				if (n.EQ.3) then
					do j= 1, n
						term = (term + x(j)*matriz1(i,j))	! Faço a operação descrita acima

					end do
				else if (n.EQ.4) then	
					do j= 1, n
						term = (term + x(j)*matriz2(i,j))	! Faço a operação descrita acima

					end do
				else if (n.EQ.5) then	
					do j= 1, n
					term = (term + x(j)*matriz3(i,j))	! Faço a operação descrita acima

					end do
				endif
				
				xaux(i) = term	! Defino a linha "i" do vetor xaux como o termo 'term'. 
				! Ainda não podemos alterar o valor do vetor original x pois ele está sendo usado na múltiplicação, 
				! então xaux está sendo usado para armazenar todos os valoes que serão substituídos 
				! de uma vez após o término da operação
				
			end do
			x = xaux	! Após a multiplicação faço a substituição dos valores de x pelos armazenados em xaux
			
			! Caso a diferença entre a última aproximação do autovalor dominante e a nova seja menor que um valor epsilon 
			! OU caso o número máximo de loops 'k' tenha sido atingido, o programa é interrompido 
			if (n.NE.5) then	
				if (abs(lambda-maxval(x)).LE.(10.d0**(-vepsilon)) .OR. contador.GE.k) then	
					cont = .False.
				end if
			! Mesmo caso da interrupção mas para a matriz 5x5 usamos minval(x) por questão de convergência
			else	
				if (abs(lambda-minval(x)).LE.(10.d0**(-vepsilon)) .OR. contador.GE.k) then	! Caso a diferença entre a última aproximação do autovalor dominante e a nova seja menor que um valor epsilon OU caso o número máximo de loops 'k' tenha sido atingido, o programa é interrompido 
					cont = .False.
				end if
			endif
			write(*,*)"Vetor:",(x(i),i=1,n)
			if (n.NE.5) then
				lambda = maxval(x)	! o autovalor dominante lambda é igual ao maior número do vetor x
				x  = x/lambda	! divido x pelo autovalor para ajustá-lo para o próximo ciclo
				write(99,*)contador,lambda	! Escrevo o autovalor e o numero da interação no loop
			else
				lambda = minval(x)	! Para o caso n = 5 tomamos o menor valor para o cálculo do vetor x
								! Caso contrário a solução fica oscilando sem saber a direção de convergência
				x  = x/lambda	! divido x pelo autovalor para ajustá-lo para o próximo ciclo
				write(99,*)contador,lambda	! Escrevo o número da interação no loop e o autovalor
			endif
		end do

		write(*,*)"O valor do autovalor dominante:"
		write(*,*)lambda		! Retorna no terminal o autovalor dominante

end program autovalor