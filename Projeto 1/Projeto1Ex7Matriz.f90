program autovalorautovetor

! Projeto 1 - Introdução à Programação
! Nome: Henrique Krastins Okuti
! Contato: henrique.okuti@usp.br

	implicit none

! Exercício 7: Método da potência para o cálculo do autovalor/autovetor dominante
!	TAREFAS:
!		(a) Explique o algoritmo por meio de um fluxograma
!		(b) Implemente o algoritmo, respeitando uma certa tolerância epsilon para a convergência do autovalor dominante, com a condição de um número máximo de iterações k_max (maior prioridade que a tolerância)
!		(c) Discuta seu palpite para o valor inicial do vetor x\vec
!		(d) Aplique o algoritmo para as matrizes Hermitianas A_1, A_2 e A_3 fornecidas 
!		(e) Verifique as respostas e discuta elas, veja se a precisão para os autovalores é a mesma que para os autovetores para um dado valor de iteração k

		real*8, dimension(:,:), allocatable	::	A
		real*8, dimension(:), allocatable	::	x,xaux
		real*8	r,vepsilon
		real*8 raiz2, raiz7, pi, um_menosraiz5, um_maisraiz5, raiz10, raiz3
		real*8	lambda,term
		integer	n, i, j, k, contador,m
		logical										::	cont= .True.
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
		matriz3(1,1) = -10
		matriz3(1,2) = um_maisraiz5
		matriz3(1,3) = -3
		matriz3(1,4) = (-13.d0/15.d0)
		matriz3(1,5) = -1
		matriz3(2,1) = um_maisraiz5
		matriz3(2,2) = -10
		matriz3(2,3) = raiz3
		matriz3(2,4) = -4
		matriz3(2,5) = -27
		matriz3(3,1) = -3
		matriz3(3,2) = raiz3
		matriz3(3,3) = um_menosraiz5
		matriz3(3,4) = -3
		matriz3(3,5) = -3
		matriz3(4,1) = -(13.d0/15.d0)
		matriz3(4,2) = -4
		matriz3(4,3) = -3
		matriz3(4,4) = -6
		matriz3(4,5) = -4
		matriz3(5,1) = -1
		matriz3(5,2) = -27
		matriz3(5,3) = -3
		matriz3(5,4) = -4
		matriz3(5,5) = -13
		
		! Confirmamos os elementos escrevendo a matriz na tela
		write(*,*)"Matriz 3:"
		i = 1
		m = 5
		n = 5
		do i=1,m
			write(*,*)(matriz3(i,j),j=1,5)
		enddo

		n = 100
		do i=1,n
			call random_number(r)
			write(98,*)r
		enddo

end program autovalorautovetor