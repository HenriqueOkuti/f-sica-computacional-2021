program teste

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

		real*8 pi

		pi = 4*atan(1.d0)
		write(*,*)pi

end program teste