program eqalgebrica

! Projeto 2 - Cálculo numérico
! Nome: Henrique Krastins Okuti
! Contato: henrique.okuti@usp.br

	implicit none

	real*8	xesquerda, xdireita, xnovo, xvelho, aleatorio_esq, aleatorio_dir, aleatorio
	real*8	erro, tolerancia
	real*8	funcao, derivada, modulofuncao, coeficienteT
	integer	contador, saida, contadoraux
	
	tolerancia = 1E-9
	coeficienteT = 0.d0
	saida = 1000
	call random_number(aleatorio_esq)
	call random_number(aleatorio_dir)

	write(31,*)"	iteracoes	","	coeficiente T	","	raiz	"
	write(311,*)"	raiz	","	funcao calculada na raiz	"
	do while (coeficienteT.LE.(2.d0))
		
		xesquerda = aleatorio_esq*(-1.d0)
		xdireita = 1.d0+aleatorio_dir
		erro = 1.d0
		contador = 0
		do while (contador.LE.saida)
			xnovo = (xesquerda + xdireita)/2.d0
			funcao = (xnovo) - tanh((xnovo)/coeficienteT)
			if (funcao.LT.(0.d0)) then
				xesquerda = xnovo
			else 
				xdireita = xnovo
			endif
			erro = abs((xesquerda - xdireita)/2.d0)
			contador = contador + 1
			if (erro.LE.tolerancia) exit
		enddo
		write(311,*)abs(xnovo), (abs(xnovo) - tanh((abs(xnovo))/coeficienteT))
		write(31,*)contador,coeficienteT,abs(xnovo)
		coeficienteT = coeficienteT + 1E-3
	enddo
	
	! Método de Newton-Raphson + Raiz positiva como função de T
	! derivada: 1 - (sech^2 (x/T)/T)
	
	tolerancia = 1E-9
	coeficienteT = 0.d0
	saida = 1000
	call random_number(aleatorio)
		
	write(32,*)"	iteracoes	","	coeficiente T	","	raiz	","	funcao na raiz calculada	"
	do while (coeficienteT.LE.(2.d0))
		xvelho = aleatorio
		erro = 0.d0
		contador = 0
		do while (contador.LE.saida)
			funcao = abs(xvelho) - tanh(abs(xvelho)/coeficienteT)
			derivada = 1.d0 - (1.d0/(coeficienteT * (cosh(abs(xvelho)/coeficienteT)**(2.d0))))
			xnovo = abs(abs(xvelho) - (funcao/derivada))
			erro = abs(xnovo - xvelho)
			modulofuncao = abs(funcao)
			xvelho = xnovo
			contador = contador + 1
			if (modulofuncao.LE.tolerancia .OR. erro.LE.tolerancia) exit
			enddo
		write(32,*)contador,coeficienteT,abs(xnovo),(abs((abs(xnovo) - tanh((abs(xnovo))/coeficienteT))))
		coeficienteT = coeficienteT + 1E-3
	enddo
	
end program eqalgebrica