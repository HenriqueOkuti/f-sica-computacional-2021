program integracaonumerica

! Projeto 2 - Cálculo numérico
! Nome: Henrique Krastins Okuti
! Contato: henrique.okuti@usp.br

	implicit none

	real*8 integral, integraltrapezio, integralsimpson, f_a, f_b, funcao, aux2
	real*8 diferenca_integral, logerro_trapezio, logerro_simpson, logh
	real*8 xi, soma, funcao_montecarlo, diferenca_montecarlo, mediafuncao
	real*8 desvio_padrao,somaquadrado, mediasomaquadrado
	real*8 h(13), precisao_montecarlo(4)
	integer i, aux1, n, nr

! Começamos calculando o valor exato da integral

	integral = (0.5)*(exp(1.d0)*(sin(1.d0)+cos(1.d0))-1)
	write(*,*)"A integral deve valer: ",integral

! Agora definimos os valores dos intervalos h a serem utilizados
	
	i = 1		! Variável de contagem para gerarmos o vetor h
	n = 13		! Final da contagem para gerarmos o vetor h
	do while (i.LE.n)
		h(i) = 2.d0**(-i)		! Aqui nós iremos gerar a sequência 1/2 , 1/4, 1/8, ... e armazenar no vetor h
		i = i+1
	enddo

! Agora começamos os cálculos da integral pelo método do trapézio

	! A primeira etapa consiste em calcular o valor no extremos pois eles possuem pesos diferentes
	
	write(*,*)"Valores nos extremos:"
	f_a = exp(0.d0)*cos(0.d0)			! Aqui apenas aplicamos x = 0 para obter f(0) 
	f_b = exp(1.d0)*cos(1.d0)			! Aqui apenas aplicamos x = 1 para obter f(1)
	write(*,*)"f(0):",f_a
	write(*,*)"f(1):",f_b
	f_a = f_a/2.d0			! Os valores dos extremos devem ser divididos por 2,
	f_b = f_b/2.d0			! então nós atualizamos os valores dividindo ambos por 2

	
	
	! A segunda etapa consiste em calcular a integral em si, vamos definir alguns valores iniciais 

	i = 1		! Inicio do contador para o vetor h(i) que contém a largura que estamos integrando
	n = 13		! Final do contador para o vetor h(i)
	aux1 = 1	! Variavel auxiliar que irá serve para indicar qual trapézio estamos calculando 
	integraltrapezio = 0
	
	!Vamos gerar um arquivo para armazenar os valores de h, integral calculada e diferença entre o resultado obtido e o resultado esperado
	write(14,*)"		h(i)		","		Integral Trapezio		","		Diferença(It-I)		","		Log(h)		","		Log(Dif_Trap)		"
	do while(i.LE.n)
		
		aux2 = h(i)**(-1.d0) - 1.d0		! Variável auxiliar que serve para indicar o último trapézio que iremos calcular
		
		do while(aux1.LE.int(aux2))
			funcao = exp(float(aux1)*h(i))*cos(float(aux1)*h(i))	! Calculamos a função no trapézio indicado
			integraltrapezio = integraltrapezio + funcao			! A integral é a soma de todos os trapézios
			aux1 = aux1 + 1											! Devemos incrementar o contador para passar para o trapézio seguinte
		
		enddo
		
		integraltrapezio = h(i)*(integraltrapezio + f_a + f_b)	! Multiplicamos todos os termos pelo tamanho do trapézio 
		diferenca_integral = abs(integraltrapezio - integral)	! Tomamos a diferença absoluta entre o resultado "experimental" e o resultado "teórico"
		logh = dlog10(h(i))
		logerro_trapezio = dlog10(abs(diferenca_integral))
		write(14,*)h(i),integraltrapezio,diferenca_integral,logh,logerro_trapezio		! Escrevemos os resultados relevantes no arquivo fort.14
		write(42,*)logh,logerro_trapezio	! Este arquivo servirá para o exercício 4
		
		integraltrapezio = 0	! Reiniciamos o valor da integral para a próxima largura
		aux1 = 1				! Reiniciamos o valor que começaremos a calcular as integrais
		i = i + 1				! Passamos para a próxima largura do trapézio
	enddo


! Agora começamos os cálculos da integral pelo método de Simpson

	write(*,*)"Valores nos extremos:"
	f_a = exp(0.d0)*cos(0.d0)			! Aqui apenas aplicamos x = 0 para obter f(0) 
	f_b = exp(1.d0)*cos(1.d0)			! Aqui apenas aplicamos x = 1 para obter f(1)
	write(*,*)"f(0):",f_a
	write(*,*)"f(1):",f_b

	! Vamos definir alguns valores iniciais 

	i = 1		! Inicio do contador para o vetor h(i) que contém a largura que estamos integrando
	n = 13		! Final do contador para o vetor h(i)
	aux1 = 1	! Variavel auxiliar que irá serve para indicar qual trapézio estamos calculando 
	integralsimpson = 0
	
	!Vamos gerar um arquivo para armazenar os valores de h, integral calculada e diferença entre o resultado obtido e o resultado esperado
	write(15,*)"		h(i)		","		Integral Simpson		","		Diferença(It-I)		","		Log(h)		","		Log(Dif_Simp)		"
	do while(i.LE.n)
		
		aux2 = h(i)**(-1.d0) - 1.d0		! Variável auxiliar que serve para indicar o último trapézio que iremos calcular
		
		do while(aux1.LE.int(aux2))
		
			if(dmod(dfloat(aux1),2.d0).EQ.0.d0) then												! Condicional para o caso par
				funcao = 2.d0*exp(float(aux1)*h(i))*cos(float(aux1)*h(i))	! Calculamos a função no trapézio indicado
				integralsimpson = integralsimpson + funcao				! A integral é a soma de todos os trapézios
				aux1 = aux1 + 1											! Devemos incrementar o contador para passar para o trapézio seguinte
			
			else													! Condicional para o caso ímpar
				funcao = 4.d0*exp(float(aux1)*h(i))*cos(float(aux1)*h(i))	! Calculamos a função no trapézio indicado
				integralsimpson = integralsimpson + funcao				! A integral é a soma de todos os trapézios
				aux1 = aux1 + 1											! Devemos incrementar o contador para passar para o trapézio seguinte			
			endif
		enddo
		
		integralsimpson = (h(i)/3.d0)*(integralsimpson + f_a + f_b)	! Multiplicamos todos os termos pelo tamanho do trapézio 
		diferenca_integral = abs(integralsimpson - integral)	! Tomamos a diferença absoluta entre o resultado "experimental" e o resultado "teórico"
		logh = dlog10(h(i))
		logerro_simpson = dlog10(diferenca_integral)
		write(15,*)h(i),integralsimpson,diferenca_integral,logh,logerro_simpson		! Escrevemos os resultados relevantes no arquivo fort.15
		write(43,*)logh,logerro_simpson	! Este arquivo servirá para o exercício 4
		
		integralsimpson = 0		! Reiniciamos o valor da integral para a próxima largura
		aux1 = 1				! Reiniciamos o valor que começaremos a calcular as integrais
		i = i + 1				! Passamos para a próxima largura do trapézio
	enddo

! Agora vamos realizar o cálculo da integral pelo método de Monte Carlo

	nr = 1
	precisao_montecarlo(1) = 1.4453421215421258E-007
	precisao_montecarlo(2) = 3.6133555036954590E-008
	precisao_montecarlo(3) = 9.0333955871102489E-009
	precisao_montecarlo(4) = 2.2583446224189174E-009
	i = 1 
	n = 4
	funcao_montecarlo = 0
	mediafuncao = 0
	desvio_padrao = 0
	somaquadrado = 0
	diferenca_montecarlo = 10
	
	write(16,*)"	h(i)	","	integral	","	iteracoes	","	desvio_padrao	"
	do while (i.LE.n)
		do while (diferenca_montecarlo.GE.precisao_montecarlo(i))
			call Random_Number(xi)
			funcao_montecarlo = (exp(xi)*cos(xi)+ funcao_montecarlo)
			mediafuncao = funcao_montecarlo / nr	
			
			somaquadrado = (exp(xi)*cos(xi))**(2.d0) + somaquadrado
			mediasomaquadrado = somaquadrado / nr
			
			desvio_padrao = sqrt((mediasomaquadrado + (mediafuncao)**(2.d0))/nr)
			
			diferenca_montecarlo = abs(mediafuncao - integral)
			nr = nr + 1

		
		enddo

	write(16,*)int(h(i+9)**(-1.d0)),mediafuncao, nr, desvio_padrao
	i = i+1
	funcao_montecarlo = 0
	mediafuncao = 0
	somaquadrado = 0
	desvio_padrao = 0
	nr = 1
		
	enddo

end program integracaonumerica