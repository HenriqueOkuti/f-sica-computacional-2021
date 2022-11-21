program mediadesviopadrao

! Projeto 1 - Introdução à Programação
! Nome: Henrique Krastins Okuti
! Contato: henrique.okuti@usp.br

	implicit none

integer*8	n,total,i,aux, io
real*8		media, desviop, somaquad, mediaquadratica, auxmedia, auxmediaquadratica


aux = 0.d0
total = 0.d0
i = 1

	!Determinação da quantidade de números no arquivo de entrada
	open(30, file = 'media.in')	! Abrimos o arquivo com os dados
	do	
		read(30,*,iostat=io)	! Lemos o arquivo com os dados
		if (io/=0) exit			! Condicional para término da leitura
		total = total + 1		! Contador da quantidade de números no arquivo
	end do					
	close(30)					! Fechamos o arquivo que estamos lendo
	write(*,*)"Total: ",total	! Escrevemos o valor encontrado para compararmos com o esperado

	!Cálculo da media
	open(31,file='media.in')	! Abrimos novamente o arquivo com os dados
	do while (i.LE.total)		! Criamos um laço que irá ler o arquivo até seu último integrante
		read(31,FMT=*)n			! Lemos a i-ésima entrada
		aux = aux + n			! Somamos a i-ésima entrada na variável auxiliar
		i = i+1					! Passamos para a entrada seguinte
	enddo
	close(31)
	
	media = aux/(total*1.d0)	! Calculamos a média em sí
	write(*,*)"Media da amostragem = ",media	! Escrevemos na tela o valor encontrado para a média
	
	!Cálculo do desvio padrão

	aux = 0.d0		! reiniciamos a variável auxiliar
	i = 1			! reiniciamos a variável de contagem
	somaquad = 0	! começamos a soma quadratica em um valor nulo
	open(32,file='media.in')	! Abrimos novamente o arquivo com os dados
	do while (i.LE.total)		! Criamos um laço que irá ler o arquivo até seu último integrante
		read(32,FMT=*)n			! Lemos a i-ésima entrada
		somaquad = (n - media)**2 + somaquad	! Aplicamos o valor de n na fórmula do desvio padrão
		i = i+1
	enddo	
	close(32)

	desviop = sqrt(somaquad /(total-1.d0))		! Calculamos o desvio padrão em sí
	write(*,*)"Desvio padrao da amostragem = ",desviop ! Escrevemos na tela o valor encontrado para o desvio padrão

	!Cálculo do desvio padrão pela expressão simplificada

	auxmedia = 0.d0				! iniciamos a variável auxiliar para a média
	auxmediaquadratica = 0.d0	! iniciamos a variável auxiliar para a média quadrática
	i = 1			! reiniciamos a variável de contagem
	open(33,file='media.in')	! Abrimos novamente o arquivo com os dados
	do while (i.LE.total)		! Criamos um laço que irá ler o arquivo até seu último integrante
		read(33,FMT=*)n			! Lemos a i-ésima entrada
		auxmedia = auxmedia + n							! Somamos a i-ésima entrada na variável auxiliar
		auxmediaquadratica = auxmediaquadratica + (n*n)	! Somamos a i-ésima entrada ao quadrado na variável auxiliar
		i = i+1					! Passamos para a entrada seguinte
	enddo
	close(33)
	

	mediaquadratica = auxmediaquadratica/(total*1.d0)	! Calculamos a media quadratica
	media = auxmedia/(total*1.d0)						! Calculamos a media
	desviop = sqrt(mediaquadratica - media**(2.d0))		! Calculamos o desvio padrão em sí
	write(*,*)"Desvio padrao da amostragem pela expressao simplificada = ",desviop ! Escrevemos na tela o valor encontrado para o desvio padrão


end program mediadesviopadrao