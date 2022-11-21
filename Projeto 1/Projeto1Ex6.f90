program numerosaleatorios

! Projeto 1 - Introdução à Programação
! Nome: Henrique Krastins Okuti
! Contato: henrique.okuti@usp.br

	implicit none

	real*8 r,aux,n,media,somaquad,desviop
	integer i,j,total, totalmax

	totalmax = 100000000
	total = 10
	do while (total.LE.totalmax)
	! Geramos 'total' números aleatórios em um arquivo
	i = 1
		do while (i.NE.total)
			call Random_Number(r)
			write(50,*)r
			i=i+1
		enddo
	rewind (50)	! Reiniciamos o arquivo que contem os dados gerados
	write(*,*)"Os arquivos foram gerados em fort.50, faremos agora a media e desvio padrao"
	write(*,*)"Total: ",total   ! Este é o número total da amostragem, mantemos essa linha para saber qual o valor que 
								! o programa está calculando
	!Calculo da media
	i = 1 	! reiniciamos a variavel de contagem
		do while (i.NE.total)
			read(50,*)n
			aux = aux + n
			i = i+1
		enddo
	media = aux/(total*1.d0)
	rewind 50	! Reiniciamos novamente o arquivo que contem os dados gerados
	
	!Calculo do desvio padrão
	aux = 0.d0		! reiniciamos a variável auxiliar
	i = 1			! reiniciamos a variável de contagem
	somaquad = 0	! comecamos a soma quadratica em um valor nulo
		do while (i.NE.total)
			read(50,*)n
				somaquad = (n - media)**2 + somaquad
			i = i+1
		enddo	
	close(50)
	desviop = sqrt(somaquad /(total-1.d0))
	write(51,*)total,media,desviop
	total = total*10

	enddo

end program numerosaleatorios