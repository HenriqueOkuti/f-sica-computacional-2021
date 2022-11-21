program fatoriais

! Projeto 1 - Introdução à Programação
! Nome: Henrique Krastins Okuti
! Contato: henrique.okuti@usp.br

	implicit none

	real*8 fat,logfat,S,i,n,erro
	real*8 pi

	i = 1
	n = 20
	fat = 1
	do while (i.LE.n)

		write(10,*)i,"! = ",fat
		i = i+1
		fat = fat*i
	enddo

! Item (b)
	write(*,*)"Agora vamos calcular o logaritmo dos fatoriais"
	i = 2
	n = 20
	fat = 2
	logfat = log(fat)
	do while (i.LE.n)
		write(11,*)"ln",i,"!"," = ",logfat
			i = i+1
			fat = fat*i
			logfat = log(fat)
	enddo

! Item (c)

!				Stirling: ln (n!) = S = n*ln(n) - n + (1/2)*ln(2*pi*n)	
	write(*,*)"Agora vamos calcular o numero de Stirling"
	i = 2
	n = 20
	pi = 4*ATAN(1.d0)
	write(*,*)"Definindo pi:",pi
	
	do while (i.LE.n)
			S = (i*log(i)) - i + ((0.5d0)*log(2d0*pi*i))
			write(12,*)"Para i = ",i,"S = ",S
			i = i+1
			
	enddo

! Exercicio (d)

	write(*,*)""
	write(*,*)"Agora vamos calcular o erro da aproximacao pelo numero de Stirling"
	
!	
!				Stirling: ln (n!) = S = n*ln(n) - n + (1/2)*ln(2*pi*n)	

	i = 1
	n = 20
	pi = 4*ATAN(1.d0)
	fat = 1d0
	logfat = 0
	S = 0
	erro = 0
	write(*,*)"Definindo pi:",pi
	write(13,*)"	i	","	i!	","	ln i!	","	S	","	erro	"

	do while (i.LE.n)
			write(13,*)i,fat,logfat,S,erro
			i = i+1
			fat = fat*i
			logfat = log(fat)
			S = (i*log(i)) - i + ((0.5d0)*log(2d0*pi*i))
			erro = (logfat - S)/logfat

	enddo

end program fatoriais


