program fgamma

! Projeto 1 - Introdução à Programação
! Nome: Henrique Krastins Okuti
! Contato: henrique.okuti@usp.br

	implicit none

	real*8 i,n,gamma,c0,c1,c2,c3,c4,c5,c6,funcaogamma,pi,somacoefic,fat,razao

	c0 = 1.000000000190015d0
	c1 = 76.18009172947146d0
	c2 = -86.50532032941677d0
	c3 = 24.01409824083091d0
	c4 = -1.231739572450155d0
	c5 = 0.1208650973866179/100d0
	c6 = -0.5395239384953/100000d0
	gamma = 5
	pi = 4*ATAN(1.d0)
	i = 1
	n = 50
	fat = 1

	write(15,*)"i	","gamma	","fatorial	","	razao	","	módulo do log	"

	do while(i.LE.n)
		somacoefic = (c0 + (c1/(i+1d0)) + (c2/(i+2d0)) + (c3/(i+3d0)) + (c4/(i+4d0)) + (c5/(i+5d0)) + (c6/(i+6d0)) )
		funcaogamma = (i+gamma+0.5d0)**(i+0.5d0) * EXP(-(i+gamma+0.5d0)) * SQRT(2.d0*pi) * somacoefic
		fat = fat*i
		razao = abs(((fat - funcaogamma) / fat))
		write(15,*)i,funcaogamma,fat,razao, abs(dlog10(razao))
		i = i+1
		
	enddo

	i = 0.5d0
	n = 5
	
	write(16,*)"i	","gamma	"
	do while(i.LE.n)
		somacoefic = (c0 + (c1/(i+1d0)) + (c2/(i+2d0)) + (c3/(i+3d0)) + (c4/(i+4d0)) + (c5/(i+5d0)) + (c6/(i+6d0)) )
		funcaogamma = (i+gamma+0.5d0)**(i+0.5d0) * EXP(-(i+gamma+0.5d0)) * SQRT(2d0*pi) * somacoefic
		write(16,*)i,funcaogamma
		i = i+1.d0
		
	enddo	


end program fgamma