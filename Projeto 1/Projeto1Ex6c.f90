program numerosaleatorios

! Projeto 1 - Introdução à Programação
! Nome: Henrique Krastins Okuti
! Contato: henrique.okuti@usp.br

	implicit none

	real*8 gamma5b2, gamma3, gamma7b2
	real*8 gamma,c0,c1,c2,c3,c4,c5,c6,funcaogamma,somacoefic
	real*8 i,n, dif3, dif4, dif5
	real*8 piexp, pi
	real*8 gamma5b2piexp, gamma3piexp, gamma7b2piexp
	real*8 volumeesfera3, volumeesfera4, volumeesfera5
	real*8 volumeesfera3exp, volumeesfera4exp, volumeesfera5exp
	real*8 volume3d, volume4d, volume5d

	piexp = 3.1415664179483276

	c0 = 1.000000000190015d0
	c1 = 76.18009172947146d0
	c2 = -86.50532032941677d0
	c3 = 24.01409824083091d0
	c4 = -1.231739572450155d0
	c5 = 0.1208650973866179/100d0
	c6 = -0.5395239384953/100000d0
	gamma = 5

	i = 0.5d0
	n = 5
	pi = piexp
	
	do while(i.LE.n)
		if (i.EQ.(2d0)) then
			gamma5b2 = funcaogamma
		endif
		
		if (i.EQ.(2.5d0)) then
			gamma3 = funcaogamma
		endif
		
		if (i.EQ.(3d0)) then
			gamma7b2 = funcaogamma
		end if	
	
	
		somacoefic = (c0 + (c1/(i+1d0)) + (c2/(i+2d0)) + (c3/(i+3d0)) + (c4/(i+4d0)) + (c5/(i+5d0)) + (c6/(i+6d0)) )
		funcaogamma = (i+gamma+0.5d0)**(i+0.5d0) * EXP(-(i+gamma+0.5d0)) * SQRT(2d0*pi) * somacoefic
		i = i+0.5d0
					
	enddo	
	write(*,*)"Valores de gamma usando pi obtido pelo metodo de monte carlo:"

	write(*,*)"Gamma 5b2 = ",gamma5b2
	write(*,*)"Gamma 3 = ",gamma3
	write(*,*)"Gamma 7b2 = ",gamma7b2

	write(*,*)"Valores de gamma exatos:"

	gamma5b2piexp	= gamma5b2		!
	gamma3piexp		= gamma3		! Armazeno os gammas calculados com pi pelo metodo de monte carlo
	gamma7b2piexp	= gamma7b2		!


	gamma5b2 = 1.3293403881791370	!
	gamma3 = 2.0000000000000000		! Estes são os valores de gamma exatos
	gamma7b2 = 3.3233509704488425	!


	write(*,*)"Gamma 5b2 = ",gamma5b2
	write(*,*)"Gamma 3 = ",gamma3
	write(*,*)"Gamma 7b2 = ",gamma7b2

	! Agora estamos munidos de gamma exato e gamma calculado usando pi obtido pelo método de monte carlo,
	! podemos prosseguir para o calculo do volume das esferas em d = 3, 4 e 5

	write(*,*)"O volume das esferas:"

	i = 3
	n = 5

	do while(i.LE.n)
		if (i.EQ.(3)) then
			volumeesfera3exp = piexp**(3d0/2d0) / gamma5b2piexp
			volumeesfera3 = pi**(3d0/2d0) / gamma5b2
		else if (i.EQ.(4)) then
			volumeesfera4exp = piexp**(2d0) / gamma3piexp
			volumeesfera4 = pi**(2d0) / gamma3
		else
			volumeesfera5exp = piexp**(5d0/2d0) / gamma7b2piexp
			volumeesfera5 = pi**(5d0/2d0) / gamma7b2
		end if	
	i = i+1
	
	enddo

	write(*,*)"Volume 3d = ",volumeesfera3exp,"(pi monte carlo)"
	write(*,*)"Volume 4d = ",volumeesfera4exp,"(pi monte carlo)"
	write(*,*)"Volume 5d = ",volumeesfera5exp,"(pi monte carlo)"


	dif3 = abs(volumeesfera3 - volumeesfera3exp)
	dif4 = abs(volumeesfera4 - volumeesfera4exp)
	dif5 = abs(volumeesfera5 - volumeesfera5exp)

	write(*,*)"Volume 3d = ",volumeesfera3
	write(*,*)"Volume 4d = ",volumeesfera4
	write(*,*)"Volume 5d = ",volumeesfera5

	write(*,*)"As diferencas nos valores:"
	write(*,*)"3d:",dif3
	write(*,*)"4d:",dif4
	write(*,*)"5d:",dif5

	volume3d = (4*atan(1.d0))**(3.d0/2.d0) /  1.32934038817913
	volume4d = (4*atan(1.d0))**(2.d0) /  2.d0
	volume5d = (4*atan(1.d0))**(5.d0/2.d0) /  3.32335097044784
	write(*,*)"Volume exato:"
	write(*,*)"Volume 3d = ",volume3d
	write(*,*)"Volume 4d = ",volume4d
	write(*,*)"Volume 5d = ",volume5d
	dif3 = abs(volumeesfera3 - volume3d)
	dif4 = abs(volumeesfera4 - volume4d)
	dif5 = abs(volumeesfera5 - volume5d)
	write(*,*)"As diferencas nos valores (exercicio 3):"
	write(*,*)"3d:",dif3
	write(*,*)"4d:",dif4
	write(*,*)"5d:",dif5
	dif3 = abs(volumeesfera3exp - volume3d)
	dif4 = abs(volumeesfera4exp - volume4d)
	dif5 = abs(volumeesfera5exp - volume5d)
	write(*,*)"As diferencas nos valores (pi monte carlo):"
	write(*,*)"3d:",dif3
	write(*,*)"4d:",dif4
	write(*,*)"5d:",dif5
	


end program numerosaleatorios