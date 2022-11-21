program taylor

! Projeto 1 - Introdução à Programação
! Nome: Henrique Krastins Okuti
! Contato: henrique.okuti@usp.br

	implicit none

	real*8 ordem1,ordem3,ordem5,ordem7,ordem9,ordem11,ordem13
	real*8 senox,pi,xmin,xmax,xcentro,fat,erro,precisao,fimloop
	real*8 fat3,fat5,fat7,fat9,fat11,fat13
	integer aux

	pi = 4*atan(1.d0)
	fat = 1
	write(*,*)"pi = ",pi
	xmin = 0.d0*pi
	xmax = 2.d0*pi
	write(*,*)"Agora estamos fazendo centrado em 0"
	write(*,*)"xmin = ", xmin
	write(*,*)"xmax = ", xmax
	write(*,*)"xcentro = ", xcentro

	fat3 = 6.d0
	fat5 = 120.d0
	fat7 = 5040.d0
	fat9 = 362880.d0
	fat11 = 39916800.d0
	fat13 = 6227020800.d0
	precisao = 1E-6

	do while (xmin.LE.xmax)  ! sin(x) centrada em 0 indo de 0 até 2pi
		
			ordem1 = (xmin)**1.d0						! Por padrão sempre 
			senox = ordem1								!começamos com a ordem 1
			erro = abs(sin(xmin) - senox)	
		
				if (erro.GT.precisao) then		! Condicional para usar a ordem 3
					ordem3 = (xmin)**3.d0 / fat3
					senox = senox - ordem3
					erro = abs(sin(xmin) - senox)			
				end if
				
				if (erro.GT.precisao) then		! Condicional para usar a ordem 5
					ordem5 = (xmin)**5.d0 / fat5
					senox = senox + ordem5
					erro = abs(sin(xmin) - senox)		
				end if
				
				if (erro.GT.precisao) then		! Condicional para usar a ordem 7
					ordem7 = (xmin)**7.d0 / fat7
					senox = senox - ordem7
					erro = abs(sin(xmin) - senox)			
				end if
			
				if (erro.GT.precisao) then		! Condicional para usar a ordem 9		
					ordem9 = (xmin)**9.d0 / fat9
					senox = senox + ordem9
					erro = abs(sin(xmin) - senox)			
				end if
			
				if (erro.GT.precisao) then		! Condicional para usar a ordem 11		
					ordem11 = (xmin)**11.d0 / fat11
					senox = senox - ordem11
					erro = abs(sin(xmin) - senox)			
				end if

				if (erro.GT.precisao) then		! Condicional para usar a ordem 13
					ordem13 = (xmin)**13.d0 / fat13
					senox = senox + ordem13
					erro = abs(sin(xmin) - senox)	
				end if

	write(23,*)xmin, senox	! Arquivo para gerarmos gráfico de xmin por seno(xmin)
	write(24,*)xmin, senox, sin(xmin), erro, dlog10(erro) ! Arquivo com todos os dados relevantes
	write(124,*)dlog10(erro)
	xmin = xmin + (pi/2.d0)	! Incremento para passarmos para o ponto x seguinte 	
	erro = 0
		
	end do

	xmin = 0.d0*pi
	xmax = 2.d0*pi
	xcentro = pi
	write(*,*)"Agora estamos fazendo centrado em pi"
	write(*,*)"xmin = ", xmin
	write(*,*)"xmax = ", xmax
	write(*,*)"xcentro = ", xcentro

	do while (xmin.LE.xmax)  ! sin(x) centrada em pi indo de 0 até 2pi

	!		Para referência, as expansões em torno de x_0 = pi:
	!		ordem1 = (xmin - xcentro)**1.d0
	!		ordem3 = (xmin - xcentro)**3.d0 / fat3
	!		ordem5 = (xmin - xcentro)**5.d0 / fat5
	!		ordem7 = (xmin - xcentro)**7.d0 / fat7
	!		ordem9 = (xmin - xcentro)**9.d0 / fat9
	!		ordem11 = (xmin - xcentro)**11.d0 / fat11
	!		ordem13 = (xmin - xcentro)**13.d0 / fat13
	!		Dessa forma seno(x) é aproximadamente:
	!		senox = -ordem1 + ordem3 - ordem5 + ordem7 - ordem9 + ordem11 - ordem13
		
			ordem1 = (xmin - xcentro)**1.d0		! Por padrão sempre começamos com a ordem 1
			senox = -ordem1
			erro = abs(sin(xmin) - senox)		
				if (erro.GT.precisao) then		! Condicional para usar a ordem 3
					ordem3 = (xmin - xcentro)**3.d0 / fat3
					senox = senox + ordem3
					erro = abs(sin(xmin) - senox)			
				end if
			
				if (erro.GT.precisao) then		! Condicional para usar a ordem 5
					ordem5 = (xmin - xcentro)**5.d0 / fat5
					senox = senox - ordem5
					erro = abs(sin(xmin) - senox)		
				end if
			
				if (erro.GT.precisao) then		! Condicional para usar a ordem 7
					ordem7 = (xmin - xcentro)**7.d0 / fat7
					senox = senox + ordem7
					erro = abs(sin(xmin) - senox)			
				end if
			
				if (erro.GT.precisao) then		! Condicional para usar a ordem 9		
					ordem9 = (xmin - xcentro)**9.d0 / fat9
					senox = senox - ordem9
					erro = abs(sin(xmin) - senox)			
				end if
			
				if (erro.GT.precisao) then		! Condicional para usar a ordem 11					
					ordem11 = (xmin - xcentro)**11.d0 / fat11
					senox = senox + ordem11
					erro = abs(sin(xmin) - senox)			
				end if

				if (erro.GT.precisao) then		! Condicional para usar a ordem 13
					ordem13 = (xmin - xcentro)**13.d0 / fat13
					senox = senox - ordem13
					erro = abs(sin(xmin) - senox)	
				end if		
		
	write(25,*)xmin, senox	! Arquivo para gerarmos gráfico de xmin por seno(xmin)
	write(125,*)xmin, sin(xmin) ! Arquivo para gerarmos gráfico de xmin por seno(xmin) exato
	write(26,*)xmin, senox, sin(xmin), erro, dlog10(erro) ! Arquivo com todos os dados relevantes
	xmin = xmin + (pi/2.d0)	! Incremento para passarmos para o ponto x seguinte 	
		
	end do

end program taylor