program celsius2fahrenheit	! Nome do programa
					! Todo o programa Fortran começa assim
!
	implicit none	! Nenhuma variável declada implicitamente !
!
	real*8 tc,tf	! Declaração de variáveis reais com dupla precisão. 
					! Temperatura em C e em F ! Padrão ao longo do curso ! 
!
	write(*,*)"Temperatura em graus celsius:"	! Leia o valor de tc do terminal !
!
	read(*,*)tc
!
	if (tc>= -274.12d0) then !Teste condicional, se satisfeito então:
!
			tf = 9.0d0*tc/5.0d0 + 32.0d0
!			Fórmula que converte C->F
!			A adição do d0 força a precisão dupla
!
			write(*,*)"Valor da temperatura em graus Farenheit:"
			write(*,*)tf
!
	else
!
			write(*,*)"Input inválido, abaixo do zero absoluto"
!
	endif !Fim do condicional
!
end program celsius2fahrenheit
