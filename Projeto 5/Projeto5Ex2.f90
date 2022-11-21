program dobras_periodo_caos

! Projeto 5 - Dinâmica populacional
! Nome: Henrique Krastins Okuti
! Contato: henrique.okuti@usp.br

	implicit none

	! Declaração de variáveis

	real*8	x, f_x, g_x, deltaX
	real*8	calculaG, coef_e
	real*8	r, x_old, x_new, x_old_e, G1, G2, diferenca, dist_i
	integer*8 i,j

	!==========================================================
	!	ITEM A:
	!	Geração de f(x) e g(x)
	!==========================================================

	deltaX = 1.d0/10000.d0
	f_x = 0.d0
	g_x = 0.d0
	r = 3.2d0

	do i = 1 , 10000 , 1
	
		write(21,*)x, f_x, g_x
		f_x = x
		g_x = calculaG(calculaG(x,r),r)
		x = x + deltaX
	
	end do
	
	
	!==========================================================
	!	ITEM A:
	!	Aplicação do mapa
	!==========================================================


	x_old = 6.d0/10.d0
	
	do i = 1 , 10000 , 1
		
		write(25,*)x_old
		
		x_old = calculaG(x_old,r)
	
	end do

	!==========================================================
	!	ITEM B:
	!	Diagrama de bifurcação
	!==========================================================

	! Esta seção do exercício foi colocada em comentários por ser muito custosa!
	!do i = 1 , 2500 , 1
	!
	!	x_old = 6.d0/10.d0
	!	r = 2.d0 + 2.d0*dble(i)/2500.d0
	!	
	!	do j = 1 , 100 , 1
	!	
	!		x_old = calculaG(x_old,r)
	!	
	!	end do
	!	
	!	do j = 1 , 500 , 1
	!	
	!		x_old = calculaG(x_old,r)
	!		write(26,*)r,x_old
	!	
	!	end do
	!	
	!
	!
	!end do

	!==========================================================
	!	ITEM D:
	!	Aplicação do mapa
	!==========================================================

	r = 3.8d0

	do j = 1 , 4 , 1
	
	x_old = dble(j)/6.d0
	
		do i = 1 , 10000 , 1
		
			write(27+j,*)x_old
		
			x_old = calculaG(x_old,r)
	
		end do
	end do

	!==========================================================
	!	ITEM D:
	!	Distância
	!==========================================================

	coef_e = 1E-8
	
	do i = 1 , 4 , 1
		
		x_old = dble(i)/6.d0
		x_old_e = x_old + coef_e
		
		do j = 1 , 50 , 1
		
			G1 = calculaG(x_old_e,r)
			G2 = calculaG(x_old,r)
		
			diferenca = G1 - G2
		
			dist_i = dabs(diferenca)
			write(40+i,*)dlog(dist_i)
		
			x_old_e = G1
			x_old = G2
		
			if (i.EQ.1) then
				write(40,*)j
		
			end if
		
		end do
		
		close(40+i)
	end do
	close(40)


end program dobras_periodo_caos

real*8	function calculaG(x,r)
	
	real*8 x,r
	
	calculaG = r*x*(1.d0-x)

return
end