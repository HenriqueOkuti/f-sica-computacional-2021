program ponto_fixo_periodo_um

! Projeto 5 - Dinâmica populacional
! Nome: Henrique Krastins Okuti
! Contato: henrique.okuti@usp.br

	implicit none

!	x_(i+1) = r * x_i * (1 - x_i)	<- [4]
!	G(x_i)  = r * r_i * (1 - x_i)
!	x_asterisco é a população máxima
!	
!	(a) Solução para r = {1 ; 2 ; 2.5} 
!		Grafique f(x) = x ; g(x) = r*x*(1-x)
!		Encontre a interseção
!		Lembrando: 0 <= x <= 1
!	(b) Implemente [4], grafique x_i por i para r = {1 ; 2 ; 2.5}
!		Compare com resultado de (a)
!		Use valores diferentes de x_0 (x_0 != 0)
!	(c) Defina a distância d_i = | G^{i} (x'_0) - G^{i} (x_0) |
!		x'_0 = x_0 + epsilon	
!

	! Declaração de variáveis

	real*8 f_x , g_x, x, deltaX, dist_i
	real*8 coef_r(3), r, coef_e, G1, G2, diferenca
	real*8 x_new, x_old, x_old_e, calculaG, derivadaG, somaG
	real*8 somax, somaxy1, somaxx, somay1, x1, y1, lyapunov
	real*8 total, delta, linear, angular(5), media , std_dev, somaquad
	integer*8 i, j, k

	! Definições de constantes utilizadas

	coef_r = (/ 1.d0 , 2.d0 , 2.5d0 /)
	deltaX = 1.d0/10000.d0
	coef_e = 0.001d0

	!==========================================================
	!	ITEM A:
	!	Geração de f(x) e g(x)
	!==========================================================

	do j = 1 , 3 , 1
	
	r = coef_r(j)

	f_x = 0.d0
	g_x = 0.d0
	x = 0.d0

		do i = 1 , 1 , 1
		
			write(50+j,*)x, f_x, g_x
	
			f_x = x
			g_x = r*x*(1.d0 - x)
			x = x + deltaX
			
	
		end do
		
	end do

	!==========================================================
	!	ITEM B:
	!	Mapa logistico
	!==========================================================

	do j = 1 , 3 , 1
	
	r = coef_r(j)

	x_old = 4.d0/6.d0

		do i = 1 , 1 , 1
		
			write(55+j,*)x_old
			
			x_new = x_old*r*(1.d0-x_old)
			x_old = x_new
	
		end do
		
	end do
	

	!==========================================================
	!	ITEM C:
	!	Distância
	!==========================================================

	
	r = coef_r(3)

	do i = 1 , 5 , 1
		
		x_old = dble(i)/6.d0
		x_old_e = x_old + coef_e
		
		do j = 1 , 50 , 1
		
			G1 = calculaG(x_old_e,r)
			G2 = calculaG(x_old,r)
		
			diferenca = G1 - G2
		
			dist_i = dabs(diferenca)
			write(60+i,*)dlog(dist_i)
		
			x_old_e = G1
			x_old = G2
		
			if (i.EQ.1) then
				write(60,*)j
		
			end if
		
		end do
		
		close(60+i)
	end do
	close(60)
	
	!==========================================================
	!	ITEM C:
	!	Expoente Lyapunov
	!==========================================================

	open(90,file = 'fort.60')
	open(91,file = 'fort.61')
	open(92,file = 'fort.62')
	open(93,file = 'fort.63')
	open(94,file = 'fort.64')
	open(95,file = 'fort.65')

	do j = 1 , 5 , 1
		somax = 0.d0
		somay1 = 0.d0
		somaxx = 0.d0
		somaxy1 = 0.d0
		
		if (j.EQ.1) then
			k = 49
		end if
		if (j.EQ.2) then
			k = 44
		end if	
		if (j.EQ.3) then
			k = 38
		end if		
		if (k.EQ.4) then
			k = 49
		end if	
		if (k.EQ.5) then
			k = 46
		end if		
		do i = 1 , k , 1
			x1 = dble(i)
			read(90+j,*)y1
			somax = somax + x1
			somay1 = somay1 + y1
			somaxx = somaxx + (x1*x1)
			somaxy1 = somaxy1 + (x1*y1)		
		end do
		total = dble(k)
		delta = (total*somaxx) - (somax*somax)
		angular(j) = ((total*somaxy1)-(somax*somay1))/delta
		linear = ((somaxx*somay1)-(somax*somaxy1))/delta
		!write(*,*)"Para x0 = ", dble(j)/6.d0
		!write(*,*)"Coeficiente angular: ",angular(j)
		!write(*,*)"Coeficiente linear: ",linear	
	end do
	media = 0.d0
	do i = 1 , 5 , 1
		media = angular(i) + media
	end do
	media = media/5.d0
	somaquad = 0.d0
	do i = 1 , 5 , 1 
		somaquad = (angular(i)-media)**(2.d0) + somaquad
		
	end do
		std_dev = dsqrt(somaquad/4.d0)
	
	write(*,*)"Expoente Lyapunov:",media
	write(*,*)"Erro:",std_dev
	
	!==========================================================
	!	ITEM D:
	!	Expoente Lyapunov
	!==========================================================
	
	r = coef_r(3)
	
	do i = 1 , 5 , 1
		
		x_old = dble(i)/6.d0
		somaG = 0.d0
		write(*,*)"X0 = ",x_old
		
		do j = 1 , 50 , 1
		
		x_old = calculaG(x_old,r)
		somaG = dlog(dabs(derivadaG(x_old,r))) + somaG
		
		
		end do
	
		lyapunov = somaG / 50.d0
		
		write(*,*)"Lyapunov = ",lyapunov
	
	end do
	
	
	
end program ponto_fixo_periodo_um

real*8	function calculaG(x,r)
	
	real*8 x,r
	
	calculaG = r*x*(1.d0-x)

return
end


real*8	function derivadaG(x,r)
	
	real*8 x,r
	
	derivadaG = r*(1.d0 - 2.d0*x)

return
end