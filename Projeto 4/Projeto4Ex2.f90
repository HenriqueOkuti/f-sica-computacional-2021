program satelite_lua

! Projeto 4 - Leis de Kepler e o problema de três corpos
! Nome: Henrique Krastins Okuti
! Contato: henrique.okuti@usp.br

	implicit none

! Declaração de variáveis

	real*8 x_lua, y_lua, r_lua, periodo_lua
	real*8 x_new, y_new, x_old, y_old, r_sat, periodo_satelite, r_sat_new
	real*8 v_x_old, v_x_new, v_y_old, v_y_new
	real*8 k_1_vx, k_2_vx, k_3_vx, k_4_vx, k_1_vy, k_2_vy, k_3_vy, k_4_vy
	real*8 k_1_x, k_2_x, k_3_x, k_4_x, k_1_y, k_2_y, k_3_y, k_4_y
	real*8 r3_satlua, r_sat_lua, x_old_SL, y_old_SL
	real*8 soma_k_vx, soma_k_vy, soma_k_x, soma_k_y
	real*8 cte_G, massaTerra, G_MT, G_ML, pi, r3_sat
	real*8 tempo, deltaT, dias_prazo
	real*8 x_old_geo, y_old_geo, phi_geo, phi, delta_phi, r_geo, delta_r, r_old, lixo
	integer*8	i, n, pontos

	pi = 4.d0*atan(1.d0)
	massaTerra = 5.9739E24
	cte_G = 6.6743E-11
	G_MT = cte_G*massaTerra
	G_ML = (1.32E-2)*G_MT

	periodo_satelite = 86400.d0		! Em segundos
	r_sat = 42244.d0
	r_lua = 384400.d0
	periodo_lua = 27.32d0*periodo_satelite		! Em segundos
	
	periodo_satelite = periodo_satelite / (60.d0*60.d0*24.d0)  ! Em dias
	periodo_lua = 27.32d0*periodo_satelite		! Em dias

	r3_sat = r_sat**(3.d0)

! =======================================
!		EXERCICIO A	"CORPOS ISOLADOS"
! =======================================

	tempo = 0.d0
	deltaT = 1.d0/1440.d0	! Irá incrementar de 1 em 1 hora
	
	! Dados iniciais do satelite
	x_old = r_sat
	y_old = 0.d0
	v_x_old = 0.d0
	!v_y_old = 2.d0*pi*r_sat/periodo_satelite
	v_y_old = dsqrt(G_MT/r_sat)
	
	do i = 1 , 144000 , 1	! Total de pontos no intervalo T = 0 até T = 100 dias 
		
		x_lua = r_lua*dcos(2.d0*pi*tempo/periodo_lua)
		y_lua = r_lua*dsin(2.d0*pi*tempo/periodo_lua)

		! RK4 - CALCULO COEFICIENTES P/ X:
		
		k_1_vx = (-1.d0)*deltaT*(G_MT/r3_sat)*x_old
		k_1_x = deltaT*v_x_old
		
		k_2_vx = (-1.d0)*deltaT*(G_MT/r3_sat)*(x_old + k_1_x/2.d0)
		k_2_x = deltaT*(v_x_old + k_1_vx/2.d0)
	
		k_3_vx = (-1.d0)*deltaT*(G_MT/r3_sat)*(x_old + k_2_x/2.d0)
		k_3_x = deltaT*(v_x_old + k_2_vx/2.d0)
		
		k_4_vx = (-1.d0)*deltaT*(G_MT/r3_sat)*(x_old + k_3_x)
		k_4_x = deltaT*(v_x_old + k_3_vx)
		
		!APLICAÇÃO DO RK4 EM X:
		
		soma_k_x = (1.d0/6.d0)*(k_1_x + 2.d0*k_2_x + 2.d0*k_3_x + k_4_x)
		x_new = x_old + soma_k_x
		
		! RK4 - CALCULO COEFICIENTES P/ Y:
		
		k_1_vy = (-1.d0)*deltaT*(G_MT/r3_sat)*y_old
		k_1_y = deltaT*v_y_old
		
		k_2_vy = (-1.d0)*deltaT*(G_MT/r3_sat)*(y_old + k_1_y/2.d0)
		k_2_y = deltaT*(v_y_old + k_1_vy/2.d0)
		
		k_3_vy = (-1.d0)*deltaT*(G_MT/r3_sat)*(y_old + k_2_y/2.d0)
		k_3_y = deltaT*(v_y_old + k_2_vy/2.d0)
		
		k_4_vy = (-1.d0)*deltaT*(G_MT/r3_sat)*(y_old + k_3_y)
		k_4_y = deltaT*(v_y_old + k_3_vy)

		!APLICAÇÃO DO RK4 EM Y:
		
		soma_k_y = (1.d0/6.d0)*(k_1_y + 2.d0*k_2_y + 2.d0*k_3_y + k_4_y)
		y_new = y_old + soma_k_y
		
		!APLICAÇÃO DO RK4 EM V_X:
		
		soma_k_vx = (1.d0/6.d0)*(k_1_vx + 2.d0*k_2_vx + 2.d0*k_3_vx + k_4_vx)
		v_x_new = v_x_old + soma_k_vx

		!APLICAÇÃO DO RK4 EM V_Y:
		
		soma_k_vy = (1.d0/6.d0)*(k_1_vy + 2.d0*k_2_vy + 2.d0*k_3_vy + k_4_vy)		
		v_y_new = v_y_old + soma_k_vy
		r_sat_new = dsqrt(x_new**(2.d0) + y_new**(2.d0))
		r3_sat = r_sat_new**(3.d0)
		write(20,*)x_lua,y_lua, i, tempo		! Dados LUA
		write(21,*)x_old,y_old, tempo			! Dados SATELITE (Posição)
		write(22,*)v_x_old,v_y_old, tempo		! Dados SATELITE (Velocidade)
		write(23,*)r_sat, r_sat_new				! Dados SATELITE (Vetor R)
		x_old = x_new
		y_old = y_new
		v_x_old = v_x_new
		v_y_old = v_y_new
		tempo = tempo + deltaT	
	end do

	close(20)
	close(21)
	close(22)
	close(23)

! =======================================
!		EXERCICIO A	"CORPOS INTERAGINDO"
! =======================================

	tempo = 0.d0
	deltaT = 1.d0/1440.d0	! Irá incrementar de 1 em 1 hora
	
	!G_ML = G_ML*100.d0			! Para ver uma perturbação perceptível
	
	! Dados iniciais do satelite
	x_old = r_sat
	y_old = 0.d0
	
	v_x_old = 0.d0
	v_y_old = dsqrt(G_MT/r_sat)
	
	r_sat_lua = dsqrt((r_sat - r_lua)**(2.d0))
	r3_satlua = r_sat_lua**(3.d0)
	
	do i = 1 , 144000 , 1	! Total de pontos no intervalo T = 0 até T = 100 dias 
		
		x_lua = r_lua*dcos(2.d0*pi*tempo/periodo_lua)
		y_lua = r_lua*dsin(2.d0*pi*tempo/periodo_lua)
		
		x_old_SL = (x_old - x_lua)
		y_old_SL = (y_old - y_lua)

		r_sat_lua = dsqrt((x_old - x_lua)**(2.d0) + (y_old - y_lua)**(2.d0))
		r3_satlua = r_sat_lua**(3.d0)

		! RK4 - CALCULO COEFICIENTES P/ X:
		
		k_1_vx = (-1.d0)*deltaT*(((G_MT/r3_sat)*x_old) + ((G_ML/r3_satlua)*x_old_SL))
		k_1_x = deltaT*v_x_old
		
		k_2_vx = (-1.d0)*deltaT*(((G_MT/r3_sat)*(x_old + k_1_x/2.d0) + (G_ML/r3_satlua)*(x_old_SL + k_1_x/2.d0)))
		k_2_x = deltaT*(v_x_old + k_1_vx/2.d0)
	
		k_3_vx = (-1.d0)*deltaT*(((G_MT/r3_sat)*(x_old + k_2_x/2.d0) + (G_ML/r3_satlua)*(x_old_SL + k_2_x/2.d0)))
		k_3_x = deltaT*(v_x_old + k_2_vx/2.d0)
		
		k_4_vx = (-1.d0)*deltaT*(((G_MT/r3_sat)*(x_old + k_3_x) + (G_ML/r3_satlua)*(x_old_SL + k_3_x)))
		k_4_x = deltaT*(v_x_old + k_3_vx)
		
		!APLICAÇÃO DO RK4 EM X:
		
		soma_k_x = (1.d0/6.d0)*(k_1_x + 2.d0*k_2_x + 2.d0*k_3_x + k_4_x)
		x_new = x_old + soma_k_x
		
		! RK4 - CALCULO COEFICIENTES P/ Y:
		
		k_1_vy = (-1.d0)*deltaT*((G_MT/r3_sat)*y_old + (G_ML/r3_satlua)*y_old_SL)
		k_1_y = deltaT*v_y_old
		
		k_2_vy = (-1.d0)*deltaT*((G_MT/r3_sat)*(y_old + k_1_y/2.d0) + (G_ML/r3_satlua)*(y_old_SL + k_1_y/2.d0))
		k_2_y = deltaT*(v_y_old + k_1_vy/2.d0)
	
		k_3_vy = (-1.d0)*deltaT*((G_MT/r3_sat)*(y_old + k_2_y/2.d0) + (G_ML/r3_satlua)*(y_old_SL + k_2_y/2.d0))
		k_3_y = deltaT*(v_y_old + k_2_vy/2.d0)
		
		k_4_vy = (-1.d0)*deltaT*((G_MT/r3_sat)*(y_old + k_3_y) + (G_ML/r3_satlua)*(y_old_SL + k_3_y))
		k_4_y = deltaT*(v_y_old + k_3_vy)

		!APLICAÇÃO DO RK4 EM Y:
		
		soma_k_y = (1.d0/6.d0)*(k_1_y + 2.d0*k_2_y + 2.d0*k_3_y + k_4_y)
		y_new = y_old + soma_k_y
		
		!APLICAÇÃO DO RK4 EM V_X:
		
		soma_k_vx = (1.d0/6.d0)*(k_1_vx + 2.d0*k_2_vx + 2.d0*k_3_vx + k_4_vx)
		v_x_new = v_x_old + soma_k_vx

		!APLICAÇÃO DO RK4 EM V_Y:
		
		soma_k_vy = (1.d0/6.d0)*(k_1_vy + 2.d0*k_2_vy + 2.d0*k_3_vy + k_4_vy)		
		v_y_new = v_y_old + soma_k_vy
		r_sat_new = dsqrt(x_new**(2.d0) + y_new**(2.d0))
		r3_sat = r_sat_new**(3.d0)
		
		write(25,*)x_lua, y_lua, i, tempo		! Dados LUA
		write(26,*)x_old, y_old, tempo			! Dados SATELITE (Posição)
		write(27,*)v_x_old, v_y_old, tempo		! Dados SATELITE (Velocidade)
		write(28,*)r_sat, r_sat_new, r_sat_lua	! Dados SATELITE (Vetor R)
		
		x_old = x_new
		y_old = y_new
		v_x_old = v_x_new
		v_y_old = v_y_new
		
		tempo = tempo + deltaT	
	end do

	close(25)
	close(26)
	close(27)
	close(28)
	

! =======================================
!		EXERCICIO B	"DESVIO DA ÓRBITA"
! =======================================

	! Basta ler os arquivos:
	! fort.21 -> x_old_geo, y_old_geo, tempo
	! fort.26 -> x_old, y_old
	! fort.23 -> lixo, r_geo
	! fort.28 -> lixo, r_old

	open(91,file = 'fort.21')
	open(92,file = 'fort.26')
	open(93,file = 'fort.23')
	open(94,file = 'fort.28')

	do i = 1 , 144000, 1
		read(91,*)x_old_geo, y_old_geo, tempo
		read(92,*)x_old, y_old
		read(93,*)lixo, r_geo
		read(94,*)lixo, r_old
		
		phi = datan2(y_old,dabs(x_old))
		phi_geo = datan2(y_old_geo,dabs(x_old_geo))
		delta_phi = phi - phi_geo
		
		delta_r = r_old - r_geo
	
		write(29,*)phi, phi_geo, dmod(delta_phi, 2.d0*pi), r_old, r_geo, delta_r, tempo
	
	end do

	close(91)
	close(92)
	close(93)
	close(94)

end program satelite_lua