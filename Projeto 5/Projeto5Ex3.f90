program modelo_predador_presa

! Projeto 5 - Dinâmica populacional
! Nome: Henrique Krastins Okuti
! Contato: henrique.okuti@usp.br

	implicit none

	! Declaração de variáveis

	real*8	a, b, c, d, x, y
	real*8	derivadaX, derivadaY, derivadaXe, derivadaYe
	real*8	v_x(4), v_y(4), deltaT, tempo
	integer*8 i


	!==========================================================
	!	ITEM B/C/D:
	!	Implementação do RK4
	!==========================================================
	
	! Condições iniciais
	deltaT = 0.002d0
	tempo = 0.d0
	x = 1.d0	! Valores utilizados: {(10,0),(0,10),(10,10),(5,10),(10,5),(10,1),(100,1)}
	y = 0.5d0	!					  {(5,5),(4,4),(3,3),(2,2),(1,1),(0.5,0.5),(0.1,0.1),(0.01,0.01)}
	
	do i = 1 , 50000 , 1
	
	write(501,*)tempo,x,y
	
	
	v_x(1) = derivadaX(x,y)
	v_y(1) = derivadaY(x,y)
	
	v_x(2) = derivadaX(x + 0.5d0*deltaT*v_x(1),y + 0.5d0*deltaT*v_y(1))
	v_y(2) = derivadaY(x + 0.5d0*deltaT*v_x(1),y + 0.5d0*deltaT*v_y(1))
	
	v_x(3) = derivadaX(x + 0.5d0*deltaT*v_x(2),y + 0.5d0*deltaT*v_y(2))
	v_y(3) = derivadaY(x + 0.5d0*deltaT*v_x(2),y + 0.5d0*deltaT*v_y(2))
	
	v_x(4) = derivadaX(x + deltaT*v_x(3),y + deltaT*v_y(3))
	v_y(4) = derivadaY(x + deltaT*v_x(3),y + deltaT*v_y(3))
	
	x = x + (deltaT/6.d0)*(v_x(1) + 2.d0*v_x(2) + 2.d0*v_x(3) + v_x(4))
	y = y + (deltaT/6.d0)*(v_y(1) + 2.d0*v_y(2) + 2.d0*v_y(3) + v_y(4))
	
	tempo = tempo + deltaT
	
	end do
	
	!==========================================================
	!	ITEM E:
	!	Modelo predador-presa
	!==========================================================
	
	! Condições iniciais
	deltaT = 0.0001d0
	tempo = 1900.d0
	x = 30.d0
	y = 4.d0
	
	do i = 1 , 200000 , 1	! Caso eu mude, utilizei: do i = 1 , 20000 , 1
	
	write(502,*)tempo,x,y
	
	v_x(1) = derivadaXe(x,y)
	v_y(1) = derivadaYe(x,y)
	
	v_x(2) = derivadaXe(x + 0.5d0*deltaT*v_x(1),y + 0.5d0*deltaT*v_y(1))
	v_y(2) = derivadaYe(x + 0.5d0*deltaT*v_x(1),y + 0.5d0*deltaT*v_y(1))
	
	v_x(3) = derivadaXe(x + 0.5d0*deltaT*v_x(2),y + 0.5d0*deltaT*v_y(2))
	v_y(3) = derivadaYe(x + 0.5d0*deltaT*v_x(2),y + 0.5d0*deltaT*v_y(2))
	
	v_x(4) = derivadaXe(x + deltaT*v_x(3),y + deltaT*v_y(3))
	v_y(4) = derivadaYe(x + deltaT*v_x(3),y + deltaT*v_y(3))
	
	x = x + (deltaT/6.d0)*(v_x(1) + 2.d0*v_x(2) + 2.d0*v_x(3) + v_x(4))
	y = y + (deltaT/6.d0)*(v_y(1) + 2.d0*v_y(2) + 2.d0*v_y(3) + v_y(4))
	
	tempo = tempo + deltaT
	
	end do	
	
	

end program modelo_predador_presa


real*8	function derivadaX(x,y)
	
	real*8 x,y,a,b
	a = 2.d0/3.d0
	b = 4.d0/3.d0
	derivadaX = (a*x) - (b*x*y)

return
end

real*8	function derivadaY(x,y)
	
	real*8 x,y,c,d
	c = 1.d0
	d = c
	derivadaY = ((-1.d0)*c*y) + (d*x*y)

return
end

real*8	function derivadaXe(x,y)
	
	real*8 x,y,a,b
	a = 0.481d0
	b = 0.025d0
	derivadaXe = (a*x) - (b*x*y)

return
end

real*8	function derivadaYe(x,y)
	
	real*8 x,y,c,d
	c = 0.927d0
	d = 0.028d0
	derivadaYe = ((-1.d0)*c*y) + (d*x*y)

return
end