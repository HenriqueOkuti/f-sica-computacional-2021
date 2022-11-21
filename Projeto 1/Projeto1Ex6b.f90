program numerosaleatorios

! Projeto 1 - Introdução à Programação
! Nome: Henrique Krastins Okuti
! Contato: henrique.okuti@usp.br

	implicit none

	real*8 r,aux,x,y,xiquad,yiquad,somaquad,pi,piesperado,precisao,precisaodesejada
	integer i,total,ndentro,ntotal

	total = 1
	piesperado = 4*atan(1.d0)
	pi = 0
	ndentro = 0
	ntotal = 0
	precisaodesejada = piesperado - 3.1415
	precisao = piesperado - pi


	do while (precisao.GE.precisaodesejada)
		i = 0
			if (total.GE.0) then
				do while (i.NE.total)
					call Random_Number(x)
					call Random_Number(y)
					xiquad = x**2
					yiquad = y**2
					somaquad = xiquad + yiquad			
					write(52,*)i,x,y,xiquad,yiquad,somaquad
					if (somaquad.LE.1) then			
						ndentro = ndentro + 1
						ntotal = ntotal + 1				
					else		
						ntotal = ntotal + 1
					end if			
					i=i+1			
				enddo			
			end if
		pi = 4.d0 * ((ndentro*1.d0)/(ntotal*1.d0))
		write(53,*)i, ndentro, ntotal, pi
		write(54,*)i, abs(dlog10(abs(((piesperado - pi)/piesperado))))

	total = total + 1
	precisao = ABS((piesperado-pi)/piesperado)

	enddo

	end program numerosaleatorios