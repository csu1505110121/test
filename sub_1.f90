	program sub
    implicit none
	integer::i,j,k1,k2,k3,l,m,n,ierror_1,ierror_2,ierror_3,ierror_4,ierror_5
	integer::ierror_6,ierror_7,ierror_8,ierror_9,ierror_10,ierror_11
	integer,allocatable,dimension(:)::resid,id
	character(len=3),allocatable,dimension(:)::pdb_symbol
	character(len=100)::line,line_1,line_2,line_3,line_4
	real,allocatable,dimension(:)::charge
	character,allocatable,dimension(:)::symbol
	integer,dimension(9221)::psf_resid,psf_id
	integer::a,b,c
	real,dimension(9221)::psf_charge
	character(len=3),dimension(9221)::psf_symbol,psf_type
	character(len=55),dimension(9221)::head
	character(len=26),dimension(9221)::tail
	open(unit=1,file='gaussian_cal/dyn_1.log',status='old',iostat=ierror_1)
	open(unit=2,file='pdb/dyn_1.pdb',status='old',iostat=ierror_2)
	j=0
	do
		read(2,'(A)',iostat=ierror_3) line_1
		if (ierror_3/=0) exit
		j=j+1
	end do
	j=j-2
	close(2)
	!write(*,*) j
	allocate(charge(j))
	allocate(symbol(j))
	allocate(resid(j))
	allocate(id(j))
	allocate(pdb_symbol(j))
	do
		read(1,'(A)',iostat=ierror_4) line
		k1=index(line,'ESP charges:')
		if (ierror_4/=0) exit
		if (k1/=0) then
			read(1,'(A)')
			do i=1,j
				read(1,100) symbol(i),charge(i)
				100 format(T9,A,T13,F9.6)
			end do
		end if
	end do
	!do l=1,j
	!	write(*,100) symbol(l),charge(l)
	!end do
	open(unit=3,file='pdb/dyn_1.pdb',status='old',iostat=ierror_6)
	m=1
	do
		read(3,'(A)',iostat=ierror_5) line_2
		k2=index(line_2,'ATOM')
		if (ierror_5/=0) exit
		if (k2/=0) then
			read(line_2,200) id(m),resid(m),pdb_symbol(m)
			200 format(T8,I4,T23,I4,T14,A)
			m=m+1
		end if
	end do


	!test the data i extract all right
	!do n=1,j
	!	write(*,*) id(n),symbol(n),resid(n),charge(n),pdb_symbol(n)
	!end do



	open(unit=4,file='structure/AMPB_Wat_Box23_1.psf',status='old',iostat=ierror_10)
	open(unit=5,file='structure/AMPB_Wat_Box23_2.psf',iostat=ierror_8)
	a=1
	b=1
	!do
	!	read(4,'(A)',iostat=ierror_11) line_4
	!	if (ierror_11/=0) exit
	!	a=a+1
	!end do
	!write(*,*) a
	!rewind(4)
	do
		read(4,'(A)',iostat=ierror_9) line_3
		if (ierror_9/=0) exit
		write(5,'(A)') line_3
		k3=index(line_3,'NATOM')
		if (k3/=0) then
			do
				if (b>=9222) exit
				read(4,300) psf_id(b),psf_type(b),psf_resid(b),psf_symbol(b),psf_charge(b),head(b),tail(b)
				300 format(T7,I4,T12,A,T21,I4,T39,A,T56,F9.6,T1,A,T65,A)
				!write(*,300)psf_id(a),psf_resid(a),psf_symbol(a),psf_charge(a)
				!write(*,*) head(a),tail(a)
				!if ) then
				!	write(5,400) head(a),charge(a),tail(a)
					400 format(A,F9.6,A)
				!else
				do n=1,j
					if (psf_id(b)==id(n) .and. psf_resid(b)==resid(b) &
						.and. psf_symbol(b)==pdb_symbol(n)) then
						write(5,400) head(b),charge(n),tail(b)
						goto 1000
					else if (psf_type(b)=='WT1'.and.psf_resid(b)==resid(n).and.psf_symbol(b)==pdb_symbol(n)) then
						write(5,400) head(b),charge(n),tail(b)
						goto 1000
					!else if (psf_resid(a)/=resid(n)) then
					!	write(5,400) head(a),psf_charge(a),tail(a)
					!else if (psf_type(a)=='CLA'.and.) then
					!	write(5,400) head(a),psf_charge(a),tail(a)
					end if
				end do
				write(5,400) head(b),psf_charge(b),tail(b)
			1000 b=b+1
			end do  
		end if          
		a=a+1
	end do
	!do c=1,9221
	!	write(*,*) psf_resid(c),psf_symbol(c),psf_charge(c),head(c),tail(c)
	!end do
	
	stop
	end program sub
