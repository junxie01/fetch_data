FC=gfortran
fflag=-ffixed-line-length-none
subjects1=fetch_data.o
subjects2=fetch_eq.o 
subjects3=fetch_seed.o 
subjects4=gen_eq_mail.o
all:fetch_data fetch_eq fetch_seed gen_eq_mail
fetch_data:$(subjects1)
	$(FC) $^ -o $@ $(fflag)
fetch_eq:$(subjects2)
	$(FC) $^ -o $@ $(fflag)
fetch_seed:$(subjects3)
	$(FC) $^ -o $@ $(fflag)
gen_eq_mail:$(subjects4)
	$(FC) $^ -o $@ $(fflag)
%.o:%.f90
	$(FC) $^ -c $(fflag)
clean:
	rm *.o
install:
	cp fetch_data fetch_eq fetch_seed gen_eq_mail ../bin
