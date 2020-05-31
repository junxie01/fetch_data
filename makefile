FC=gfortran
fflag=-ffixed-line-length-none
subjects1=fetch_data.o
subjects2=fetch_eq.o 
subjects3=fetch_seed.o 
subjects4=gen_eq_mail.o
subjects5=fetch_data_specific_day.o
all:../bin/fetch_data ../bin/fetch_eq ../bin/fetch_seed ../bin/gen_eq_mail  ../bin/fetch_data_specific_day
../bin/fetch_data:$(subjects1)
	$(FC) $^ -o $@ $(fflag)
../bin/fetch_eq:$(subjects2)
	$(FC) $^ -o $@ $(fflag)
../bin/fetch_seed:$(subjects3)
	$(FC) $^ -o $@ $(fflag)
../bin/gen_eq_mail:$(subjects4)
	$(FC) $^ -o $@ $(fflag)
../bin/fetch_data_specific_day:$(subjects5)
	$(FC) $^ -o $@ $(fflag)
%.o:%.f90
	$(FC) $^ -c $(fflag)
clean:
	rm *.o
